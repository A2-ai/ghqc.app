#' @import shiny
#' @importFrom shinyvalidate InputValidator sv_required
#' @importFrom glue glue
#' @importFrom log4r warn error info debug
#' @importFrom shinyjs enable disable addClass removeClass delay
#' @importFrom waiter Waiter spin_1 spin_2 waiter_hide
#' @importFrom gert git_status
#' @importFrom rprojroot find_rstudio_root_file
NULL

ghqc_status_server <- function(id,
                               all_ghqc_milestone_names,
                               default_milestones,
                               org,
                               repo,
                               local_commits,
                               remote_commits,
                               current_branch,
                               remote) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # This section ensures that when an error occurs, the app stops
    # When an error occurs, the session ends. The other instance of this is when
    # the user clicks reset.
    # The logic here prevents the app from stopping when reset is clicked
    session$onSessionEnded(function() {
      if (!isTRUE(isolate(reset_triggered()))) {
        stopApp()
      }
    })

    # make sure inputs are loaded
    observe({
      req(all_ghqc_milestone_names,
          default_milestones,
          org,
          repo,
          local_commits,
          remote_commits,
          current_branch,
          remote)
    })

    # makes sure the column headers re-align with the columns when sidebar is toggled in and out
    observeEvent(input$toggle_sidebar, {
      show_table(FALSE)

      shinyjs::delay(150, {
        session$onFlushed(function() {
          show_table(TRUE)
        }, once = TRUE)
      })
    })

    w <- waiter::Waiter$new(
      id = ns("main_container"),
      html = tagList(waiter::spin_1(), h4("Generating table...")),
      color = "rgba(0,0,0,0.5)"
    )

    # reactives
    reset_triggered <- reactiveVal(FALSE)
    show_table <- reactiveVal(FALSE)
    cached_status <- reactiveVal(NULL)
    last_milestones <- reactiveVal(NULL)
    status_cache <- reactiveVal(list())
    non_qc_repo_cache <- reactiveVal(list())
    prev_directories <- reactiveVal(NULL)

    # variable reactives
    current_branch_rv <- reactiveVal(current_branch)
    local_commits_rv <- reactiveVal(local_commits)
    remote_commits_rv <- reactiveVal(remote_commits)

    # comment reactives
    comment_details <- reactiveVal(NULL)
    post_trigger <- reactiveVal(FALSE)

    # cache previously rendered sets of milestones
    milestone_key <- function(milestones) {
      paste(sort(milestones), collapse = "|")
    }

    repo_cache_key <- function(milestones, directories) {
      paste(
        paste(sort(milestones), collapse = "|"),
        paste(sort(directories), collapse = "|"),
        sep = "::"
      )
    }

    # if there's no cache, wait 2 seconds to make sure user if actually done selecting multiple milestones
    selected_milestones <- reactive({
      current <- selected_raw()
      key <- milestone_key(current)

      if (key %in% names(status_cache())) {
        current
      } else {
        selected_debounced()
      }
    })

    selected_raw <- reactive({
      input$selected_milestones
    })

    selected_debounced <- selected_raw %>% debounce(1000)




    run_generate <- function(milestones = selected_milestones()) {
      req(milestones)
      current_milestones <- milestones
      cache <- status_cache()

      missing <- setdiff(current_milestones, names(cache))

      # if milestones cnot in cache, re-run ghqc_status
      if (length(missing) > 0) {
        w$show()
        for (milestone in missing) {
          debug(.le$logger, glue("Fetching statuses for uncached Milestone: {milestone}"))
          result <- ghqc_status(
            milestone_names = milestone,
            org,
            repo,
            current_branch_rv(),
            local_commits_rv(),
            remote_commits_rv(),
            include_non_issue_repo_files = FALSE
          )
          cache[[milestone]] <- list(
            status = result$status,
            relevant_files = result$relevant_files,
            repo_files = NULL
          )
        }

        status_cache(cache)
        w$hide()
      }

      combined_status <- do.call(rbind, lapply(cache[milestones], `[[`, "status"))
      cached_status(combined_status)

      last_milestones(current_milestones)
      show_table(TRUE)
      waiter_hide()
    } # run_generate

    # generate table with default milestones when app is first loaded
    observeEvent(selected_milestones(), {
      if (is.null(cached_status())) {
        run_generate()
      }
    }, once = TRUE)

    observeEvent(selected_milestones(), {
      run_generate()
    })

    # filter data based on filters in sidebar
    filtered_data <- reactive({
      req(show_table())
      req(cached_status())
      req(input$qc_status_filter)

      df <- combined_status_with_repo_files()

      # QC Status Filter
      if (input$qc_status_filter == "On track") {
        on_track <- c(
          "QC in progress",
          "QC complete"
        )
        df <- df[df$`QC Status` %in% on_track, ]
      }
      else if (input$qc_status_filter == "Needs attention") {
        needs_attention <- c(
          "Pull current QC commit",
          "Comment current QC commit",
          "Local uncommitted file changes after Issue closure",
          "Local unpushed commits with file changes after Issue closure",
          "Pushed file changes after Issue closure",
          "Uncommented pushed file changes before Issue closure"
          )

        df <- df[df$`QC Status` %in% needs_attention, ]

      }

      if (!is.null(input$file_directory_filter) && length(input$file_directory_filter) > 0) {
        df <- df[dirname(df$`File without url`) %in% input$file_directory_filter, ]
      }

      df
    })

    relevant_files <- reactive({
      req(last_milestones())
      cache <- status_cache()
      do.call(rbind, lapply(cache[last_milestones()], `[[`, "relevant_files"))
    })

    base_file_list <- reactive({
      df <- cached_status()

      if (isTruthy(input$show_repo_files)) {
        milestones <- last_milestones()
        selected_dirs <- input$file_directory_filter
        key <- repo_cache_key(milestones, selected_dirs)
        repo_cache <- non_qc_repo_cache()

        if (!key %in% names(repo_cache)) {
          relevant <- relevant_files()
          files_with_issues <- df$`File without url`

          repo_df <- create_non_issue_repo_files_df(
            files_with_issues = files_with_issues,
            local_commits = local_commits_rv(),
            remote_commits = remote_commits_rv(),
            all_relevant_files = relevant,
            selected_dirs = selected_dirs
          )

          repo_cache[[key]] <- repo_df
          non_qc_repo_cache(repo_cache)
        }

        df <- rbind(df, repo_cache[[key]])
      }

      df
    })

    combined_status_with_repo_files <- reactive({
      debug(.le$logger, "combined_status_with_repo_files() triggered")
      input$show_repo_files

      df <- base_file_list()

      if (!is.null(input$file_directory_filter) && length(input$file_directory_filter) > 0) {
        df <- df[dirname(df$`File without url`) %in% input$file_directory_filter, ]
      }

      df
    })

    # get a list of the directories of QC files
    file_directories <- reactive({
      debug(.le$logger, "file_directories() triggered")
      df <- base_file_list()
      files <- df$`File without url`

      if (is.null(files) || length(files) == 0 || !is.character(files)) {
        return(character(0))
      }

      unique(dirname(files))
    })

    observeEvent(file_directories(), {
      debug(.le$logger, "observeEvent(file_directories()) triggered")
      new_dirs <- file_directories()
      old_dirs <- prev_directories()

      if (!identical(sort(new_dirs), sort(old_dirs))) {
        prev_directories(new_dirs)

        shinyjs::delay(200, {
          updateSelectizeInput(
            session,
            inputId = "file_directory_filter",
            choices = new_dirs,
            selected = isolate(input$file_directory_filter),
            label = "File Directory Filter",
            options = list(
              placeholder = "(Optional)"
            )
          )
        })
      }
    }) # file_directory_filter

    button <- function(ns) {
      function(i) {
        row_id <- sprintf("row_%d", i)
        sprintf(
          '<button id="%s" type="button" class="btn btn-sm btn-primary"
        onclick="Shiny.setInputValue(\'%s\', {row: %d, nonce: Math.random()});">
        Comment
       </button>',
          ns(paste0("button_", row_id)),
          ns("show_modal_row"),
          i
        )
      }
    }


    ############ OUTPUT

    observe({ # only show "Show Non-QC Files" button when there are dirs selected in the file directory filter
      if (!is.null(input$file_directory_filter) && length(input$file_directory_filter) > 0) {
        shinyjs::show("show_repo_files_wrapper")
      }
      else {
        shinyjs::hide("show_repo_files_wrapper")
      }
    })

    output$sidebar <- renderUI({
      tagList(
        # Milestones
        selectizeInput(ns("selected_milestones"),
                       "Milestones",
                       choices = all_ghqc_milestone_names,
                       selected = c(default_milestones),
                       multiple = TRUE,
                       width = "100%",
                       options = list(placeholder = "(required)")
        ),
        # QC Status Filter
        selectInput(
          ns("qc_status_filter"),
          "QC Status Filter",
          choices = c("All", "On track", "Needs attention"),
          selected = "All",
          width = "100%"
        ),
        # Show QCer
        div(
          class = "form-group shiny-input-container",
          tags$label(
            style = "display: flex; align-items: center; justify-content: flex-start; gap: 8px; font-weight: 600; font-size: 13px; color: #333;",
            "Show QCer",
            tags$input(
              id = ns("show_qcer"),
              type = "checkbox",
              style = "transform: scale(1.2) translateY(-2px);"
            )
          )
        ),
        # File Directory Filter
        selectizeInput(
          ns("file_directory_filter"),
          label = NULL,
          choices = NULL,
          multiple = TRUE,
          width = "100%"
        ),
        # Show non-QC repo files
        div(
          id = ns("show_repo_files_wrapper"),
          style = "display: none;",
          div(
            class = "form-group shiny-input-container",
            tags$label(
              style = "display: flex; align-items: center; justify-content: flex-start; gap: 8px; font-weight: 600; font-size: 13px; color: #333;",
              tags$span("Show non-QC files"),
              tags$input(
                id = ns("show_repo_files"),
                type = "checkbox",
                class = "form-check-input",
                style = "transform: scale(1.2) translateY(-2px);"
              )
            )
          )
        ) # Show non-QC repo files
      ) # tagList
    }) # output$sidebar

    parse_current_qc_commit <- function(diagnostics_html) {
      lines <- stringr::str_split(diagnostics_html, "<li>")[[1]]
      current_qc_line <- lines[stringr::str_detect(lines, "Current QC commit:|Final QC commit:")]
      current_qc_commit <- stringr::str_match(current_qc_line, "blob/([a-f0-9]{7,40})")[, 2]
    }

    parse_issue_info <- function(html_string) {
      matches <- stringr::str_match(
        html_string,
        'href="(https://[^"]+/issues/(\\d+))"'
      )
      issue_url <- matches[2]
      issue_number <- matches[3]

      list(
        issue_url = issue_url,
        issue_number = issue_number
      )
    }

    generate_comment_html <- function(body) {
      path <- create_gfm_file(body)
      readLines(path, warn = FALSE) %>% paste(collapse = "\n")
    }

    determine_comparator_commit <- function(qc_status) {
      # if on QC branch, just return the latest remote commit on current branch
      # else if QC branch was merged, give the latest remote commit on merged-to branch (regardless if there have been file changes since merging)
      qc_branch_merged <- stringr::str_detect(qc_status, "^QC branch merged to")
      if (!qc_branch_merged) {
        remote_commits <- remote_commits_rv()
        return(remote_commits[1])
      }
      else {
        matches <- stringr::str_match(qc_status, "^QC branch merged to ([^/]+)/(.+)$")
        remote_name <- matches[2]
        branch <- matches[3]
        remote_commits <- get_remote_commits(remote_name, branch)
        return(remote_commits[1])
      }
    }

    observeEvent(input$show_modal_row, {
      row_index <- input$show_modal_row$row
      df <- filtered_data()
      req(nrow(df) >= row_index)

      current_qc_commit <- parse_current_qc_commit(df$Diagnostics[row_index])
      parsed_issue_info <- parse_issue_info(df$File[row_index])
      comparator_commit <- determine_comparator_commit(df$`QC Status`[row_index])

      comment_details(list(
        current_qc_commit = current_qc_commit,
        comparator_commit = comparator_commit,
        issue_number = parsed_issue_info$issue_number,
        issue_url = parsed_issue_info$issue_url
      ))

      html <- generate_comment_html(comment_body_string())

      showModal(modalDialog(
        title = tags$div(
          tags$span("Comment Preview", style = "float: left; font-weight: bold; font-size: 20px; margin-top: 5px;"),
          actionButton(ns("return"), "Cancel", style = "color: red;"),
          actionButton(ns("proceed_post"), "Post Comment"),
          style = "text-align: right;"
        ),
        footer = NULL,
        easyClose = TRUE,
        HTML(html)
      ))
    })



    comment_body_string <- reactive({
      details <- comment_details()
      req(details)

      tryCatch({
        create_comment_body(
          owner = org,
          repo = repo,
          message = input$message,
          issue_number = details$issue_number,
          diff = TRUE,
          comparator_commit = details$comparator_commit,
          reference_commit = details$current_qc_commit,
          remote = remote
        )
      }, error = function(e) {
        rlang::abort(conditionMessage(e))
      })
    })

    preview_comment <- reactive({
      req(preview_trigger())
      req(comment_body_string())
      preview_trigger(FALSE)


      html_file_path <- create_gfm_file(comment_body_string())
      custom_html <- readLines(html_file_path, warn = FALSE) %>% paste(collapse = "\n")

    })

    post_comment <- reactive({
      req(post_trigger())
      req(comment_body_string())
      post_trigger(FALSE)
      details <- comment_details()

      w_pc <- create_waiter(ns, "Posting comment...")
      w_pc$show()
      on.exit(w_pc$hide())

      tryCatch(
        {
          post_resolve_comment(owner = org,
                               repo = repo,
                               issue_number = details$issue_number,
                               body = comment_body_string())

          showModal(modalDialog(
            title = tags$div(
              actionButton(ns("dismiss_modal"), "Dismiss"),
              style = "text-align: right;"
            ),
            footer = NULL,
            easyClose = TRUE,
            tags$p("Current QC commit commented successfully."),
            tags$a(href = details$issue_url, "Click here to visit the updated Issue on Github", target = "_blank")
          ))
        },
        error = function(e) {
          rlang::abort(conditionMessage(e))
        }
      )
    })



    observeEvent(show_table(), {
      if (show_table()) {
        output$main_panel_dynamic <- renderUI({
          div(
            id = ns("main_panel_wrapper"),
            DT::dataTableOutput(ns("status_table"))
          )
        })
      }
      else {
        output$main_panel_dynamic <- renderUI({ NULL })
      }
    })




    output$status_table <- DT::renderDataTable({
      debug(.le$logger, "status_table re-rendered")
      req(show_table())
      df <- filtered_data()

      # milestone without url and  file without url columns
      df <- df[, !colnames(df) %in% c("Milestone without url", "File without url")]


      # if only one milestone, don't need milestone column
      if (length(input$selected_milestones) == 1) {
        df <- df[, colnames(df) != "Milestone"]
      }

      if (!isTruthy(input$show_qcer)) {
        df <- df[, colnames(df) != "QCer", drop = FALSE]
      }

      okay_to_comment_qc_statuses <- c("QC in progress",
                                       "Comment current QC commit",
                                       "QC complete",
                                       "Pushed file changes after Issue closure",
                                       "Uncommented pushed file changes before Issue closure"
                                       )


      comment <- sapply(1:nrow(df), function(i) {
        row <- df[i, ]
        qc_branch_merged <- stringr::str_detect(row$`QC Status`, "^QC branch merged to")

        is_an_issue <- row$`Issue State` %in% c("Open", "Closed")
        has_valid_git_status <- is.na(row$`Git Status`) || row$`Git Status` != "File does not exist locally"
        has_valid_qc_status <- row$`QC Status` %in% okay_to_comment_qc_statuses || qc_branch_merged

        if (is_an_issue && has_valid_git_status && has_valid_qc_status) {
          button(ns)(i)
        }
        else {
          NA_character_
        }

      })

      df <- cbind(df,
                    Comment = comment,
                    stringsAsFactors = FALSE)

      pretty_table <- DT::datatable(
        df,
        escape = FALSE,
        selection = 'single',
        rownames = FALSE,
        class = "stripe hover compact display",
        filter = 'top',
        options = list(
          pageLength = -1, # shows all the rows
          lengthChange = FALSE,
          paging = FALSE,
          searching = TRUE,
          info = TRUE,
          dom = 'fit',
          scrollY = "calc(100vh - 240px)",
          scrollCollapse = TRUE,
          destroy = TRUE,
          drawCallback = DT::JS("
      function(settings) {
        var table = this.api();

        function fixHeaderAlignment() {
          table.columns.adjust();
        }

        setTimeout(fixHeaderAlignment, 300);

        $(window).off('resize.dt').on('resize.dt', function() {
          fixHeaderAlignment();
        });

        const sidebar = document.querySelector('[id$=\"-sidebar\"]');
        if (sidebar && typeof ResizeObserver !== 'undefined') {
          new ResizeObserver(() => {
            setTimeout(fixHeaderAlignment, 300);
          }).observe(sidebar);
        }
      }
    ")
        )

      ) %>%
        # format Issue State column
        DT::formatStyle(
          "Issue State",
          color = DT::styleEqual(
            c("Open", "Closed"),
            c("#a94442", "green")
          )
        ) %>%
        # format QC Status column
        DT::formatStyle(
          "QC Status",
          color = DT::styleEqual(
            c("QC in progress",
              "QC complete",
              "Pull current QC commit",
              "Comment current QC commit",
              "Local uncommitted file changes after Issue closure",
              "Local unpushed commits with file changes after Issue closure",
              "Pushed file changes after Issue closure",
              "Uncommented pushed file changes before Issue closure"
              ),
            c("green", "green", "#a94442", "#a94442", "#a94442", "#a94442", "#a94442", "#a94442"),
            default = "black"
          )
        ) %>%
        # format Git Status column
        DT::formatStyle(
          "Git Status",
          color = DT::styleEqual(
            c("Up-to-date",
              "Remote file changes",
              "File does not exist locally",
              "Local uncommitted file changes ",
              "Local unpushed commits with file changes"
              ),
            c("green", "#a94442", "#a94442", "#a94442", "#a94442"),
            default = "black"
          )
        ) %>%
        DT::formatStyle(
          'File',
          `white-space` = 'normal',
          `word-wrap` = 'break-word',
          `max-width` = '200px'
        )

      pretty_table
    })

    observeEvent(input$close, {
      debug(.le$logger, glue::glue("App was closed through the close button."))
      stopApp()
    })


    observeEvent(input$proceed_post, {
      debug(.le$logger, glue::glue("post comment button proceeded and modal removed."))
      removeModal()
      post_trigger(TRUE)
    })

    observe({
      post_comment()
    })


    observeEvent(input$return, {
      debug(.le$logger, glue::glue("Comment button returned and modal removed."))
      removeModal()
    })

    observeEvent(input$dismiss_modal, {
      debug(.le$logger, "Dismiss clicked – resetting app")
      removeModal()
      reset_app()
    }, ignoreInit = TRUE)

    observeEvent(input$reset, {
      debug(.le$logger, glue::glue("App was reset through the reset button."))
      reset_app()
    }) # input$reset

    reset_app <- function() {
      debug(.le$logger, glue::glue("App was reset."))

      current_milestones <- input$selected_milestones

      reset_triggered(TRUE)
      cached_status(NULL)
      last_milestones(NULL)
      status_cache(list())
      non_qc_repo_cache(list())

      show_table(FALSE)

      shinyjs::delay(100, {
        updateSelectizeInput(
          session,
          inputId = "selected_milestones",
          selected = current_milestones
        )
      })

      current_branch_rv(gert::git_branch())
      local_commits_rv(get_local_commits())
      remote_commits_rv(get_remote_commits(remote$name, current_branch_rv()))

      shinyjs::delay(300, {
        show_table(TRUE)
        run_generate(current_milestones)
      })
    }

    return(input)
  }) # moduleServer
} # ghqc_status_server
