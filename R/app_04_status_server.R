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
                               remote,
                               ahead_behind_status,
                               files_changed_in_remote_commits,
                               files_changed_in_unpushed_local_commits,
                               files_with_uncommitted_local_changes) {

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
          remote,
          ahead_behind_status,
          files_changed_in_remote_commits,
          files_changed_in_unpushed_local_commits,
          files_with_uncommitted_local_changes
          )
    })

    # makes sure the column headers re-align with the columns when sidebar is toggled in and out
    # observeEvent(input$toggle_sidebar, {
    #   show_table(FALSE)
    #
    #   shinyjs::delay(150, {
    #     session$onFlushed(function() {
    #       show_table(TRUE)
    #     }, once = TRUE)
    #   })
    # })

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
    ahead_behind_status_rv <- reactiveVal(ahead_behind_status)
    files_changed_in_remote_commits_rv <- reactiveVal(files_changed_in_remote_commits)
    files_changed_in_unpushed_local_commits_rv <- reactiveVal(files_changed_in_unpushed_local_commits)
    files_with_uncommitted_local_changes_rv <- reactiveVal(files_with_uncommitted_local_changes)

    # comment reactives
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

    selected_debounced <- selected_raw %>% debounce(2000)




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
            ahead_behind_status_rv(),
            files_changed_in_remote_commits_rv(),
            files_changed_in_unpushed_local_commits_rv(),
            files_with_uncommitted_local_changes_rv()
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
        df <- df[dirname(df$file_name) %in% input$file_directory_filter, ]
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
          files_with_issues <- df$file_name

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
        df <- df[dirname(df$file_name) %in% input$file_directory_filter, ]
      }

      df
    })

    # get a list of the directories of QC files
    file_directories <- reactive({
      debug(.le$logger, "file_directories() triggered")
      df <- base_file_list()
      files <- df$file_name

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

    observeEvent(input$show_modal_row, {
      row_index <- input$show_modal_row$row
      df <- filtered_data()
      req(nrow(df) >= row_index)

      path <- create_gfm_file(comment_body_string())
      html <- readLines(path, warn = FALSE) %>% paste(collapse = "\n")

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
      row_index <- input$show_modal_row$row
      df <- filtered_data()
      req(df)

      tryCatch({
        create_comment_body(
          owner = org,
          repo = repo,
          message = NULL,
          issue_number = df[row_index, ]$issue_number,
          diff = TRUE,
          comparator_commit = df[row_index, ]$comparator_commit,
          reference_commit = df[row_index, ]$latest_qc_commit,
          remote = remote # TODO
        )
      }, error = function(e) {
        rlang::abort(conditionMessage(e))
      })
    })

    post_comment <- reactive({
      req(post_trigger())
      req(comment_body_string())
      row_index <- input$show_modal_row$row
      df <- filtered_data()
      req(df)

      post_trigger(FALSE)

      w_pc <- create_waiter(ns, "Posting comment...")
      w_pc$show()
      on.exit(w_pc$hide())

      tryCatch(
        {
          post_resolve_comment(owner = org,
                               repo = repo,
                               issue_number = df[row_index, ]$issue_number,
                               body = comment_body_string())

          showModal(modalDialog(
            title = tags$div(
              actionButton(ns("dismiss_modal"), "Dismiss"),
              style = "text-align: right;"
            ),
            footer = NULL,
            easyClose = TRUE,
            tags$p("Current QC commit commented successfully."),
            tags$a(href = df[row_index, ]$issue_url, "Click here to visit the updated Issue on Github", target = "_blank")
          ))
        },
        error = function(e) {
          rlang::abort(conditionMessage(e))
        }
      )
    }) # post_comment

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

      # add comment button
      df$Comment <- sapply(1:nrow(df), function(i) {
        row <- df[i, ]

        if (row$Comment == TRUE) {
          button(ns)(i)
        }
        else {
          NA_character_
        }

      })

      # remove info columns
      df <- df[, !colnames(df) %in% c("milestone_name",
                                      "file_name",
                                      "issue_number",
                                      "latest_qc_commit",
                                      "comparator_commit",
                                      "issue_url")]


      # if only one milestone, don't need milestone column
      if (length(input$selected_milestones) == 1) {
        df <- df[, colnames(df) != "Milestone"]
      }

      if (!isTruthy(input$show_qcer)) {
        df <- df[, colnames(df) != "QCer", drop = FALSE]
      }

      pretty_table <- DT::datatable(
        df,
        escape = FALSE,
        selection = "none",
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
          #scrollY = "calc(100vh - 240px)",
          scrollCollapse = TRUE#,
          #destroy = TRUE#,
          # this forces the column headers to sync with the columns when the sidebar is closed
    #       drawCallback = DT::JS("
    #   function(settings) {
    #     var table = this.api();
    #
    #     function fixHeaderAlignment() {
    #       table.columns.adjust();
    #     }
    #
    #     setTimeout(fixHeaderAlignment, 300);
    #
    #     $(window).off('resize.dt').on('resize.dt', function() {
    #       fixHeaderAlignment();
    #     });
    #
    #     const sidebar = document.querySelector('[id$=\"-sidebar\"]');
    #     if (sidebar && typeof ResizeObserver !== 'undefined') {
    #       new ResizeObserver(() => {
    #         setTimeout(fixHeaderAlignment, 300);
    #       }).observe(sidebar);
    #     }
    #   }
    # ")
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
              "Local uncommitted file changes",
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
      debug(.le$logger, "Dismiss clicked - resetting app")
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

      # recompute reactiveVal variables
      current_branch_rv(gert::git_branch())
      local_commits_rv(get_local_commits())
      remote_commits_rv(get_remote_commits(remote$name, current_branch_rv()))
      ahead_behind_status_rv(check_ahead_behind())
      files_changed_in_remote_commits_rv(get_files_changed_in_remote_commits(remote_commits_rv(), ahead_behind_status_rv()))
      files_changed_in_unpushed_local_commits_rv(get_files_changed_in_unpushed_local_commits(local_commits_rv(), ahead_behind_status_rv()))
      files_with_uncommitted_local_changes_rv(get_files_with_uncommitted_local_changes())

      shinyjs::delay(300, {
        show_table(TRUE)
        run_generate(current_milestones)
      })
    }

    return(input)
  }) # moduleServer
} # ghqc_status_server
