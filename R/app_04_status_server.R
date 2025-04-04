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
                               root_dir,
                               local_commits,
                               remote_commits,
                               current_branch) {

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

    # make sure inputs are loaded
    observe({
      req(all_ghqc_milestone_names,
          default_milestones,
          org,
          repo,
          root_dir,
          local_commits,
          remote_commits,
          current_branch)
    })


    run_generate <- function(milestones = selected_milestones()) {
      req(milestones)
      current_milestones <- milestones
      cache <- status_cache()

      missing <- setdiff(current_milestones, names(cache))

      # if milestones cnot in cache, re-run ghqc_status
      if (length(missing) > 0) {
        w$show()
        for (milestone in missing) {
          debug(.le$logger, glue("Fetching milestone: {milestone}"))
          result <- ghqc_status(
            milestone_names = milestone,
            org,
            repo,
            root_dir,
            current_branch,
            local_commits,
            remote_commits,
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
        df <- df[df$`QC Status` %in% c("QC in progress", "QC complete"), ]
      }
      else if (input$qc_status_filter == "Needs attention") {
        df <- df[!df$`QC Status` %in% c("QC in progress", "QC complete"), ]
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

    # Reactive that includes all files regardless of directory filter
    base_file_list <- reactive({
      df <- cached_status()

      if (isTruthy(input$show_repo_files)) {
        milestones <- last_milestones()
        selected_dirs <- isolate(input$file_directory_filter)
        key <- repo_cache_key(milestones, selected_dirs)
        repo_cache <- non_qc_repo_cache()

        if (!key %in% names(repo_cache)) {
          relevant <- relevant_files()
          files_with_issues <- df$`File without url`

          repo_df <- create_non_issue_repo_files_df(
            files_with_issues = files_with_issues,
            local_commits = local_commits,
            remote_commits = remote_commits,
            root_dir = root_dir,
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
      df <- base_file_list()

      if (!is.null(input$file_directory_filter) && length(input$file_directory_filter) > 0) {
        df <- df[dirname(df$`File without url`) %in% input$file_directory_filter, ]
      }

      df
    })

    file_directories <- reactive({
      df <- base_file_list()

      files <- df$`File without url`

      if (is.null(files) || length(files) == 0 || !is.character(files)) return(character(0))

      unique(dirname(files))
    })

    observeEvent(file_directories(), {
      directories <- file_directories()

      shinyjs::delay(200, {
        updateSelectizeInput(
          session,
          inputId = "file_directory_filter",
          server = TRUE,
          choices = directories,
          selected = isolate(input$file_directory_filter),
          label = "File Directory Filter",
          options = list(
            placeholder = "(Optional)"
          )
        )
      })


    }) # file_directory_filter


    ############ OUTPUT

    observe({
      if (!is.null(input$file_directory_filter) && length(input$file_directory_filter) > 0) {
        shinyjs::show("show_repo_files_wrapper")
      } else {
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
              style = "transform: translateY(-1px);"
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
              "Show non-QC files",
              tags$input(
                id = ns("show_repo_files"),
                type = "checkbox",
                style = "transform: translateY(-1px);"
              )
            )
          )
        ) #

      ) # tagList
    }) # output$sidebar


    observeEvent(show_table(), {
      if (show_table()) {
        output$main_panel_dynamic <- renderUI({
          div(
            id = ns("main_panel_wrapper"),
            style = "flex-grow: 1; overflow: hidden;",
            DT::dataTableOutput(ns("status_table"))
          )
        })
      } else {
        output$main_panel_dynamic <- renderUI({ NULL })
      }
    })


    output$status_table <- DT::renderDataTable({
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

      pretty_table <- DT::datatable(
        df,
        escape = FALSE,
        rownames = FALSE,
        class = "stripe hover compact",
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
          destroy = TRUE
        ),
        callback = DT::JS("
  var table = this.api();

  function fixHeaderAlignment() {
    table.columns.adjust().draw(false);
  }

  // Redraw on init
  setTimeout(fixHeaderAlignment, 300);

  // Redraw on window resize
  $(window).off('resize.dt').on('resize.dt', function() {
    fixHeaderAlignment();
  });

  // Observe the sidebar div collapsing/expanding
  const sidebar = document.querySelector('[id$=\"-sidebar\"]');
  if (sidebar && typeof ResizeObserver !== 'undefined') {
    new ResizeObserver(() => {
      setTimeout(fixHeaderAlignment, 300);
    }).observe(sidebar);
  }
")

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
            c("QC in progress", "QC Complete", "Relevant file"),
            c("green", "green", "black"),
            default = "#a94442"
          )
        ) %>%
        # format Git Status column
        DT::formatStyle(
          "Git Status",
          color = DT::styleEqual(
            c("Up-to-date"),
            c("green"),
            default = "#a94442"
          )
        )

      pretty_table
    })

    observeEvent(input$close, {
      debug(.le$logger, glue::glue("App was closed through the close button."))
      stopApp()
    })



    observeEvent(input$reset, {
      debug(.le$logger, glue::glue("App was reset through the reset button."))

      current_milestones <- input$selected_milestones

      reset_triggered(TRUE)
      cached_status(NULL)
      last_milestones(NULL)
      status_cache(list())
      non_qc_repo_cache(list())

      show_table(FALSE)

      # keep the current milestones selected, but regenerate the statuses
      shinyjs::delay(100, {
        updateSelectizeInput(
          session,
          inputId = "selected_milestones",
          selected = current_milestones
        )
      })

      shinyjs::delay(300, {
        show_table(TRUE)
        run_generate(current_milestones)
      })
    }) # input$reset

    return(input)
  }) # moduleServer
} # ghqc_status_server
