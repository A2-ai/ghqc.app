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
                               remote_name,
                               local_commit_log,
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

    w <- waiter::Waiter$new(
      id = ns("main_panel_dynamic"),
      html = tagList(waiter::spin_1(), h4("Generating table...")),
      color = "rgba(0,0,0,0.5)"
    )

    # reactives
    reset_triggered <- reactiveVal(FALSE)
    show_table <- reactiveVal(FALSE)
    cached_status <- reactiveVal(NULL)
    last_milestones <- reactiveVal(NULL)
    status_cache <- reactiveVal(list())

    # cache previously rendered sets of milestones
    milestone_key <- function(milestones) {
      paste(sort(milestones), collapse = "|")
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

    # make sure inputs are loaded
    observe({
      req(all_ghqc_milestone_names,
          default_milestones,
          org,
          repo,
          root_dir,
          remote_name,
          local_commit_log,
          current_branch)
    })


    run_generate <- function() {
      req(selected_milestones())
      current_milestones <- selected_milestones()
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
            remote_name,
            local_commit_log,
            current_branch,
            include_non_issue_repo_files = FALSE
          )
          cache[[milestone]] <- result
        }

        status_cache(cache)
        w$hide()
      }

      combined <- do.call(rbind, cache[current_milestones])
      cached_status(combined)

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

      df <- cached_status()

      if (input$qc_status_filter == "On track ✅") {
        df <- df[df$`QC Status` %in% c("QC in progress", "QC complete"), ]
      } else if (input$qc_status_filter == "Needs attention ❌") {
        df <- df[!df$`QC Status` %in% c("QC in progress", "QC complete"), ]
      }

      df
    })



    ############ OUTPUT
    output$sidebar <- renderUI({
      tagList(
        tags$style(HTML("
        table.dataTable thead input {
          width: 100% !important;
          box-sizing: border-box;
        }
                        ")),

        selectizeInput(ns("selected_milestones"),
                       "Milestone",
                       choices = all_ghqc_milestone_names,
                       selected = c(default_milestones),
                       multiple = TRUE,
                       width = "100%",
                       options = list(placeholder = "(required)")
        ),

        # button to generate table
        #actionButton(ns("generate"), "Generate with Milestones", class = "btn-primary"),

        selectInput(
          ns("qc_status_filter"),
          "QC Status Filter",
          choices = c("All", "On track ✅", "Needs attention ❌"),
          selected = "All"
        )
      )


    }) # output$sidebar

    observeEvent(show_table(), {
      if (show_table()) {
        output$main_panel_dynamic <- renderUI({
          div(
            style = "height: calc(100vh - 180px); overflow-y: auto; padding: 10px;",
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
          dom = 'fit'
        )
      ) %>%
        # format Issue State column
        DT::formatStyle(
          "Issue State",
          color = DT::styleEqual(
            c("open", "closed"),
            c("#a94442", "green")
          )
        ) %>%
        # format QC Status column
        DT::formatStyle(
          "QC Status",
          color = DT::styleEqual(
            c("QC in progress", "QC Complete"),
            c("green", "green"),
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
      reset_triggered(TRUE)
      cached_status(NULL)
      last_milestones(NULL)
      session$reload()
    })

    return(input)
  }) # moduleServer
} # ghqc_status_server
