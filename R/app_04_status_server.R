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
    reset_triggered <- reactiveVal(FALSE)
    show_table <- reactiveVal(FALSE)
    cached_status <- reactiveVal(NULL)
    last_milestones <- reactiveVal(NULL)

    w <- waiter::Waiter$new(
      id = ns("main_panel_dynamic"),
      html = tagList(waiter::spin_1(), h4("Generating table...")),
      color = "rgba(0,0,0,0.5)"
    )

    session$onSessionEnded(function() {
      if (!isTRUE(isolate(reset_triggered()))) {
        stopApp()
      }
    })

    selected_milestones <- reactive({
      input$selected_milestones
    }) %>% debounce(2000)

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

      # if milestones changed, re-run ghqc_status
      if (!identical(selected_milestones(), last_milestones())) {
        w$show()
        status <- ghqc_status(
          milestone_names = selected_milestones(),
          org,
          repo,
          root_dir,
          remote_name,
          local_commit_log,
          current_branch,
          include_non_issue_repo_files = FALSE
        )
        cached_status(status)
        last_milestones(selected_milestones())
        w$hide()
      }
      show_table(TRUE)
      waiter_hide()
    } # run_generate


    # observeEvent(input$generate, {
    #   run_generate()
    # })

    observeEvent(selected_milestones(), {
      if (is.null(cached_status())) {
        run_generate()
      }
    }, once = TRUE)

    observeEvent(selected_milestones(), {
      run_generate()
    })


    filtered_data <- reactive({
      req(show_table())
      req(cached_status())

      df <- cached_status()

      if (input$diagnostics_filter == "None") {
        df <- df[is.na(df$Diagnostics), ]
      } else if (input$diagnostics_filter == "Available") {
        df <- df[!is.na(df$Diagnostics), ]
      }

      df
    })



    ############ OUTPUT
    output$sidebar <- renderUI({
      tagList(
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
          ns("diagnostics_filter"),
          "Diagnostics Filter",
          choices = c("All", "None", "Available"),
          selected = "All"
        ),
      ) # tagList
    }) # output$sidebar

    observeEvent(show_table(), {
      if (show_table()) {
        output$main_panel_dynamic <- renderUI({
          DT::dataTableOutput(ns("status_table"))
        })
      } else {
        output$main_panel_dynamic <- renderUI({ NULL })
      }
    })


    output$status_table <- DT::renderDataTable({
      req(show_table())
      df <- filtered_data()

      # if only one milestone, don't need milestone column
      if (length(input$selected_milestones) == 1) {
        df <- df[, colnames(df) != "Milestone"]
      }

      # remove file without url column
      df <- df[, colnames(df) != "File without url"]

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
          columnDefs = list(
            list(visible = FALSE, targets = which(colnames(df) == "Diagnostics_Filter") - 1)
          )
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
            c("up-to-date"),
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
