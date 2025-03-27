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
                               most_recent_milestone,
                               org,
                               repo,
                               root_dir,
                               remote_name,
                               local_commit_log,
                               current_branch) {

  moduleServer(id, function(input, output, session) {
    # This section ensures that when an error occurs, the app stops
    # When an error occurs, the session ends. The other instance of this is when
    # the user clicks reset.
    # The logic here prevents the app from stopping when reset is clicked
    reset_triggered <- reactiveVal(FALSE)
    show_table <- reactiveVal(FALSE)
    cached_status <- reactiveVal(NULL)
    last_milestones <- reactiveVal(NULL)

    session$onSessionEnded(function() {
      if (!isTRUE(isolate(reset_triggered()))) {
        stopApp()
      }
    })

    ns <- session$ns

    observe({
      req(all_ghqc_milestone_names,
          most_recent_milestone,
          org,
          repo,
          root_dir,
          remote_name,
          local_commit_log,
          current_branch)

      waiter_hide()
    })

    w <- waiter::Waiter$new(
      id = ns("main_panel_dynamic"),
      html = tagList(waiter::spin_1(), h4("Generating table...")),
      color = "rgba(0,0,0,0.5)"
    )

    observeEvent(input$generate, {
      w$show()
      show_table(TRUE)

      shinyjs::delay(500, w$hide())
    })


    observeEvent(input$generate, {
      req(input$selected_milestones)

      if (!identical(input$selected_milestones, last_milestones())) {
        # if milestones changed, re-run ghqc_status
        status <- ghqc_status(
          milestone_names = input$selected_milestones,
          org,
          repo,
          root_dir,
          remote_name,
          local_commit_log,
          current_branch,
          include_non_issue_repo_files = FALSE
        )

        cached_status(status)
        last_milestones(input$selected_milestones)
      }

      show_table(TRUE)
    })

    filtered_data <- eventReactive(input$generate, {
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
                       selected = most_recent_milestone,
                       multiple = TRUE,
                       width = "100%",
                       options = list(placeholder = "(required)")
        ),

        selectInput(
          ns("diagnostics_filter"),
          "Diagnostics Filter",
          choices = c("All", "None", "Available"),
          selected = "All"
        ),

        # button to generate table
        actionButton(ns("generate"), "Generate Table", class = "btn-primary")
      )
    }) # output$sidebar

    output$main_panel_dynamic <- renderUI({
      req(show_table())

      tagList(
        DT::dataTableOutput(ns("status_table"))
      )
    }) #output$main_panel_dynamic

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
