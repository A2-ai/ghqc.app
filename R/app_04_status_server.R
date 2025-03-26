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
                               default_statuses,
                               org,
                               repo,
                               root_dir,
                               token,
                               remote_name,
                               local_commit_log,
                               current_branch) {

  moduleServer(id, function(input, output, session) {
    # This section ensures that when an error occurs, the app stops
    # When an error occurs, the session ends. The other instance of this is when
    # the user clicks reset.
    # The logic here prevents the app from stopping when reset is clicked
    reset_triggered <- reactiveVal(FALSE)
    session$onSessionEnded(function() {
      if (!isTRUE(isolate(reset_triggered()))) {
        stopApp()
      }
    })

    ns <- session$ns

    observe({
      req(all_ghqc_milestone_names, default_statuses)
      waiter_hide()
    })

    status <- reactive({
      req(input$selected_milestones)
      ghqc_status(milestone_names = input$selected_milestones,
                  org,
                  repo,
                  root_dir,
                  token,
                  remote_name,
                  local_commit_log,
                  current_branch,
                  include_non_issue_repo_files = FALSE
                  )
    })

    output$sidebar <- renderUI({
      tagList(
        selectizeInput(ns("selected_milestones"),
                       "Milestone",
                       choices = all_ghqc_milestone_names,
                       multiple = TRUE,
                       width = "100%",
                       options = list(placeholder = "(required)")
        ),

      )
    }) # output$sidebar

    output$main_panel_dynamic <- renderUI({
      tagList(
        DT::dataTableOutput(ns("status_table"))
      )
    }) #output$main_panel_dynamic

    output$status_table <- DT::renderDataTable({
      df <- status()
      colnames(df) <- c("Milestone", "File", "URL", "Issue State", "QC Status", "Git Status", "QCer", "Diagnostics")

      DT::datatable(
        df,
        rownames = FALSE,
        class = "stripe hover compact",
        option = list(
          pageLength = -1, # show all rows
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
            default = "#a94442"  # everything else is red
          )
        ) %>%
        DT::formatStyle(
          "Git Status",
          color = DT::styleEqual(
            c("up-to-date"),
            c("green"),
            default = "#a94442"  # everything else is red
          )
        )

    })

    observeEvent(input$close, {
      debug(.le$logger, glue::glue("App was closed through the close button."))
      stopApp()
    })

    observeEvent(input$reset, {
      debug(.le$logger, glue::glue("App was reset through the reset button."))
      reset_triggered(TRUE)
      session$reload()
    })

    return(input)
  }) # moduleServer
} # ghqc_status_server
