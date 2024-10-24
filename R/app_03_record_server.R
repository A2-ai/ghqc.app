#' @import shiny
#' @importFrom shinyjs enable disable addClass removeClass
#' @importFrom log4r warn error info debug
#' @importFrom purrr map_df
#' @importFrom gert git_status git_ahead_behind
#' @importFrom shinyjs enable disable addClass removeClass delay
#' @importFrom waiter Waiter spin_1 spin_2 waiter_hide
#' @importFrom gert git_ahead_behind git_status
#' @importFrom rprojroot find_rstudio_root_file
NULL

ghqc_record_server <- function(id, remote, org, repo, all_milestones) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    report_trigger <- reactiveVal(FALSE)

    observe({
      req(remote)
      waiter_hide()
    })

    closed_milestones <- reactive({
      req(org, repo)
      w_gh <- create_waiter(ns, sprintf("Fetching Milestone data for %s in %s...", repo, org))
      w_gh$show()
      on.exit(w_gh$hide())

      tryCatch(
        {
          closed_milestones <- get_closed_milestone_names(org = org, repo = repo)
          milestone_list_url <- get_milestone_list_url()
          if (length(closed_milestones) == 0) {
            warn_icon_html <- "<span style='font-size: 24px; vertical-align: middle;'>&#9888;</span>"
            showModal(
              modalDialog(
                title = tags$div(
                  tags$span("Warning", style = "float: left; font-weight: bold; font-size: 20px;"),
                  modalButton("Dismiss"),
                  style = "overflow: hidden; text-align: right;"
                ),

                HTML(warn_icon_html, glue::glue("There were no closed Milestones found in {org}/{repo}.<br>
                                             Close relevant Milestones on GitHub to indicate finished QC, then click \"Reset\" in the top right corner of this app.<div style=\"margin-bottom: 9px;\"></div>")),
                tags$a(href = milestone_list_url, "Click here to close Milestones on GitHub", target = "_blank"),
                easyClose = TRUE,
                footer = NULL
              )
            )
            warn(.le$logger, glue::glue("There were no closed Milestones found in {org}/{repo}. Close relevant Milestones on GitHub to indicate finished QC."))
          } # length(closed_milestones) == 0
          rev(closed_milestones)
        },
        error = function(e) {
          error(.le$logger, glue::glue("There was an error retrieving closed Milestones: {e$message}"))
          showModal(modalDialog("Error in getting Milestones: ", e$message, footer = NULL))
        }
      )
    })


    observeEvent(input$closed_only, {
      # if closed
      if (input$closed_only) {
        placeholder <- ifelse(length(closed_milestones()) == 0, "No closed Milestones", "Select closed Milestones")

        updateSelectizeInput(
          session,
          "select_milestone",
          choices = closed_milestones(),
          options = list(placeholder = placeholder)
        )
      }

      # if not closed
      else {
        placeholder <- ifelse(length(all_milestones) == 0, "No Milestones", "Select Milestones")

        updateSelectizeInput(
          session,
          "select_milestone",
          choices = all_milestones,
          options = list(placeholder = placeholder)
        )
      }
    })

    observe({
      debug(.le$logger, glue::glue("generate_report buttons are inactivated."))
      removeClass("generate_report", "enabled-btn")
      addClass("generate_report", "disabled-btn")

      num_milestones_selected <- length(input$select_milestone)

      if (num_milestones_selected > 0) {
        debug(.le$logger, glue::glue("generate_report buttons are activated because there are {num_milestones_selected} selected Milestones"))
        removeClass("generate_report", "disabled-btn")
        addClass("generate_report", "enabled-btn")
      }
    })

    modal_check <- eventReactive(input$generate_report, {
      w_check_status <- create_waiter(ns, glue::glue("Checking QC status in selected Milestone(s)..."))
      w_check_status$show()
      on.exit(w_check_status$hide())

      determine_modal_message_report(org, repo, input$select_milestone)
    })

    observeEvent(input$generate_report, {
      req(modal_check())

      if (!is.null(modal_check()$message)) {
        showModal(modalDialog(
          title = tags$div(tagList(
            if (modal_check()$state == "warning") {
              tags$span("Warning", style = "float: left; font-weight: bold; font-size: 20px;")
              actionButton(ns("proceed"), "Proceed Anyway")
            },
            actionButton(ns("return"), "Return")
          ), style = "text-align: right;"),
          HTML(paste("It is recommended that relevant GitHub Issues and Milestones are closed upon completion of QC, and checklists within GitHub Issues are checked to indicate QCed items.<br><br> You may want to double check the following items for outstanding QC progress:<br><br>", modal_check()$message)),
          tags$style(HTML("
        .modal-content {
          word-wrap: break-word; /* Allows long text to break into new lines */
          overflow-wrap: break-word;
          max-width: 100%; /* Ensures content fits within the modal */
        }
      ")),
          footer = NULL,
          easyClose = TRUE
        ))
      }
      else {
        report_trigger(TRUE)
      }
    })

    observe({
      req(report_trigger())
      report_trigger(FALSE)

      milestone_num_str <- ifelse(length(input$select_milestone) == 1, "Milestone", "Milestones")
      milestones <- glue::glue_collapse(input$select_milestone, sep = ", ", last = " and ")

      w_generate_report <- create_waiter(ns, glue::glue("Generating report for {milestone_num_str}: {milestones}..."))
      w_generate_report$show()
      on.exit(w_generate_report$hide())

      tryCatch({
        pdf_path <- ghqc_report(
          milestone_names = input$select_milestone,
          input_name = input$pdf_name,
          just_tables = input$just_tables,
          location = input$pdf_location
        )

        showModal(
          modalDialog(
            title = tags$div(modalButton("Dismiss"), style = "text-align: right;"),
            footer = NULL,
            easyClose = TRUE,
            tags$p(glue::glue("QC report generated successfully: {pdf_path}"))
          )
        ) #showModal
      },
      error = function(e) {
        error_icon_html <- "<span style='font-size: 24px; vertical-align: middle;'>&#10071;</span>"
        showModal(
          modalDialog(
            title = tags$div(
            tags$span("Error", style = "float: left; font-weight: bold; font-size: 20px;"),
            modalButton("Dismiss"),
            style = "overflow: hidden; text-align: right;"
          ),
          HTML(error_icon_html, e$message, "<br>"),
          easyClose = TRUE,
          footer = NULL
        ))
      }) # tryCatch
    })

    observeEvent(input$return, {
      debug(.le$logger, glue::glue("Comment button returned and modal removed."))
      removeModal()
    })

    observeEvent(input$close, {
      debug(.le$logger, glue::glue("App was closed through the close button."))
      stopApp()
    })

    observeEvent(input$reset, {
      debug(.le$logger, glue::glue("App was reset through the reset button."))
      session$reload()
    })

    observeEvent(input$proceed, {
      debug(.le$logger, glue::glue("Report QC items action proceeded and modal removed."))
      removeModal()
      report_trigger(TRUE)
    })

    observeEvent(input$return, {
      debug(.le$logger, glue::glue("Report QC items action returned and modal removed."))
      removeModal()
    })

    return(input)
  })
}
