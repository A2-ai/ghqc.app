#' @import shiny
#' @importFrom shinyjs enable disable addClass removeClass
#' @importFrom log4r warn error info debug
#' @importFrom purrr map_df
#' @importFrom shinyjs enable disable addClass removeClass delay
#' @importFrom waiter Waiter spin_1 spin_2 waiter_hide
#' @importFrom rprojroot find_rstudio_root_file
NULL

ghqc_record_server <- function(id, all_milestones_in, closed_milestones_in, all_milestone_names_in, all_closed_milestone_names_in) {
  moduleServer(id, function(input, output, session) {
    iv <- shinyvalidate::InputValidator$new()
    iv$add_rule("select_milestone", shinyvalidate::sv_required())

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
    report_trigger <- reactiveVal(FALSE)
    all_milestones_rv <- reactiveVal(all_milestones_in)
    closed_milestones_rv <- reactiveVal(closed_milestones_in)
    all_milestone_names_rv <- reactiveVal(all_milestone_names_in)
    closed_milestone_names_rv <- reactiveVal(all_closed_milestone_names_in)
    issue_objects_rv <- reactiveVal(NULL)
    statuses_rv <- reactiveVal(NULL)
    selected_milestones_rv <- reactiveVal(NULL)

    observe({
      waiter_hide()
    })

    reactive({
      w_gh <- create_waiter(ns, sprintf("Fetching Milestone data for %s in %s...", .le$repo, .le$org))
      w_gh$show()
      on.exit(w_gh$hide())

      closed_milestones <- closed_milestones_rv()
      req(closed_milestones)

      tryCatch(
        {
          if (length(closed_milestones) == 0) {
            milestone_list_url <- get_milestone_list_url()
            warn_icon_html <- "<span style='font-size: 24px; vertical-align: middle;'>&#9888;</span>"
            showModal(
              modalDialog(
                title = tags$div(
                  tags$span("Warning", style = "float: left; font-weight: bold; font-size: 20px; margin-top: 5px;"),
                  tags$div(modalButton("Dismiss"), style = "text-align: right;"),
                  style = "#overflow: hidden; text-align: right;"
                ),

                HTML(warn_icon_html, glue::glue("There were no closed Milestones found in {.le$org}/{.le$repo}.<br>
                                             It is recommended to close relevant Milestones on GitHub to indicate finished QC.<div style=\"margin-bottom: 9px;\"></div>")),
                tags$span(
                  tags$a(href = milestone_list_url, target = "_blank", HTML("Click here to close Milestones on GitHub.")),
                  "Next, click \"Reset\" in the top right corner of the app."
                ),
                easyClose = TRUE,
                footer = NULL
              )
            )
            warn(.le$logger, glue::glue("There were no closed Milestones found in {.le$org}/{.le$repo}. Close relevant Milestones on GitHub to indicate finished QC."))
          } # length(closed_milestones) == 0
        },
        error = function(e) {
          error(.le$logger, glue::glue("There was an error retrieving closed Milestones: {conditionMessage(e)}"))
          rlang::abort(glue::glue("There was an error retrieving closed Milestones: {conditionMessage(e)}"))
        }
      )
    })


    observeEvent(input$closed_only, {
      closed_milestone_names <- closed_milestone_names_rv()
      all_milestone_names <- all_milestone_names_rv()
      req(closed_milestone_names, all_milestone_names)

      # if closed
      if (input$closed_only) {
        placeholder <- ifelse(length(closed_milestone_names) == 0, "No closed Milestones", "Select closed Milestones")

        updateSelectizeInput(
          session,
          "select_milestone",
          choices = closed_milestone_names,
          options = list(placeholder = placeholder)
        )
      }

      # if not closed
      else {
        placeholder <- ifelse(length(all_milestone_names) == 0, "No Milestones", "Select Milestones")

        updateSelectizeInput(
          session,
          "select_milestone",
          choices = all_milestone_names,
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

      all_milestones <- all_milestones_rv()
      milestone_names <- input$select_milestone
      milestone_objects <- purrr::map(milestone_names, ~ get_milestone_object_from_milestone_name(milestone_name = .x,
                                                                                 milestone_objects = all_milestones
                                                                                 ))
      selected_milestones_rv(milestone_objects)

      res <- determine_modal_message_report(milestone_objects)
      issue_objects_rv(res$issue_objects)
      statuses_rv(res$statuses)
      return(res)
    })

    observeEvent(input$generate_report, {
      req(modal_check())
      if (!is.null(modal_check()$message)) {
        if (modal_check()$state == "warning") {
          showModal(modalDialog(
            title = tags$div(
              tags$span("Warning", style = "float: left; font-weight: bold; font-size: 20px; margin-top: 5px;"),
              actionButton(ns("proceed"), "Proceed Anyway"),
              actionButton(ns("return"), "Return"),
              style = "text-align: right;"
            ),
            HTML(paste(glue::glue("When QC is complete, it is recommended that:
            <ul>
            <li>Milestones are closed</li>
            <li>{get_checklist_display_name_var(capitalized = TRUE, plural = TRUE)} are complete</li>
            <li>Issues are approved with <code>ghqc_status_app()</code></li>
            </ul>
            You may want to review the following on GitHub for outstanding QC progress:<br>"),
                       modal_check()$message)),
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

      w_generate_report <- create_waiter(ns, glue::glue("Generating QC Record for {milestone_num_str}: {milestones}..."))
      w_generate_report$show()
      on.exit(w_generate_report$hide())

      tryCatch({
        pdf_path <- ghqc_record(
          milestone_objects = selected_milestones_rv(),
          issue_objects = issue_objects_rv(),
          statuses = statuses_rv(),
          input_name = input$pdf_name,
          just_tables = input$just_tables,
          location = input$pdf_location
        )

        showModal(
          modalDialog(
            title = tags$div(
              tags$span("QC recorded", style = "float: left; font-weight: bold; font-size: 20px; margin-top: 5px;"),
              modalButton("Dismiss"),
              style = "text-align: right;"
            ),
            footer = NULL,
            easyClose = TRUE,
            tags$p(glue::glue("PDF location: {pdf_path}"))
          )
        ) #showModal
      },
      error = function(e) {
        error(.le$logger, glue::glue("There was an error generating the QC Record: {conditionMessage(e)}"))
        rlang::abort(glue::glue("There was an error generating the QC Record: {conditionMessage(e)}"))
      }) # tryCatch
    })

    observeEvent(input$return, {
      debug(.le$logger, glue::glue("Button returned and modal removed."))
      removeModal()
    })

    observeEvent(input$close, {
      debug(.le$logger, glue::glue("App was closed through the close button."))
      stopApp()
    })

    observeEvent(input$reset, {
      session$reload()
      debug(.le$logger, glue::glue("App was reset through the reset button."))
      reset_triggered(TRUE)

      all_milestones <- get_all_non_empty_ghqc_milestone_objects()
      closed_milestones <- get_closed_milestone_objects_from_all_milestone_objects(all_milestones)
      all_milestone_names <- get_milestone_names_from_milestone_objects(all_milestones)
      closed_milestone_names <- get_milestone_names_from_milestone_objects(closed_milestones)

      all_milestones_rv(all_milestones)
      closed_milestones_rv(closed_milestones)
      all_milestone_names_rv(all_milestone_names)
      closed_milestone_names_rv(closed_milestone_names)


    })

    observeEvent(input$proceed, {
      debug(.le$logger, glue::glue("Generate QC Record action proceeded and modal removed."))
      removeModal()
      report_trigger(TRUE)
    })

    observeEvent(input$return, {
      debug(.le$logger, glue::glue("Generate QC Record action returned and modal removed."))
      removeModal()
    })

    iv$enable()
    return(input)
  })
}
