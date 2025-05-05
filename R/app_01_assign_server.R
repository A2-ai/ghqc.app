#' @import shiny
#' @importFrom shinyvalidate InputValidator sv_required
#' @importFrom glue glue
#' @importFrom log4r warn error info debug
#' @importFrom shinyjs enable disable addClass removeClass delay
#' @importFrom waiter Waiter spin_1 spin_2 waiter_hide
#' @importFrom gert git_status
#' @importFrom rprojroot find_rstudio_root_file
NULL

ghqc_assign_server <- function(id, root_dir, checklists, members, open_milestone_names) {
  iv <- shinyvalidate::InputValidator$new()

  observe({
    req(root_dir)
      waiter_hide()
  })


  root_dir_reactive <- reactive({
    root_dir
  })

  selected_items <- treeNavigatorServer(
    id,
    rootFolder = root_dir_reactive,
    search = FALSE,
    #pattern = exclude_patterns(),
    all.files = FALSE
  )

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

    if (length(open_milestone_names) == 0) {
      updateSelectizeInput(
        session,
        "milestone_existing",
        options = list(placeholder = "No Open Milestones")
      )
    }

    qc_trigger <- reactiveVal(FALSE)

    w_load_items <- Waiter$new(
      id = ns("content"),
      html = tagList(
        spin_2(),
      ),
      color = "white"
    )

    rv_milestone <- reactiveVal(NULL)

    observe({
      req(open_milestone_names)

      updateSelectizeInput(
        session,
        "milestone_existing",
        choices = open_milestone_names
      )
    })

    observe({
      req(input$milestone_toggle)
      milestone_toggle <- input$milestone_toggle
      if (milestone_toggle == "New") {
        req(input$milestone)
        rv_milestone(input$milestone)
      } else if (milestone_toggle == "Existing") {
        req(input$milestone_existing)
        rv_milestone(input$milestone_existing)
      }
    })

    output$sidebar <- renderUI({
      tagList(
        radioButtons(ns("milestone_toggle"), "Milestone State", choices = c("New", "Existing"), inline = TRUE),
        conditionalPanel(
          condition = "input.milestone_toggle == `New`", ns = ns,
          textInput(ns("milestone"),
                    "Milestone Name",
                    placeholder = "Milestone Name",
                    width = "100%"
          )
        ),
        conditionalPanel(
          condition = "input.milestone_toggle == `Existing`", ns = ns,
          selectizeInput(ns("milestone_existing"),
                         "Select Existing Milestone",
                         choices = "",
                         multiple = FALSE,
                         width = "100%",
                         options = list(placeholder = "(required)")
          ),
        ),
        conditionalPanel(
          condition = "input.milestone_toggle == `New`", ns = ns,
          textAreaInput(
            ns("milestone_description"),
            "Milestone Description",
            placeholder = "(optional)",
            width = "100%"
          )
        ),
        div(
          style = "font-family: \"Helvetica Neue\", Helvetica, Arial, sans-serif !important; font-weight: bold;",
          "Select File(s) for QC"
        ),
        treeNavigatorUI(ns("treeNavigator"))
      )
    })

    observe({
      req(input$milestone_toggle)
      if (input$milestone_toggle == "New") {
        iv$add_rule("milestone", shinyvalidate::sv_required())
      }
      else {
        iv$add_rule("milestone_existing", shinyvalidate::sv_required())
      }
    })

    rv_issue_titles <- reactiveVal()

    observe({
      req(rv_milestone())

      issue_titles_with_root_dir <- tryCatch(
        {
          issues_in_milestone <- get_all_issues_in_milestone(milestone_name = rv_milestone())
          issue_titles <- sapply(issues_in_milestone, function(issue) issue$title)
          rv_issue_titles(issue_titles) # assign to reactiveVal

          file.path(basename(root_dir), issue_titles)
        },
        error = function(e) {
          debug(.le$logger, glue::glue("There was no Milestones to query: {conditionMessage(e)}"))
          return(list())
        }
      )

      session$sendCustomMessage("highlightPaths", issue_titles_with_root_dir)
    })

    qc_items <- reactive({
      req(root_dir, selected_items())
      tryCatch(
        {
          relevant_files_list <- tryCatch({
            relevant_files()
          }, error = function(e){
            NULL
          })
          file_data <- extract_file_data(input, selected_items(), relevant_files_list)
        },
        error = function(e) {
          error(.le$logger, glue::glue("There was an error extracting file data from {selected_items()}:{conditionMessage(e)}"))
          stopApp()
          rlang::abort(conditionMessage(e))
        }
      )
    })

    output$main_panel_static <- renderUI({
      div(
        style = "display: flex; justify-content: flex-end; padding-bottom: 20px;",
        actionButton(ns("file_info"),
                     label = HTML(glue::glue("<span style='font-size:2.0em;'>Preview all available {get_checklist_display_name_var(plural = TRUE)}</span>")),
                     class = "preview-button",
                     style = "min-width: auto; display: inline-block; text-align: right; line-height: 2em; height: 2em;"
        ) #actionButton
      ) #div

    })

    relevant_files <- reactiveVal(list())

    output$validation_message <- renderUI({
      validate(
        need(length(selected_items()) > 0,
             HTML("<div style='color: #d9534f;'>No files selected</div>")
        )
      )
    })

    output$main_panel_dynamic <- renderUI({
      req(selected_items())
      tryCatch({
        if (length(selected_items()) == 0) {
          return(HTML("<div style='font-size: small !important; font-family: \"Helvetica Neue\", Helvetica, Arial, sans-serif !important; color: #a94442; font-weight: 700;'>No files selected (required)</div>"))
        }

        w_load_items$show()

        log_string <- glue::glue_collapse(selected_items(), sep = ", ")
        debug(.le$logger, glue::glue("Files selected for QC: {log_string}"))

        relevant_files_list <- tryCatch({
          relevant_files()
        }, error = function(e){
          NULL
        })

        list <- render_selected_list(
          input = input,
          ns = ns,
          items = selected_items(),
          checklist_choices = checklists,
          relevant_files = relevant_files_list,
          output = output
        )

        isolate_rendered_list(input = input,
                              session = session,
                              items = selected_items(),
                              members = members)

        session$sendCustomMessage("adjust_grid", id) # finds the width of the files and adjusts grid column spacing based on values
        return(list)
      }, error = function(e) {
        error(.le$logger, glue::glue("There was an error rendering items in right panel: {conditionMessage(e)}"))
        stopApp()
        rlang::abort(conditionMessage(e))
      })
    })


    observe({
      req(input$adjust_grid_finished) # retrieve msg through js when adjust grid is done
      req(selected_items())
      items <- selected_items()
      for (name in items) {
        checklist_input_id <- generate_input_id("checklist", name)
        debug(.le$logger, glue::glue("Adding validation rule for {checklist_input_id}"))
        iv$add_rule(checklist_input_id, shinyvalidate::sv_required())
      }

      w_load_items$hide()
    })

    observeEvent(selected_items(), {
      req(checklists)
      items <- selected_items()
      for (name in items) {
        log_string <- glue::glue_collapse(items, sep = ", ")
        debug(.le$logger, glue::glue("Preview buttons created for: {log_string}"))
        tryCatch(
          {
            create_button_preview_event(input, name = name)
            associate_relevant_files_button_event(input = input, output = output, name = name, ns = ns, root_dir = root_dir, relevant_files = relevant_files)
            create_checklist_preview_event(input = input, name = name, checklists = checklists)
          },
          error = function(e) {
            error(.le$logger, glue::glue("There was an error creating the preview buttons: {conditionMessage(e)}"))
            stopApp()
            rlang::abort(conditionMessage(e))
          }
        )
      }
    })


    modal_check <- eventReactive(input$create_qc_items, {
      req(qc_items())
      tryCatch(
        {
          file_names <- sapply(qc_items(), function(x) x$name)
          uncommitted_git_files <- git_status()$file
          git_sync_status <- check_ahead_behind()
          untracked_selected_files <- Filter(function(file) check_if_qc_file_untracked(file), file_names)

          issue_titles <- rv_issue_titles()
        },
        error = function(e) {
          error(.le$logger, glue::glue("There was an error retrieving one of the status_checks items: {conditionMessage(e)}"))
          stopApp()
          rlang::abort(conditionMessage(e))
        }
      )

      determine_modal_message(
        selected_files = file_names,
        uncommitted_git_files = uncommitted_git_files,
        untracked_selected_files = untracked_selected_files,
        git_sync_status = git_sync_status,
        issue_titles = issue_titles
      )
    })

    observeEvent(input$create_qc_items, {
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
            HTML(modal_check()$message),
            footer = NULL,
            easyClose = TRUE
          ))
        }

        else if (modal_check()$state == "error") {
          showModal(modalDialog(
            title = tags$div(
              tags$span("Error", style = "float: left; font-weight: bold; font-size: 20px; margin-top: 5px;"),
              actionButton(ns("return"), "Return"),
              style = "text-align: right;"
            ),
            HTML(modal_check()$message),
            footer = NULL,
            easyClose = TRUE
          ))
        }
      }
      else {
        qc_trigger(TRUE)
      }
    })

    observe({
      req(qc_trigger())
      qc_trigger(FALSE)

      w_create_qc_items <- create_waiter(ns, "Assigning QC file(s)...")
      w_create_qc_items$show()
      tryCatch(
        {
          yaml <- create_yaml(milestone = rv_milestone(),
                              description = input$milestone_description,
                              files = qc_items()
                              )

          create_checklists(yaml)
          removeClass("create_qc_items", "enabled-btn")
          addClass("create_qc_items", "disabled-btn")
        },
        error = function(e) {
          error(.le$logger, glue::glue("There was an error creating the Milestone {qc_items()}: {conditionMessage(e)}"))
          stopApp()
          rlang::abort(conditionMessage(e))
        }
      )

      w_create_qc_items$hide()
      milestone_url <- get_milestone_url(rv_milestone())

      custom_checklist_selected <- function() {
        qc_items <- qc_items()
        any(sapply(qc_items, function(x) x$checklist_type == "Custom"))
      }

      custom_note <- ifelse(custom_checklist_selected(),
                             HTML(glue::glue("Remember to manually edit Custom {get_checklist_display_name_var(plural = TRUE)} on GitHub.")),
                             ""
                             )

      showModal(
        modalDialog(
          title = tags$div(
            tags$span("QC assigned", style = "float: left; font-weight: bold; font-size: 20px; margin-top: 5px;"),
            modalButton("Dismiss"), style = "text-align: right;"),
          footer = NULL,
          easyClose = TRUE,
          tags$p(custom_note),
          tags$a(href = milestone_url, "Click here to view the Milestone on GitHub", target = "_blank")
        )
      )
    })

    #--- checklist info button begin
    observeEvent(input$file_info, {
      req(checklists)
      debug(.le$logger, glue::glue("file_info button was triggered."))

      showModal(
        modalDialog(
          title = tags$div(modalButton("Dismiss"), style = "text-align: right;"),
          footer = NULL,
          easyClose = TRUE,
          glue::glue("Each selected file will require a {get_checklist_display_name_var()} type. Each {get_checklist_display_name_var()} type will have its own items associated with it."),
          "See below for a reference of all types and their items.",
          br(),
          br(),
          selectInput(ns("checklist_info"), NULL, choices = names(checklists), width = "100%"),
          uiOutput(ns("file_info_panel"))
        )
      )
    })


    observeEvent(input$checklist_input_id, {
      selected_checklist <- input$checklist_input_id

      observeEvent(input$preview_input_id, {
        showModal(
          modalDialog(
            title = tags$div(modalButton("Dismiss"), style = "text-align: right;"),
            footer = NULL,
            easyClose = TRUE,
            glue::glue("Each selected file will require a {get_checklist_display_name_var()} type. Each {get_checklist_display_name_var()} type will have its own items associated with it."),
            "See below for a reference of all types and their items.",
            br(),
            br(),
            selectInput(ns("checklist_info"), NULL, choices = names(checklists), width = "100%"),
            uiOutput(ns("file_info_panel"))
          )
        )
      })
    })

    output$file_info_panel <- renderUI({
      req(checklists)
      req(input$checklist_info)
      debug(.le$logger, glue::glue("{get_checklist_display_name_var(capitalized = TRUE)} selected for review: {input$checklist_info}"))

      info <- checklists[[input$checklist_info]]

      log_string <- glue::glue_collapse(info, sep = "\n")
      debug(.le$logger, glue::glue("Items found in the {get_checklist_display_name_var()}: \n{log_string}"))

      list <- convert_list_to_ui(info) # checklists needs additional formatting for list of named elements

      tags$ul(list)
    })
    #--- checklist info button end

    observe({
      debug(.le$logger, glue::glue("create_qc_items buttons are inactivated."))
      removeClass("create_qc_items", "enabled-btn")
      addClass("create_qc_items", "disabled-btn")

      if (length(selected_items()) > 0 && isTruthy(rv_milestone())) {
        relevant_files_list <- tryCatch({
          relevant_files()
        }, error = function(e){
          NULL
        })

        file_data <- extract_file_data(input, selected_items(), relevant_files_list)
        if (!is.null(file_data)) {
          debug(.le$logger, glue::glue("create_qc_items buttons are activated because there are {length(selected_items())} selected items and milestone is named {rv_milestone()}"))
          removeClass("create_qc_items", "disabled-btn")
          addClass("create_qc_items", "enabled-btn")
        }

      }
    })

    observeEvent(input$proceed, {
      debug(.le$logger, glue::glue("Create Issues action proceeded and modal removed."))
      removeModal()
      qc_trigger(TRUE)
    })

    observeEvent(input$return, {
      debug(.le$logger, glue::glue("Assign action returned and modal removed."))
      removeModal()
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

    iv$enable()
    return(input)
  })
}
