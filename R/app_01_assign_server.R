#' @import shiny
#' @importFrom shinyvalidate InputValidator sv_required
#' @importFrom glue glue
#' @importFrom log4r warn error info debug
#' @importFrom shinyjs enable disable addClass removeClass delay
#' @importFrom waiter Waiter spin_1 spin_2 waiter_hide
#' @importFrom gert git_ahead_behind git_status
#' @importFrom rprojroot find_rstudio_root_file
NULL

ghqc_assign_server <- function(id, remote, root_dir, checklists, org, repo, members, milestone_list) {
  iv <- shinyvalidate::InputValidator$new()


  observe({
    req(remote, root_dir)
      waiter_hide()
  })

  observe({
    req(root_dir)
    if (getwd() != root_dir) {
      setwd(root_dir)
      info(.le$logger, glue::glue("Directory changed to project root: {root_dir)}"))
    }
  })

  root_dir_reactive <- reactive({
    root_dir
  })

  selected_items <- treeNavigatorServer(
    id,
    rootFolder = root_dir_reactive,
    search = FALSE,
    pattern = exclude_patterns(),
    all.files = FALSE
  )

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    if (length(milestone_list) == 0) {
      updateSelectizeInput(
        session,
        "milestone_existing",
        options = list(placeholder = "No Existing Milestones")
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
      req(milestone_list)

      updateSelectizeInput(
        session,
        "milestone_existing",
        choices = milestone_list
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
        selectizeInput(
          ns("assignees"),
          "Select Assignee(s)",
          choices = "No assignee",
          multiple = TRUE,
          width = "100%",
          options = list(
            closeAfterSelect = TRUE
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

    observe({
      req(org, members)
      w_gh <- create_waiter(ns, sprintf("Fetching organization and member data for %s ...", org))
      w_gh$show()
      on.exit(w_gh$hide())

      updateSelectizeInput(
        session,
        "assignees",
        server = TRUE,
        choices = members,
        options = list(
          placeholder = "(optional)",
          valueField = "username",
          labelField = "username",
          searchField = c("username", paste0("name")),
          render = I(
            '{ option: function(item, escape) {
if (item.name !== null) {
return "<div><strong>" + escape(item.username) + "</strong> (" + escape(item.name) +") </div>" } else {
return "<div><strong>" + escape(item.username) + "</div>"
}
}
}'
          )
        )
      )
    })

    observe({
      req(org, repo, rv_milestone())

      issue_titles <- tryCatch(
        {
          issues_in_milestone <- get_all_issues_in_milestone(owner = org, repo = repo, milestone_name = rv_milestone())
          issue_titles <- sapply(issues_in_milestone, function(issue) issue$title)
          issue_titles_with_root_dir <- file.path(basename(root_dir), issue_titles)
          issue_titles_with_root_dir
        },
        error = function(e) {
          debug(.le$logger, glue::glue("There was no Milestones to query: {e$message}"))
          return(list())
        }
      )

      session$sendCustomMessage("highlightPaths", issue_titles)
    })

    qc_items <- reactive({
      req(root_dir, selected_items())
      tryCatch(
        {
          file_data <- extract_file_data(input, selected_items())
        },
        error = function(e) {
          error(.le$logger, glue::glue("There was an error extracting file data from {selected_items()}:{e$message}"))
          rlang::abort(e$message)
        }
      )
    })

    output$main_panel_static <- renderUI({
      div(
        style = "display: flex; justify-content: flex-end; padding-bottom: 20px;",
        actionButton(ns("file_info"),
                     label = HTML("<span style='font-size:2.0em;'>Preview all available checklists</span>"),
                     class = "preview-button",
                     style = "min-width: auto; display: inline-block; text-align: center; line-height: 2em; height: 2em;"
        ) #actionButton
      ) #div

    })

    output$main_panel_dynamic <- renderUI({
      validate(need(length(selected_items()) > 0, "No files selected"))
      w_load_items$show()

      log_string <- glue::glue_collapse(selected_items(), sep = ", ")
      debug(.le$logger, glue::glue("Files selected for QC: {log_string}"))

      list <- render_selected_list(input, ns, iv, items = selected_items(), checklist_choices = checklists)
      isolate_rendered_list(input, session, selected_items())

      session$sendCustomMessage("adjust_grid", id) # finds the width of the files and adjusts grid column spacing based on values
      return(list)
    })

    observe({
      req(input$adjust_grid_finished) # retrieve msg through js when adjust grid is done
      w_load_items$hide()
    })

    observeEvent(selected_items(), {
      items <- selected_items()
      for (name in items) {
        log_string <- glue::glue_collapse(items, sep = ", ")
        debug(.le$logger, glue::glue("Preview buttons created for: {log_string}"))
        tryCatch(
          {
            create_button_preview_event(input, name = name)
          },
          error = function(e) {
            error(.le$logger, glue::glue("There was an error creating the preview buttons: {e$message}"))
            rlang::abort(e$message)
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
          git_sync_status <- git_ahead_behind()
          untracked_selected_files <- Filter(function(file) check_if_qc_file_untracked(file), file_names)

          issues_in_milestone <- tryCatch(
            {
              get_all_issues_in_milestone(owner = org, repo = repo, milestone_name = rv_milestone())
            },
            error = function(e) {
              debug(.le$logger, glue::glue("There were no Milestones to query: {e$message}"))
              return(list())
            }
          )
        },
        error = function(e) {
          error(.le$logger, glue::glue("There was an error retrieving one of the status_checks items: {e$message}"))
          rlang::abort(e$message)
        }
      )

      determine_modal_message(
        selected_files = file_names,
        uncommitted_git_files = uncommitted_git_files,
        untracked_selected_files = untracked_selected_files,
        git_sync_status = git_sync_status,
        issues_in_milestone = issues_in_milestone
      )
    })

    observeEvent(input$create_qc_items, {
      req(modal_check())

      if (!is.null(modal_check()$message)) {
        showModal(modalDialog(
          title = tags$div(tagList(
            if (modal_check()$state == "warning") {
              actionButton(ns("proceed"), "Proceed Anyway")
            },
            actionButton(ns("return"), "Return")
          ), style = "text-align: right;"),
          HTML(modal_check()$message),
          footer = NULL,
          easyClose = TRUE
        ))
      } else {
        qc_trigger(TRUE)
      }
    })

    observe({
      req(qc_trigger())
      qc_trigger(FALSE)

      w_create_qc_items <- create_waiter(ns, "Assigning file(s) for QC ...")
      w_create_qc_items$show()
      tryCatch(
        {
          create_yaml("test",
                      org = org,
                      repo = repo,
                      milestone = rv_milestone(),
                      description = input$milestone_description,
                      files = qc_items()
          )

          create_checklists("test.yaml")
          removeClass("create_qc_items", "enabled-btn")
          addClass("create_qc_items", "disabled-btn")
        },
        error = function(e) {
          error(.le$logger, glue::glue("There was an error creating the Milestone {qc_items()}: {e$message}"))
          rlang::abort(e$message)
        }
      )

      w_create_qc_items$hide()
      milestone_url <- get_milestone_url(org, repo, rv_milestone())

      custom_checklist_selected <- function() {
        qc_items <- qc_items()
        any(sapply(qc_items, function(x) x$checklist_type == "Custom"))
      }
      success_note <- {
        if (custom_checklist_selected()) {
          HTML("Issue(s) created successfully.<br><b>Remember to manually edit Custom QC checklists on GitHub.</b>")
        }
        else {
          "Issue(s) created successfully."
        }
      }

      showModal(
        modalDialog(
          title = tags$div(modalButton("Dismiss"), style = "text-align: right;"),
          footer = NULL,
          easyClose = TRUE,
          tags$p(success_note),
          tags$a(href = milestone_url, "Click here to visit the Milestone on Github", target = "_blank")
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
          "Each file input will require a checklist type. Each checklist type will have its own items associated with it.",
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
            "Each file input will require a checklist type. Each checklist type will have its own items associated with it.",
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
      debug(.le$logger, glue::glue("Checklist selected for review: {input$checklist_info}"))

      info <- checklists[[input$checklist_info]]

      log_string <- glue::glue_collapse(info, sep = "\n")
      debug(.le$logger, glue::glue("Items found in the checklist: \n{log_string}"))

      list <- convert_list_to_ui(info) # checklists needs additional formatting for list of named elements

      tags$ul(list)
    })
    #--- checklist info button end

    observe({
      debug(.le$logger, glue::glue("create_qc_items buttons are inactivated."))
      removeClass("create_qc_items", "enabled-btn")
      addClass("create_qc_items", "disabled-btn")

      if (length(selected_items()) > 0 && isTruthy(rv_milestone())) {
        file_data <- extract_file_data(input, selected_items())
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
      debug(.le$logger, glue::glue("Create QC items action returned and modal removed."))
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

    #HERE
    observeEvent(selected_items(), {
      req(checklists)
      items <- selected_items()
      for (name in items) {
        log_string <- glue::glue_collapse(items, sep = ", ")
        debug(.le$logger, glue::glue("Preview buttons created for: {log_string}"))
        tryCatch(
          {
            create_checklist_preview_event(input, iv, ns, name = name, checklists)
          },
          error = function(e) {
            error(.le$logger, glue::glue("There was an error creating the preview buttons: {e$message}"))
            rlang::abort(e$message)
          }
        )
      }
    })

    iv$enable()
    return(input)
  })
}
