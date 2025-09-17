#' @import shiny
#' @importFrom shinyvalidate InputValidator sv_required
#' @importFrom glue glue
#' @importFrom log4r warn error info debug
#' @importFrom shinyjs enable disable addClass removeClass delay
#' @importFrom waiter Waiter spin_1 spin_2 waiter_hide
#' @importFrom gert git_status
#' @importFrom rprojroot find_rstudio_root_file
NULL

ghqc_archive_server <- function(id, root_dir, checklists, members, open_milestone_names) {
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
    inputted_milestone_rv <- reactiveVal(NULL)
    relevant_files <- reactiveVal(list())
    issue_titles_in_existing_milestone_rv <- reactiveVal(NULL)
    issues_in_existing_milestone_rv <- reactiveVal(NULL)

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
        options = list(placeholder = "No open Milestones")
      )
    }

    qc_trigger <- reactiveVal(FALSE)

    w_load_items <- Waiter$new(
      id = ns("main_panel_dynamic"),
      html = tagList(
        spin_2(),
      ),
      color = "white"
    )



    observe({
      req(open_milestone_names)

      updateSelectizeInput(
        session,
        "milestone_existing",
        choices = open_milestone_names
      )
    })


    output$sidebar <- renderUI({
      tagList(
        selectizeInput(
          ns("milestone_existing"),
          "Select Existing Milestone",
          choices = "",
          multiple = TRUE,
          width = "100%"
        ),
        div(
          style = "font-family: \"Helvetica Neue\", Helvetica, Arial, sans-serif !important; font-weight: bold;",
          "Select File(s) for QC"
        ),
        treeNavigatorUI(ns("treeNavigator"))
      )
    })


    observe({
      req(inputted_milestone_rv())

      issue_titles_with_root_dir <- tryCatch(
        {
          milestone <- get_milestone_from_name(inputted_milestone_rv())

          if (!is.null(milestone)) {
            issues_in_milestone <- get_all_issues_in_milestone_from_milestone_number(milestone_name = milestone$title,
                                                                                     milestone_number = milestone$number)
            issue_titles <- sapply(issues_in_milestone, function(issue) issue$title)

            issue_titles_in_existing_milestone_rv(issue_titles) # assign to reactiveVal
            issues_in_existing_milestone_rv(issues_in_milestone)

            file.path(basename(root_dir), issue_titles)
          }
          else {
            info(.le$logger, glue::glue("Inputted milestone {inputted_milestone_rv()} does not yet exist"))
            issue_titles_in_existing_milestone_rv(NULL) # assign to reactiveVal
            issues_in_existing_milestone_rv(NULL)

            list()
          }

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
                     label = HTML(glue::glue("<span style='font-size:2.0em;'>Preview {get_checklist_display_name_var()} options</span>")),
                     class = "preview-button",
                     style = "min-width: auto; display: inline-block; text-align: right; line-height: 2em; height: 2em;"
        ) #actionButton
      ) #div

    })



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

        list <- archive_render_selected_list(
          input = input,
          ns = ns,
          items = selected_items(),
        )

        session$sendCustomMessage("adjust_grid", id)
        return(list)
      }, error = function(e) {
        error(.le$logger, glue::glue("There was an error rendering items in right panel: {conditionMessage(e)}"))
        stopApp()
        rlang::abort(conditionMessage(e))
      })
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
