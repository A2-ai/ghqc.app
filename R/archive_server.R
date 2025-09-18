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
        textInput(ns("archive_name"), "Archive name", value = ""),
        selectizeInput(
          ns("milestone_existing"),
          "Select Existing Milestone",
          choices = "",
          multiple = TRUE,
          width = "100%"
        ),
        shinyWidgets::prettyCheckbox("flatten", "Flatten file paths", value = FALSE, icon = icon("check")),
        shinyWidgets::prettyCheckbox("issue_open", "Include open issues", value = FALSE, icon = icon("check")),
        shinyWidgets::prettyCheckbox("relevant_files", "Include relevant files", value = FALSE, icon = icon("check")
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
            issue_titles_in_existing_milestone_rv(NULL)
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



    output$validation_message <- renderUI({
      validate(
        need(length(selected_items()) > 0,
             HTML("<div style='color: #d9534f;'>No files selected</div>")
        )
      )
    })

    issues_in_repo <- get_all_issues_in_repo()

    issues_milestone_df <- purrr::map_dfr(issues_in_repo, function(it) {
      init_qc_commit <- get_init_qc_commit_from_issue_body(it$body)
      latest_qc_commit <- get_qc_commit_info(
        file_name         = gsub(" ", "_", it$title),
        issue_body        = it$body,
        num_comments      = it$comments,
        comments_url      = it$comments_url,
        initial_qc_commit = init_qc_commit
      )$latest_qc_commit

      tibble::tibble(
        issue_number     = it$number,
        milestone_title  = it$milestone$title,
        title            = it$title,
        state            = it$state,
        latest_qc_commit = latest_qc_commit
        # , relevant_file = rel_file  # <- add back once rel_file is defined
      )
    })

    archive_files <- issues_milestone_df %>%
      dplyr::pull(title) %>%
      purrr::discard(is.na) %>%
      unique() %>%
      sort()

    current_branch <- gert::git_branch()

    local_commits <- get_local_commits()

    local_log_output <- system(
      "git log --pretty=format:'%H|%an|%ae|%ad|%s'  --date=format:'%Y-%m-%d %H:%M:%S' --name-status",
      intern = TRUE
    )

    local_commit_df <- parse_local_log(local_log_output)

    # This is the master commit list
    local_commit_df <- dplyr::bind_rows(
      local_commit_df %>%
        dplyr::select(title, commit) %>%
        dplyr::mutate(milestone_title = NA_character_, state = NA_character_),
      issues_milestone_df %>%
        dplyr::rename(commit = latest_qc_commit) %>%
        dplyr::select(title, commit, milestone_title, state)
    ) %>%
      dplyr::distinct(title, commit, milestone_title, state, .keep_all = TRUE) %>%
      dplyr::arrange(title)


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

        archive_isolate_rendered_list(input = input,
                              session = session,
                              items = selected_items(),
                              local_commit_df = local_commit_df
        )
        session$sendCustomMessage("adjust_grid", id)
        return(list)
      }, error = function(e) {
        error(.le$logger, glue::glue("There was an error rendering items in right panel: {conditionMessage(e)}"))
        stopApp()
        rlang::abort(conditionMessage(e))
      })
    })

    observeEvent(input$create_archive, ignoreInit = TRUE, {
      archive_name <- input$archive_name

       if (is.null(archive_name) || archive_name == "") {
        showNotification("Please enter an archive name.", type = "error")
         return()
       }

      archive_selected_items(input, session, items = selected_items(),
                             archive_name = archive_name)
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
