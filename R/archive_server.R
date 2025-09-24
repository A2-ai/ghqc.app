#' @import shiny
#' @importFrom shinyvalidate InputValidator sv_required
#' @importFrom glue glue
#' @importFrom log4r warn error info debug
#' @importFrom shinyjs enable disable addClass removeClass delay
#' @importFrom waiter Waiter spin_1 spin_2 waiter_hide
#' @importFrom gert git_status
#' @importFrom rprojroot find_rstudio_root_file
NULL

ghqc_archive_server <- function(id, root_dir, all_milestone_names, open_milestone_names) {
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
    issue_titles_in_existing_milestone_rv <- reactiveVal(NULL)
    issues_in_existing_milestone_rv <- reactiveVal(NULL)
    reset_triggered <- reactiveVal(FALSE)
    session$onSessionEnded(function() {
      if (!isTRUE(isolate(reset_triggered()))) {
        stopApp()
      }
    })

    ns <- session$ns

    w_load_items <- Waiter$new(
      id = ns("main_panel_dynamic"),
      html = tagList(
        spin_2(),
      ),
      color = "white"
    )

    observe({
      req(all_milestone_names)

      updateSelectizeInput(
        session,
        "milestone_existing",
        choices = all_milestone_names
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
        textInput(ns("archive_name"), "Archive name", value = ""),
        shinyWidgets::prettyCheckbox(ns("flatten"), "Flatten file paths", value = FALSE, icon = icon("check")),
        shinyWidgets::prettyCheckbox(ns("issue_open"), "Include open issues", value = FALSE, icon = icon("check")),
        shinyWidgets::prettyCheckbox(ns("relevant_files"), "Include relevant files", value = FALSE, icon = icon("check")),
        div(
          style = "font-family: \"Helvetica Neue\", Helvetica, Arial, sans-serif !important; font-weight: bold;",
          "Select Additional File(s) for Archive"
        ),
        treeNavigatorUI(ns("treeNavigator"))
      )
    })

    shown_milestones <- reactiveVal(character(0))

    observe({
      req(inputted_milestone_rv())

      if (inputted_milestone_rv() %in% open_milestone_names) {
        shown_milestones_list <- shown_milestones()

        if (!(inputted_milestone_rv() %in% shown_milestones_list)) {
          showModal(modalDialog(
            title = "Milestone Found",
            paste("The milestone", inputted_milestone_rv(), "exists in the open milestones."),
            easyClose = TRUE,
            footer = tagList(
              modalButton("Close")
            )
          ))

          shown_milestones(c(shown_milestones_list, inputted_milestone_rv()))
        }
      }

      issue_titles_with_root_dir <- tryCatch({
        milestone <- get_milestone_from_name(inputted_milestone_rv())

        if (!is.null(milestone)) {
          issues_in_milestone <- get_all_issues_in_milestone_from_milestone_number(milestone_name = milestone$title,
                                                                                   milestone_number = milestone$number)
          issue_titles <- sapply(issues_in_milestone, function(issue) issue$title)

          issue_titles_in_existing_milestone_rv(issue_titles)
          issues_in_existing_milestone_rv(issues_in_milestone)

          file.path(basename(root_dir), issue_titles)
        } else {
          info(.le$logger, glue::glue("Inputted milestone {inputted_milestone_rv()} does not yet exist"))
          issue_titles_in_existing_milestone_rv(NULL)
          issues_in_existing_milestone_rv(NULL)

          list()
        }
      }, error = function(e) {
        debug(.le$logger, glue::glue("There was no Milestones to query: {conditionMessage(e)}"))
        return(list())
      })

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

    # Create the base issues_milestone_df (without relevant files)
    issues_milestone_df <- purrr::map_dfr(issues_in_repo, function(it) {
      init_qc_commit <- get_init_qc_commit_from_issue_body(it$body)
      latest_qc_commit <- get_qc_commit_info(
        file_name         = gsub(" ", "_", it$title),
        issue_body        = it$body,
        num_comments      = it$comments,
        comments_url      = it$comments_url,
        initial_qc_commit = init_qc_commit
      )$latest_qc_commit

      relevant_file <- get_relevant_files(it, it$milestone$title)$relevant_file_name %>%
        paste0(collapse = ", ")

      tibble::tibble(
        issue_number     = it$number,
        milestone_title  = it$milestone$title,
        title            = it$title,
        state            = it$state,
        latest_qc_commit = latest_qc_commit,
        relevant_file    = relevant_file  # Add relevant file column here
      )
    })

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




    selected_milestones_rv <- reactiveVal(NULL)

    observe({
      selected_milestones <- input$milestone_existing
      selected_milestones_rv(selected_milestones)

      req(selected_milestones)

      if (length(selected_milestones) <= 1) {
        return()
      }

      new_selected_milestone <- selected_milestones[length(selected_milestones)]

      new_filtered_df <- local_commit_df %>%
        filter(milestone_title == new_selected_milestone)

      new_selected_titles <- unique(new_filtered_df$title)

      prev_selected_milestones <- selected_milestones[1:(length(selected_milestones) - 1)]

      for (prev_milestone in prev_selected_milestones) {
        prev_filtered_df <- local_commit_df %>%
          filter(milestone_title == prev_milestone)

        prev_selected_titles <- unique(prev_filtered_df$title)

        duplicate_titles <- intersect(new_selected_titles, prev_selected_titles)

        if (length(duplicate_titles) > 0) {
          showModal(modalDialog(
            title = "Duplicate Files Found",
            paste("There are duplicate files detected between milestones. Please select a different milestone"),
            easyClose = TRUE,
            footer = tagList(
              modalButton("Close")
            )
          ))

          valid_selection <- selected_milestones[1:(length(selected_milestones) - 1)]

          updateSelectizeInput(session, "milestone_existing",
                               choices = all_milestone_names,
                               selected = valid_selection)

          return()
        }
      }
    })

    warned_duplicates <- reactiveVal(character(0))

    archive_files <- reactive({
      selected_milestones <- selected_milestones_rv()

      archive_files <- character(0)

      if (!is.null(selected_milestones) && length(selected_milestones) > 0) {
        filtered_issues <- issues_milestone_df %>%
          {
            if (!isTRUE(input$issue_open)) {
              dplyr::filter(., .data$state == "closed")
            } else {
              .
            }
          } %>%
          dplyr::filter(.data$milestone_title %in% !!selected_milestones)

        relevant_files_checked <- isTRUE(input$relevant_files)  # More explicit check for TRUE

        if (relevant_files_checked) {
          archive_files <- filtered_issues %>%
            dplyr::mutate(relevant_file = trimws(relevant_file), title = trimws(title)) %>%
            dplyr::mutate(relevant_file = dplyr::na_if(relevant_file, ""), title = dplyr::na_if(title, "")) %>%
            dplyr::select(relevant_file, title) %>%
            unlist()
          archive_files <- unique(archive_files)

        } else {
          archive_files <- filtered_issues %>%
            dplyr::pull(title) %>%
            unique() %>%
            sort()
        }
      }

      tree_selected_files <- selected_items()
      duplicate_files <- intersect(archive_files, tree_selected_files)

      # Store the duplicates that need to trigger the modal
      duplicates_to_warn <- setdiff(duplicate_files, warned_duplicates())

      if (length(duplicates_to_warn) > 0) {
        # Show modal for each unique duplicate file, once
        showModal(modalDialog(
          title = "Duplicate Files Found",
          paste("The following files are already in selected milestones:",
                paste(duplicates_to_warn, collapse = ", ")),
          easyClose = TRUE,
          footer = tagList(
            modalButton("Close")
          )
        ))

        # Update warned_duplicates only after modal is shown
        warned_duplicates(c(warned_duplicates(), duplicates_to_warn))
      }

      archive_files <- unique(c(archive_files, setdiff(tree_selected_files, duplicate_files)))
    })


    milestone_commit_df <- reactive({
      base <- local_commit_df


      archive_files_list <- archive_files()
      if (length(archive_files_list) == 0) {
        return(base[0, c("title", "commit", "milestone_title"), drop = FALSE])
      }

      base %>%
        filter(title %in% archive_files_list) %>%  # Filter by archive files (reactive)
        distinct(title, commit, milestone_title, .keep_all = TRUE) %>%  # Ensure distinct entries
        select(title, commit, milestone_title) %>%
        arrange(title)
    })

    output$main_panel_dynamic <- renderUI({
      tryCatch({
        df <- milestone_commit_df()
        items_from_milestone_df <- if (!is.null(df) && nrow(df) > 0) {
          sort(unique(na.omit(df$title)))
        } else {
          character(0)
        }

        w_load_items$show()
        session$sendCustomMessage("adjust_grid", id)

        ui_parts <- list()

        if (length(items_from_milestone_df) > 0) {
          ui_parts <- c(
            list(
              milestone_archive_render(
                input  = input,
                ns     = ns,
                items  = items_from_milestone_df,
                depth  = 0,
                output = output
              ),
              milestone_archive_isolate_rendered_list(
                input = input,
                session = session,
                items  = items_from_milestone_df,
                milestone_commit_df = milestone_commit_df
              )
            )
          )
        }


        do.call(tagList, ui_parts)
      }, error = function(e) {
        error(.le$logger, glue::glue("There was an error rendering items in right panel: {conditionMessage(e)}"))
        stopApp(); rlang::abort(conditionMessage(e))
      })
    })

    observeEvent(input$create_archive, ignoreInit = TRUE, {
      archive_name <- input$archive_name

      if (is.null(archive_name) || archive_name == "") {
        showNotification("Please enter an archive name.", type = "error")
        return()
      }

      archive_selected_items(
        input        = input,
        session      = session,
        items        = selected_items(),
        archive_name = input$archive_name,
        flatten      = isTRUE(input$flatten),
        milestone_commit_df = milestone_commit_df
      )
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
