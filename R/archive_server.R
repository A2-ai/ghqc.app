#' @import shiny
#' @importFrom shinyvalidate InputValidator sv_required
#' @importFrom glue glue
#' @importFrom log4r warn error info debug
#' @importFrom shinyjs enable disable addClass removeClass delay
#' @importFrom waiter Waiter spin_1 spin_2 waiter_hide
#' @importFrom gert git_status
#' @importFrom rprojroot find_rstudio_root_file
NULL

ghqc_archive_server <- function(id, root_dir, open_milestone_names) {
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

    observe({
      req(inputted_milestone_rv())

      issue_titles_with_root_dir <- tryCatch({
        milestone <- get_milestone_from_name(inputted_milestone_rv())

        if (!is.null(milestone)) {
          issues_in_milestone <- get_all_issues_in_milestone_from_milestone_number(milestone_name = milestone$title,
                                                                                   milestone_number = milestone$number)
          issue_titles <- sapply(issues_in_milestone, function(issue) issue$title)

          issue_titles_in_existing_milestone_rv(issue_titles)  # assign to reactiveVal
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

      # If only one milestone is selected, no need to check for duplicates
      if (length(selected_milestones) <= 1) {
        return()
      }

      # Get the newly selected milestone (last one selected)
      new_selected_milestone <- selected_milestones[length(selected_milestones)]

      # Get the titles of the newly selected milestone
      new_filtered_df <- local_commit_df %>%
        filter(milestone_title == new_selected_milestone)

      new_selected_titles <- unique(new_filtered_df$title)

      # Get all the previously selected milestones (all but the newly selected one)
      prev_selected_milestones <- selected_milestones[1:(length(selected_milestones) - 1)]

      # Check each previously selected milestone to see if there are duplicates with the new one
      for (prev_milestone in prev_selected_milestones) {
        # Get the titles of the previous selected milestone
        prev_filtered_df <- local_commit_df %>%
          filter(milestone_title == prev_milestone)

        prev_selected_titles <- unique(prev_filtered_df$title)

        # Find duplicates by checking for overlapping titles between the newly selected and previous milestones
        duplicate_titles <- intersect(new_selected_titles, prev_selected_titles)

        if (length(duplicate_titles) > 0) {
          # Show the modal warning with duplicate titles
          showModal(modalDialog(
            title = "Duplicate Files Found",
            paste("There are duplicate files detected between milestones. Please select a different milestone"),
            easyClose = TRUE,
            footer = tagList(
              modalButton("Close")
            )
          ))

          # Deselect the newly selected milestone with duplicates and keep the valid milestones
          valid_selection <- selected_milestones[1:(length(selected_milestones) - 1)]  # Keep the previous valid selections

          # Update the selectize input, keeping the valid selections selected
          updateSelectizeInput(session, "milestone_existing",
                               choices = open_milestone_names,
                               selected = valid_selection)  # Keep only the valid selections

          return()  # Exit the loop after rejecting the new milestone
        }
      }
    })

    archive_files <- reactive({
      selected_milestones <- selected_milestones_rv()  # Access the reactive value here

      if (is.null(selected_milestones) || length(selected_milestones) == 0) {
        return(character(0))  # Return an empty vector if no milestones are selected
      }

      # Check if the relevant_files checkbox is TRUE (checked)
      relevant_files_checked <- isTRUE(input$relevant_files)  # More explicit check for TRUE

      if (relevant_files_checked) {
        # When 'relevant_files' is checked (TRUE), combine relevant_file and title as 'archive_files'
        archive_files <- issues_milestone_df %>%
          filter(milestone_title %in% !!selected_milestones) %>%  # Apply milestone filter
          dplyr::mutate(relevant_file = trimws(relevant_file), title = trimws(title)) %>%  # Trim whitespaces
          dplyr::mutate(relevant_file = na_if(relevant_file, ""), title = na_if(title, "")) %>%  # Handle empty strings
          dplyr::select(relevant_file, title) %>%  # Select relevant_file and title columns
          unlist()  # Combine into a single vector

        # Keep only unique values
        archive_files <- unique(archive_files)

      } else {
        archive_files <- issues_milestone_df %>%
          filter(milestone_title %in% !!selected_milestones) %>%
          pull(title) %>%
          unique() %>%
          sort()
      }

    })



    milestone_commit_df <- reactive({
      base <- local_commit_df

      # If archive_files is empty, return an empty data frame with the same columns
      archive_files_list <- archive_files()
      if (length(archive_files_list) == 0) {
        return(base[0, c("title", "commit", "milestone_title"), drop = FALSE])
      }

      base %>%
        filter(title %in% archive_files_list) %>%  # Filter by archive files (reactive)
        distinct(title, commit, milestone_title, .keep_all = TRUE) %>%  # Ensure distinct entries
        select(title, commit, milestone_title) %>%  # Select relevant columns
        arrange(title)  # Sort by title
    })

    observeEvent(input$relevant_files, {
      # When the relevant_files checkbox changes, print the milestone_commit_df
      df <- milestone_commit_df()  # Get the current data from milestone_commit_df
      if (!is.null(df) && nrow(df) > 0) {
        print("Milestone Commit DF:")
        print(df)  # Print the dataframe to console (or use other debugging methods)
      } else {
        print("No data in milestone_commit_df")
      }
    }, ignoreInit = TRUE)

    output$main_panel_dynamic <- renderUI({
      tryCatch({
        df <- milestone_commit_df()
        items_from_milestone_df <- if (!is.null(df) && nrow(df) > 0) {
          sort(unique(na.omit(df$title)))
        } else {
          character(0)
        }

        sel <- selected_items()
        has_sel <- !is.null(sel) && length(sel) > 0

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

        ui_parts <- c(
          ui_parts,
          list(
            additonal_archive_render_selected_list(
              input = input,
              ns = ns,
              items = sel,
              depth = 0,
              output = output,
              milestone_commit_df = milestone_commit_df
            )
          ),
          additonal_archive_isolate_rendered_list(
            input = input,
            session = session,
            items   = sel,
            local_commit_df = local_commit_df,
            milestone_commit_df = milestone_commit_df
          )
        )

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
