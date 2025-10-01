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
    # Gets closed milestones so it can default to closed milestones
    observe({
      req(all_milestone_names, open_milestone_names)

      closed_milestone_names <- setdiff(all_milestone_names, open_milestone_names)


      prev <- isolate(input$milestone_existing)
      selected <- if (is.null(prev)) character(0) else intersect(prev, closed_milestone_names)

      updateSelectizeInput(
        session,
        "milestone_existing",
        choices  = closed_milestone_names,
        selected = selected
      )
    })
    # Adds left side ui

    output$sidebar <- renderUI({
      tagList(
        selectizeInput(
          ns("milestone_existing"),
          "Select Existing Milestone",
          choices = "",
          multiple = TRUE,
          width = "100%"
        ),
        shinyWidgets::prettyCheckbox(ns("open_milestone"), "Include open milestones", value = FALSE, icon = icon("check")),
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

    # Open milestone toggle making it so all milestones are available if the button is checked
    observe({
      req(all_milestone_names, open_milestone_names)

      # Default = closed milestones
      milestone_choices <- setdiff(all_milestone_names, open_milestone_names)

      # If "Include open milestones" is checked, switch to all milestones
      if (isTRUE(input$open_milestone)) {
        milestone_choices <- all_milestone_names
      }

      # Preserve only valid selections
      prev <- isolate(input$milestone_existing)
      selected <- if (is.null(prev)) character(0) else intersect(prev, milestone_choices)

      updateSelectizeInput(
        session,
        "milestone_existing",
        choices  = milestone_choices,
        selected = selected
      )
    })

    # Defaults archive name to auto populated
    repo_name <- {
      rn <- tryCatch(basename(normalizePath(root_dir)), error = function(e) root_dir)
      rn <- as.character(rn)[1]
      if (is.na(rn) || !nzchar(rn)) rn <- "repo"
      rn
    }

    last_auto_suggest <- reactiveVal(NULL)
    user_override     <- reactiveVal(FALSE)

    auto_archive_name <- reactive({
      sel <- input$milestone_existing
      if (is.null(sel)) sel <- character(0)
      sel <- as.character(sel)

      rn <- as.character(repo_name)[1]

      # Base name: repo-milestone1-milestone2
      nm <- paste(c(rn, sel), collapse = "-")
      nm <- gsub("\\s+", "-", nm)

      # Full path with .zip extension
      file.path("archive", paste0(nm, ".zip"))
    })

    # Update text input with the suggested path
    observe({
      sugg <- auto_archive_name()
      if (is.null(sugg) || !nzchar(sugg)) return()
      updateTextInput(session, "archive_name", placeholder = sugg)
    })


    issues_in_repo <- get_all_issues_in_repo()

    # Create the base issues_milestone_df
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
        relevant_file    = relevant_file
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



    # Duplicate files across milestones warning
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

    # Gets files that are going to be archived, gives warning if a selected file is a duplicate of a file in a global milestone
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

    # Get master commit list with archive files in it

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

    items_from_milestone_df <- reactive({
      df <- milestone_commit_df()
      if (!is.null(df) && nrow(df) > 0) {
        return(sort(unique(na.omit(df$title))))
      } else {
        return(character(0))
      }
    })


  # Renders ui
    output$main_panel_dynamic <- renderUI({
      tryCatch({
        w_load_items$show()
        session$sendCustomMessage("adjust_grid", id)

        shiny::tagList(
          shiny::div(
            id = ns("grid_container"),
            class = "grid-container-depth-0",
            style = "display: grid; grid-template-columns: 1fr 1.2fr 1.2fr; gap: 10px 12px; align-items: start;",
            shiny::div(shiny::tags$strong("Files")),
            shiny::div(shiny::tags$strong("Milestones")),
            shiny::div(shiny::tags$strong("Commits"))
          )
        )
      }, error = function(e) {
        error(.le$logger, glue::glue("There was an error rendering items in right panel: {conditionMessage(e)}"))
        stopApp(); rlang::abort(conditionMessage(e))
      })
    })


    rendered_items <- shiny::reactiveVal(character(0))

    observeEvent(items_from_milestone_df(), {
      current_items  <- items_from_milestone_df()
      previous_items <- rendered_items()

      items_to_add    <- setdiff(current_items,  previous_items)
      items_to_remove <- setdiff(previous_items, current_items)

      rendered_items(current_items)
      # adds ability to add or remove a selected item
      if (length(items_to_remove)) {
        for (it in items_to_remove) {
          selector <- paste0("[id='", session$ns(generate_input_id("item_row", it)), "']")
          try(shiny::removeUI(selector = selector, multiple = TRUE, immediate = TRUE), silent = TRUE)
        }
      }

      if (length(items_to_add)) {
        for (it in items_to_add) {
          local({
            current_it <- it

            shiny::insertUI(
              selector = paste0("#", session$ns("grid_container")),
              where = "beforeEnd",
              ui = create_single_item_ui(current_it, session$ns)
            )
            #gets all file info(milestones and commits)
            df_all  <- milestone_commit_df()
            df_item <- df_all %>% dplyr::filter(.data$title == current_it)

            milestone_choices <- df_item %>%
              dplyr::pull(.data$milestone_title) %>%
              unique() %>%
              sort()

            commits_all <- df_item %>%
              dplyr::pull(.data$commit) %>%
              unique()
            # matches global milestones to milestones in a file
            selected_globals <- input$milestone_existing
            if (is.null(selected_globals)) selected_globals <- character(0)
            matching_globals <- selected_globals[selected_globals %in% milestone_choices]

            if (length(matching_globals) >= 1) {
              milestone_choices  <- matching_globals
              milestone_selected <- matching_globals[1]
              commit_choices     <- NULL
              commit_selected    <- NULL

              is_from_tree <- isTRUE(it %in% selected_items())

              if (is_from_tree && !isTRUE(input$issue_open)) {
                open_globals <- df_item %>%
                  dplyr::filter(.data$milestone_title %in% matching_globals) %>%
                  dplyr::count(.data$milestone_title, name = "n") %>%
                  dplyr::filter(.data$n == 1) %>%
                  dplyr::pull(.data$milestone_title)

                if (length(open_globals) > 0) {
                  shiny::showModal(
                    shiny::modalDialog(
                      title = "Warning",
                      paste0(
                        "The file '", it,
                        "' is in an OPEN issue for global milestone",
                        if (length(open_globals) > 1) "s " else " ",
                        paste0("'", open_globals, "'", collapse = ", "),
                        "."
                      ),
                      easyClose = TRUE,
                      footer = shiny::modalButton("OK")
                    )
                  )
                }
              }
            } else {
              # if there isn't a matching milestone allow user to pick one
              milestone_choices  <- c("N/A", milestone_choices)
              milestone_selected <- "N/A"
              commit_choices     <- commits_all
              commit_selected    <- NULL
            }

            session$onFlushed(function() {
              shiny::updateSelectizeInput(
                session,
                generate_input_id("milestone", current_it),
                choices  = milestone_choices,
                selected = milestone_selected,
                server   = TRUE
              )
              if (!is.null(commit_choices)) {
                shiny::updateSelectizeInput(
                  session,
                  generate_input_id("commit", current_it),
                  choices  = commit_choices,
                  selected = commit_selected,
                  server   = TRUE
                )
              }
            }, once = TRUE)

            # commits now match local milestone selection N/A allows all options
            milestone_input_id <- generate_input_id("milestone", current_it)
            commit_input_id    <- generate_input_id("commit",    current_it)


            shiny::observeEvent(input[[milestone_input_id]], {
              sel <- input[[milestone_input_id]]

              df_all  <- milestone_commit_df()
              df_item <- df_all %>% dplyr::filter(.data$title == current_it)

              commits_all <- df_item %>% dplyr::pull(.data$commit) %>% unique()

              if (is.null(sel) || identical(sel, "N/A") || identical(sel, "")) {
                shiny::updateSelectizeInput(
                  session, commit_input_id,
                  choices  = commits_all,
                  selected = NULL,
                  server   = TRUE
                )
              } else {
                commits_filtered <- df_item %>%
                  dplyr::filter(.data$milestone_title == sel) %>%
                  dplyr::pull(.data$commit) %>%
                  unique()

                shiny::updateSelectizeInput(
                  session, commit_input_id,
                  choices  = commits_filtered,
                  selected = if (length(commits_filtered)) commits_filtered[[1]] else NULL,
                  server   = TRUE
                )
              }
            }, ignoreInit = FALSE)


          })
        }
      }

      if (length(current_items) > 0) {
        session$sendCustomMessage("adjust_grid", id)
      }
      w_load_items$hide()
    }, ignoreInit = FALSE)


    observeEvent(list(input$milestone_existing, milestone_commit_df()), {
      current_items <- rendered_items()
      if (length(current_items) == 0) return(invisible())

      selected_globals <- input$milestone_existing
      if (is.null(selected_globals)) selected_globals <- character(0)

      df_all <- milestone_commit_df()

      for (it in current_items) {
        df_item <- df_all %>% dplyr::filter(.data$title == it)
        is_from_tree <- isTRUE(it %in% selected_items())
        milestone_choices_item <- df_item %>%
          dplyr::pull(.data$milestone_title) %>%
          unique() %>%
          sort()

        matching_globals <- selected_globals[selected_globals %in% milestone_choices_item]

        milestone_input_id <- generate_input_id("milestone", it)
        commit_input_id    <- generate_input_id("commit",    it)

        if (length(matching_globals) >= 1) {
          shiny::updateSelectizeInput(
            session, milestone_input_id,
            choices  = matching_globals,
            selected = matching_globals[[1]],
            server   = TRUE
          )
          if (is_from_tree && !isTRUE(input$issue_open)) {
            open_globals <- df_item %>%
              dplyr::filter(.data$milestone_title %in% matching_globals) %>%
              dplyr::count(.data$milestone_title, name = "n") %>%
              dplyr::filter(.data$n == 1) %>%
              dplyr::pull(.data$milestone_title)

            if (length(open_globals) > 0) {
              shiny::showModal(
                shiny::modalDialog(
                  title = "Warning",
                  paste0(
                    "The file '", it,
                    "' is in an OPEN issue for global milestone",
                    if (length(open_globals) > 1) "s " else " ",
                    paste0("'", open_globals, "'", collapse = ", "),
                    "."
                  ),
                  easyClose = TRUE,
                  footer = shiny::modalButton("OK")
                )
              )
            }
          }
        } else {
          commits_all <- df_item %>% dplyr::pull(.data$commit) %>% unique()

          shiny::updateSelectizeInput(
            session, milestone_input_id,
            choices  = c("N/A", milestone_choices_item),
            selected = "N/A",
            server   = TRUE
          )

          shiny::updateSelectizeInput(
            session, commit_input_id,
            choices  = commits_all,
            selected = NULL,
            server   = TRUE
          )
        }
      }
    }, ignoreInit = TRUE)

    observeEvent(input$create_archive, ignoreInit = TRUE, {
      items <- items_from_milestone_df()

      # Warn if flattening would create basename collisions among the current items
      if (isTRUE(input$flatten) && length(items) >= 2) {
        bn <- basename(items)
        dup_bn <- unique(bn[duplicated(bn)])
        if (length(dup_bn) > 0) {
          groups <- lapply(dup_bn, function(nm) {
            paths <- items[bn == nm]
            tagList(
              lapply(paths, function(p) tagList("• ", tags$code(p), tags$br())),
              tags$br()
            )
          })

          showModal(modalDialog(
            title = "Duplicate File Names When Flattening",
            tagList(
              p("These files would end up with the same name after flattening. ",
                "Please unselect one of these files."),
              div(groups)
            ),
            easyClose = TRUE,
            footer = tagList(modalButton("Close"))
          ))

          return()
        }
      }
      # Use user's typed value if provided; otherwise fall back to the suggestion
      raw_val <- input$archive_name
      if (is.null(raw_val)) raw_val <- ""


      archive_name <- auto_archive_name()
      if (is.null(archive_name)) fallback <- ""

      # Effective archive name = typed value or suggestion
      archive_name <- trimws(if (nzchar(raw_val)) raw_val else archive_name)


      archive_selected_items(
        input        = input,
        session      = session,
        items        = selected_items(),
        archive_name = archive_name,
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
