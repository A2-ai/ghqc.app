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

    items_from_milestone_df <- reactive({
      df <- milestone_commit_df()
      if (!is.null(df) && nrow(df) > 0) {
        return(sort(unique(na.omit(df$title))))
      } else {
        return(character(0))
      }
    })



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


    ui_mutation_in_progress <- reactiveVal(FALSE)
    rendered_items <- shiny::reactiveVal(character(0))

    # --- your original incremental renderer (unchanged except the init tweak for multiple globals) ---
    observeEvent(items_from_milestone_df(), {
      current_items  <- items_from_milestone_df()
      previous_items <- rendered_items()

      items_to_add    <- setdiff(current_items,  previous_items)
      items_to_remove <- setdiff(previous_items, current_items)

      rendered_items(current_items)

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

            # PRECOMPUTE (reactive-safe here)
            df_all  <- milestone_commit_df()
            df_item <- df_all %>% dplyr::filter(.data$title == current_it)

            milestone_choices <- df_item %>%
              dplyr::pull(.data$milestone_title) %>%
              unique() %>%
              sort()

            commits_all <- df_item %>%
              dplyr::pull(.data$commit) %>%
              unique()

            selected_globals <- input$milestone_existing
            if (is.null(selected_globals)) selected_globals <- character(0)
            matching_globals <- selected_globals[selected_globals %in% milestone_choices]

            if (length(matching_globals) >= 1) {
              init_milestone_choices  <- matching_globals
              init_milestone_selected <- matching_globals[1]
              init_commit_choices     <- NULL
              init_commit_selected    <- NULL
            } else {
              init_milestone_choices  <- c("N/A", milestone_choices)
              init_milestone_selected <- "N/A"
              init_commit_choices     <- commits_all
              init_commit_selected    <- NULL
            }

            session$onFlushed(function() {
              shiny::updateSelectizeInput(
                session,
                generate_input_id("milestone", current_it),
                choices  = init_milestone_choices,
                selected = init_milestone_selected,
                server   = TRUE
              )
              if (!is.null(init_commit_choices)) {
                shiny::updateSelectizeInput(
                  session,
                  generate_input_id("commit", current_it),
                  choices  = init_commit_choices,
                  selected = init_commit_selected,
                  server   = TRUE
                )
              }
            }, once = TRUE)

            # Per-item local milestone -> commit coupling (unchanged; listens ONLY to local milestone)
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



    # observeEvent(
    #   list(input$milestone_existing, milestone_commit_df()),  # re-run on change
    #   {
    #     items_rendered <- rendered_items()
    #     if (length(items_rendered) == 0) return()
    #
    #     milestone_df   <- milestone_commit_df()
    #     selected_global <- input$milestone_existing %||% character(0)
    #
    #     # Only do special behavior when exactly one global milestone is selected.
    #     if (length(selected_global) == 1) {
    #       for (name in items_rendered) {
    #         milestone_input_id <- generate_input_id("milestone", name)
    #         commit_input_id    <- generate_input_id("commit", name)
    #
    #         df_item <- milestone_df %>% dplyr::filter(.data$title == name)
    #
    #         # All milestones this item can have
    #         item_milestones <- df_item %>%
    #           dplyr::pull(.data$milestone_title) %>%
    #           unique() %>%
    #           sort()
    #
    #         if (selected_global %in% item_milestones) {
    #           # This item belongs to the selected global milestone → lock it
    #           updateSelectizeInput(
    #             session,
    #             milestone_input_id,
    #             choices  = selected_global,      # single option → effectively locked
    #             selected = selected_global,
    #             server   = TRUE
    #           )
    #
    #           # Show only commits under that milestone
    #           commit_choices <- df_item %>%
    #             dplyr::filter(.data$milestone_title == selected_global) %>%
    #             dplyr::pull(.data$commit) %>%
    #             unique()
    #
    #           default_commit <- if (length(commit_choices) > 0) commit_choices[[1]] else NULL
    #
    #           updateSelectizeInput(
    #             session,
    #             commit_input_id,
    #             choices  = commit_choices,
    #             selected = default_commit,
    #             server   = TRUE
    #           )
    #
    #           # Optional: truly disable the milestone select so the user can’t change it.
    #           # If you use shinyjs, uncomment:
    #           # shinyjs::disable(session$ns(milestone_input_id))
    #
    #         } else {
    #           # Item does NOT belong to the selected global milestone → leave it as-is (free reign)
    #           # (No updates here on purpose.)
    #           next
    #         }
    #       }
    #       return()
    #     }
    #   },
    #   ignoreInit = TRUE
    # )

        # observeEvent(input[[milestone_input_id]], {
        #
        #   selected_milestone_input <- input[[milestone_input_id]]
        #   if (selected_milestone_input == "N/A" || selected_milestone_input == "") {
        #     commit_choices <- milestone_df_item %>%
        #       dplyr::pull(.data$commit) %>%
        #       unique()
        #
        #     updateSelectizeInput(
        #       session,
        #       commit_input_id,
        #       choices  = commit_choices,  # Show all commits for the item
        #       selected = NULL,  # Allow free selection
        #       server   = TRUE
        #     )
        #   } else {
        #     commit_choices <- milestone_df_item %>%
        #       dplyr::filter(.data$milestone_title == selected_milestone_input) %>%
        #       dplyr::pull(.data$commit) %>%
        #       unique()
        #
        #     updateSelectizeInput(
        #       session,
        #       commit_input_id,
        #       choices  = commit_choices,
        #       selected = commit_choices,
        #       server   = TRUE
        #     )
        #   }
        # })








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
