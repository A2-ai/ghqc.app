#' @import shiny
#' @importFrom shinyvalidate InputValidator sv_required
#' @importFrom dplyr filter pull select distinct mutate
#' @importFrom tidyr separate_rows
#' @importFrom glue glue
#' @importFrom log4r warn error info debug
#' @importFrom shinyjs enable disable addClass removeClass delay
#' @importFrom waiter Waiter spin_1 spin_2 waiter_hide
#' @importFrom gert git_status
#' @importFrom rprojroot find_rstudio_root_file
#' @importFrom purrr map_dfr
#' @importFrom tibble tibble
#' @importFrom utils zip
NULL

ghqc_archive_server <- function(id, root_dir, milestone_df, local_branch) {
  observe({
    req(root_dir)
    waiter_hide()
  })

  root_dir_rv <- reactive(root_dir)

  selected_files <- treeNavigatorServer(
    id,
    rootFolder = root_dir_rv,
    search = FALSE,
    all.files = FALSE
  )

  repo_name <- normalizePath(root_dir, mustWork = FALSE) |>
    basename() |>
    as.character()

  if (is.na(repo_name) || !nzchar(repo_name)) {
    repo_name <- "repo"
  }

  validator <- shinyvalidate::InputValidator$new()

  moduleServer(id, function(input, output, session) {
    reset_triggered <- reactiveVal(FALSE)
    session$onSessionEnded(function() {
      if (!isTRUE(isolate(reset_triggered()))) {
        stopApp()
      }
    })

    w_load_items <- waiter::Waiter$new(
      id = session$ns("main_panel_dynamic"),
      html = shiny::tagList(waiter::spin_2()),
      color = "white"
    )

    w_full_screen <- waiter::Waiter$new(
      id = NULL,
      html = shiny::tagList(
        waiter::spin_2(),
        shiny::tags$br(),
        shiny::tags$h4(
        )
      ),
      color = "rgba(255, 255, 255, 0.9)"
    )

    w_open_issues <- waiter::Waiter$new(
      id = NULL, # Full screen waiter
      html = shiny::tagList(
        waiter::spin_2(),
        shiny::tags$br(),
        shiny::tags$h4(
          "Fetching open issues...",
          style = "color: #333; margin-top: 20px;"
        )
      ),
      color = "rgba(255, 255, 255, 0.9)"
    )

    # Panel Setup Start
    output$sidebar <- shiny::renderUI({
      shiny::tagList(
        shiny::selectizeInput(
          session$ns("selected_milestones"),
          label = "Select Milestone(s)",
          choices = "",
          multiple = TRUE,
          width = "100%"
        ),
        shiny::checkboxInput(
          session$ns("include_open_milestones"),
          "Include Open Milestones",
          value = FALSE
        ),
        shiny::checkboxInput(
          session$ns("include_relevant_files"),
          "Include Relevant Files",
          value = FALSE
        ),
        shiny::checkboxInput(
          session$ns("include_open_issues"),
          "Include Open Issues",
          value = FALSE
        ),
        shiny::textInput(
          session$ns("archive_name"),
          "Archive Name",
          value = ""
        ),
        shiny::checkboxInput(
          session$ns("flatten"),
          "Flatten Directory Structure",
          value = FALSE
        ),
        shiny::div(
          style = "font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif !important; font-weight: bold;",
          "Select Additional File(s) for Archive"
        ),
        treeNavigatorUI(session$ns("treeNavigator"))
      )
    })

    # set milestone choices based on open milestones checkbox
    # keeps milestone on the input if open milestone check box is checked
    shiny::observeEvent(input$include_open_milestones, {
      milestone_choices <- if (input$include_open_milestones) {
        milestone_df$name
      } else {
        milestone_df |>
          dplyr::filter(!open) |>
          dplyr::pull(name)
      }
      previously_selected <- shiny::isolate(input$selected_milestones)
      if (is.null(previously_selected)) {
        previously_selected <- ""
      }

      shiny::updateSelectizeInput(
        session,
        "selected_milestones",
        choices = milestone_choices,
        selected = previously_selected
      )
    })
    # Deriving outside to use when archiving files
    suggested_archive_name <- shiny::reactive({
      validator$add_rule("archive_name", shinyvalidate::sv_required())
      name <- c(repo_name, input$selected_milestones) |>
        paste0(collapse = "-") |>
        gsub(pattern = " ", replacement = "-")

      file.path("archive", paste0(name, ".zip"))
    })

    shiny::observe({
      if (
        is.null(suggested_archive_name()) || !nzchar(suggested_archive_name())
      ) {
        return()
      }
      shiny::updateTextInput(
        session,
        "archive_name",
        value = suggested_archive_name()
      )
    })

    # Panel Setup End

    # Collect Issues and Commits - Start with closed issues only
    issues_df <- shiny::reactiveVal()

    # Load closed issues at startup
    closed_issues <- get_closed_issues_in_repo() |>
      purrr::map_dfr(function(issue) {
        debug(
          .le$logger,
          glue::glue("Processing closed issue #{issue$number}: {issue$title}")
        )
        qc_result <- get_qc_approval_or_latest(issue)
        issue_branch <- get_branch_from_issue_body(issue$body)
        relevant_files <- get_relevant_files(issue, issue$milestone$title) |>
          dplyr::pull(relevant_file_name) |>
          paste0(collapse = ", ")

        tibble::tibble(
          issue_number = issue$number,
          milestone_name = issue$milestone$title,
          title = issue$title,
          open = identical(issue$state, "open"),
          qc_commit = qc_result$commit,
          approved = qc_result$approved,
          relevant_files = relevant_files,
          issue_branch = issue_branch
        )
      })

    # Set initial data to closed issues only
    issues_df(closed_issues)

    # Cache for open issues to avoid refetching
    open_issues_cache <- shiny::reactiveVal(NULL)

    # Handle loading open issues when checkbox is checked
    shiny::observeEvent(
      input$include_open_issues,
      {
        if (input$include_open_issues) {
          # Check if we already have open issues cached
          if (is.null(open_issues_cache())) {
            # Show full-screen waiter while loading open issues
            w_open_issues$show()

            # Load open issues and cache them
            debug(
              .le$logger,
              "Loading open issues due to checkbox being checked (first time)"
            )
            open_issues <- get_open_issues_in_repo() |>
              purrr::map_dfr(function(issue) {
                debug(
                  .le$logger,
                  glue::glue(
                    "Processing open issue #{issue$number}: {issue$title}"
                  )
                )
                qc_result <- get_qc_approval_or_latest(issue)
                issue_branch <- get_branch_from_issue_body(issue$body)
                relevant_files <- get_relevant_files(
                  issue,
                  issue$milestone$title
                ) |>
                  dplyr::pull(relevant_file_name) |>
                  paste0(collapse = ", ")

                tibble::tibble(
                  issue_number = issue$number,
                  milestone_name = issue$milestone$title,
                  title = issue$title,
                  open = identical(issue$state, "open"),
                  qc_commit = qc_result$commit,
                  approved = qc_result$approved,
                  relevant_files = relevant_files,
                  issue_branch = issue_branch
                )
              })

            # Cache the open issues
            open_issues_cache(open_issues)

            info(
              .le$logger,
              glue::glue("Loaded and cached {nrow(open_issues)} open issues")
            )

            # Hide full-screen waiter after open issues are found
            w_open_issues$hide()
          } else {
            # Use cached open issues
            debug(.le$logger, "Using cached open issues")
            open_issues <- open_issues_cache()
          }

          # Combine closed and open issues
          combined_issues <- dplyr::bind_rows(issues_df(), open_issues)
          issues_df(combined_issues)

          info(
            .le$logger,
            glue::glue(
              "Combined issues: {nrow(issues_df())} total ({nrow(closed_issues)} closed + {nrow(open_issues)} open)"
            )
          )
        } else {
          # Reset to only closed issues
          debug(
            .le$logger,
            "Resetting to closed issues only due to checkbox being unchecked"
          )
          issues_df(closed_issues)
        }
      },
      ignoreInit = TRUE
    )

    branch_df <- shiny::reactive({
      req(issues_df())
      issues_df() |>
        dplyr::select(milestone_name, issue_branch) |>
        dplyr::distinct(milestone_name, issue_branch) |>
        dplyr::mutate(branch = issue_branch)
    })

    local_commits <- get_local_log() %>%
      dplyr::mutate(approved = FALSE)

    issues <- shiny::reactive({
      req(issues_df())
      issues_df() %>%
        select(
          commit = qc_commit,
          milestone_name,
          file = title,
          approved = approved
        )
    })

    commit_df <- shiny::reactive({
      req(issues())
      full_join(
        local_commits %>% dplyr::select(commit, file, approved),
        issues(),
        by = c("commit", "file")
      ) %>%
        mutate(approved = coalesce(approved.y, approved.x)) %>%
        select(commit, file, milestone_name, approved)
    })

    # Reactive to track if archive files are being updated (prevents UI triggers)
    milestone_file_dups <- shiny::reactiveVal(FALSE)

    # Reactive to track if open issues handler has dealt with duplicates
    open_issues_handled_dups <- shiny::reactiveVal(FALSE)

    # Duplicate checker for milestone selection changes - handles duplicates from milestone conflicts
    shiny::observeEvent(c(input$selected_milestones,input$include_open_issues), {
      # Don't proceed if open issues handler has already dealt with duplicates
      if (open_issues_handled_dups()) {
        milestone_file_dups(FALSE)
        return()
      }

      selected_milestones <- input$selected_milestones
      if (length(selected_milestones) <= 1) {
        milestone_file_dups(FALSE)
        open_issues_handled_dups(FALSE)
        return()
      }

      debug(
        .le$logger,
        glue::glue(
          "Checking for duplicate files across milestones: {paste(selected_milestones, collapse = ', ')}"
        )
      )
      subset_issues <- issues_df() |>
        dplyr::filter(.data$milestone_name %in% selected_milestones)

      dup_titles <- subset_issues$title |>
        trimws() |>
        (\(x) x[!is.na(x) & nzchar(x)])()
      dup_titles <- unique(dup_titles[duplicated(dup_titles)])

      if (length(dup_titles) == 0) {
        milestone_file_dups(FALSE)
        open_issues_handled_dups(FALSE)
        return()
      }

      warn(
        .le$logger,
        glue::glue(
          "Duplicate files found across milestones: {paste(dup_titles, collapse = ', ')}"
        )
      )

      # Find which milestone to remove by identifying the one with duplicates
      # For each duplicate file, find which milestones contain it and remove the second one
      milestone_to_remove <- length(selected_milestones) # default to latest
      milestone_to_remove_name <- selected_milestones[milestone_to_remove]

      for (dup_title in dup_titles) {
        milestones_with_dup <- subset_issues |>
          dplyr::filter(.data$title == dup_title) |>
          dplyr::pull(.data$milestone_name) |>
          unique()

        # Find which of the selected milestones contain this duplicate file
        selected_with_dup <- intersect(selected_milestones, milestones_with_dup)

        if (length(selected_with_dup) >= 2) {
          # Find the positions of these milestones in the selection order
          positions <- match(selected_with_dup, selected_milestones)
          # Remove the second occurrence (later in selection order)
          milestone_to_remove <- sort(positions)[2]
          milestone_to_remove_name <- selected_milestones[milestone_to_remove]
          break # Exit after finding the first duplicate to resolve
        }
      }

      shiny::showModal(
        shiny::modalDialog(
          title = shiny::tags$div(
            style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
            shiny::tags$div(
              shiny::modalButton("Return"),
              style = "flex: 0 0 auto;"
            ),
            shiny::tags$div(
              "Duplicate Files Found",
              style = "flex: 1 1 auto; text-align: center; font-weight: bold; font-size: 20px;"
            ),
            shiny::tags$div(style = "flex: 0 0 auto;")
          ),
          glue::glue("There are duplicate files detected between milestones. The milestone '{milestone_to_remove_name}' (which contains the duplicate) has been removed."),
          easyClose = TRUE,
          footer = NULL
        )
      )

      shiny::updateSelectizeInput(
        session,
        "selected_milestones",
        selected = selected_milestones[-milestone_to_remove]
      )

      milestone_file_dups(TRUE)
    })

    # Duplicate checker for open issues checkbox - only uncheck if open issues INTRODUCES new duplicates
    shiny::observeEvent(
      input$include_open_issues,
      {
        if (!input$include_open_issues) {
          open_issues_handled_dups(FALSE)
          return()
        } # Only check when turning ON open issues

        selected_milestones <- input$selected_milestones
        if (length(selected_milestones) <= 1) {
          open_issues_handled_dups(FALSE)
          return()
        }

        debug(
          .le$logger,
          glue::glue(
            "Checking if including open issues introduces new duplicates across milestones: {paste(selected_milestones, collapse = ', ')}"
          )
        )

        # First check: do duplicates exist WITHOUT open issues?
        closed_only_issues <- issues_df() |>
          dplyr::filter(.data$milestone_name %in% selected_milestones, !open)

        closed_only_dups <- closed_only_issues$title |>
          trimws() |>
          (\(x) x[!is.na(x) & nzchar(x)])()
        closed_only_dups <- unique(closed_only_dups[duplicated(
          closed_only_dups
        )])

        # If duplicates already existed without open issues, let the milestone observer handle it
        if (length(closed_only_dups) > 0) {
          debug(
            .le$logger,
            "Duplicates already exist without open issues - letting milestone observer handle it"
          )
          open_issues_handled_dups(FALSE)
          return()
        }

        # Second check: do duplicates exist WITH open issues?
        all_issues <- issues_df() |>
          dplyr::filter(.data$milestone_name %in% selected_milestones)

        all_dups <- all_issues$title |>
          trimws() |>
          (\(x) x[!is.na(x) & nzchar(x)])()
        all_dups <- unique(all_dups[duplicated(all_dups)])

        # If no new duplicates were introduced by open issues, we're done
        if (length(all_dups) == 0) {
          open_issues_handled_dups(FALSE)
          return()
        }

        # Including open issues introduced NEW duplicates, uncheck open issues
        warn(
          .le$logger,
          glue::glue(
            "Open issues introduced new duplicate files: {paste(all_dups, collapse = ', ')}"
          )
        )

        # Set flag to prevent milestone handler from also acting
        open_issues_handled_dups(TRUE)

        shiny::showModal(
          shiny::modalDialog(
            title = shiny::tags$div(
              style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
              shiny::tags$div(
                shiny::modalButton("Return"),
                style = "flex: 0 0 auto;"
              ),
              shiny::tags$div(
                "Duplicate Files Found",
                style = "flex: 1 1 auto; text-align: center; font-weight: bold; font-size: 20px;"
              ),
              shiny::tags$div(style = "flex: 0 0 auto;")
            ),
            "Including open issues introduced duplicate files. Open issues have been unchecked.",
            easyClose = TRUE,
            footer = NULL
          )
        )

        # Uncheck the open issues checkbox instead of removing milestones
        shiny::updateCheckboxInput(
          session,
          "include_open_issues",
          value = FALSE
        )

      },
      ignoreInit = TRUE
    )

    previously_warned_duplicates <- shiny::reactiveVal()
    archive_files <- shiny::reactiveVal()

    # Reactive value that tracks any changes on the right side of the archive app
    right_side_changes <- shiny::reactive({
      # Track files (left column of right side)
      files_state <- archive_files()

      # Track all milestone and commit selections for rendered items
      current_items <- rendered_items()
      selections_state <- list()

      if (length(current_items) > 0) {
        for (item in current_items) {
          milestone_id <- generate_input_id("milestone", item)
          commit_id <- generate_input_id("commit", item)

          selections_state[[paste0("milestone_", item)]] <- input[[milestone_id]]
          selections_state[[paste0("commit_", item)]] <- input[[commit_id]]
        }
      }

      # Create a comprehensive state object that captures all right side changes
      list(
        files = files_state,
        selections = selections_state,
        rendered_items = current_items,
        timestamp = Sys.time()
      )
    })

    # Track waiter state to prevent multiple shows/hides
    waiter_active <- reactiveVal(FALSE)

    # Debounced reactive to detect when changes have stopped
    right_side_stable <- shiny::debounce(right_side_changes, 300)

    # Show waiter when right side starts changing (only if not already active)
    shiny::observeEvent(right_side_changes(), {
      if (!waiter_active()) {
        # Hide right side content with CSS visibility to preserve scroll
        shinyjs::runjs(glue::glue("$('#{session$ns('grid_container')}').css('visibility', 'hidden');"))
        # Show full-screen waiter (grays out everything, spinner in center of screen)
        w_full_screen$show()
        waiter_active(TRUE)
      }
    }, ignoreInit = TRUE)

    # Hide waiter only when changes have completely stopped and stabilized
    shiny::observeEvent(right_side_stable(), {
      if (waiter_active()) {
        w_full_screen$hide()
        # Show right side content again
        shinyjs::runjs(glue::glue("$('#{session$ns('grid_container')}').css('visibility', 'visible');"))
        waiter_active(FALSE)
      }
    }, ignoreInit = TRUE)
    shiny::observeEvent(
      c(
        input$selected_milestones,
        selected_files(),
        input$include_open_issues,
        input$include_relevant_files,
        issues_df()
      ),
      {
        # Log include open issues checkbox changes
        if (!is.null(input$include_open_issues)) {
          debug(
            .le$logger,
            glue::glue("Include Open Issues: {input$include_open_issues}")
          )
        }

        # Log include relevant files checkbox changes
        if (!is.null(input$include_relevant_files)) {
          debug(
            .le$logger,
            glue::glue("Include Relevant Files: {input$include_relevant_files}")
          )
        }
        issue_files <- issues_df() |>
          dplyr::filter(milestone_name %in% input$selected_milestones)
        # isTRUE handles null
        if (!isTRUE(input$include_open_issues)) {
          issue_files <- issue_files |> dplyr::filter(!open)
        }

        files <- issue_files |>
          dplyr::pull(title) |>
          trimws() |>
          (\(x) x[!is.na(x) & nzchar(x)])() |>
          unique()

        if (isTRUE(input$include_relevant_files)) {
          rel <- issue_files |>
            dplyr::mutate(
              relevant_files = ifelse(
                is.na(relevant_files) | relevant_files == "",
                NA_character_,
                relevant_files
              )
            ) |>
            tidyr::separate_rows(relevant_files, sep = ",") |>
            dplyr::pull(relevant_files) |>
            trimws() |>
            (\(x) x[!is.na(x) & nzchar(x)])() |>
            unique()

          files <- union(files, rel)
        }

        files <- c(files, selected_files())
        duplicated_files <- files[duplicated(files)]

        if (
          length(duplicated_files) == 0 ||
          all(duplicated_files %in% previously_warned_duplicates())
        ) {
          archive_files(files)
          return()
        }

        previously_warned_duplicates(c(
          previously_warned_duplicates(),
          duplicated_files
        ))

        archive_files(unique(files))

        shiny::showModal(shiny::modalDialog(
          title = shiny::tags$div(
            style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
            shiny::tags$div(
              shiny::modalButton("Return"),
              style = "flex: 0 0 auto;"
            ),
            shiny::tags$div(
              "Duplicate Files Found",
              style = "flex: 1 1 auto; text-align: center; font-weight: bold; font-size: 20px;"
            ),
            shiny::tags$div(style = "flex: 0 0 auto;")
          ),
          paste(
            "The following files are in selected milestones:",
            paste(duplicated_files, collapse = ", "),
            ". To avoid conflicts, please uncheck these files."
          ),
          easyClose = TRUE,
          footer = NULL
        ))
      }
    )

    output$main_panel_dynamic <- renderUI({
      session$sendCustomMessage("adjust_grid", id)

      shiny::tagList(
        shiny::div(
          id = session$ns("grid_container"),
          class = "grid-container-depth-0",
          style = "display: grid; grid-template-columns: 1fr 1.2fr 1.2fr; gap: 10px 12px; align-items: start;",
          shiny::div(shiny::tags$strong("Files")),
          shiny::div(shiny::tags$strong("Milestones")),
          shiny::div(shiny::tags$strong("Commits"))
        )
      )
    })

    rendered_items <- shiny::reactiveVal(character(0))

    # Timer for waiter after items change

    shiny::observeEvent(
      archive_files(),
      {
        items_to_add <- setdiff(archive_files(), rendered_items())
        items_to_remove <- setdiff(rendered_items(), archive_files())


        # Log items being rendered
        if (length(items_to_add) > 0) {
          info(
            .le$logger,
            glue::glue(
              "Rendering {length(items_to_add)} new items: {paste(items_to_add, collapse = ', ')}"
            )
          )
        }
        rendered_items(archive_files())

        if (length(items_to_remove) > 0) {
          info(
            .le$logger,
            glue::glue(
              "Removing {length(items_to_remove)} items from render: {paste(items_to_remove, collapse = ', ')}"
            )
          )
        }

        # Handle removals first
        for (item in items_to_remove) {
          milestone_id <- generate_input_id("milestone", item)
          row_id <- generate_input_id("item_row", item)

          shiny::updateSelectizeInput(
            session,
            inputId = milestone_id,
            choices = character(0),
            selected = NULL,
            server = TRUE
          )

          observeEvent(
            input[[milestone_id]],
            {
              shiny::removeUI(selector = paste0("#", session$ns(row_id)))
            },
            once = TRUE,
            ignoreInit = FALSE
          )

          file_id <- session$ns(generate_input_id("item_row", item))
          attr_selector <- paste0("[id='", file_id, "']")
          shiny::removeUI(selector = attr_selector)
        }

        for (item in items_to_add) {
          shiny::insertUI(
            selector = paste0("#", session$ns("grid_container")),
            where = "beforeEnd",
            ui = create_single_item_ui(item, session$ns)
          )

          local({
            this_item <- item

            validator$add_rule(
              generate_input_id("milestone", this_item),
              function(value) {
                file_commits_df <- commit_df() |>
                  dplyr::filter(file == this_item)
                milestone_choices <- file_commits_df$milestone_name |>
                  Filter(f = Negate(is.na))

                if (length(milestone_choices) == 0) {
                  return("No milestones available")
                }

                if (!nzchar(value) || is.null(value)) {
                  return(NULL)
                }
                issue_milestone_selected <- value

                if (
                  is.null(issue_milestone_selected) ||
                  issue_milestone_selected == ""
                ) {
                  return(NULL)
                }
                open_milestone_names <- issues_df() %>%
                  dplyr::filter(title == this_item, open) %>%
                  dplyr::pull(milestone_name)

                if (issue_milestone_selected %in% open_milestone_names) {
                  return(
                    "Issue is open"
                  )
                }
                return(NULL)
              }
            )

            validator$add_rule(
              generate_input_id("commit", this_item),
              function(value) {
                selected_milestones <- input$selected_milestones
                milestone_branch <- branch_df()$branch[
                  branch_df()$milestone_name %in% selected_milestones
                ]

                if (!is.null(value) && nzchar(value)) {
                  return(NULL)
                }

                if (length(file_commits_df$commit) == 0) {
                  return("No available commits")
                }

                if (
                  length(selected_milestones) == 0 ||
                  local_branch %in% milestone_branch
                ) {
                  return("Required")
                }

                return(
                  "Current branch does not match any issue branch. May be missing relevant commits"
                )
              }
            )

            # Add preview click handler
            shiny::observeEvent(
              input[[generate_input_id("preview", this_item)]],
              {
                commit_value <- input[[generate_input_id("commit", this_item)]]
                show_file_preview_modal(this_item, commit_value)
              },
              ignoreInit = TRUE
            )


            file_commits_df <- commit_df() |> dplyr::filter(file == this_item)
            session$onFlushed(
              function() {
                shiny::updateSelectizeInput(
                  session,
                  inputId = generate_input_id("milestone", this_item),
                  choices = c(
                    "",
                    file_commits_df$milestone_name |>
                      Filter(f = Negate(is.na))
                  ),
                  selected = ""
                )
                # Create named choices: display format as names, long SHA as values
                commit_choices <- setNames(
                  local_commits$commit[match(
                    file_commits_df$commit,
                    local_commits$commit
                  )],
                  local_commits$commit_display[match(
                    file_commits_df$commit,
                    local_commits$commit
                  )]
                )
                # Filter out any NA values
                commit_choices <- commit_choices[
                  !is.na(names(commit_choices)) & !is.na(commit_choices)
                ]

                shiny::updateSelectizeInput(
                  session,
                  generate_input_id("commit", this_item),
                  choices = commit_choices,
                  selected = ""
                )
              },
              once = TRUE
            )
          })
        }
      },
      ignoreInit = TRUE
    )
    shiny::observeEvent(
      c(
        rendered_items(),
        input$selected_milestones,
        input$include_open_issues,
        milestone_file_dups(),
        open_issues_handled_dups()
      ),
      {
        # Require both milestone_file_dups and open_issues_handled_dups to be FALSE to proceed
        req(!milestone_file_dups() && !open_issues_handled_dups())
        for (item in rendered_items()) {
          milestone_input <- input[[generate_input_id("milestone", item)]] %||%
            ""
          selected_globals <- input$selected_milestones %||% character(0)

          # Check if we need to clear invalid selections
          if (nzchar(milestone_input)) {
            file_commits_df <- commit_df() |> dplyr::filter(.data$file == item)
            milestone_choices <- file_commits_df$milestone_name |>
              Filter(f = Negate(is.na))

            # Only clear if the current selection is not available for this specific item
            if (!milestone_input %in% milestone_choices) {
              # Clear the selectize input
              shiny::updateSelectizeInput(
                session,
                inputId = generate_input_id("milestone", item),
                choices = milestone_choices,
                selected = "",
                server = TRUE
              )
              milestone_input <- ""
            }
          }

          #  Only skip if milestone is already set to the correct matching milestone
          file_commits_df <- commit_df() |> dplyr::filter(.data$file == item)
          milestone_choices <- file_commits_df$milestone_name |>
            Filter(f = Negate(is.na))
          matching_milestones <- selected_globals[
            selected_globals %in% milestone_choices
          ]

          if (length(matching_milestones) > 0) {
            milestone_choices <- matching_milestones
          }

          if (
            length(matching_milestones) > 0 &&
            nzchar(milestone_input) &&
            milestone_input == matching_milestones[1]&&
            length(milestone_choices) == 1
          ) {
            next
          }

          file_commits_df <- commit_df() |> dplyr::filter(.data$file == item)
          milestone_choices <- file_commits_df$milestone_name |>
            Filter(f = Negate(is.na))
          matching_milestones <- selected_globals[
            selected_globals %in% milestone_choices
          ]

          if (length(matching_milestones) > 0) {
            milestone_choices <- matching_milestones
          }

          # Handle include_open_issues toggle - update milestone choices but preserve selection logic
          include_open_issues_changed <- !is.null(input$include_open_issues)

          if (include_open_issues_changed) {
            # Recalculate milestone choices with the updated issues_df that includes/excludes open issues
            file_commits_df <- commit_df() |> dplyr::filter(.data$file == item)
            milestone_choices <- file_commits_df$milestone_name |>
              Filter(f = Negate(is.na))
            matching_milestones <- selected_globals[
              selected_globals %in% milestone_choices
            ]

            if (length(matching_milestones) > 0) {
              milestone_choices <- matching_milestones
            }

            shiny::updateSelectizeInput(
              session,
              inputId = generate_input_id("milestone", item),
              choices = milestone_choices,
              selected = milestone_input,
              server = TRUE
            )

            # Skip further processing if milestone is already set to the correct matching milestone
            if (
              length(matching_milestones) > 0 &&
              nzchar(milestone_input) &&
              milestone_input == matching_milestones[1] &&
              length(milestone_choices) == 1
            ) {
              next
            }
          }

          # Don't rerender if milestone is already set to the correct matching milestone
          if (
            length(matching_milestones) != 0 &&
            nzchar(milestone_input) &&
            milestone_input == matching_milestones[1] &&
            length(milestone_choices) == 1
          ) {
            next
          }
          local({
            this_item <- item
            file_commits_df <- commit_df() |>
              dplyr::filter(.data$file == this_item)

            milestone_choices <- file_commits_df$milestone_name |>
              Filter(f = Negate(is.na))
            selected_globals <- input$selected_milestones
            if (is.null(selected_globals)) {
              selected_globals <- character(0)
            }

            matching_milestones <- selected_globals[
              selected_globals %in% milestone_choices
            ]

            if (length(matching_milestones) != 0) {
              # Global milestones take priority
              milestone_selected <- matching_milestones[1]

              session$onFlushed(
                function() {
                  shiny::updateSelectizeInput(
                    session,
                    inputId = generate_input_id("milestone", this_item),
                    choices = matching_milestones,
                    selected = milestone_selected,
                    server = TRUE
                  )
                },
                once = TRUE
              )
            }
            shiny::observeEvent(
              input[[generate_input_id("milestone", this_item)]],
              {
                milestone_selection <- input[[generate_input_id(
                  "milestone",
                  this_item
                )]]

                commits_shas <- commit_df() |>
                  dplyr::filter(.data$file == this_item) |>
                  dplyr::pull(.data$commit) |>
                  unique()

                if (!(milestone_selection %in% c(""))) {
                  commits_shas <- commit_df() |>
                    dplyr::filter(
                      .data$file == this_item,
                      .data$milestone_name %in% milestone_selection
                    ) |>
                    dplyr::pull(.data$commit) |>
                    unique()
                }

                # Create named choices: display format as names, long SHA as values
                if (length(commits_shas) > 0) {
                  commit_choices <- setNames(
                    commits_shas,
                    paste0(
                      stringr::str_extract(commits_shas, "^.{1,7}"),
                      " | ",
                      local_commits$message[match(
                        commits_shas,
                        local_commits$commit
                      )]
                    )
                  )
                  commit_choices <- commit_choices[
                    !is.na(names(commit_choices)) & !is.na(commit_choices)
                  ]
                } else {
                  commit_choices <- character(0)
                }

                # Preserve existing commit selection if it's still valid
                current_commit_selection <- input[[generate_input_id("commit", this_item)]]
                new_selection <- if (!is.null(current_commit_selection) &&
                                     current_commit_selection %in% commit_choices) {
                  # Keep existing selection if it's still available
                  current_commit_selection
                } else if (!(milestone_selection %in% c("")) && length(commit_choices)) {
                  # Only auto-select first commit if no previous selection exists
                  commit_choices[[1]]
                } else {
                  character(0)
                }

                shiny::updateSelectizeInput(
                  session,
                  generate_input_id("commit", this_item),
                  choices = commit_choices,
                  selected = new_selection,
                  server = TRUE
                )
              },
              ignoreInit = TRUE
            )
          })
        }
      },
      ignoreInit = FALSE
    )


    observeEvent(input$create_archive, ignoreInit = TRUE, {
      info(
        .le$logger,
        "Create archive button clicked - starting archive creation process"
      )

      # pull from reactive and clean to a character vector
      items_raw <- archive_files() %||% character(0)
      archive_items <- as.character(unlist(items_raw, use.names = FALSE))
      archive_items <- archive_items[nzchar(archive_items)] # drop only empty strings

      # Warn if flattening would create basename collisions among UNIQUE items only
      if (isTRUE(input$flatten) && length(archive_items) >= 2) {
        # First, deduplicate the archive items (same file selected multiple ways is OK)
        unique_archive_items <- unique(archive_items)

        # Only check for flattening conflicts among unique file paths
        if (length(unique_archive_items) >= 2) {
          flatten_file <- basename(unique_archive_items)
          dup_flatten_file <- unique(flatten_file[
            duplicated(flatten_file) & !is.na(flatten_file)
          ])

          if (length(dup_flatten_file) > 0) {
            warn(
              .le$logger,
              glue::glue(
                "Filename conflicts detected when flattening: {paste(dup_flatten_file, collapse = ', ')}"
              )
            )
            groups <- lapply(dup_flatten_file, function(nm) {
              paths <- unique_archive_items[
                !is.na(basename(unique_archive_items)) &
                  basename(unique_archive_items) == nm
              ]
              tagList(
                lapply(paths, function(p) {
                  tagList("\u2022", tags$code(p), tags$br())
                }),
                tags$br()
              )
            })

            showModal(modalDialog(
              title = "Duplicate File Names When Flattening",
              tagList(
                p(
                  "These files would end up with the same name after flattening. ",
                  "Please unselect one of these files."
                ),
                div(groups)
              ),
              easyClose = TRUE,
              footer = tagList(modalButton("Close"))
            ))
            return()
          }
        }
      }

      # Use the archive name from the input field
      archive_name <- trimws(input$archive_name)
      info(.le$logger, glue::glue("Creating archive: {archive_name}"))

      archive_selected_items(
        input = input,
        session = session,
        archive_name = archive_name,
        root_dir = root_dir,
        flatten = isTRUE(input$flatten),
        archive_items = archive_items,
        commit_df = commit_df()
      )
    })

    #Checking for required errors
    observe({
      has_required_error <- FALSE

      if (is.null(input$archive_name) || !nzchar(trimws(input$archive_name))) {
        has_required_error <- TRUE
      }

      current_items <- rendered_items()
      if (length(current_items) == 0) {
        # No items to archive
        has_required_error <- TRUE
      } else {
        for (item in current_items) {
          commit_id <- generate_input_id("commit", item)
          commit_value <- input[[commit_id]]

          if (is.null(commit_value) || !nzchar(commit_value)) {
            has_required_error <- TRUE
            break
          }
        }
      }

      if (has_required_error) {
        shinyjs::disable("create_archive")
      } else {
        shinyjs::enable("create_archive")
      }
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

    validator$enable()
  })
}
