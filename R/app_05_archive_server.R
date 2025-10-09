#' @import shiny
#' @importFrom shinyvalidate InputValidator
#' @importFrom dplyr
#' @importFrom tidyr
#' @importFrom glue glue
#' @importFrom log4r warn error info debug
#' @importFrom shinyjs enable disable addClass removeClass delay
#' @importFrom waiter Waiter spin_1 spin_2 waiter_hide
#' @importFrom gert git_status
#' @importFrom rprojroot find_rstudio_root_file
#' @importFrom purrr map_dfr
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
        shiny::checkboxInput(session$ns("include_open_milestones"), "Include Open Milestones", value = FALSE),
        shiny::checkboxInput(session$ns("include_open_issues"), "Include Open Issues", value = FALSE),
        shiny::checkboxInput(session$ns("include_relevant_files"), "Include Relevant Files", value = FALSE),
        shiny::textInput(session$ns("archive_name"), "Archive Name", value = ""),
        shiny::checkboxInput(session$ns("flatten"), "Flatten Directory Structure", value = FALSE),
        shiny::div(
          style = "font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif !important; font-weight: bold;",
          "Select Additional File(s) for Archive"
        ),
        treeNavigatorUI(session$ns("treeNavigator"))
      )
    })
    # set milestone choices based on open milestones checkbox
    shiny::observeEvent(input$include_open_milestones, {
      milestone_choices <- if (input$include_open_milestones) {
        milestone_df$name
      } else {
        milestone_df |>
          dplyr::filter(!open) |>
          dplyr::pull(name)
      }

      previously_selected <- shiny::isolate(input$selected_milestones)
      if (is.null(previously_selected)) previously_selected <- ""

      shiny::updateSelectizeInput(
        session,
        "selected_milestones",
        choices = milestone_choices,
        selected = previously_selected
      )
    })
    # Deriving outside to use when archiving files
    suggested_archive_name <- shiny::reactive({
      name <- c(repo_name, input$selected_milestones) |>
        paste0(collapse = "-") |>
        gsub(pattern = " ", replacement = "-")

      file.path("archive", paste0(name, ".zip"))
    })

    shiny::observe({
      if (is.null(suggested_archive_name()) || !nzchar(suggested_archive_name())) return()
      shiny::updateTextInput(
        session,
        "archive_name",
        placeholder = suggested_archive_name()
      )
    })

    # Panel Setup End

    # Collect Issues and Commits
    issues_df <- get_all_issues_in_repo() |>
      purrr::map_dfr(function(issue) {
        qc_commit <- get_qc_approval_or_latest(issue)
        issue_branch <- get_branch_from_issue_body(issue$body)
        relevant_files <- get_relevant_files(issue, issue$milestone$title) |>
          dplyr::pull(relevant_file_name) |>
          paste0(collapse = ", ")

        tibble::tibble(
          issue_number = issue$number,
          milestone_name = issue$milestone$title,
          title = issue$title,
          open = identical(issue$state, "open"),
          qc_commit = qc_commit,
          relevant_files = relevant_files,
          issue_branch = issue_branch
        )
      })

    branch_df <- issues_df |>
      dplyr::select(milestone_name, issue_branch) |>
      dplyr::distinct(milestone_name, issue_branch) |>
      dplyr::mutate(branch = issue_branch)

    local_commits <- get_local_log()

    issues <- issues_df %>%
      transmute(
        commit = qc_commit,
        milestone_name,
        file = title
      )

    local <- local_commits %>%
      transmute(
        commit,
        file
      )

    commit_df <- full_join(local, issues, by = c("commit", "file")) %>%
      select(commit, file, milestone_name)

    shiny::observeEvent(input$selected_milestones, {
      selected_milestones <- input$selected_milestones
      if (length(selected_milestones) <= 1) return()

      subset_issues <- issues_df |>
        dplyr::filter(.data$milestone_name %in% selected_milestones)

      dup_titles <- subset_issues$title |>
        trimws() |>
        (\(x) x[!is.na(x) & nzchar(x)])()
      dup_titles <- unique(dup_titles[duplicated(dup_titles)])

      if (length(dup_titles) == 0) return()

      shiny::showModal(
        shiny::modalDialog(
          title = shiny::tags$div(
            style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
            shiny::tags$div(shiny::modalButton("Return"), style = "flex: 0 0 auto;"),
            shiny::tags$div(
              "Duplicate Files Found",
              style = "flex: 1 1 auto; text-align: center; font-weight: bold; font-size: 20px;"
            ),
            shiny::tags$div(style = "flex: 0 0 auto;")
          ),
          "There are duplicate files detected between milestones. Please select a different milestone",
          easyClose = TRUE,
          footer = NULL
        )
      )

      shiny::updateSelectizeInput(
        session,
        "selected_milestones",
        selected = selected_milestones[-length(selected_milestones)]
      )
    })

    previously_warned_duplicates <- shiny::reactiveVal()
    archive_files <- shiny::reactiveVal()

    shiny::observeEvent(
      c(input$selected_milestones, selected_files(), input$include_open_issues, input$include_relevant_files),
      {
        issue_files <- issues_df |>
          dplyr::filter(milestone_name %in% input$selected_milestones)

        if (!input$include_open_issues) {
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
            shiny::tags$div(style = "flex: 0 0 auto;") # Empty right side
          ),
          paste(
            "The following files are already in selected milestones:",
            paste(duplicated_files, ", ")
          ),
          easyClose = TRUE,
          footer = NULL
        ))
      })

    output$main_panel_dynamic <- renderUI({
      w_load_items$show()
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

    shiny::observeEvent(
      archive_files(),
      {
        items_to_add <- setdiff(archive_files(), rendered_items())
        items_to_remove <- setdiff(rendered_items(), archive_files())
        rendered_items(archive_files())

        if (length(items_to_remove) != 0) {
          for (item in items_to_remove) {
            file_id <- session$ns(generate_input_id("item_row", item))
            attr_selector <- paste0("[id='", file_id, "']")
            shiny::removeUI(selector = attr_selector)
          }
        }

        if (length(items_to_add) != 0) {
          for (item in items_to_add) {
            shiny::insertUI(
              selector = paste0("#", session$ns("grid_container")),
              where = "beforeEnd",
              ui = create_single_item_ui(item, session$ns)
            )

            local({
              this_item <- item

              validator$add_rule(generate_input_id("milestone", this_item) , function(value){
                if (!nzchar(value) || is.null(value)) {
                  return(NULL)
                }
                issue_milestone_selected <- value

                if (is.null(issue_milestone_selected) || issue_milestone_selected == "") {
                  return(NULL)
                }
                open_milestone_names <- issues_df %>%
                  dplyr::filter(title == this_item, open) %>%
                  dplyr::pull(milestone_name)

                if (issue_milestone_selected %in% open_milestone_names) {
                  return(
                    "Issue is open"
                  )
                }
                return(NULL)
              })

              validator$add_rule(generate_input_id("commit", this_item), function(value) {
                selected_milestones <- input$selected_milestones
                milestone_branch <- branch_df$branch[branch_df$milestone_name %in% selected_milestones]

                if (!is.null(value) && nzchar(value)) {
                  return(NULL)
                }

                if (length(selected_milestones) == 0 || local_branch %in% milestone_branch) {
                  return("Required")
                }

                return("You may be missing commits since the selected file is not on the correct branch")
              })


              file_commits_df <- commit_df |> dplyr::filter(file == this_item)
              session$onFlushed(function() {
                shiny::updateSelectizeInput(
                  session,
                  inputId = generate_input_id("milestone", this_item),
                  choices = c("", file_commits_df$milestone_name |> Filter(f = Negate(is.na))),
                  selected = ""
                )
                shiny::updateSelectizeInput(
                  session,
                  generate_input_id("commit", this_item),
                  choices = c(file_commits_df$commit),
                  selected = ""
                )
              }, once = TRUE)
            })
          }
        }
      },
      ignoreInit = TRUE
    )
    shiny::observeEvent(c(rendered_items(), input$selected_milestones, input$include_open_issues),
                        {
                          for (item in rendered_items()) {
                            local({
                              this_item <- item
                              file_commits_df <- commit_df |> dplyr::filter(.data$file == this_item)

                              milestone_choices <- file_commits_df$milestone_name |> Filter(f = Negate(is.na))
                              selected_globals <- input$selected_milestones
                              if (is.null(selected_globals)) selected_globals <- character(0)

                              matching_milestones <- selected_globals[selected_globals %in% milestone_choices]

                              # Dupicate files across milestones already checked
                              if (length(matching_milestones) != 0) {
                                milestone_selected <- matching_milestones[1]

                                session$onFlushed(function() {
                                  shiny::updateSelectizeInput(
                                    session,
                                    inputId = generate_input_id("milestone", this_item),
                                    choices = matching_milestones,
                                    selected = milestone_selected,
                                    server = TRUE
                                  )
                                }, once = TRUE)
                              }
                              shiny::observeEvent(
                                input[[generate_input_id("milestone", this_item)]],
                                {
                                  milestone_selection <- input[[generate_input_id("milestone", this_item)]]

                                  commits_choices <- commit_df |>
                                    dplyr::filter(.data$file == this_item) |>
                                    dplyr::pull(.data$commit) |>
                                    unique()

                                  if (!(milestone_selection %in% c(""))) {
                                    commits_choices <- commit_df |>
                                      dplyr::filter(.data$file == this_item, .data$milestone_name %in% milestone_selection) |>
                                      dplyr::pull(.data$commit) |>
                                      unique()
                                  }

                                  shiny::updateSelectizeInput(
                                    session,
                                    generate_input_id("commit", this_item),
                                    choices = commits_choices,
                                    selected = if (!(milestone_selection %in% c("")) && length(commits_choices)) {
                                      commits_choices[[1]]
                                    } else {
                                      character(0)
                                    },
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
      # pull from reactive and clean to a character vector
      items_raw <- archive_files() %||% character(0)
      archive_items <- as.character(unlist(items_raw, use.names = FALSE))
      archive_items <- archive_items[nzchar(archive_items)]  # drop only empty strings

      # Warn if flattening would create basename collisions among the current items
      if (isTRUE(input$flatten) && length(archive_items) >= 2) {
        bn <- basename(archive_items)
        dup_bn <- unique(bn[duplicated(bn) & !is.na(bn)])
        if (length(dup_bn) > 0) {
          groups <- lapply(dup_bn, function(nm) {
            paths <- archive_items[!is.na(bn) & bn == nm]
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
      archive_name <- input$archive_name
      if (is.null(archive_name)) archive_name <- ""

      archive_name <- suggested_archive_name()
      if (is.null(archive_name)) archive_name <- ""   # fix: set the variable, not `fallback`

      # Effective archive name = typed value or suggestion
      archive_name <- trimws(if (nzchar(archive_name)) archive_name else archive_name)

      archive_selected_items(
        input         = input,
        session       = session,
        archive_name  = archive_name,
        flatten       = isTRUE(input$flatten),
        archive_items = archive_items
      )
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
