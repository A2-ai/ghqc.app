ghqc_archive_server <- function(id, root_dir, milestone_df, local_branch) {
  #validator <- shinyvalidate::InputValidator$new()

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

  moduleServer(id, function(input, output, session) {
    reset_triggered <- reactiveVal(FALSE)
    session$onSessionEnded(function() {
      if (!isTRUE(isolate(reset_triggered()))) {
        stopApp()
      }
    })

    w_load_items <- waiter::Waiter$new(
      id = session$ns("main_panel_dynamic"),
      html = shiny::tagList(
        waiter::spin_2(),
      ),
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
        shiny::checkboxInput(
          session$ns("include_open_milestones"),
          "Include Open Milestones",
          value = FALSE
        ),
        shiny::checkboxInput(
          session$ns("include_open_issues"),
          "Include Open Issues",
          value = FALSE
        ),
        shiny::checkboxInput(
          session$ns("include_relevant_files"),
          "Include Relevant Files",
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
          style = "font-family: \"Helvetica Neue\", Helvetica, Arial, sans-serif !important; font-weight: bold;",
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
        placeholder = suggested_archive_name()
      )
    })

    # Panel Setup End

    # Collect Issues and Commits
    issues_df <- get_all_issues_in_repo() |>
      purrr::map_dfr(function(issue) {
        qc_commit <- get_qc_approval_or_latest(issue)$commit
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
          branch = issue_branch,
        )
      })

    local_commits <- get_local_log() |>
      dplyr::mutate(branch = local_branch, milestone_name = NA_character_)

    # Bind local and issue commits in case issue commits are either non-file changing OR on a different branch
    # Letting local commits override the issue commmits
    commit_df <- local_commits |>
      dplyr::bind_rows(
        issues_df |>
          dplyr::mutate(message = NA_character_) |>
          dplyr::select(
            commit = qc_commit,
            file = title,
            message,
            branch,
            milestone_name
          )
      ) |>
      dplyr::distinct(commit, file, .keep_all = TRUE)

    shiny::observeEvent(input$selected_milestones, {
      selected_milestones <- input$selected_milestones
      if (length(selected_milestones) <= 1) {
        return()
      }

      if (!any(duplicated(issues_df$title))) {
        return()
      }

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
        "There are duplicate files detected between milestones. Please select a different milestone",
        easyClose = TRUE,
        footer = NULL
      ))

      shiny::updateSelectizeInput(
        session,
        "selected_milestones",
        selected = selected_milestones[-length(selected_milestones)]
      )
    })

    previously_warned_duplicates <- shiny::reactiveVal()
    archive_files <- shiny::reactiveVal()
    shiny::observeEvent(c(input$selected_milestones, selected_files()), {
      shiny::req(input$selected_milestones)

      issue_files <- issues_df |>
        dplyr::filter(milestone_name %in% input$selected_milestones)

      if (!input$include_open_issues) {
        issue_files <- issue_files |>
          dplyr::filter(!open)
      }

      # TODO: add rel files

      files <- issue_files |>
        dplyr::pull(title) |>
        c(selected_files())

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
          rendered_items(character(0))
        }

        if (length(items_to_add) != 0) {
          for (item in items_to_add) {
            shiny::insertUI(
              selector = paste0("#", session$ns("grid_container")),
              where = "beforeEnd",
              ui = create_single_item_ui(item, session$ns)
            )

            file_commits_df <- commit_df |>
              dplyr::filter(file == item)

            shiny::updateSelectizeInput(
              session,
              generate_input_id("milestone", item),
              choices = c(
                "",
                file_commits_df$milestone_name |> Filter(f = Negate(is.na))
              ),
              selected = NULL
            )
            shiny::updateSelectizeInput(
              session,
              generate_input_id("commit", item),
              # TODO: Add "" option and then shiny validate
              choices = c(file_commits_df$commit),
              selected = NULL
            )
          }
        }
        browser()
      },
      ignoreInit = TRUE
    )
  })
}

get_local_log <- function() {
  local_log_output <- system(
    "git log --pretty=format:'%H|%an|%ae|%ad|%s'  --date=format:'%Y-%m-%d %H:%M:%S' --name-status",
    intern = TRUE
  ) |>
    as.character() |>
    trimws()
  lines <- local_log_output[nzchar(local_log_output)]

  is_commit <- grepl("^[0-9a-f]{40}\\|", lines)
  commits <- ifelse(
    is_commit,
    sub("^([0-9a-f]{40}).*$", "\\1", lines),
    NA_character_
  )

  # Extract commit messages from commit lines
  commit_messages <- ifelse(
    is_commit,
    sub("^[0-9a-f]{40}\\|[^|]*\\|[^|]*\\|[^|]*\\|(.*)$", "\\1", lines),
    NA_character_
  )

  for (i in seq_along(commits)) {
    if (is.na(commits[i]) && i > 1) {
      commits[i] <- commits[i - 1]
      commit_messages[i] <- commit_messages[i - 1]
    }
  }

  is_change <- grepl("^([A-Z](?:\\d{1,3})?)\\t", lines, perl = TRUE)

  lines <- lines[is_change]
  commits <- commits[is_change]
  commit_messages <- commit_messages[is_change]

  status <- sub("^([A-Z](?:\\d{1,3})?)\\t.*$", "\\1", lines, perl = TRUE)

  split2 <- strsplit(lines, "\t", fixed = TRUE)

  files <- vapply(
    seq_along(split2),
    function(i) {
      s <- status[i]
      p <- split2[[i]]
      if (length(p) >= 3 && grepl("^[RC]\\d{0,3}$", s)) {
        p[[3]]
      } else if (length(p) >= 2) {
        p[[2]]
      } else {
        NA_character_
      }
    },
    character(1)
  )

  out <- data.frame(
    commit = commits,
    file = files,
    message = commit_messages,
    stringsAsFactors = FALSE
  )
  rownames(out) <- NULL
  out
}

get_qc_approval_or_latest <- function(issue) {
  init_commit <- list(
    commit = get_init_qc_commit_from_issue_body(issue$body),
    event = "initial"
  )
  if (issue$comments == 0) {
    return(init_commit)
  }
  comments <- get_imageless_comments(issue$comments_url)
  commit_candidate <- NULL
  unapproved <- FALSE
  for (i in seq_along(comments)) {
    comment <- comments[i, ]
    if (grepl("# QC Unapproved", comment$body)) {
      unapproved <- TRUE
      next
    }

    comment_metadata <- get_comment_metadata(comment$body)
    if (length(comment_metadata) == 0) {
      next
    }

    approved_qc_commit <- comment_metadata$`approved qc commit`
    if (!is.null(approved_qc_commit)) {
      if (!unapproved) {
        return(list(
          commit = approved_qc_commit,
          event = "approved"
        ))
      }
      # treat approved qc commit as normal notification if previously unapproved
      if (is.null(commit_candidate)) {
        commit_candidate <- list(
          commit = approved_qc_commit,
          event = "notification"
        )
      }
    }

    current_qc_commit <- comment_metadata$`current commit`
    if (!is.null(current_qc_commit) && is.null(commit_candidate)) {
      commit_candidate <- list(
        commit = approved_qc_commit,
        event = "notification"
      )
    }
  }

  if (is.null(commit_candidate)) {
    init_commit
  } else {
    commit_candidate
  }
}
