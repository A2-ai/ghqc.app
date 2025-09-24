#parsing for all commits change where this is in the app
parse_local_log <- function(lines, keep_status = FALSE) {
  lines <- as.character(lines)
  lines <- trimws(lines)
  lines <- lines[nzchar(lines)]

  is_commit <- grepl("^[0-9a-f]{40}\\|", lines)
  commits   <- ifelse(is_commit, sub("^([0-9a-f]{40}).*$", "\\1", lines), NA_character_)

  for (i in seq_along(commits)) {
    if (is.na(commits[i]) && i > 1) commits[i] <- commits[i-1]
  }

  is_change <- grepl("^([A-Z](?:\\d{1,3})?)\\t", lines, perl = TRUE)

  lines   <- lines[is_change]
  commits <- commits[is_change]

  status <- sub("^([A-Z](?:\\d{1,3})?)\\t.*$", "\\1", lines, perl = TRUE)


  split2 <- strsplit(lines, "\t", fixed = TRUE)

  files <- vapply(seq_along(split2), function(i) {
    s <- status[i]
    p <- split2[[i]]
    if (length(p) >= 3 && grepl("^[RC]\\d{0,3}$", s)) {
      p[[3]]
    } else if (length(p) >= 2) {
      p[[2]]
    } else {
      NA_character_
    }
  }, character(1))

  out <- data.frame(commit = commits, title = files, stringsAsFactors = FALSE)
  if (keep_status) out$status <- status
  rownames(out) <- NULL
  out
}


milestone_archive_render <- function(input, ns, items, depth = 0, output) {
  tryCatch({
    debug(.le$logger, glue::glue(
      "Rendering selected list with items: {paste(items, collapse = ', ')}"
    ))

    # Create a container div for the grid layout with flex properties for wrapping
    ul <- div(
      class = paste("grid-container", "depth", depth, sep = "-"),
      style = "display: grid; grid-template-columns: 1fr 1.2fr 1.2fr; gap: 10px 12px; align-items: start; word-wrap: break-word; height: auto;"
    )

    # Header row for files, milestones, and commits
    ul <- tagAppendChild(ul, div(tags$strong("Files")))
    ul <- tagAppendChild(ul, div(tags$strong("Milestones")))
    ul <- tagAppendChild(ul, div(tags$strong("Commits")))

    # Loop through each item to add rows dynamically
    for (name in items) {
      modified_name <- gsub("([^a-zA-Z0-9])", "\\1<wbr>", htmltools::htmlEscape(name))
      milestone_input_id <- generate_input_id("milestone", name)
      commit_input_id    <- generate_input_id("commit", name)

      # Wrap the file name with 'word-wrap: break-word' and ensure proper height handling
      ul <- tagAppendChild(ul, div(class = "form-control", HTML(modified_name),
                                   style = "word-wrap: break-word; white-space: normal; height: auto;"))

      ul <- tagAppendChild(ul, selectizeInput(
        ns(milestone_input_id), label = NULL, choices = NULL,
        multiple = FALSE, width = "100%", options = list(closeAfterSelect = TRUE)
      ))

      ul <- tagAppendChild(ul, selectizeInput(
        ns(commit_input_id), label = NULL, choices = NULL,
        multiple = FALSE, width = "100%", options = list(closeAfterSelect = TRUE)
      ))
    }

    debug(.le$logger, "Rendered selected list successfully")
    ul
  },
  error = function(e) {
    items <- glue::glue_collapse(items, sep = ", ")
    error_message <- glue::glue("Error rendering selected {items}: {conditionMessage(e)}")
    log4r::error(.le$logger, error_message)
    stopApp()
    rlang::abort(error_message)
  })
}

milestone_archive_isolate_rendered_list <- function(input, session, items, milestone_commit_df) {
  df <- milestone_commit_df()
  if (is.null(df) || nrow(df) == 0) return(invisible(NULL))

  selected_milestones <- input$milestone_existing  # List of selected milestones

  for (name in items) {
    debug(.le$logger, glue::glue("Updating selectize inputs for item: {name}"))

    milestone_input_id <- generate_input_id("milestone", name)
    commit_input_id    <- generate_input_id("commit", name)

    df_item <- df %>%
      dplyr::filter(.data$title == !!name)

    # Get all unique milestone choices for this item (remove NAs)
    milestone_choices <- df_item %>%
      dplyr::mutate(milestone_title = ifelse(is.na(.data$milestone_title), "N/A", .data$milestone_title)) %>%
      dplyr::pull(.data$milestone_title) %>%
      unique() %>%
      sort()

    # Check for matching milestones from the selected ones
    matching_milestones <- intersect(selected_milestones, milestone_choices)

    if (length(matching_milestones) > 0) {
      # If the item has a matching milestone, force the milestone and commit selection

      # Set milestone to the first selected milestone (force milestone selection)
      updateSelectizeInput(
        session,
        milestone_input_id,
        choices  = matching_milestones,  # Show only the selected milestone
        selected = matching_milestones[1],  # Force the selected milestone as the only selection
        server   = TRUE
      )

      # Get commit choices associated with the selected milestone
      commit_choices <- df_item %>%
        dplyr::filter(.data$milestone_title == matching_milestones[1]) %>%
        dplyr::pull(.data$commit) %>%
        unique()

      # Default commit (based on the first commit for the selected milestone)
      default_commit <- if (length(commit_choices) > 0) commit_choices[[1]] else "No commits"

      # Force the commit selection based on the selected milestone
      updateSelectizeInput(
        session,
        commit_input_id,
        choices  = commit_choices,
        selected = default_commit,   # Pre-select the commit for the selected milestone
        server   = TRUE
      )
    } else {
      # If the selected milestone is not available for this item, allow free selection and default to "N/A"

      # Set milestone input to "N/A" (allow free selection)
      updateSelectizeInput(
        session,
        milestone_input_id,
        choices  = c("N/A", milestone_choices),  # Include "N/A" as an option for free selection
        selected = "N/A",  # Default to "N/A"
        server   = TRUE
      )

      # If "N/A" is selected, allow free selection for commits
      updateSelectizeInput(
        session,
        commit_input_id,
        choices  = df_item %>% dplyr::pull(.data$commit) %>% unique(),  # Show all commits for the item
        selected = NULL,  # No pre-selected commit
        server   = TRUE
      )

      # Dynamic update of commit choices based on milestone selection
      observeEvent(input[[milestone_input_id]], {
        selected_milestone <- input[[milestone_input_id]]  # Get the selected milestone
        if (!is.null(selected_milestone) && selected_milestone != "N/A") {
          # If a milestone is selected, update the commit choices based on the selected milestone
          commit_choices <- df_item %>%
            dplyr::filter(.data$milestone_title == selected_milestone) %>%
            dplyr::pull(.data$commit) %>%
            unique()

          updateSelectizeInput(
            session,
            commit_input_id,
            choices  = commit_choices,  # Show commits for the selected milestone
            selected = NULL,  # Allow the user to freely select a commit
            server   = TRUE
          )
        } else {
          # If "N/A" is selected, allow free selection and show all commit options
          commit_choices <- df_item %>%
            dplyr::pull(.data$commit) %>%
            unique()

          updateSelectizeInput(
            session,
            commit_input_id,
            choices  = commit_choices,  # Show all commits for the item
            selected = NULL,  # Allow free selection
            server   = TRUE
          )
        }
      })
    }
  }

  invisible(NULL)
}



additonal_archive_render_selected_list <- function(input, ns, items = NULL, depth = 0, output, milestone_commit_df) {
  tryCatch({
    debug(.le$logger, glue::glue(
      "Rendering selected list with items: {paste(items, collapse = ', ')}"
    ))

    # Initialize a reactiveValues to track files that have triggered the modal
    if (!exists("shown_modal_files", envir = .GlobalEnv)) {
      shown_modal_files <<- reactiveValues(shown = character(0))  # Define globally for server scope
    }

    # Get the list of selected files from the milestone_commit_df
    selected_files <- milestone_commit_df() %>%
      dplyr::pull(.data$title) %>%
      unique()

    # Check if any item is already selected in the milestone commit dataset
    milestone_selected_files <- items[items %in% selected_files]

    # Track which files have already triggered the modal
    if (length(milestone_selected_files) > 0) {
      # Filter out files that have already been shown
      files_to_show_modal <- setdiff(milestone_selected_files, shown_modal_files$shown)

      if (length(files_to_show_modal) > 0) {
        # Show the modal warning only once for each file
        for (file in files_to_show_modal) {
          showModal(modalDialog(
            title = "Warning",
            paste("The following file has already been selected in a milestone:",
                  file),
            easyClose = TRUE,
            footer = modalButton("Close")
          ))

          # After showing the modal, add the file to the shown list in reactiveValues
          shown_modal_files$shown <- c(shown_modal_files$shown, file)
        }
      }
    }

    # Now check if any items are unselected (if not in selected files)
    # If a file is unselected, remove it from the shown list so that the modal can appear again if reselected
    unselected_files <- setdiff(shown_modal_files$shown, milestone_selected_files)
    if (length(unselected_files) > 0) {
      # Remove unselected files from the list of shown files
      shown_modal_files$shown <- setdiff(shown_modal_files$shown, unselected_files)
    }

    # Filter out items that are already selected in the milestone commit dataset
    items_to_render <- setdiff(items, selected_files)

    # If there are no items left to render, return an empty div
    if (length(items_to_render) == 0) {
      return(div())  # Return an empty div when no items are left to render
    }

    ul <- div(
      class = paste("grid-container", "depth", depth, sep = "-"),
      style = "display: grid; grid-template-columns: 1fr 1.2fr 1.2fr; gap: 10px 12px; align-items: start; word-wrap: break-word; height: auto;"
    )

    # Header row for files, milestones, and commits
    ul <- tagAppendChild(ul, div(tags$strong("Files")))
    ul <- tagAppendChild(ul, div(tags$strong("Milestones")))
    ul <- tagAppendChild(ul, div(tags$strong("Commits")))

    for (name in items_to_render) {
      modified_name <- gsub("([^a-zA-Z0-9])", "\\1<wbr>", htmltools::htmlEscape(name))
      milestone_input_id <- generate_input_id("milestone", name)
      commit_input_id    <- generate_input_id("commit", name)

      # Wrap the file name with 'word-wrap: break-word' and ensure proper height handling
      ul <- tagAppendChild(ul, div(class = "form-control", HTML(modified_name),
                                   style = "word-wrap: break-word; white-space: normal; height: auto;"))

      ul <- tagAppendChild(ul, selectizeInput(
        ns(milestone_input_id), label = NULL, choices = NULL,
        multiple = FALSE, width = "100%", options = list(closeAfterSelect = TRUE)
      ))

      ul <- tagAppendChild(ul, selectizeInput(
        ns(commit_input_id), label = NULL, choices = NULL,
        multiple = FALSE, width = "100%", options = list(closeAfterSelect = TRUE)
      ))
    }

    debug(.le$logger, "Rendered selected list successfully")
    ul
  },
  error = function(e) {
    items <- glue::glue_collapse(items, sep = ", ")
    error_message <- glue::glue("Error rendering selected {items}: {conditionMessage(e)}")
    log4r::error(.le$logger, error_message)
    stopApp()
    rlang::abort(error_message)
  })
}

additonal_archive_isolate_rendered_list <- function(input, session, items, local_commit_df, milestone_commit_df) {
  for (name in items) {
    debug(.le$logger, glue::glue("Updating selectize inputs for item: {name}"))

    milestone_input_id <- generate_input_id("milestone", name)
    commit_input_id    <- generate_input_id("commit", name)

    # Filter df_item for the current file
    df_item <- local_commit_df %>%
      dplyr::filter(.data$title == !!name)

    # Replace NA values in milestone_title with "N/A"
    df_item <- df_item %>%
      dplyr::mutate(milestone_title = ifelse(is.na(.data$milestone_title), "N/A", .data$milestone_title))

    # Get the list of already selected milestones for this item from the milestone_commit_df
    selected_milestones <- milestone_commit_df() %>%
      dplyr::filter(.data$title == !!name) %>%
      dplyr::pull(.data$milestone_title) %>%
      unique()

    # Initialize selected_milestone and selected_commit to NULL by default
    selected_milestone <- NULL
    selected_commit <- NULL

    if (length(selected_milestones) > 0) {
      selected_milestone <- selected_milestones[[1]]
      selected_commit <- milestone_commit_df() %>%
        dplyr::filter(.data$title == !!name & .data$milestone_title == selected_milestone) %>%
        dplyr::pull(.data$commit) %>%
        unique() %>%
        .[[1]]

      df_item <- df_item %>%
        dplyr::filter(.data$milestone_title == selected_milestone & .data$commit == selected_commit)

      milestone_choices <- selected_milestone
      commit_choices <- selected_commit
    } else {
      milestone_choices <- df_item %>%
        dplyr::pull(.data$milestone_title) %>%
        unique() %>%
        sort()

      commit_choices <- df_item %>%
        dplyr::pull(.data$commit) %>%
        unique()
    }

    updateSelectizeInput(
      session,
      milestone_input_id,
      choices  = milestone_choices,
      selected = ifelse(is.null(selected_milestone), "N/A", selected_milestone),
      server   = TRUE,
      options = list(
        disabled = !is.null(selected_milestone)
      )
    )

    updateSelectizeInput(
      session,
      commit_input_id,
      choices  = df_item %>% dplyr::pull(.data$commit) %>% unique(),
      selected = NULL,
      server   = TRUE,
      options  = list(
        placeholder = "Select a commit",
        disabled = FALSE
      )
    )

    updateSelectizeInput(
      session,
      milestone_input_id,
      choices  = milestone_choices,
      selected = ifelse(is.null(selected_milestone), "N/A", selected_milestone),  # Default to "N/A" if no milestone is selected
      server   = TRUE,
      options = list(
        disabled = !is.null(selected_milestone)  # Disable if already selected
      )
    )

    # Update commit selectize input
    updateSelectizeInput(
      session,
      commit_input_id,
      choices  = commit_choices,
      selected = selected_commit,  # Preselect the commit if it was already selected
      server   = TRUE,
      options = list(
        disabled = !is.null(selected_milestone)  # Disable if already selected
      )
    )

    observeEvent(input[[milestone_input_id]], {
      selected_milestone <- input[[milestone_input_id]]

      if (selected_milestone == "N/A") {
        # If "N/A" is selected, include all commits and default to placeholder
        filtered_commits <- commit_choices
        selected_value <- ""   # this forces placeholder
      } else {
        # Filter commits based on the selected milestone
        filtered_commits <- df_item %>%
          dplyr::filter(.data$milestone_title == selected_milestone) %>%
          dplyr::pull(.data$commit) %>%
          unique()

        selected_value <- filtered_commits[1]  # default to first commit
      }

      updateSelectizeInput(
        session,
        commit_input_id,
        choices  = filtered_commits,
        selected = selected_value,
        options = list(
          placeholder = "Select a commit"
        ),
        server   = TRUE
      )
    })
  }
}

archive_selected_items <- function(input, session, items, archive_name, flatten = FALSE, milestone_commit_df = NULL) {
  ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
  archive_dir <- paste0(archive_name, "_", ts)
  dir.create(archive_dir, recursive = TRUE, showWarnings = FALSE)

  # 1) pull titles from the milestone df (if provided)
  milestone_items <- character(0)
  if (!is.null(milestone_commit_df)) {
    df <- if (is.function(milestone_commit_df)) milestone_commit_df() else milestone_commit_df
    if (!is.null(df) && nrow(df) > 0 && "title" %in% names(df)) {
      milestone_items <- sort(unique(stats::na.omit(df$title)))
    }
  }

  items_all <- unique(c(items %||% character(0), milestone_items))

  for (item in items_all) {
    commit_input_id <- generate_input_id("commit", item)
    sel_commit      <- input[[commit_input_id]]
    if (is.null(sel_commit) || identical(sel_commit, "")) next  # skip if no commit chosen

    script_contents <- get_script_contents(item, sel_commit)

    file_path <- if (isTRUE(flatten)) {
      file.path(archive_dir, basename(item))
    } else {
      file.path(archive_dir, item)
    }

    dir.create(dirname(file_path), recursive = TRUE, showWarnings = FALSE)
    writeLines(script_contents, file_path, useBytes = TRUE)
  }

  showNotification(paste("Archived to:", archive_dir), type = "message")
}

