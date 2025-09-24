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

  selected_milestones <- input$milestone_existing

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
        choices  = matching_milestones,
        selected = matching_milestones[1],
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


archive_selected_items <- function(input, session, items, archive_name, flatten = FALSE, milestone_commit_df = NULL) {
  ts <- format(Sys.time(), "%Y%m%d_%H%M%S")

  base_dir <- "archives"
  if (!dir.exists(base_dir)) {
    dir.create(base_dir)
  }

  archive_dir <- file.path(base_dir, paste0(archive_name, "_", ts))

  milestone_items <- character(0)
  if (!is.null(milestone_commit_df)) {
    df <- if (is.function(milestone_commit_df)) milestone_commit_df() else milestone_commit_df
    if (!is.null(df) && nrow(df) > 0 && "title" %in% names(df)) {
      milestone_items <- sort(unique(stats::na.omit(df$title)))
    }
  }

  items_all <- unique(c(items %||% character(0), milestone_items))

  temp_files <- character(0)

  for (item in items_all) {
    commit_input_id <- generate_input_id("commit", item)
    sel_commit      <- input[[commit_input_id]]
    if (is.null(sel_commit) || identical(sel_commit, "")) next

    script_contents <- get_script_contents(item, sel_commit)

    file_path <- if (isTRUE(flatten)) {
      file.path(archive_dir, basename(item))
    } else {
      file.path(archive_dir, item)
    }

    temp_files <- c(temp_files, file_path)
    writeLines(script_contents, file_path, useBytes = TRUE)
  }

  zip_file <- paste0(archive_dir, ".zip")
  zip(zip_file, files = temp_files)

  unlink(temp_files, recursive = TRUE)

  showNotification(paste("Archived and zipped to:", zip_file), type = "message")
}
