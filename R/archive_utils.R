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



additonal_archive_render_selected_list <- function(input, ns, items = NULL, depth = 0, output) {
  tryCatch({
    debug(.le$logger, glue::glue(
      "Rendering selected list with items: {paste(items, collapse = ', ')}"
    ))


    ul <- div(
      class = paste("grid-container", "depth", depth, sep = "-"),
      style = "display:grid;grid-template-columns:1fr 1.2fr 1.2fr;gap:10px 12px;align-items:start;"
    )

    # Header row
    ul <- tagAppendChild(ul, div(tags$strong("Files")))
    ul <- tagAppendChild(ul, div(tags$strong("Milestones")))
    ul <- tagAppendChild(ul, div(tags$strong("Commits")))

    for (name in items) {
      modified_name <- gsub("([^a-zA-Z0-9])", "\\1<wbr>", htmltools::htmlEscape(name))
      milestone_input_id <- generate_input_id("milestone", name)
      commit_input_id    <- generate_input_id("commit", name)


      ul <- tagAppendChild(ul, div(class = "form-control", HTML(modified_name)))

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



additonal_archive_isolate_rendered_list <- function(input, session, items, local_commit_df) {
  for (name in items) {
    debug(.le$logger, glue::glue("Updating selectize inputs for item: {name}"))

    milestone_input_id <- generate_input_id("milestone", name)
    commit_input_id    <- generate_input_id("commit", name)

    df_item <- local_commit_df %>%
      dplyr::filter(.data$title == !!name)

    # Replace NA values in milestone_title with "N/A"
    df_item <- df_item %>%
      dplyr::mutate(milestone_title = ifelse(is.na(.data$milestone_title), "N/A", .data$milestone_title))

    milestone_choices <- df_item %>%
      dplyr::pull(.data$milestone_title) %>%
      unique() %>%
      sort()

    commit_choices <- df_item %>%
      dplyr::pull(.data$commit) %>%
      unique()

    # Update milestone choices and set default to "N/A"
    updateSelectizeInput(
      session,
      milestone_input_id,
      choices  = milestone_choices,
      selected = "N/A",
      server   = TRUE
    )

    # Update commit choices based on the milestone selection
    observeEvent(input[[milestone_input_id]], {
      selected_milestone <- input[[milestone_input_id]]

      # If "N/A" is selected, include all commits
      if (selected_milestone == "N/A") {
        filtered_commits <- commit_choices  # Show all commits
      } else {
        # Filter commits based on the selected milestone
        filtered_commits <- df_item %>%
          dplyr::filter(.data$milestone_title == selected_milestone) %>%
          dplyr::pull(.data$commit) %>%
          unique()
      }

      # Update the commit selectize input with the filtered commits
      updateSelectizeInput(
        session,
        commit_input_id,
        choices  = filtered_commits,
        selected = "",  # Reset the selected commit to empty string (forces placeholder)
        options = list(
          placeholder = "Select a commit"  # Placeholder text
        ),
        server   = TRUE
      )
    })
  }
}

archive_selected_items <- function(input, session, items, archive_name, flatten = FALSE) {
  ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
  archive_dir <- paste0(archive_name, "_", ts)
  dir.create(archive_dir, recursive = TRUE, showWarnings = FALSE)

  for (item in items) {
    commit_input_id <- generate_input_id("commit", item)
    sel_commit      <- input[[commit_input_id]]
    if (is.null(sel_commit) || identical(sel_commit, "")) next

    script_contents <- get_script_contents(item, sel_commit)

    # decide output path based on flatten flag
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

