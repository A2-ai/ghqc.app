
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

create_single_item_ui <- function(name, ns) {
  milestone_raw <- generate_input_id("milestone", name)
  commit_raw    <- generate_input_id("commit", name)

  shiny::tags$div(
    id = ns(generate_input_id("item_row", name)),
    style = "display: contents;",  # children participate in parent grid

    # Files cell
    shiny::tags$div(
      class = "form-control",
      style = "word-wrap: break-word; white-space: normal; height: auto;",
      HTML(htmltools::htmlEscape(name))
    ),

    shiny::selectizeInput(
      inputId  = ns(milestone_raw),
      label    = NULL,
      choices  = character(0),
      multiple = FALSE,
      width    = "100%",
      options  = list(
        closeAfterSelect = TRUE,
        placeholder      = "Select a milestone (optional)"
      )
    ),

    shiny::selectizeInput(
      inputId = ns(commit_raw),
      label   = NULL,
      choices = character(0),
      multiple = FALSE,
      width = "100%",
      options = list(closeAfterSelect = TRUE,
        placeholder      = "Select a commit (required)"
        )
    )
  )
}


# Helper: apply the "global milestones override" rule to one item
global_milestone_override <- function(it) {
  df_all  <- milestone_commit_df()
  if (is.null(df_all) || nrow(df_all) == 0) return(invisible())

  milestone_input_id <- generate_input_id("milestone", it)
  commit_input_id    <- generate_input_id("commit",    it)

  df_item <- df_all %>% dplyr::filter(.data$title == it)

  item_milestones <- df_item %>% dplyr::pull(.data$milestone_title) %>% unique() %>% sort()
  item_commits    <- df_item %>% dplyr::pull(.data$commit)           %>% unique()

  global_milestone <- input$global_milestone %||% character(0)
  matching_globals <- intersect(global_milestone, item_milestones)

  current_sel <- input[[milestone_input_id]]

  if (length(matching_globals) >= 1) {
    # GLOBALS OVERRIDE: restrict to matching globals only
    next_sel <- if (!is.null(current_sel) && current_sel %in% matching_globals) current_sel else matching_globals[[1]]

    shiny::updateSelectizeInput(
      session, milestone_input_id,
      choices  = matching_globals,
      selected = next_sel,
      server   = TRUE,
      options  = list(placeholder = 'Select a milestone…')
    )

    commits_filtered <- df_item %>%
      dplyr::filter(.data$milestone_title == next_sel) %>%
      dplyr::pull(.data$commit) %>%
      unique()

    shiny::updateSelectizeInput(
      session, commit_input_id,
      choices  = commits_filtered,
      selected = if (length(commits_filtered)) commits_filtered[[1]] else NULL,
      server   = TRUE
    )

  } else {
    # No overlap with globals: restore full milestones
    shiny::updateSelectizeInput(
      session, milestone_input_id,
      choices  = item_milestones,
      selected = if (!is.null(current_sel) && current_sel %in% item_milestones) current_sel else NULL,
      server   = TRUE,
      options  = list(placeholder = 'Select a milestone…')
    )

    shiny::updateSelectizeInput(
      session, commit_input_id,
      choices  = item_commits,
      selected = NULL,
      server   = TRUE
    )
  }
}



archive_selected_items <- function(input, session, items, archive_name, flatten = FALSE, milestone_commit_df = NULL) {

  # Collect milestone items, if provided
  milestone_items <- character(0)
  if (!is.null(milestone_commit_df)) {
    df <- if (is.function(milestone_commit_df)) milestone_commit_df() else milestone_commit_df
    if (!is.null(df) && nrow(df) > 0 && "title" %in% names(df)) {
      milestone_items <- sort(unique(stats::na.omit(df$title)))
    }
  }

  items_all <- unique(c(items %||% character(0), milestone_items))

  # Stage dir
  stage_dir <- file.path(tempdir(), "archive_stage")
  dir.create(stage_dir, recursive = TRUE, showWarnings = FALSE)

  rel_files <- character(0)

  # Top-level folder inside the zip = filename without extension
  zip_stem <- tools::file_path_sans_ext(basename(archive_name))
  top_dir  <- paste0(zip_stem, "/")

  for (item in items_all) {
    commit_input_id <- generate_input_id("commit", item)
    sel_commit      <- input[[commit_input_id]]
    if (is.null(sel_commit) || identical(sel_commit, "")) next

    script_contents <- get_script_contents(item, sel_commit)

    rel_path <- if (isTRUE(flatten)) {
      paste0(top_dir, basename(item))
    } else {
      paste0(top_dir, gsub("\\\\", "/", item))
    }

    abs_path <- file.path(stage_dir, rel_path)
    dir.create(dirname(abs_path), recursive = TRUE, showWarnings = FALSE)
    writeLines(script_contents, abs_path, useBytes = TRUE)

    rel_files <- c(rel_files, gsub("\\\\", "/", rel_path))
  }

  if (!length(rel_files)) {
    showNotification("No files to archive.", type = "warning")
    unlink(stage_dir, recursive = TRUE, force = TRUE)
    return(invisible(NULL))
  }

  ## Compute absolute output path BEFORE setwd()
  owd <- getwd()
  zip_file_abs <- normalizePath(
    if (grepl("^(?:/|[A-Za-z]:)", archive_name)) archive_name else file.path(owd, archive_name),
    mustWork = FALSE
  )

  # Ensure directory exists, overwrite if exists
  out_dir <- dirname(zip_file_abs)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  if (file.exists(zip_file_abs)) unlink(zip_file_abs, force = TRUE)

  on.exit(setwd(owd), add = TRUE)
  setwd(stage_dir)

  utils::zip(zipfile = zip_file_abs, files = rel_files)

  setwd(owd)
  unlink(stage_dir, recursive = TRUE, force = TRUE)
  showNotification(paste("Archived and zipped to:", zip_file_abs), type = "message")
  invisible(zip_file_abs)
}
