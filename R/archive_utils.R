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
      HTML(htmltools::htmlEscape(name)),
      style = "word-wrap: break-word; white-space: normal;"
    ),

    # Milestones cell (NOTE: inputId is namespaced!)
    shiny::selectizeInput(
      inputId = ns(milestone_raw),
      label   = NULL,
      choices = character(0),
      multiple = FALSE,
      width = "100%",
      options = list(closeAfterSelect = TRUE)
    ),

    # Commits cell (NOTE: inputId is namespaced!)
    shiny::selectizeInput(
      inputId = ns(commit_raw),
      label   = NULL,
      choices = character(0),
      multiple = FALSE,
      width = "100%",
      options = list(closeAfterSelect = TRUE)
    )
  )
}

archive_selected_items <- function(input, session, items, archive_name, flatten = FALSE, milestone_commit_df = NULL) {
  ts <- format(Sys.time(), "%Y%m%d_%H%M%S")

  base_dir <- "archive"
  if (!dir.exists(base_dir)) dir.create(base_dir, recursive = TRUE)

  # Collect milestone items, if provided
  milestone_items <- character(0)
  if (!is.null(milestone_commit_df)) {
    df <- if (is.function(milestone_commit_df)) milestone_commit_df() else milestone_commit_df
    if (!is.null(df) && nrow(df) > 0 && "title" %in% names(df)) {
      milestone_items <- sort(unique(stats::na.omit(df$title)))
    }
  }

  items_all <- unique(c(items %||% character(0), milestone_items))

  # Staging dir
  stage_dir <- file.path(tempdir(), paste0("archive_stage_", ts))
  dir.create(stage_dir, recursive = TRUE, showWarnings = FALSE)

  rel_files <- character(0)

  # Always include a top-level folder
  top_dir <- paste0(archive_name, "/")

  for (item in items_all) {
    commit_input_id <- generate_input_id("commit", item)
    sel_commit      <- input[[commit_input_id]]
    if (is.null(sel_commit) || identical(sel_commit, "")) next

    script_contents <- get_script_contents(item, sel_commit)

    # Path inside the zip
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

  if (length(rel_files) == 0) {
    showNotification("No files to archive.", type = "warning")
    unlink(stage_dir, recursive = TRUE, force = TRUE)
    return(invisible(NULL))
  }

  zip_file <- file.path(base_dir, paste0(archive_name, "_", ts, ".zip"))

  owd <- getwd()
  on.exit(setwd(owd), add = TRUE)
  setwd(stage_dir)

  utils::zip(
    zipfile = normalizePath(file.path(owd, zip_file), mustWork = FALSE),
    files   = rel_files
  )

  setwd(owd)
  unlink(stage_dir, recursive = TRUE, force = TRUE)

  showNotification(paste("Archived and zipped to:", zip_file), type = "message")
}
