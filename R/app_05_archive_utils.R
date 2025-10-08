
#parsing for all commits change where this is in the app
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


get_qc_approval_or_latest <- function(issue) {
  init_commit <- get_init_qc_commit_from_issue_body(issue$body)

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
        return(
          approved_qc_commit
        )
      }
      # treat approved qc commit as normal notification if previously unapproved
      if (is.null(commit_candidate)) {
        commit_candidate <-
          approved_qc_commit
      }
    }

    current_qc_commit <- comment_metadata$`current commit`
    if (!is.null(current_qc_commit) && is.null(commit_candidate)) {
      commit_candidate <-
        approved_qc_commit
    }
  }

  if (is.null(commit_candidate)) {
    init_commit
  } else {
    commit_candidate
  }
}


archive_selected_items <- function(input,
                                   session,
                                   archive_name,
                                   flatten = FALSE,
                                   archive_items = character(0),
                                   milestone_df = NULL) {

  # Collect milestone items, if provided


  # Combine: explicit archive_items + milestone-derived items
  archive_items <- unique(c(archive_items))
  archive_items <- archive_items[nzchar(archive_items)]

  # Stage dir
  stage_dir <- file.path(tempdir(), "archive_stage")
  dir.create(stage_dir, recursive = TRUE, showWarnings = FALSE)

  rel_files <- character(0)

  zip_stem <- tools::file_path_sans_ext(basename(archive_name))
  top_dir  <- paste0(zip_stem, "/")

  for (item in archive_items) {
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


