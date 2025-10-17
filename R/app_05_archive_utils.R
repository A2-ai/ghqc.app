#' @import shiny
#' @importFrom htmltools HTML htmlEscape
#' @importFrom tools file_path_sans_ext
#' @importFrom utils zip
#' @importFrom stringr str_extract
#' @importFrom glue glue
#' @importFrom log4r debug info warn error
NULL

#' Get Local Log
#'
#' parsing function that finds all the commits for each file in the repo
#'
#' @return A data frame of commits files and messages in the repo
#' @noRd
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

  # Create short SHAs for display (using existing logic pattern from format_commit_shas.R)
  short_shas <- stringr::str_extract(commits, "^.{1,7}")

  commit_display <- ifelse(
    !is.na(commits) & !is.na(commit_messages) & !is.na(short_shas),
    glue::glue("{short_shas} | {commit_messages}"),
    NA_character_
  )

  out <- data.frame(
    commit = commits,
    short_sha = short_shas,
    commit_display = commit_display,
    file = files,
    message = commit_messages,
    stringsAsFactors = FALSE
  )
  rownames(out) <- NULL
  out
}

#' Create Single Item UI
#'
#' Generates UI elements for a single file item in the archive interface, including
#' milestone and commit selection controls arranged in a grid layout.
#'
#' @param name Character string. The file name/path to be displayed and processed.
#' @param ns Function. Shiny namespace function for creating namespaced input IDs.
#'
#' @return A shiny.tag object containing the complete UI elements for the file item,
#'   including the file display, milestone selector, and commit selector.
#'
#'
#' The generated UI elements use unique IDs based on the file name and are properly
#' namespaced for use within Shiny modules. Input validation and dynamic choices
#' are handled by the server-side logic.
#'
#' @noRd
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

#' Get QC Approval or Latest Commit
#'
#' Retrieves the most recent approved QC commit for a given issue, or falls back
#' to the latest commit if no approval is found. Handles QC approval status and
#' unapproval events from issue comments.
#'
#' @param issue List. A GitHub issue object containing issue metadata including
#'   body text, comments count, and comments URL.
#'
#' @return List with two elements: commit (character string) and approved (logical).
#'   The commit hash of the approved QC commit or the latest available commit,
#'   and whether it was explicitly approved.
#'
#'
#' @noRd
get_qc_approval_or_latest <- function(issue) {
  init_commit <- get_init_qc_commit_from_issue_body(issue$body)

  if (issue$comments == 0) {
    return(list(commit = init_commit, approved = FALSE))
  }
  comments <- get_imageless_comments(issue$comments_url)
  commit_candidate <- NULL
  unapproved <- FALSE
  approved <- FALSE

  for (i in seq_along(comments)) {
    comment <- comments[i, ]
    if (grepl("# QC Unapproved", comment$body)) {
      unapproved <- TRUE
      approved <- FALSE
      next
    }

    comment_metadata <- get_comment_metadata(comment$body)
    if (length(comment_metadata) == 0) {
      next
    }

    approved_commit <- comment_metadata$`approved qc commit`
    if (!is.null(approved_commit)) {
      if (!unapproved) {
        return(list(commit = approved_commit, approved = TRUE))
      }
      # treat approved qc commit as normal notification if previously unapproved
      if (is.null(commit_candidate)) {
        commit_candidate <- approved_commit
        approved <- FALSE
      }
    }

    current_qc_commit <- comment_metadata$`current commit`
    if (!is.null(current_qc_commit) && is.null(commit_candidate)) {
      commit_candidate <- current_qc_commit
      approved <- FALSE
    }
  }

  if (is.null(commit_candidate)) {
    list(commit = init_commit, approved = FALSE)
  } else {
    list(commit = commit_candidate, approved = approved)
  }
}

#' Generate Archive Metadata JSON
#'
#' Creates a JSON metadata file containing information about the archive creation,
#' including creator, timestamp, and detailed file information with commit data.
#'
#' @param input Reactive input object from Shiny session containing user selections.
#' @param archive_items Character vector. File paths included in the archive.
#' @param commit_df Data frame containing commit information with columns:
#'   commit, file, milestone_name, approved.
#' @param flatten Logical. Whether the directory structure was flattened.
#'
#' @return Character string containing the JSON metadata.
#'
#' @noRd
generate_archive_metadata <- function(input, archive_items, commit_df, flatten = FALSE) {

  creator <- tryCatch({
    get_user()
  }, error = function(e) {
    "unknown"
  })

  created_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")


  files_metadata <- list()

  for (item in archive_items) {
    commit_input_id <- generate_input_id("commit", item)
    milestone_input_id <- generate_input_id("milestone", item)

    sel_commit <- input[[commit_input_id]]
    sel_milestone <- input[[milestone_input_id]]


    if (is.null(sel_commit) || identical(sel_commit, "")) next

    file_commit_info <- commit_df[commit_df$file == item & commit_df$commit == sel_commit, ]
    approved <- if (is.null(sel_milestone) || sel_milestone == "") {
      "null"
    } else if (nrow(file_commit_info) > 0) {
      any(file_commit_info$approved, na.rm = TRUE)
    } else {
      FALSE
    }


    archive_file <- if (isTRUE(flatten)) {
      basename(item)
    } else {
      gsub("\\\\", "/", item)
    }

    file_metadata <- list(
      archive_file = archive_file,
      repository_file = gsub("\\\\", "/", item),
      commit = sel_commit,
      milestone = if (is.null(sel_milestone) || sel_milestone == "") NULL else sel_milestone,
      approved = approved
    )

    files_metadata <- append(files_metadata, list(file_metadata))
  }

  # Create complete metadata structure
  metadata <- list(
    creator = creator,
    created_at = created_at,
    files = files_metadata
  )

  # Convert to JSON with pretty formatting
  jsonlite::toJSON(metadata, pretty = TRUE, auto_unbox = TRUE, null = "null")
}

#' Archive Selected Items
#'
#' Creates a ZIP archive containing selected files at specific commit versions.
#' Handles file staging, directory structure options, and archive creation with
#' proper error handling and user notifications.
#'
#' @param input Reactive input object from Shiny session containing user selections.
#' @param session Shiny session object for accessing namespaced inputs and notifications.
#' @param archive_name Character string. The desired name/path for the archive file.
#' @param flatten Logical. If TRUE, removes directory structure and places all files
#'   in the root of the archive. Default is FALSE.
#' @param archive_items Character vector. File paths to be included in the archive.
#'   Default is empty character vector.
#'
#' @return Character string (invisible). The absolute path to the created ZIP file,
#'   or NULL if no files were archived.
archive_selected_items <- function(input,
                                   session,
                                   archive_name,
                                   flatten = FALSE,
                                   archive_items = character(0),
                                   commit_df = NULL
) {


  archive_items <- unique(c(archive_items))
  archive_items <- archive_items[nzchar(archive_items)]

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

  # Generate and add metadata JSON file
  metadata_json <- generate_archive_metadata(input, archive_items, commit_df, flatten)
  metadata_path <- paste0(top_dir, "ghqc_archive_metadata.json")
  metadata_abs_path <- file.path(stage_dir, metadata_path)
  writeLines(metadata_json, metadata_abs_path, useBytes = TRUE)
  rel_files <- c(rel_files, gsub("\\\\", "/", metadata_path))

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

#' Get Closed Issues in Repository
#'
#' Fetches only closed issues from the repository for faster loading.
#'
#' @return List of closed issues from the repository
#' @noRd
get_closed_issues_in_repo <- function() {
  debug(.le$logger, glue::glue("Retrieving closed Issue(s) from repo: {.le$repo}..."))

  closed_issues <- list()
  page <- 1

  repeat {
    res <- gh::gh("GET /repos/:org/:repo/issues",
                  .api_url = .le$github_api_url,
                  org = .le$org,
                  repo = .le$repo,
                  state = "closed",
                  per_page = 100,
                  page = page)

    if (length(res) == 0) break
    closed_issues <- c(closed_issues, res)
    page <- page + 1
  }

  issues <- get_only_ghqc_issues(closed_issues)
  info(.le$logger, glue::glue("Retrieved {length(issues)} closed Issue(s) from repo: {.le$repo}"))
  return(issues)
}

#' Get Open Issues in Repository
#'
#' Fetches only open issues from the repository for lazy loading.
#'
#' @return List of open issues from the repository
#' @noRd
get_open_issues_in_repo <- function() {
  debug(.le$logger, glue::glue("Retrieving open Issue(s) from repo: {.le$repo}..."))

  open_issues <- list()
  page <- 1

  repeat {
    res <- gh::gh("GET /repos/:org/:repo/issues",
                  .api_url = .le$github_api_url,
                  org = .le$org,
                  repo = .le$repo,
                  state = "open",
                  per_page = 100,
                  page = page)

    if (length(res) == 0) break
    open_issues <- c(open_issues, res)
    page <- page + 1
  }

  issues <- get_only_ghqc_issues(open_issues)
  info(.le$logger, glue::glue("Retrieved {length(issues)} open Issue(s) from repo: {.le$repo}"))
  return(issues)
}

