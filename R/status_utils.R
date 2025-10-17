# commit log may be remote commits, local commits, etc
last_commit_that_changed_file_after_latest_qc_commit <- function(file, latest_qc_commit, head_commit) {
  last_commit_that_changed_file <- NULL
  commit_time <- NULL

  # if there are any local commits newer than the latest_qc_commit
  # did the file actually change in any of these commits?
  commit_range <- paste0(latest_qc_commit, "..", head_commit)
  args <- c("log",
            "--pretty=format:'%H|%ad'",
            "--date=iso",
            commit_range,
            "--",
            file)

  log_output <- tryCatch({
    result <- system2("git", args = args, stdout = TRUE, stderr = TRUE)
    status <- attr(result, "status")
    if (!is.null(status) && status != 0) {
      stop(glue::glue("Git log failed with status {status}: {paste(result, collapse = '\n')}"))
    }
    result
  }, error = function(e) {
    rlang::abort(glue::glue("Error running git log: {conditionMessage(e)}"))
  })

  if (length(log_output) > 0) {
    split_lines <- strsplit(log_output, "\\|")
    last_commit_that_changed_file <- split_lines[[1]][1]
    commit_time <- split_lines[[1]][2]
  }
  return(list(
    last_commit_that_changed_file = last_commit_that_changed_file,
    commit_time = commit_time
  ))
}

get_action_column <- function(qc_status, diagnostics, git_status, latest_qc_commit, comparator_commit, initial_qc_commit) {
  # The order in which buttons are added is important to influence user behavior
  # e.g. Notifying file changes is more important than reposting the last qc commit

  message <- function(msg) list(message = msg)

  # Priority #1: "Unapprove"
  if (qc_status %in% c("Approved")) { # want this before git_status logic
    return(list(options = "Unapprove (light)"))
  }

  if (qc_status %in% c("Approved; subsequent file changes", "Issue re-opened after approval")) {
    return(list(options = "Unapprove (danger)"))
  }

  # Priority #2: Any messages about fixing out-of-sync git
  if (stringr::str_detect(git_status, "View on QC branch:")) {
    return(message("Switch to QC branch"))
  }

  if (stringr::str_detect(git_status, "Deleted QC branch:")) {
    return(message("Restore QC branch"))
  }

  qc_msg <- switch(
    qc_status,
    "Initial QC commit posted" = "Pull to begin QC",
    "Notification posted" = "Pull to resume QC",
    "QC branch deleted before approval" = "Restore and switch to QC branch",
    NULL
  )
  if (!is.null(qc_msg)) return(message(qc_msg))


  valid_git_status <- !is.na(git_status) && git_status == "Up to date"
  if (!valid_git_status) {
    git_msg <- switch(
      git_status,
      "Remote file changes" = "Pull to resume QC",
      "Local uncommitted file changes" = "Commit and push to resume QC",
      "Local unpushed commits with file changes" = "Push to resume QC",
      "Synchronize repository to resume QC" # default message
    )
    return(message(git_msg))
  } # invalid git status

  opts <- character(0)

  # Priority #3: "Notify file changes" button
  changes_after_closure <- qc_status == "Closed without approval" && stringr::str_detect(diagnostics, "Commit difference")
  if (qc_status == "File changes to post" || changes_after_closure) {
    opts <- c(opts, "Notify file changes")
  }

  # Priority #2: "Approve" button
  valid_qc_status <- qc_status %in% c("Awaiting approval", "File changes to post", "Closed without approval")
  if (valid_qc_status) {
    opts <- c(opts, "Approve")
  }

  # Priority #3: "Notify last remote commit" button
  possible_updates <- latest_qc_commit != comparator_commit
  if (qc_status == "Awaiting approval" && possible_updates) {
    opts <- c(opts, "Notify last remote commit")
  }

  # Priority #4: "Repost last QC notification" button
  if (latest_qc_commit != initial_qc_commit) {
    opts <- c(opts, "Repost last QC notification")
  }

  res <- if (length(opts) > 0) {
    list(options = opts)
  }
  else {
    message(NULL)
  }

  return(res)
} # get_action_column


get_comment_metadata <- function(body) {
  metadata_section <- stringr::str_match(body, "(?s)## Metadata(.*)")[2]
  metadata_lines <- stringr::str_trim(unlist(strsplit(metadata_section, "\n")))

  metadata <- list()

  if (!is.na(metadata_section)) {
    for (line in metadata_lines) {
      if (stringr::str_detect(line, "^[*-]")) {
        key_value <- stringr::str_match(line, "[*-]\\s*(.*?):\\s*(.*)")[2:3]
        metadata[[key_value[1]]] <- key_value[2]
      }
    }
  } # if any metadata
  return(metadata)
}

get_hyperlinked_commit <- function(long_commit, file) {
  short_commit <- substr(long_commit, 1, 7)
  sha_url <- file.path(.le$full_repo_url, "blob", long_commit, file)
  hyperlinked_commit <- glue::glue('<a href="{sha_url}" target="_blank">{short_commit}</a>')
  return(hyperlinked_commit)
}

get_hyperlinked_commit_diff <- function(old_commit, new_commit) {
  commit_diff_url <- file.path(.le$full_repo_url, "compare", glue::glue("{old_commit}..{new_commit}"))
  hyperlinked_commit_diff <- glue::glue('<a href="{commit_diff_url}" target="_blank">Commit difference</a>')
  return(hyperlinked_commit_diff)
}

get_hyperlinked_last_remote_commit_diff <- function(old_commit, new_commit) {
  commit_diff_url <- file.path(.le$full_repo_url, "compare", glue::glue("{old_commit}..{new_commit}"))
  short_new_commit <- substr(new_commit, 1, 7)
  hyperlinked_commit_diff <- glue::glue('<a href="{commit_diff_url}" target="_blank">{short_new_commit}</a>')
  return(hyperlinked_commit_diff)
}

get_imageless_comments <- function(comments_url) {
  comments <- gh::gh(comments_url, .api_url = .le$github_api_url)
  comments <- rev(comments)
  comments_df <- do.call(rbind, lapply(comments, function(x) as.data.frame(t(unlist(x)), stringsAsFactors = FALSE)))
}

get_qc_commit_info <- function(file_name, issue_body, num_comments, comments_url, initial_qc_commit) {
  res <- list(
    previous_qc_commit = NA_character_,
    latest_qc_commit = initial_qc_commit, # initialize as initial qc commit, update if needed
    approve_comment = data.frame(body = NA_character_, html_url = NA_character_, id = NA_character_),
    qc_approved = FALSE
  )

  if (num_comments == 0) {
    return(res)
  }

  comments <- get_imageless_comments(comments_url)

  # start from latest comment (rev), check if a notify or approve comment (i.e. if it has metadata)
  # if it is, get the current/approved qc commit, then break
  for (i in seq_along(comments)) {
    comment_row <- comments[i, ]
    comment_body <- comment_row$body
    comment_metadata <- get_comment_metadata(comment_body)

    if (length(comment_metadata) > 0) {
      approved_qc_commit <-  comment_metadata$`approved qc commit`

      if (!is.null(approved_qc_commit)) {
        res$approve_comment <- data.frame(body = comment_row$body,
                                    html_url = comment_row$html_url,
                                    id = comment_row$id
                                    )
        #res$approve_comment <- comment_row
        res$latest_qc_commit <- approved_qc_commit
        res$qc_approved <- TRUE
        return(res)
      }

      current_qc_commit <- comment_metadata$`current commit`
      previous_qc_commit <- comment_metadata$`previous commit`

      if (!is.null(current_qc_commit)) {
        res$latest_qc_commit <- current_qc_commit
        res$previous_qc_commit <- previous_qc_commit
        return(res)
      }
    } # if any metadata
  } # for comments

  # else, no comments were Notification or Approval comments
  return(res)
}

format_diagnostics_list <- function(items) {
  list_items <- glue::glue('<li style="margin-bottom: 3px;">{items}</li>')
  glue::glue('
    <ul style="list-style-type: disc; margin: 0; padding-left: 1em;">
      {glue::glue_collapse(list_items, sep = "\n")}
    </ul>
  ')
}

get_relevant_files <- function(issue, milestone_name) {
  # parse issue body for associated files
  issue_body <- issue$body

  if (!stringr::str_detect(issue_body, "## Relevant files")) { # if no associated relevant files
    return( # return an empty data frame
      dplyr::tibble(
        relevant_file_name = character(),
        qc_file_name = character(),
        relevant_file_url = character(),
        relevant_file_note = character(),
        milestone_name = character(),
        issue_number = integer()
      )
    )
  } # if no associated relevant files

  relevant_files_section <- stringr::str_extract(
    issue_body,
    "## Relevant files[\\s\\S]*?(?=\\n#{1,6} )"
  )

  relevant_files_section <- gsub("\r\n", "\n", relevant_files_section)
  file_pattern <- "- \\*\\*(.*?)\\*\\*\\s*- \\[`(.*?)`\\]\\((.*?)\\)(?:\\s*>\\s*(.*?))?(?:\\n|$)"

  matches <- stringr::str_match_all(relevant_files_section, file_pattern)[[1]]

  relevant_files_df <- tryCatch({
    data.frame(
      relevant_file_name = matches[,3],
      qc_file_name = issue$title,
      relevant_file_url = matches[,4],
      relevant_file_note = stringr::str_trim(matches[,5]),
      milestone_name = milestone_name,
      issue_number = issue$number,
      stringsAsFactors = FALSE
    )
  }, error = function(e) {
    data.frame(
      relevant_file_name = character(0),
      qc_file_name = character(0),
      relevant_file_url = character(0),
      relevant_file_note = character(0),
      milestone_name = character(0),
      issue_number = numeric(0),
      stringsAsFactors = FALSE
    )
  })


  return(relevant_files_df)
} # get_relevant_files


vspace <- function() {
  "<div style=\"margin-top: 3px;\"></div>"
}

empty_tibble <- function() {
  return(
    dplyr::tibble(
      milestone_name = character(),
      milestone_with_url = character(),
      file_name = character(),
      file_with_url = character(),
      issue_state = character(),
      qc_status = character(),
      git_status = character(),
      diagnostics = character(),
      issue_number = integer(),
      initial_qc_commit = character(),
      latest_qc_commit = character(),
      previous_qc_commit = character(),
      comparator_commit = character(),
      aapprove_comment = character(),
      issue_url = character(),
      action = character(),
      qcer = character()
    )
  )

}
