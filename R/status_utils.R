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



get_approve_column <- function(qc_status, git_status) {
  if (qc_status == "Approved") {
    return("none")
  }

  if (qc_status == "Approved; subsequent file changes") { # want this before git_status logic
    return("Delete \"QC Approved\" comment to resume QC") # (probably don't want to do a hard reset)
  }

  if (qc_status == "Initial QC commit posted") { # git_status can be anything
    return("Pull to begin QC")
  }

  if (stringr::str_detect(git_status, "View on QC branch:")) {
    return("Switch to QC branch")
  }

  if (stringr::str_detect(git_status, "Deleted QC branch:")) {
    return("Restore QC branch")
  }

  valid_git_status <- !is.na(git_status) && git_status == "Up to date"
  if (!valid_git_status) {
    if (git_status == "Remote file changes") {
      return("Pull to resume QC")
    }

    if (git_status == "Local uncommitted file changes") {
      return("Commit and push to resume QC")
    }

    if (git_status == "Local unpushed commits with file changes") {
      return("Push to resume QC")
    }

    return("Synchronize repository")
  } # invalid git status


  # else, git_status is NA or "Up to date"
  valid_qc_status <- qc_status %in% c("Awaiting approval", "File changes to post", "Closed without approval")
  if (valid_qc_status) {
    return("approve")
  }

  # Needs further explanation

  # "Notification posted" is possible when git status is "Up to date". In this case, force sync because the post was intentional
  if (qc_status == "Notification posted") {
    return("Pull to resume QC")
  }

  if (qc_status == "Issue re-opened after approval") {
    return("Close Issue or delete \"QC Approved\" comment to resume QC")
  }

  if (qc_status == "QC branch deleted before approval") {
    return("Restore and switch to QC branch")
  }

  else {
    return("none")
  }

}

get_notify_column <- function(qc_status, diagnostics, git_status, latest_qc_commit, comparator_commit) {
  has_valid_git_status <- is.na(git_status) || git_status == "Up to date" # allowing git status to be NA in case when QC branch deleted and merged

  if (!has_valid_git_status) { # don't give option to notify if git status not up to date
    return("none")
  }

  # don't give option to notify if comparator commit same as qc commit, it would be redundant to comment the same commit twice
  possible_updates <- ifelse(latest_qc_commit != comparator_commit, TRUE, FALSE)

  if (!possible_updates) {
    return("none")
  }

  # see how pertinent a QC notification is (i.e. hard == pretty pertinent, soft == probably not pertinent)

  # hard notify statuses are qc statuses for which there are file changes and there's a good reason to notify
  hard_notify_qc_statuses <- c("File changes to post",
                               "Approved; subsequent file changes" # git status if up-to-date so this is fine
                               )

  # soft notify statuses are statuses where there's no changes in the qc file to notify,
  # but the user may still want to update
  # the issue - maybe a relevant file changed or something like that
  soft_notify_qc_statuses <- c("Awaiting approval")

  changes_after_closure <- qc_status == "Closed without approval" && stringr::str_detect(diagnostics, "Commit difference")
  no_changes_after_closure <- qc_status == "Closed without approval" && !stringr::str_detect(diagnostics, "Commit difference")

  has_hard_notify_qc_status <- qc_status %in% hard_notify_qc_statuses || changes_after_closure
  has_soft_notify_qc_status <- qc_status %in% soft_notify_qc_statuses || no_changes_after_closure

  if (has_hard_notify_qc_status) {
    return("hard")
  }

  else if (has_soft_notify_qc_status) {
    return("soft")
  }

  else {
    return("none")
  }
}

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

get_hyperlinked_commit <- function(long_commit, file, repo_url) {
  short_commit <- substr(long_commit, 1, 7)
  sha_url <- file.path(repo_url, "blob", long_commit, file)
  hyperlinked_commit <- glue::glue('<a href="{sha_url}" target="_blank">{short_commit}</a>')
  return(hyperlinked_commit)
}

get_hyperlinked_commit_diff <- function(repo_url, old_commit, new_commit) {
  commit_diff_url <- file.path(repo_url, "compare", glue::glue("{old_commit}..{new_commit}"))
  hyperlinked_commit_diff <- glue::glue('<a href="{commit_diff_url}" target="_blank">Commit difference</a>')
  return(hyperlinked_commit_diff)
}

get_imageless_comments <- function(comments_url) {
  comments <- gh::gh(comments_url, .api_url = .le$github_api_url)
  comments_df <- do.call(rbind, lapply(comments, function(x) as.data.frame(t(unlist(x)), stringsAsFactors = FALSE)))
}

get_latest_qc_commit <- function(file_name, issue_body, num_comments, comments_url, initial_qc_commit) {
  if (num_comments == 0) {
    return(list(latest_qc_commit = initial_qc_commit,
                qc_approved = FALSE
                ))
  }

  comments <- get_imageless_comments(comments_url)$body

  # start from latest comment, check if a notify or approve comment (i.e. if it has metadata)
  # if it is, get the current/approved qc commit, then break
  for (comment in rev(comments)) {
    comment_metadata <- get_comment_metadata(comment)
    if (length(comment_metadata) > 0) {
      approved_qc_commit <-  comment_metadata$`approved qc commit`
      if (!is.null(approved_qc_commit)) {
        return(list(
          latest_qc_commit = approved_qc_commit,
          qc_approved = TRUE
        ))
      }
      current_qc_commit <- comment_metadata$`current commit`
      if (!is.null(current_qc_commit)) {
        return(list(
          latest_qc_commit = current_qc_commit,
          qc_approved = FALSE
        ))
      }
    } # if any metadata
  } # for comments


  return(list(
    latest_qc_commit = initial_qc_commit,
    qc_approved = FALSE
  ))
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

  file_pattern <- "- \\*\\*(.*?)\\*\\*\\s*- \\[`(.*?)`\\]\\((.*?)\\)(?:\\s*>\\s*(.*?))?(?:\\n|$)"

  matches <- stringr::str_match_all(relevant_files_section, file_pattern)[[1]]

  relevant_files_df <- data.frame(
    relevant_file_name = matches[,3],
    qc_file_name = issue$title,
    relevant_file_url = matches[,4],
    relevant_file_note = stringr::str_trim(matches[,5]),
    milestone_name = milestone_name,
    issue_number = issue$number,
    stringsAsFactors = FALSE
  )

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
      comparator_commit = character(),
      issue_url = character(),
      notify = character(),
      approve = character(),
      qcer = character()
    )
  )
}
