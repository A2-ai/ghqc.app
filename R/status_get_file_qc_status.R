get_file_qc_status <- function(file,
                               issue_state,
                               git_status,
                               local_commits,
                               remote_commits,
                               latest_qc_commit,
                               repo_url,
                               qc_approved) {

  latest_qc_commit_short <- get_hyperlinked_commit(latest_qc_commit, file, repo_url)

  # qc approved
  if (qc_approved) {
    # Open and qc approved
    if (issue_state == "Open") {
      return(issue_reopened_after_qc_approval(latest_qc_commit_short))
    } # Open and qc approved

    # Closed and qc approved
    else if (issue_state == "Closed") {
      # check for file changes since QC approval
      if (git_status == "Local uncommitted file changes") {
        return(local_uncommitted_file_changes_after_qc_approval(latest_qc_commit_short))
      }

      if (git_status == "Local unpushed commits with file changes") {
        return(local_unpushed_commits_with_file_changes_after_qc_approval(file, latest_qc_commit, local_commits, latest_qc_commit_short))
      }

      last_remote_commit_that_changed_file_after_aproved_qc_commit <- last_commit_that_changed_file_after_latest_qc_commit(file,
                                                                                                                           latest_qc_commit,
                                                                                                                           head_commit = remote_commits[1])$last_commit_that_changed_file
      if (!is.null(last_remote_commit_that_changed_file_after_aproved_qc_commit)) {
        return(pushed_file_changes_after_approved_qc_commit(last_remote_commit_that_changed_file_after_aproved_qc_commit, latest_qc_commit_short, repo_url))
      }

      return(approved(latest_qc_commit_short))
    } # Closed and qc approved
  } # qc approved

  # qc not approved
  else {
    # Open and qc not approved
    if (issue_state == "Open") {
      if (!latest_qc_commit %in% local_commits) { # if local commit is older than the latest_qc_commit (even if it didn't change the file - "No file difference" is possible)
        return(notification_posted(local_commits, remote_commits, file, repo_url, latest_qc_commit_short, latest_qc_commit))
      }

      last_remote_commit_that_changed_file <- last_commit_that_changed_file_after_latest_qc_commit(file,
                                                                                                   latest_qc_commit,
                                                                                                   head_commit = remote_commits[1])$last_commit_that_changed_file
      if (!is.null(last_remote_commit_that_changed_file)) {
        return(notification_pending(last_remote_commit_that_changed_file, file, repo_url, latest_qc_commit, latest_qc_commit_short))
      }

      return(in_progress(latest_qc_commit_short))
    } # Open and qc not approved

    # Closed and qc not approved
    else if (issue_state == "Closed") {
      return(approval_pending(latest_qc_commit_short))
    } # Closed and qc not approved
  } # qc not approved
} # get_file_qc_status



### QC Statuses



issue_reopened_after_qc_approval <- function(latest_qc_commit_short) {
  qc_status <- "Issue re-opened after QC approval"

  diagnostics_list <- format_diagnostics_list(list(glue::glue("Approved QC commit: {latest_qc_commit_short}")))
  diagnostics <- glue::glue("Close Issue to complete QC, or delete QC approval comment to resume QC review.{vspace()}
                                {diagnostics_list}")

  list(qc_status = qc_status,
       diagnostics = diagnostics
  )
} # issue_reopened_after_qc_approval

approved <- function(latest_qc_commit_short) {
  qc_status <- "Approved"
  diagnostics <- format_diagnostics_list(list(glue::glue("Approved QC commit: {latest_qc_commit_short}")))

  list(qc_status = qc_status,
       diagnostics = diagnostics
  )
} # approved

### This is not the same as git_status == "Remote file changes"

notification_posted <- function(local_commits, remote_commits, file, repo_url, latest_qc_commit_short, latest_qc_commit) {
  qc_status <- "Notification posted"
  latest_local_commit <- local_commits[1]
  local_commit_pushed <- latest_local_commit %in% remote_commits

  # give hyperlink if latest local commit is pushed to remote
  local_commit_short <- ifelse(local_commit_pushed,
                               get_hyperlinked_commit(latest_local_commit, file, repo_url),
                               substr(latest_local_commit, 1, 7))

  diagnostics_items <- list(
    glue::glue("Last posted commit: {latest_qc_commit_short}"),
    glue::glue("Local commit: {local_commit_short}")
  )

  if (local_commit_pushed) {
    # only give commit diff if local commit is pushed
    commit_diff_url <- get_hyperlinked_commit_diff(repo_url,
                                                   old_commit = latest_local_commit,
                                                   new_commit = latest_qc_commit)
    diagnostics_items <- append(diagnostics_items, commit_diff_url)
  }

  diagnostics <- format_diagnostics_list(diagnostics_items)

  list(qc_status = qc_status,
       diagnostics = diagnostics
  )
} # notification_posted

notification_pending <- function(last_remote_commit_that_changed_file, file, repo_url, latest_qc_commit, latest_qc_commit_short) {
  qc_status <- "Notification pending"
  last_commit_that_changed_file_short <- get_hyperlinked_commit(last_remote_commit_that_changed_file, file, repo_url)
  commit_diff_url <- get_hyperlinked_commit_diff(repo_url,
                                                 old_commit = latest_qc_commit,
                                                 new_commit = last_remote_commit_that_changed_file)

  diagnostics <- format_diagnostics_list(list(
    glue::glue("Last posted commit: {latest_qc_commit_short}"),
    glue::glue("Last file change: {last_commit_that_changed_file_short}"),
    commit_diff_url
  ))

  list(qc_status = qc_status,
       diagnostics = diagnostics
  )
} # notification_pending

in_progress <- function(latest_qc_commit_short) {
  qc_status <- "In progress"
  diagnostics <- format_diagnostics_list(list(glue::glue("Last posted commit: {latest_qc_commit_short}")))

  list(qc_status = qc_status,
       diagnostics = diagnostics
  )
} # in_progress

local_uncommitted_file_changes_after_qc_approval <- function(latest_qc_commit_short) {
  qc_status <- "Local uncommitted file changes after QC approval"
  diagnostics <- format_diagnostics_list(list(glue::glue("Approved QC commit: {latest_qc_commit_short}")))

  list(qc_status = qc_status,
       diagnostics = diagnostics
  )
} # local_uncommitted_file_changes_after_qc_approval

local_unpushed_commits_with_file_changes_after_qc_approval <- function(file, latest_qc_commit, local_commits, latest_qc_commit_short) {
  qc_status <- "Local unpushed commits with file changes after QC approval"

  last_local_commit_that_changed_file <- last_commit_that_changed_file_after_latest_qc_commit(file,
                                                                                              latest_qc_commit,
                                                                                              head_commit = local_commits[1])$last_commit_that_changed_file
  last_local_commit_that_changed_file_short <-  substr(last_local_commit_that_changed_file, 1, 7)

  diagnostics <- format_diagnostics_list(list(
    glue::glue("Approved QC commit: {latest_qc_commit_short}"),
    glue::glue("Last local file change: {last_local_commit_that_changed_file_short}")
  ))

  list(qc_status = qc_status,
       diagnostics = diagnostics
  )
} # local_unpushed_commits_with_file_changes_after_qc_approval

pushed_file_changes_after_approved_qc_commit <- function(last_remote_commit_that_changed_file_after_aproved_qc_commit, latest_qc_commit_short, repo_url) {
  qc_status <- "Pushed file changes after approved QC commit"

  last_file_change_short <- get_hyperlinked_commit(last_remote_commit_that_changed_file_after_aproved_qc_commit, file, repo_url)
  commit_diff_url <- get_hyperlinked_commit_diff(repo_url,
                                                 old_commit = latest_qc_commit,
                                                 new_commit = last_remote_commit_that_changed_file_after_aproved_qc_commit)

  diagnostics <- format_diagnostics_list(list(
    glue::glue("Approved QC commit: {latest_qc_commit_short}"),
    glue::glue("Last file change: {last_file_change_short}"),
    commit_diff_url
  ))

  list(qc_status = qc_status,
       diagnostics = diagnostics
  )
} # pushed_file_changes_after_approved_qc_commit

approval_pending <- function(latest_qc_commit_short) {
  qc_status <- "Approval pending"
  diagnostics <- format_diagnostics_list(list(glue::glue("Last posted commit: {latest_qc_commit_short}")))

  list(qc_status = qc_status,
       diagnostics = diagnostics
  )
} # approval_pending

