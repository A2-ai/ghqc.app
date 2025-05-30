get_file_qc_status <- function(file,
                               issue_state,
                               git_status,
                               local_commits,
                               remote_commits,
                               latest_qc_commit,
                               initial_qc_commit,
                               qc_approved) {


  latest_qc_commit_short <- get_hyperlinked_commit(latest_qc_commit, file)
  last_remote_file_change_after_qc_commit <- last_commit_that_changed_file_after_latest_qc_commit(file,
                                                                                                  latest_qc_commit,
                                                                                                  head_commit = remote_commits[1])$last_commit_that_changed_file


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

      if (!is.null(last_remote_file_change_after_qc_commit)) {
        return(pushed_file_changes_after_approved_qc_commit(last_remote_file_change_after_qc_commit, latest_qc_commit_short, file, latest_qc_commit))
      }

      return(approved(latest_qc_commit_short))
    } # Closed and qc approved
  } # qc approved

  # qc not approved
  else {
    # Open and qc not approved
    if (issue_state == "Open") {
      if (!latest_qc_commit %in% local_commits) { # if local commit is older than the latest_qc_commit (even if it didn't change the file - "No file difference" is possible)
        if (latest_qc_commit == initial_qc_commit) {
          return(initial_qc_commit_posted(local_commits, remote_commits, file, latest_qc_commit_short, latest_qc_commit))
        }
        else {
          return(notification_posted(local_commits, remote_commits, file, latest_qc_commit_short, latest_qc_commit))
        }
      }

      if (!is.null(last_remote_file_change_after_qc_commit)) {
        return(file_changes_to_post(git_status, last_remote_file_change_after_qc_commit, file, latest_qc_commit, latest_qc_commit_short))
      }

      return(in_progress(latest_qc_commit_short, latest_qc_commit, last_remote_commit = remote_commits[1]))
    } # Open and qc not approved

    # Closed and qc not approved
    else if (issue_state == "Closed") {
      return(requires_approval(latest_qc_commit_short, last_remote_file_change_after_qc_commit, file, latest_qc_commit))
    } # Closed and qc not approved
  } # qc not approved
} # get_file_qc_status









### QC Statuses


### This is not the same as git_status == "Remote file changes". A notification can be posted when their aren't file changes
notification_posted <- function(local_commits, remote_commits, file, latest_qc_commit_short, latest_qc_commit) {
  qc_status <- "Notification posted"
  latest_local_commit <- local_commits[1]
  local_commit_pushed <- latest_local_commit %in% remote_commits

  # give hyperlink if latest local commit is pushed to remote
  local_commit_short <- ifelse(local_commit_pushed,
                               get_hyperlinked_commit(latest_local_commit, file),
                               substr(latest_local_commit, 1, 7))

  diagnostics_items <- list(
    glue::glue("Last posted commit: {latest_qc_commit_short}"),
    glue::glue("Local commit: {local_commit_short}")
  )

  if (local_commit_pushed) {
    # only give commit diff if local commit is pushed
    commit_diff_url <- get_hyperlinked_commit_diff(old_commit = latest_local_commit,
                                                   new_commit = latest_qc_commit)
    diagnostics_items <- append(diagnostics_items, commit_diff_url)
  }

  diagnostics <- format_diagnostics_list(diagnostics_items)

  list(qc_status = qc_status,
       diagnostics = diagnostics
  )
} # notification_posted

issue_reopened_after_qc_approval <- function(latest_qc_commit_short) {
  qc_status <- "Issue re-opened after approval"

  diagnostics <- format_diagnostics_list(list(glue::glue("Approved QC commit: {latest_qc_commit_short}")))

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



initial_qc_commit_posted <- function(local_commits, remote_commits, file, latest_qc_commit_short, latest_qc_commit) {
  qc_status <- "Initial QC commit posted"
  latest_local_commit <- local_commits[1]
  local_commit_pushed <- latest_local_commit %in% remote_commits

  # give hyperlink if latest local commit is pushed to remote
  local_commit_short <- ifelse(local_commit_pushed,
                               get_hyperlinked_commit(latest_local_commit, file),
                               substr(latest_local_commit, 1, 7))

  diagnostics_items <- list(
    glue::glue("Initial QC commit: {latest_qc_commit_short}"),
    glue::glue("Local commit: {local_commit_short}")
  )

  if (local_commit_pushed) {
    # only give commit diff if local commit is pushed
    commit_diff_url <- get_hyperlinked_commit_diff(old_commit = latest_local_commit,
                                                   new_commit = latest_qc_commit)
    diagnostics_items <- append(diagnostics_items, commit_diff_url)
  }

  diagnostics <- format_diagnostics_list(diagnostics_items)

  list(qc_status = qc_status,
       diagnostics = diagnostics
  )
}

file_changes_to_post <- function(git_status, last_remote_file_change_after_qc_commit, file, latest_qc_commit, latest_qc_commit_short) {
  # don't check for remote file changes off-the-bat in if-then logic, because would rather say "notification posted" if one was posted
  qc_status <- ifelse(!is.na(git_status) && git_status == "Remote file changes", "File changes to pull", "File changes to post")

  last_commit_that_changed_file_short <- get_hyperlinked_commit(last_remote_file_change_after_qc_commit, file)
  commit_diff_url <- get_hyperlinked_commit_diff(old_commit = latest_qc_commit,
                                                 new_commit = last_remote_file_change_after_qc_commit)

  diagnostics <- format_diagnostics_list(list(
    glue::glue("Last posted commit: {latest_qc_commit_short}"),
    glue::glue("Last file change: {last_commit_that_changed_file_short}"),
    commit_diff_url
  ))

  list(qc_status = qc_status,
       diagnostics = diagnostics
  )
} # file_changes_to_post

in_progress <- function(latest_qc_commit_short, last_remote_commit, latest_qc_commit) {
  qc_status <- "Awaiting approval"

  diagnostics_list <- list(glue::glue("Last posted commit: {latest_qc_commit_short}"))

  if (latest_qc_commit != last_remote_commit) {
    last_remote_commit_short <- get_hyperlinked_last_remote_commit_diff(latest_qc_commit, last_remote_commit)
    diagnostics_list <- append(diagnostics_list, glue::glue("Last remote commit: {last_remote_commit_short}"))
  }

  diagnostics <- format_diagnostics_list(diagnostics_list)

  list(qc_status = qc_status,
       diagnostics = diagnostics
  )
} # in_progress

local_uncommitted_file_changes_after_qc_approval <- function(latest_qc_commit_short) {
  # qc_status <- "Local uncommitted file changes after approval"
  qc_status <- "Approved; subsequent file changes"
  diagnostics <- format_diagnostics_list(list(glue::glue("Approved QC commit: {latest_qc_commit_short}")))

  list(qc_status = qc_status,
       diagnostics = diagnostics
  )
} # local_uncommitted_file_changes_after_qc_approval

local_unpushed_commits_with_file_changes_after_qc_approval <- function(file, latest_qc_commit, local_commits, latest_qc_commit_short) {
  # qc_status <- "Local unpushed commits with file changes after approved QC commit"
  qc_status <- "Approved; subsequent file changes"

  last_local_change_after_qc_commit <- last_commit_that_changed_file_after_latest_qc_commit(file,
                                                                                              latest_qc_commit,
                                                                                              head_commit = local_commits[1])$last_commit_that_changed_file
  last_local_change_after_qc_commit_short <-  substr(last_local_change_after_qc_commit, 1, 7)

  diagnostics <- format_diagnostics_list(list(
    glue::glue("Approved QC commit: {latest_qc_commit_short}"),
    glue::glue("Last local file change: {last_local_change_after_qc_commit_short}")
  ))

  list(qc_status = qc_status,
       diagnostics = diagnostics
  )
} # local_unpushed_commits_with_file_changes_after_qc_approval

pushed_file_changes_after_approved_qc_commit <- function(last_remote_file_change_after_qc_commit, latest_qc_commit_short, file, latest_qc_commit) {
  # qc_status <- "Pushed file changes after approved QC commit"
  qc_status <- "Approved; subsequent file changes"

  last_file_change_short <- get_hyperlinked_commit(last_remote_file_change_after_qc_commit, file)
  commit_diff_url <- get_hyperlinked_commit_diff(old_commit = latest_qc_commit,
                                                 new_commit = last_remote_file_change_after_qc_commit)

  diagnostics <- format_diagnostics_list(list(
    glue::glue("Approved QC commit: {latest_qc_commit_short}"),
    glue::glue("Last file change: {last_file_change_short}"),
    commit_diff_url
  ))

  list(qc_status = qc_status,
       diagnostics = diagnostics
  )
} # pushed_file_changes_after_approved_qc_commit

requires_approval <- function(latest_qc_commit_short, last_remote_file_change_after_qc_commit, file, latest_qc_commit) {
  qc_status <- "Closed without approval"
  diagnostics_items <- list(glue::glue("Last posted commit: {latest_qc_commit_short}"))

  # if file changes since last notification
  if (!is.null(last_remote_file_change_after_qc_commit)) {
    last_commit_that_changed_file_short <- get_hyperlinked_commit(last_remote_file_change_after_qc_commit, file)
    commit_diff_url <- get_hyperlinked_commit_diff(old_commit = latest_qc_commit,
                                                   new_commit = last_remote_file_change_after_qc_commit)

    diagnostics_items <- append(diagnostics_items, c(glue::glue("Last file change: {last_commit_that_changed_file_short}"),
                                                     commit_diff_url))
  }

  diagnostics <- format_diagnostics_list(diagnostics_items)

  list(qc_status = qc_status,
       diagnostics = diagnostics
  )
} # requires_approval



get_file_qc_status_non_local_qc_branch <- function(file,
                                                   issue_state,
                                                   remote_commits,
                                                   latest_qc_commit,
                                                   qc_approved) {
  # QC status for files on different branches are less precise
  # because we can't know the local git status
  latest_qc_commit_short <- get_hyperlinked_commit(latest_qc_commit, file)
  last_remote_file_change_after_qc_commit <- last_commit_that_changed_file_after_latest_qc_commit(file,
                                                                                                  latest_qc_commit,
                                                                                                  head_commit = remote_commits[1])$last_commit_that_changed_file

  # qc approved
  if (qc_approved) {
    # Open and qc approved
    if (issue_state == "Open") {
      return(issue_reopened_after_qc_approval(latest_qc_commit_short))
    } # Open and qc approved

    # Closed and qc approved
    else if (issue_state == "Closed") {
      # can't check for local file changes since QC approval
      # but can check for remote file changes since QC approval
      if (!is.null(last_remote_file_change_after_qc_commit)) {
        return(pushed_file_changes_after_approved_qc_commit(last_remote_file_change_after_qc_commit, latest_qc_commit_short, file, latest_qc_commit))
      }

      return(approved(latest_qc_commit_short))
    } # Closed and qc approved
  } # qc approved

  # qc not approved
  else {
    # Open and qc not approved
    if (issue_state == "Open") {
      # can't check if remote changes have been pulled

      if (!is.null(last_remote_file_change_after_qc_commit)) {
        return(file_changes_to_post(git_status = NA_character_, last_remote_file_change_after_qc_commit, file, latest_qc_commit, latest_qc_commit_short))
      }

      return(in_progress(latest_qc_commit_short, last_remote_commit = remote_commits[1]), latest_qc_commit)
    } # Open and qc not approved

    # Closed and qc not approved
    else if (issue_state == "Closed") {
      return(requires_approval(latest_qc_commit_short, last_remote_file_change_after_qc_commit, file, latest_qc_commit))
    } # Closed and qc not approved
  } # qc not approved
}

