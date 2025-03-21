#' Title
#'
#' @param owner
#' @param repo
#' @param milestone_name
#'
#' @return
#' @export
#'
#' @import dplyr purrr
ghqc_status <- function(owner, repo, milestone_name) {
  # TODO: have some functionality to gray out Issues in the Milestone that were
  # initialized on a different branch than the local user's branch

  git_fetch(prune = TRUE)
  creds <- check_github_credentials()
  token <- creds$token
  remote_name <- creds$remote$name

  issues <- get_all_issues_in_milestone(owner, repo, milestone_name)

  issues_df <- map_df(issues, function(issue) {

    issue_number <- issue$number
    branch <- get_branch_from_metadata(owner, repo, issue_number)
    local_commit_log <- gert::git_log(branch, max = 9999)
    remote_commit_log <- gert::git_log(glue::glue("{remote_name}/{branch}"), max = 9999)
    # latest_qc_commit is the most recent commented commit in file's issue
    latest_qc_commit <- get_latest_qc_commit(owner, repo, issue_number, token)

    # get column values for file
    file_name <- issue$title
    url <- issue$html_url
    issue_state <- issue$state
    git_status <- get_file_git_status(file_name,
                                      local_commits = local_commit_log$commit,
                                      remote_commits = remote_commit_log$commit)

    qc_status <- get_file_qc_status(file = file_name,
                                    issue_state = issue_state,
                                    git_status = git_status,
                                    local_commit_log = local_commit_log,
                                    remote_commit_log = remote_commit_log,
                                    latest_qc_commit = latest_qc_commit,
                                    issue_closed_at = issue$closed_at)
    qcer <- issue$assignee$login

    tibble(
      milestone_name = milestone_name,
      file_name = file_name,
      url = url,
      issue_state = issue_state,
      git_status = git_status,
      qc_status = qc_status,
      qcer = qcer,
    )
  })
  # TODO: add rest of repo files, determine whether they're assoc files or not

  return(issues_df)
}

file_has_uncommitted_local_changes <- function(file) {
  status <- gert::git_status()
  uncommitted_local_changes <- ifelse(file %in% status$file, TRUE, FALSE)
  return(uncommitted_local_changes)
}

file_changed_in_unpushed_local_commits <- function(file, local_commits) {
  ahead_behind_status <- check_ahead_behind()
  if (ahead_behind_status$ahead == 0) {
    return(FALSE)
  }

  # get latest pushed remote commit
  remote_commit <- local_commits[1 + ahead_behind_status$ahead]

  # get set of unpushed commits
  unpushed_commits <- local_commits[1:(which(local_commits == remote_commit) - 1)]

  # check if file in set of unpushed commits
  if (length(unpushed_commits > 0)) {
    file_changed_in_unpushed_commit <- any(sapply(unpushed_commits, function(unpushed_commit) {
      diff <- gert::git_diff(unpushed_commit) # get the set of files that changed in this commit
      file %in% diff$old # check if the file is in the set of files
    }))
    return(file_changed_in_unpushed_commit)
  }
  return(FALSE) # no unpushed local commits
}

file_changed_in_remote_commits <- function(file, remote_commits) {
  ahead_behind_status <- check_ahead_behind()
  if (ahead_behind_status$behind == 0) {
    return(FALSE)
  }

  # get latest pushed local commit
  local_commit <- remote_commits[1 + ahead_behind_status$behind]
  unpulled_commits <- remote_commits[1:(which(remote_commits == local_commit) - 1)]

  if (length(unpulled_commits > 0)) {
    file_changed_in_unpulled_commit <- any(sapply(unpulled_commits, function(unpulled_commit) {
      diff <- gert::git_diff(unpulled_commit) # get the set of files that changed in this commit
      file %in% diff$old # check if the file is in the set of files
    }))
    return(file_changed_in_unpulled_commit)
  }
  return(FALSE) # no unpulled local commits
}

get_file_git_status <- function(file, local_commits, remote_commits) {
  if (!file.exists(file)) {
    rlang::abort(glue::glue("file {file} does not exist"))
  }

  if (file_changed_in_remote_commits(file, remote_commits)) {
    return("remote file changes")
  }
  else if (file_changed_in_unpushed_local_commits(file, local_commits)) {
    return("local unpushed commits with file changes")
  }
  else if (file_has_uncommitted_local_changes(file)) {
    return("local uncommitted file changes")
  }
  else {
    return("up-to-date")
  }
}

# commit log may be remote commits, local commits, etc
last_commit_that_changed_file_after_latest_qc_commit <- function(file, latest_qc_commit, commit_log) {

  commits <- commit_log$commit

  # if there are any commits in the log **that change the file** and are newer than the latest_qc_commit
  commits_after_latest_qc_commit <- commits[1:(which(commits == latest_qc_commit) - 1)]
  last_commit_that_changed_file <- NULL
  commit_time <- NULL

  # if there are any local commits newer than the latest_qc_commit
  if (length(commits_after_latest_qc_commit > 0)) {
    # did the file actually change in any of these commits?
    for (commit in commits_after_latest_qc_commit) {
      diff <- gert::git_diff(commit)  # get the set of files that changed in this commit
      if (file %in% diff$old) { # check if the file is in the set of files
        last_commit_that_changed_file <- commit
        commit_time <- commit_log[which(commit_log$commit == last_commit_that_changed_file), ]$time
        break  # exit after first matching commit in log
      }
    }
  } # if any commits after latest qc commit
  return(list(
    last_commit_that_changed_file = last_commit_that_changed_file,
    commit_time = commit_time
    ))
}

get_latest_qc_commit <- function(owner, repo, issue_number, token) {
  init_commit <- get_init_qc_commit(owner, repo, issue_number)
  latest_qc_commit <- init_commit

  comments <- get_issue_comments(owner, repo, issue_number, token)$body

  # start from latest comment, check if a resolve comment (i.e. if it has metadata)
  # if it does, get the current commit, then break
  for (comment in rev(comments)) {
    comment_metadata <- get_comment_metadata(comment)
    if (length(comment_metadata) > 0) {
      latest_qc_commit <- comment_metadata$`current commit`
      break
    }
  }

  return(latest_qc_commit)
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

get_file_qc_status <- function(file,
                               issue_state,
                               git_status,
                               local_commit_log,
                               remote_commit_log,
                               latest_qc_commit,
                               issue_closed_at) {

  local_commits <- local_commit_log$commit
  remote_commits <- remote_commit_log$commit

  ## For open issues
  if (issue_state == "open") {
    # if local commit is older than the latest_qc_commit (even if it didn't change the file)
    # if the file has changed remotely and the latest_qc_commit isn't in the local git log history, then QC update to pull
    if (git_status == "remote file changes" && !latest_qc_commit %in% local_commits) {
      return("QC update to pull")
    }

    last_commit_that_changed_file <- last_commit_that_changed_file_after_latest_qc_commit(file,
                                                                                            latest_qc_commit,
                                                                                            commit_log = local_commit_log)$last_commit_that_changed_file
    if (!is.null(last_commit_that_changed_file)) {
      return("QC update to comment")
    }

    return("QC in progress")
  } # open

  ## For closed issues
  else if (issue_state == "closed") {
    if (git_status == "uncommitted file changes") {
      return("local uncommitted file changes after Issue closure")
    }

    if (git_status == "local unpushed commits with file changes") {
      return("local unpushed commits with file changes with file changes after Issue closure")
    }

    # if there exists a commit that changed the file after the latest qc commit,
    file_change_info <- last_commit_that_changed_file_after_latest_qc_commit(file,
                                                                              latest_qc_commit,
                                                                              commit_log = remote_commit_log)
    last_commit_that_changed_file <- file_change_info$last_commit_that_changed_file

    if (!is.null(last_commit_that_changed_file)) {
      # was the commit before or after Issue closure?
      commit_time <- as.POSIXct(file_change_info$commit_time)
      issue_close_time <- as.POSIXct(issue_closed_at)

      if (commit_time < issue_close_time) {
        return("uncommented pushed file changes before Issue closure")
      }

      return("pushed file changes after Issue closure")
    } # if file changed

   return("QC Complete")
  } # closed

  ## For non-issue files
  else if (issue_state == "no issue") {
    # TODO: Associated file

    # NA
    return("NA")
  }

  else {
    rlang::abort(glue::glue("unrecognized issue state {issue_state}"))
  }

} # get_file_qc_status

