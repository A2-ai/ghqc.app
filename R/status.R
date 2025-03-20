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

  creds <- check_github_credentials()
  token <- creds$token

  issues <- get_all_issues_in_milestone(owner, repo, milestone_name)
  issues_df <- map_df(issues, function(issue) {
    file_name <- issue$title
    url <- issue$html_url
    issue_state <- issue$state
    git_status <- get_file_git_status(file_name)
    qc_status <- get_file_qc_status(owner = owner,
                                    repo = repo,
                                    issue_number = issue$number,
                                    issue_state = issue_state,
                                    git_status = git_status,
                                    token = token)
    qcer <- issue$assignee$login

    tibble(
      file_name = file_name,
      url = url,
      issue_state = issue_state,
      git_status = git_status,
      qc_status = qc_status,
      qcer = qcer,
    )
  })
  # TODO: add rest of repo files, determine where they're assoc files or not
}

get_file_git_status <- function(file) {
  if (!file.exists(file)) {
    rlang::abort(glue::glue("file {file} does not exist"))
  }

  if (file_has_remote_commits(file)) {
    return("remote file changes")
  }
  else if (file_has_unpushed_commits(file)) {
    return("local unpushed commits")
  }
  else if (file_has_uncommitted_local_changes(file)) {
    return("local uncommitted file changes")
  }
  else {
    return("up-to-date")
  }
}

file_has_uncommitted_local_changes <- function(file) {
  status <- gert::git_status()
  uncommitted_local_changes <- ifelse(file %in% status$file, TRUE, FALSE)
  return(uncommitted_local_changes)
}

file_has_unpushed_commits <- function(file) {
  ahead_behind_status <- check_ahead_behind()
  if (ahead_behind_status$ahead == 0) {
    return(FALSE)
  }

  # get local commits
  local_commits <- gert::git_log(max = 9999)$commit

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

file_has_remote_commits <- function(file) {
  ahead_behind_status <- check_ahead_behind()
  if (ahead_behind_status$behind == 0) {
    return(FALSE)
  }

  # get remote commits
  remote_commits <- gert::git_log(ref = "@{upstream}", max = 9999)$commit

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

get_file_qc_status <- function(owner, repo, issue_number, issue_state, git_status, token) {
  # latest_qc_commit == latest commented commit in file's issue
  latest_qc_commit <- get_latest_qc_commit(owner, repo, issue_number, token)

  ## For open issues
  if (issue_state == "open") {
    # QC update to pull
    # if local commit is older than the latest_qc_commit (even if it didn't change the file)
    local_commits <- gert::git_log(max = 9999)$commit
    # if the file has changed remotely and the latest_qc_commit isn't in the local git log history, then QC update to pull
    if (git_status == "remote file changes" && !latest_qc_commit %in% local_commits) {
      return("QC update to pull")
    }

    # QC update to comment
    # if there are any local commits **that change the file** and are newer than the latest_qc_commit
    commits_after_latest_qc_commit <- local_commits[1:(which(local_commits == latest_qc_commit) - 1)]
    # if there are any local commits newer than the latest_qc_commit
    if (length(commits_after_latest_qc_commit > 0)) {
      # did the file actually change in any of these commits?
      file_changed_after_latest_qc_commit <- any(sapply(commits_after_latest_qc_commit, function(commit) {
        diff <- gert::git_diff(commit) # get the set of files that changed in this commit
        file %in% diff$old # check if the file is in the set of files
      }))
      if (file_changed_after_latest_qc_commit) {
        return("QC update to comment")
      }
    }

    # QC in progress
    return("QC in progress")
  } # open

  ## For closed issues
  else if (issue_state == "closed") {
    # local uncommitted file changes after Issue closure
    # if file_git_status is uncommitted file changes

    # local unpushed commits after Issue closure
    # if file_git_status is local unpushed commits

    # pushed file changes after Issue closure


    # uncommented pushed file changes before Issue closure

  } # closed


  # uncommented pushed file changes before Issue closure

  ## For non-issue files
  else if (issue_state == "no issue") {
    # Associated file

    # NA
  }

  else {
    rlang::abort(glue::glue("unrecognized issue state {issue_state}"))
  }

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




