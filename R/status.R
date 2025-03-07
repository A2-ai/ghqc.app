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

  check_github_credentials() # TODO: remove after testing

  issues <- get_all_issues_in_milestone(owner, repo, milestone_name)
  issues_df <- map_df(issues, function(x) {
    tibble(
      file = x$title,
      url = x$html_url,
      state = x$state,
      QCer = x$assignee$login
    )
  })
  # TODO: add rest of repo files, determine where they're assoc files or not
}

get_file_git_status <- function(file) {
  if (!file.exists(file)) {
    rlang::abort("file does not exist")
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
  remote_commits <- gert::git_log(ref = "@{upstream}")$commit

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

get_file_qc_status <- function(file, file_git_status) {
  # TODO
}




