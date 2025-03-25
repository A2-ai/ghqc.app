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
ghqc_status <- function(owner, repo, milestone_names) {
  # TODO: have some functionality to gray out Issues in the Milestone that were
  # initialized on a different branch than the local user's branch

  git_fetch(prune = TRUE)
  root_dir <- rproj_root_dir()
  creds <- check_github_credentials()
  token <- creds$token
  remote_name <- creds$remote$name

  local_log_output <- system("git log --pretty=format:'%H|%an|%ae|%ad|%s'  --date=format:'%Y-%m-%d %H:%M:%S'", intern = TRUE)
  local_commit_log <- read.csv(text = local_log_output, sep = "|", header = FALSE, stringsAsFactors = FALSE)
  names(local_commit_log) <- c("commit", "author_name", "author_email", "time", "message")

  milestone_dfs <- sapply(milestone_names, function(milestone_name) {
    issues <- get_all_issues_in_milestone(owner, repo, milestone_name)

    issues_df <- map_df(issues, function(issue) {
      issue_number <- issue$number
      # branch from metadata might be different from current branch
      branch <- get_branch_from_metadata(owner, repo, issue_number)

      remote_log_output <- system(glue::glue("git log {remote_name}/{branch} --pretty=format:'%H|%an|%ae|%ad|%s'  --date=format:'%Y-%m-%d %H:%M:%S'"), , intern = TRUE)
      remote_commit_log <- read.csv(text = remote_log_output, sep = "|", header = FALSE, stringsAsFactors = FALSE)
      names(remote_commit_log) <- c("commit", "author_name", "author_email", "time", "message")

      # latest_qc_commit is the most recent commented commit in file's issue
      latest_qc_commit <- get_latest_qc_commit(owner, repo, issue_number, token)

      # get column values for file
      file_name <- issue$title
      url <- issue$html_url
      issue_state <- issue$state
      git_status <- get_file_git_status(file_name,
                                        local_commits = local_commit_log$commit,
                                        remote_commits = remote_commit_log$commit)

      qc_status_info <- get_file_qc_status(file = file_name,
                                           issue_state = issue_state,
                                           git_status = git_status,
                                           local_commit_log = local_commit_log,
                                           remote_commit_log = remote_commit_log,
                                           latest_qc_commit = latest_qc_commit,
                                           issue_closed_at = issue$closed_at)
      qc_status <- qc_status_info$qc_status
      diagnostics <- qc_status_info$diagnostics
      qcer <- issue$assignee$login

      tibble(
        milestone_name = milestone_name,
        file_name = file_name,
        url = url,
        issue_state = issue_state,
        qc_status = qc_status,
        git_status = git_status,
        qcer = qcer,
        diagnostics = diagnostics
      )
    })
  }) # milestone_dfs

  all_milestones_df <- dplyr::bind_rows(milestone_dfs)

  # add rest of repo files, determine whether they're assoc files or not
  files_with_issues <- unique(all_milestones_df$file_name)
  files_in_repo <- list.files(path = root_dir, recursive = TRUE)
  files_without_issues <- files_in_repo[!files_in_repo %in% files_with_issues]

  repo_files_df <- map_df(files_without_issues, function(file) {
    # get current branch
    branch <- gert::git_branch()
    remote_log_output <- system(glue::glue("git log {remote_name}/{branch} --pretty=format:'%H|%an|%ae|%ad|%s'  --date=format:'%Y-%m-%d %H:%M:%S'"), , intern = TRUE)
    remote_commit_log <- read.csv(text = remote_log_output, sep = "|", header = FALSE, stringsAsFactors = FALSE)
    names(remote_commit_log) <- c("commit", "author_name", "author_email", "time", "message")

    git_status <- get_file_git_status(file,
                                      local_commits = local_commit_log$commit,
                                      remote_commits = remote_commit_log$commit)

    tibble(
      milestone_name = "NA",
      file_name = file,
      url = NA,
      issue_state = "no Issue",
      qc_status = "NA",
      git_status = git_status,
      qcer = "NA",
      diagnostics = "NA"
    )
  })

  all_files_df <- dplyr::bind_rows(all_milestones_df, repo_files_df)

  return(all_files_df)
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
  index_before_latest_qc_commit <- which(commits == latest_qc_commit) - 1

  commits_after_latest_qc_commit <- {
    if (index_before_latest_qc_commit == 0) list()
    else commits[1:index_before_latest_qc_commit]
  }

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

  latest_qc_commit_short <- substr(latest_qc_commit, 1, 7)

  ## For open issues
  if (issue_state == "open") {
    # if local commit is older than the latest_qc_commit (even if it didn't change the file)
    # if the file has changed remotely and the latest_qc_commit isn't in the local git log history, then QC update to pull
    if (git_status == "remote file changes" && !latest_qc_commit %in% local_commits) {
      local_commit_short <- local_commits[1]
      return(list(qc_status = "QC update to pull",
                  diagnostics = glue::glue("current qc commit: {latest_qc_commit_short}, local commit: {local_commit_short}")
                  ))
    }

    last_remote_commit_that_changed_file <- last_commit_that_changed_file_after_latest_qc_commit(file,
                                                                                            latest_qc_commit,
                                                                                            commit_log = remote_commit_log)$last_commit_that_changed_file
    if (!is.null(last_remote_commit_that_changed_file)) {
      last_commit_that_changed_file_short <- substr(last_remote_commit_that_changed_file, 1, 7)
      return(list(qc_status = "QC update to comment",
                  diagnostics = glue::glue("current qc commit: {latest_qc_commit_short}, most recent remote file change in commit: {last_commit_that_changed_file_short}")
                  ))
    }

    return(list(qc_status = "QC in progress",
                diagnostics = "NA"
                ))
  } # open

  ## For closed issues
  else if (issue_state == "closed") {
    if (git_status == "local uncommitted file changes") {
      return(list(qc_status = "local uncommitted file changes after Issue closure",
                  diagnostics = glue::glue("final qc commit: {latest_qc_commit_short}, local uncommitted file changes after final qc commit")
                  ))
    }

    if (git_status == "local unpushed commits with file changes") {
      last_local_commit_that_changed_file <- last_commit_that_changed_file_after_latest_qc_commit(file,
                                                                                            latest_qc_commit,
                                                                                            commit_log = local_commit_log)$last_commit_that_changed_file
      last_local_commit_that_changed_file_short <-  substr(last_local_commit_that_changed_file, 1, 7)

      return(list(qc_status = "local unpushed commits with file changes with file changes after Issue closure",
                  diagnostics = glue::glue("final qc commit: {latest_qc_commit_short}, most recent local file change in commit: {last_local_commit_that_changed_file_short}")
                                           ))
    }

    # if there exists a commit that changed the file after the latest qc commit,
    file_change_info <- last_commit_that_changed_file_after_latest_qc_commit(file,
                                                                              latest_qc_commit,
                                                                              commit_log = remote_commit_log)
    last_remote_commit_that_changed_file <- file_change_info$last_commit_that_changed_file

    if (!is.null(last_remote_commit_that_changed_file)) {
      # was the commit before or after Issue closure?
      commit_time <- as.POSIXct(file_change_info$commit_time)
      issue_close_time <- as.POSIXct(issue_closed_at, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

      last_commit_that_changed_file_short <- substr(last_remote_commit_that_changed_file, 1, 7)

      if (commit_time < issue_close_time) {
        return(list(qc_status = "uncommented pushed file changes before Issue closure",
                    diagnostics = glue::glue("final qc commit: {latest_qc_commit_short}, most recent file change in commit: {last_commit_that_changed_file_short}")
                    ))
      }
      return(list(qc_status = "pushed file changes after Issue closure",
             diagnostics = glue::glue("final qc commit: {latest_qc_commit_short}, most recent file change in commit: {last_commit_that_changed_file_short}")
             ))
    } # if file changed

   return(list(qc_status = "QC Complete",
               diagnostics = "NA"))
  } # closed

  ## For non-issue files
  else if (issue_state == "no issue") {
    # TODO: Associated file

    # NA
    return(list(qc_status = "NA",
                diagnostics = "NA"
                ))
  }

  else {
    rlang::abort(glue::glue("unrecognized issue state {issue_state}"))
  }

} # get_file_qc_status

