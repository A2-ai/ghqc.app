#' @import dplyr purrr
#' @importFrom log4r info debug warn error
ghqc_status <- function(milestone_names,
                        org,
                        repo,
                        root_dir,
                        remote_name,
                        local_commit_log,
                        current_branch,
                        include_non_issue_repo_files) {

  local_log_output <- system("git log --pretty=format:'%H|%an|%ae|%ad|%s'  --date=format:'%Y-%m-%d %H:%M:%S'", intern = TRUE)
  local_commit_log <- read.csv(text = local_log_output, sep = "|", header = FALSE, stringsAsFactors = FALSE)
  names(local_commit_log) <- c("commit", "author_name", "author_email", "time", "message")


  all_relevant_files <- list()



  status_df <- map_df(milestone_names, function(milestone_name) {
    issues <- get_all_issues_in_milestone(org, repo, milestone_name)
    issues_df <- map_df(issues, function(issue) {
      # get column values for file
      file_name <- issue$title
      debug(.le$logger, glue::glue("Retrieving QC status for {file_name}..."))
      file_url <- issue$html_url
      file_with_url <- glue::glue('<a href="{file_url}" target="_blank">{file_name}</a>')

      # update relevant files list
      relevant_files_in_issue <- get_relevant_files(issue, milestone_name)
      all_relevant_files <<- dplyr::bind_rows(all_relevant_files, relevant_files_in_issue)
      debug(.le$logger, glue::glue("Updated relevant files list"))

      issue_number <- issue$number
      # branch from metadata might be different from current branch
      metadata_branch <- get_branch_from_metadata(org, repo, issue_number)

      remote_log_output <- system(glue::glue("git log {remote_name}/{metadata_branch} --pretty=format:'%H|%an|%ae|%ad|%s'  --date=format:'%Y-%m-%d %H:%M:%S'"), , intern = TRUE)
      remote_commit_log <- read.csv(text = remote_log_output, sep = "|", header = FALSE, stringsAsFactors = FALSE)
      names(remote_commit_log) <- c("commit", "author_name", "author_email", "time", "message")
      debug(.le$logger, glue::glue("Retrieved remote commit log"))



      # latest_qc_commit is the most recent commented commit in file's issue
      latest_qc_commit <- get_latest_qc_commit(org, repo, issue_number)
      debug(.le$logger, glue::glue("Retrieved current QC commit for {file_name}: {latest_qc_commit}"))

      repo_url <- stringr::str_extract(file_url, ".*(?=/issues)")

      # capitalize Open and Closed
      issue_state <- ifelse(issue$state == "open", "Open", "Closed")
      debug(.le$logger, glue::glue("Retrieving git status for {file_name}..."))
      start_time <-  Sys.time()
      git_status <- get_file_git_status(file_name,
                                        local_commits = local_commit_log$commit,
                                        remote_commits = remote_commit_log$commit)
      end_time <- Sys.time()
      elapsed <- round(as.numeric(difftime(end_time, start_time, units = "secs")), 3)
      debug(.le$logger, glue::glue("Retrieved git status for {file_name} in {elapsed} seconds"))

      tryCatch({
        debug(.le$logger, glue::glue("Retrieving QC status info for {file_name}..."))
        start_time <-  Sys.time()
        qc_status_info <- get_file_qc_status(file = file_name,
                                             issue_state = issue_state,
                                             git_status = git_status,
                                             local_commit_log = local_commit_log,
                                             remote_commit_log = remote_commit_log,
                                             latest_qc_commit = latest_qc_commit,
                                             issue_closed_at = issue$closed_at,
                                             metadata_branch = metadata_branch,
                                             current_branch = current_branch,
                                             repo_url = repo_url
        )
        end_time <- Sys.time()
        elapsed <- round(as.numeric(difftime(end_time, start_time, units = "secs")), 3)
        debug(.le$logger, glue::glue("Retrieved QC status info for {file_name} in {elapsed} seconds"))
      }, error = function(e) {
        qc_status_info <- list(
          qc_status = "Error",
          diagnostics = conditionMessage(e)
        )
      })

      qc_status <- qc_status_info$qc_status
      diagnostics <- qc_status_info$diagnostics
      qcer <- ifelse(!is.null(issue$assignee$login), issue$assignee$login, "No QCer")

      milestone_url <- get_milestone_url(org, repo, milestone_name)
      milestone_with_url <- glue::glue('<a href="{milestone_url}" target="_blank">{milestone_name}</a>')

      res <- tibble(
        milestone_name = milestone_name,
        milestone_with_url = milestone_with_url,
        file_name = file_name,
        file_with_url = file_with_url,
        issue_state = issue_state,
        qc_status = qc_status,
        git_status = git_status,
        diagnostics = diagnostics,
        qcer = qcer
      ) # tibble

      info(.le$logger, glue::glue("Retrieved QC status for {file_name}"))

      return(res)
    }) # issues_df
  }) # status_df

  if (include_non_issue_repo_files) {
    files_with_issues <- unique(status_df$file_name)
    repo_files_df <- create_non_issue_repo_files_df(files_with_issues, remote_name, current_branch, local_commit_log, root_dir, all_relevant_files)
    status_df <- dplyr::bind_rows(status_df, repo_files_df)
  }

  # table editing: add filters, sort, etc

  # rename columns
  colnames(status_df) <- c("Milestone without url", "Milestone", "File without url", "File", "Issue State", "QC Status", "Git Status", "Diagnostics", "QCer")

  # make factors
  status_df <- status_df %>%
    dplyr::mutate(across(
      c(`Issue State`, `QC Status`, `Git Status`, QCer),
      as.factor
    ))

  return(list(
    status = status_df,
    relevant_files = all_relevant_files
    )
  )
}


create_non_issue_repo_files_df <- function(files_with_issues, remote_name, current_branch, local_commit_log, root_dir, all_relevant_files) {
  files_with_issues <- unique(files_with_issues)

  # add rest of repo files, determine whether they're relevant files or not
  git_files <- gert::git_ls(repo = root_dir)$path
  files_in_repo <- git_files[!stringr::str_detect(git_files, "^\\.") & !stringr::str_detect(git_files, "\\.Rproj$")]
  files_without_issues <- files_in_repo[!files_in_repo %in% files_with_issues]

  remote_log_output <- system(glue::glue("git log {remote_name}/{current_branch} --pretty=format:'%H|%an|%ae|%ad|%s'  --date=format:'%Y-%m-%d %H:%M:%S'"), , intern = TRUE)
  remote_commit_log <- read.csv(text = remote_log_output, sep = "|", header = FALSE, stringsAsFactors = FALSE)
  names(remote_commit_log) <- c("commit", "author_name", "author_email", "time", "message")

  repo_files_df <- map_df(files_without_issues, function(file) {
    debug(.le$logger, glue::glue("Retrieving git status for {file}..."))
    start_time <- Sys.time()
    git_status <- get_file_git_status(file,
                                      local_commits = local_commit_log$commit,
                                      remote_commits = remote_commit_log$commit)
    end_time <- Sys.time()
    elapsed <- round(as.numeric(difftime(end_time, start_time, units = "secs")), 3)
    debug(.le$logger, glue::glue("Retrieved git status for {file} in {elapsed} seconds"))

    qc_status_info <- {
      if (file %in% all_relevant_files$relevant_file_name) {
        # get rows in which file is the relevant file
        relevant_file_instances <- all_relevant_files[which(all_relevant_files$relevant_file_name == file), ]
        # loop over rows
        qc_file_strings <- lapply(split(relevant_file_instances, seq_len(nrow(relevant_file_instances))), function(relevant_file_instance) {
          qc_file <- relevant_file_instance$qc_file_name
          issue_number <- relevant_file_instance$issue_number
          milestone_name <- relevant_file_instance$milestone_name
          qc_file_string <- glue::glue("#{issue_number}: {qc_file} ({milestone_name})")
          return(qc_file_string)
        })

        qc_status_string <- glue::glue_collapse(qc_file_strings, sep = ", ", last = " and ")

        list(qc_status = "Relevant file",
             diagnostics = glue::glue("Relevant file in Issues: {qc_status_string}")
        )
      }
      else {
        list(qc_status = NA,
             diagnostics = NA)
      }
    } # qc_status_info

    tibble(
      `Milestone without url` = "No Milestone",
      Milestone = "No Milestone",
      `File without url` = file,
      File = file,
      `Issue State` = "No Issue",
      `QC Status` = qc_status_info$qc_status,
      `Git Status` = git_status,
      Diagnostics = qc_status_info$diagnostics,
      QCer = NA
    )
  })
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
    return("File does not exist locally")
  }

  if (file_changed_in_remote_commits(file, remote_commits)) {
    return("Remote file changes")
  }
  else if (file_changed_in_unpushed_local_commits(file, local_commits)) {
    return("Local unpushed commits with file changes")
  }
  else if (file_has_uncommitted_local_changes(file)) {
    return("Local uncommitted file changes")
  }
  else {
    return("Up-to-date")
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

get_imageless_comments <- function(org, repo, issue_number) {
  comments <- gh::gh(
    "GET /repos/:org/:repo/issues/:issue_number/comments", .api_url = .le$github_api_url,
    org = org,
    repo = repo,
    issue_number = issue_number
  )
  comments_df <- do.call(rbind, lapply(comments, function(x) as.data.frame(t(unlist(x)), stringsAsFactors = FALSE)))
}

get_latest_qc_commit <- function(org, repo, issue_number) {
  init_commit <- get_init_qc_commit(org, repo, issue_number)
  latest_qc_commit <- init_commit

  comments <- get_imageless_comments(org, repo, issue_number)$body

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

get_hyperlinked_commit <- function(long_commit, file, repo_url) {
  short_commit <- substr(long_commit, 1, 7)
  sha_url <- file.path(repo_url, "blob", short_commit, file)
  hyperlinked_commit <- glue::glue('<a href="{sha_url}" target="_blank">{short_commit}</a>')
  return(hyperlinked_commit)
}

get_hyperlinked_commit_diff <- function(repo_url, old_commit, new_commit) {
  commit_diff_url <- file.path(repo_url, "compare", glue::glue("{old_commit}..{new_commit}"))
  hyperlinked_commit_diff <- glue::glue('<a href="{commit_diff_url}" target="_blank">Commit difference</a>')
  return(hyperlinked_commit_diff)
}

get_file_qc_status <- function(file,
                               issue_state,
                               git_status,
                               local_commit_log,
                               remote_commit_log,
                               latest_qc_commit,
                               issue_closed_at,
                               metadata_branch,
                               current_branch,
                               repo_url) {

  if (metadata_branch != current_branch) {
    return(list(
      qc_status = "QC Status not available",
      diagnostics = glue::glue("QC initialized on branch: {metadata_branch}<br>
                               Current branch: {current_branch}<br>
                               Switch to {metadata_branch} to view QC status.")
    ))
  }

  local_commits <- local_commit_log$commit
  remote_commits <- remote_commit_log$commit

  latest_qc_commit_short <- get_hyperlinked_commit(latest_qc_commit, file, repo_url)

  ## For open issues
  if (issue_state == "Open") {

    ### # Pull current QC commit
    if (!latest_qc_commit %in% local_commits) {  # if local commit is older than the latest_qc_commit (even if it didn't change the file - "No file difference" is possible)
      latest_local_commit <- local_commits[1]
      local_commit_pushed <- latest_local_commit %in% remote_commits

      # give hyperlink if latest local commit is pushed to remote
      local_commit_short <- ifelse(local_commit_pushed,
                                   get_hyperlinked_commit(latest_local_commit, file, repo_url),
                                   substr(latest_local_commit, 1, 7))

      # likewise, only give commit diff if local commit is pushed
      commit_diff_url <- ifelse(local_commit_pushed,
                            paste0("<br>", get_hyperlinked_commit_diff(repo_url, latest_qc_commit, latest_local_commit)),
                            "")


      return(list(qc_status = "Pull current QC commit",
                  diagnostics = glue::glue("Local commit is behind current QC commit.<br>
                                           Current QC commit was updated with ghqc_resolve_app.<br>
                                           Current QC commit: {latest_qc_commit_short}<br>
                                           Local commit: {local_commit_short}
                                           {commit_diff_url}") # no <br> in above line^ because there might not be a commit_diff_url
                  ))
    } # Pull current QC commit

    ### Comment current QC commit
    last_remote_commit_that_changed_file <- last_commit_that_changed_file_after_latest_qc_commit(file,
                                                                                            latest_qc_commit,
                                                                                            commit_log = remote_commit_log)$last_commit_that_changed_file
    if (!is.null(last_remote_commit_that_changed_file)) {
      last_commit_that_changed_file_short <- get_hyperlinked_commit(last_remote_commit_that_changed_file, file, repo_url)
      commit_diff_url <- get_hyperlinked_commit_diff(repo_url, latest_qc_commit, last_remote_commit_that_changed_file)


      return(list(qc_status = "Comment current QC commit",
                  diagnostics = glue::glue("Remote file commit is ahead of current QC commit.<br>
                                           Update the current QC commit with ghqc_resolve_app.<br>
                                           Current QC commit: {latest_qc_commit_short}<br>
                                           Most recent remote file change in commit: {last_commit_that_changed_file_short}<br>
                                           {commit_diff_url}")
                  ))
    } # Comment current QC commit

    ### QC in progress
    return(list(qc_status = "QC in progress",
                diagnostics = glue::glue("Current QC commit: {latest_qc_commit_short}")
                ))
  } # open

  ## For closed issues
  else if (issue_state == "Closed") {
    ### Local uncommitted file changes after Issue closure
    if (git_status == "Local uncommitted file changes") {
      return(list(qc_status = "Local uncommitted file changes after Issue closure",
                  diagnostics = glue::glue("Final QC commit: {latest_qc_commit_short}")
                  ))
    }

    ### Local unpushed commits with file changes with file changes after Issue closure
    if (git_status == "Local unpushed commits with file changes") {
      last_local_commit_that_changed_file <- last_commit_that_changed_file_after_latest_qc_commit(file,
                                                                                            latest_qc_commit,
                                                                                            commit_log = local_commit_log)$last_commit_that_changed_file
      last_local_commit_that_changed_file_short <-  substr(last_local_commit_that_changed_file, 1, 7)

      last_local_commit_that_changed_file_pushed <- last_local_commit_that_changed_file %in% remote_commits

      # give hyperlink if latest local commit is pushed to remote
      local_commit_short <- ifelse(last_local_commit_that_changed_file_pushed,
                                   get_hyperlinked_commit(last_local_commit_that_changed_file, file, repo_url),
                                   substr(last_local_commit_that_changed_file, 1, 7))

      # likewise, only give commit diff if local commit is pushed
      commit_diff_url <- ifelse(last_local_commit_that_changed_file_pushed,
                                paste0("<br>", get_hyperlinked_commit_diff(repo_url, latest_qc_commit, last_local_commit_that_changed_file)),
                                "")

      return(list(qc_status = "Local unpushed commits with file changes after Issue closure",
                  diagnostics = glue::glue("Final QC commit: {latest_qc_commit_short}<br>
                                           Most recent local file change in commit: {last_local_commit_that_changed_file_short}
                                           {commit_diff_url}")
                                           ))
    }

    ### Pushed file changes
    # if there exists a remote commit that changed the file after the latest qc commit,
    file_change_info <- last_commit_that_changed_file_after_latest_qc_commit(file,
                                                                              latest_qc_commit,
                                                                              commit_log = remote_commit_log)
    last_remote_commit_that_changed_file <- file_change_info$last_commit_that_changed_file

    if (!is.null(last_remote_commit_that_changed_file)) {
      # was the commit before or after Issue closure?
      commit_time <- as.POSIXct(file_change_info$commit_time)
      issue_close_time <- as.POSIXct(issue_closed_at, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

      last_commit_that_changed_file_short <- get_hyperlinked_commit(last_remote_commit_that_changed_file, file, repo_url)
      commit_diff_url <- get_hyperlinked_commit_diff(repo_url, latest_qc_commit, last_remote_commit_that_changed_file)

      if (commit_time < issue_close_time) {
        return(list(qc_status = "Uncommented pushed file changes before Issue closure",
                    diagnostics = glue::glue("Final QC commit: {latest_qc_commit_short}<br>
                                             Most recent remote file change in commit: {last_commit_that_changed_file_short}<br>
                                             {commit_diff_url}")
                    ))
      }
      return(list(qc_status = "Pushed file changes after Issue closure",
             diagnostics = glue::glue("Final QC commit: {latest_qc_commit_short}<br>
                                      Most recent remote file change in commit: {last_commit_that_changed_file_short}<br>
                                      {commit_diff_url}")
             ))
    } # if file changed

   return(list(qc_status = "QC Complete",
               diagnostics = glue::glue("Final QC commit: {latest_qc_commit_short}")))
  } # closed

  ## For non-issue files
  else if (issue_state == "no issue") {
    return(list(qc_status = NA,
                diagnostics = NA
                ))
  }

  else {
    rlang::abort(glue::glue("unrecognized issue state {issue_state}"))
  }

} # get_file_qc_status

get_relevant_files <- function(issue, milestone_name) {
  # parse issue body for associated files
  issue_body <- issue$body

  if (!stringr::str_detect(issue_body, "## Relevant files")) { # if no associated relevant files
    return( # return an empty data frame
      tibble(
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

  file_pattern <- "- \\*\\*(.*?)\\*\\*\\s*- \\[`.*?`\\]\\((.*?)\\)(?:\\s*>\\s*(.*?))?(?:\\n|$)"

  matches <- stringr::str_match_all(relevant_files_section, file_pattern)[[1]]

  relevant_files_df <- data.frame(
    relevant_file_name = matches[,2],
    qc_file_name = issue$title,
    relevant_file_url = matches[,3],
    relevant_file_note = stringr::str_trim(matches[,4]),
    milestone_name = milestone_name,
    issue_number = issue$number,
    stringsAsFactors = FALSE
  )

  return(relevant_files_df)
} # get_relevant_files

