get_git_statuses <- function(files,
                             local_commits,
                             remote_commits,
                             ahead_behind_status,
                             files_changed_in_remote_commits,
                             files_changed_in_unpushed_local_commits,
                             files_with_uncommitted_local_changes
                             ) {

  # get files that exist locally
  files_exist_locally <- fs::file_exists(files)

  # assign everything to a data frame
  git_statuses <- purrr::map2_chr(files, files_exist_locally, function(file, exists) {
    if (!exists) {
      return("File does not exist locally")
    }
    else if (file %in% files_changed_in_remote_commits) {
      return("Remote file changes")
    }
    else if (file %in% files_changed_in_unpushed_local_commits) {
      return("Local unpushed commits with file changes")
    }
    else if (file %in% files_with_uncommitted_local_changes) {
      return("Local uncommitted file changes")
    }
    else {
      return("Up-to-date")
    }
  })

  return(
    dplyr::tibble(
      file_name = files,
      git_status = git_statuses
    )
  )

} # assign_git_statuses_to_milestone_files

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

get_files_changed_in_remote_commits <- function(remote_commits, ahead_behind_status) {
  debug(.le$logger, "get_files_changed_in_remote_commits")
  # if there aren't any unpulled commits, there cant be any files with changes in remote commits
  if (ahead_behind_status$behind == 0) {
    return(character(0))
  }

  # get latest pushed local commit
  local_commit <- remote_commits[1 + ahead_behind_status$behind]
  most_recent_remote_commit <- remote_commits[1]

  start_time <- Sys.time()
  args <- c("diff", local_commit, most_recent_remote_commit, "--name-only")

  changed_files <- tryCatch(
    system2("git", args = args, stdout = TRUE, stderr = NULL),
    error = function(e) {
      rlang::abort(glue::glue("Error running git diff between {local_commit} and {most_recent_remote_commit}: {conditionMessage(e)}"))
    }
  )

  # filter na and empty values
  changed_files <- changed_files[!is.na(changed_files) & changed_files != ""]
  debug(.le$logger, glue::glue("get_files_changed_in_remote_commits total time: {difftime(Sys.time(), start_time)}"))

  return(changed_files)
} # get_files_changed_in_remote_commits

get_files_changed_in_unpushed_local_commits <- function(local_commits, ahead_behind_status) {
  debug(.le$logger, "get_files_changed_in_unpushed_local_commits")
  # if there aren't any unpushed commits, there cant be any files with changes in local commits
  if (ahead_behind_status$ahead == 0) {
    return(character(0))
  }

  # get latest pushed remote commit
  remote_commit <- local_commits[1 + ahead_behind_status$ahead]
  most_recent_local_commit <- local_commits[1]

  start_time <- Sys.time()
  args <- c("diff", remote_commit, most_recent_local_commit, "--name-only")

  changed_files <- tryCatch(
    system2("git", args = args, stdout = TRUE, stderr = NULL),
    error = function(e) {
      rlang::abort(glue::glue("Error running git diff between {remote_commit} and {most_recent_local_commit}: {conditionMessage(e)}"))
    }
  )

  # filter changed files
  changed_files <- changed_files[!is.na(changed_files) & changed_files != ""]

  debug(.le$logger, glue::glue("get_files_changed_in_unpushed_local_commits total time: {difftime(Sys.time(), start_time)}"))
  return(changed_files)
} # get_files_changed_in_unpushed_local_commits

get_files_with_uncommitted_local_changes <- function() {
  debug(.le$logger, "get_files_with_uncommitted_local_changes")
  start_time <- Sys.time()
  status <- gert::git_status()
  changed_files <- unique(status$file)

  changed_files <- changed_files[!is.na(changed_files) & changed_files != ""]
  debug(.le$logger, glue::glue("get_files_with_uncommitted_local_changes total time {difftime(Sys.time(), start_time)}"))

  return(changed_files)
}

get_local_commits <- function() {
  local_log_output <- system("git log --pretty=format:'%H|%an|%ae|%ad|%s'  --date=format:'%Y-%m-%d %H:%M:%S'", intern = TRUE)
  local_commit_log <- utils::read.csv(text = local_log_output, sep = "|", header = FALSE, stringsAsFactors = FALSE)
  names(local_commit_log) <- c("commit", "author_name", "author_email", "time", "message")
  debug(.le$logger, glue::glue("Retrieved local commit log"))

  return(local_commit_log$commit)
}

get_remote_commits <- function(remote_name, current_branch) {
  remote_log_output <- system(glue::glue("git log {remote_name}/{current_branch} --pretty=format:'%H|%an|%ae|%ad|%s'  --date=format:'%Y-%m-%d %H:%M:%S'"), , intern = TRUE)
  remote_commit_log <- utils::read.csv(text = remote_log_output, sep = "|", header = FALSE, stringsAsFactors = FALSE)
  names(remote_commit_log) <- c("commit", "author_name", "author_email", "time", "message")
  debug(.le$logger, glue::glue("Retrieved remote commit log"))

  return(remote_commit_log$commit)
}

get_remote_commits_full_name <- function(remote) {
  remote_log_output <- system(glue::glue("git log {remote} --pretty=format:'%H|%an|%ae|%ad|%s'  --date=format:'%Y-%m-%d %H:%M:%S'"), , intern = TRUE)
  remote_commit_log <- utils::read.csv(text = remote_log_output, sep = "|", header = FALSE, stringsAsFactors = FALSE)
  names(remote_commit_log) <- c("commit", "author_name", "author_email", "time", "message")
  debug(.le$logger, glue::glue("Retrieved remote commit log"))

  return(remote_commit_log$commit)
}


check_remote_branch_deleted <- function(branch_name) {
  remote_refs <- gert::git_remote_ls()

  branch_refs <- grep("^refs/heads/", remote_refs$ref, value = TRUE)
  remote_branch_names <- sub("^refs/heads/", "", branch_refs)

  is_deleted <- !(branch_name %in% remote_branch_names)
  return(is_deleted)
}

find_merged_into <- function(commit_sha) {
  #get all merge commits in the repo
  merge_commits <- system2("git", c("rev-list", "--all", "--merges"), stdout = TRUE)
  if (length(merge_commits) == 0) return(NULL)

  for (merge_commit in merge_commits) {
    merge_commit <- trimws(merge_commit)

    # get parent 2 (the merged-in branch)
    parent2 <- tryCatch(
      system2("git", c("rev-parse", paste0(merge_commit, "^2")), stdout = TRUE),
      error = function(e) NA_character_
    )

    if (length(parent2) == 0 || is.na(parent2)) next
    parent2 <- trimws(parent2)

    # check if the deleted-branch commit is in the ancestry of parent2
    is_merged <- system2("git", c("merge-base", "--is-ancestor", commit_sha, parent2),
                         stdout = NULL, stderr = NULL)

    if (is_merged == 0) {
      # get parent 1 (branch that received the merge)
      parent1 <- trimws(system2("git", c("rev-parse", paste0(merge_commit, "^1")), stdout = TRUE))

      # find branches that point to parent1 or contain it in history
      candidate_branches <- system2("git", c("branch", "-r", "--contains", merge_commit), stdout = TRUE)
      candidate_branches <- trimws(candidate_branches)
      candidate_branches <- candidate_branches[!grepl("HEAD ->", candidate_branches)]

      # filter to branches where parent1 is in their ancestry
      merged_to <- NULL
      for (branch in candidate_branches) {
        branch_tip <- tryCatch(
          trimws(system2("git", c("rev-parse", branch), stdout = TRUE)),
          error = function(e) NA_character_
        )
        if (!is.na(branch_tip)) {
          # is parent1 in this branch's history?
          reachable <- system2("git", c("merge-base", "--is-ancestor", parent1, branch_tip),
                               stdout = NULL, stderr = NULL)
          if (reachable == 0) {
            merged_to <- branch
            break
          }
        }
      }

      return(merged_to)
    }
  }

  return(NULL)
}

get_sign_off_column <- function(git_status) {
  valid_git_status <- !is.na(git_status) && git_status == "Up-to-date"
  return(valid_git_status)
}

get_notify_column <- function(qc_status, git_status, latest_qc_commit, comparator_commit) {
  has_valid_git_status <- is.na(git_status) || git_status == "Up-to-date" # allowing git status to be NA in case when QC branch deleted and merged

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
  hard_notify_qc_statuses <- c("File changes to notify",
                                "Pushed file changes after Issue closure",
                                "Uncommented pushed file changes before Issue closure"
                                )

  # soft notify statuses are statuses where there's no changes in the qc file to notify,
  # but the user may still want to update
  # the issue - maybe a relevant file changed or something like that
  soft_notify_qc_statuses <- c("QC in progress",
                               "QC complete",
                               "File changes since QC branch merged and deleted") # yes there are file changes, but since the merge has already happened, make it soft

  has_hard_notify_qc_status <- qc_status %in% hard_notify_qc_statuses
  has_soft_notify_qc_status <- qc_status %in% soft_notify_qc_statuses

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
