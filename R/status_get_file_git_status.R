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
      return("Up to date")
    }
  })

  return(
    dplyr::tibble(
      file_name = files,
      git_status = git_statuses
    )
  )

} # get_git_statuses

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
  local_commit_log <- parse_commit_log(local_log_output)
  debug(.le$logger, glue::glue("Retrieved local commit log"))

  return(local_commit_log$commit)
}

get_remote_commits <- function(current_branch) {
  remote_log_output <- system(glue::glue("git log {.le$remote_name}/{current_branch} --pretty=format:'%H|%an|%ae|%ad|%s'  --date=format:'%Y-%m-%d %H:%M:%S'"), , intern = TRUE)
  remote_commit_log <- parse_commit_log(remote_log_output)
  debug(.le$logger, glue::glue("Retrieved remote commit log"))

  return(remote_commit_log$commit)
}

get_remote_commits_full_name <- function(remote) {
  remote_log_output <- system(glue::glue("git log {remote} --pretty=format:'%H|%an|%ae|%ad|%s'  --date=format:'%Y-%m-%d %H:%M:%S'"), , intern = TRUE)
  remote_commit_log <- parse_commit_log(remote_log_output)
  debug(.le$logger, glue::glue("Retrieved remote commit log"))

  return(remote_commit_log$commit)
}

parse_commit_log <- function(log_output) {
  commit_log <- utils::read.csv(text = log_output, sep = "|", header = FALSE, stringsAsFactors = FALSE, quote = "")
  names(commit_log) <- c("commit", "author_name", "author_email", "time", "message")
  return(commit_log)
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
