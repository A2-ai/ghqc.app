# get_file_git_status <- function(file, local_commits, remote_commits) {
#   if (!file.exists(file)) {
#     return("File does not exist locally")
#   }
#
#   if (file_changed_in_remote_commits(file, remote_commits)) {
#     return("Remote file changes")
#   }
#   else if (file_changed_in_unpushed_local_commits(file, local_commits)) {
#     return("Local unpushed commits with file changes")
#   }
#   else if (file_has_uncommitted_local_changes(file)) {
#     return("Local uncommitted file changes")
#   }
#   else {
#     return("Up-to-date")
#   }
# }



get_git_statuses <- function(files, local_commits, remote_commits) {
  # get files with remote changes
  files_changed_in_remote_commits <- get_files_changed_in_remote_commits(remote_commits)

  # get files with local unpushed commits
  files_changed_in_unpushed_local_commits <- get_files_changed_in_unpushed_local_commits(local_commits)

  # get files with local uncommitted file changes
  files_with_uncommitted_local_changes <- get_files_with_uncommitted_local_changes()

  # get files that exist locally
  files_exist_locally <- fs::file_exists(files)

  # assign everything to a data frame
  git_statuses <- purrr::map2_chr(files, files_exist_locally, function(file, exists) {
    if (!exists) {
      return("File does not exist locally")
    } else if (file %in% files_changed_in_remote_commits) {
      return("Remote file changes")
    } else if (file %in% files_changed_in_unpushed_local_commits) {
      return("Local unpushed commits with file changes")
    } else if (file %in% files_with_uncommitted_local_changes) {
      return("Local uncommitted file changes")
    } else {
      return("Up-to-date")
    }
  })

  return(
    tibble::tibble(
      file_name = files,
      git_status = git_statuses
    )
  )

} # assign_git_statuses_to_milestone_files

get_files_changed_in_remote_commits <- function(remote_commits) {
  # if there aren't any unpulled commits, there cant be any files with changes in remote commits
  ahead_behind_status <- check_ahead_behind()
  if (ahead_behind_status$behind == 0) {
    return(character(0))
  }

  # get latest pushed local commit
  local_commit <- remote_commits[1 + ahead_behind_status$behind]
  unpulled_commits <- remote_commits[1:(which(remote_commits == local_commit) - 1)]

  changed_files <- unique(unlist(lapply(unpulled_commits, function(commit) {
    diff <- gert::git_diff(commit)
    c(diff$old, diff$new)
  })))

  # filter na and empty values
  changed_files <- changed_files[!is.na(changed_files) & changed_files != ""]

  return(changed_files)
} # get_files_changed_in_remote_commits

get_files_changed_in_unpushed_local_commits <- function(local_commits) {
  # if there aren't any unpushed commits, there cant be any files with changes in local commits
  ahead_behind_status <- check_ahead_behind()
  if (ahead_behind_status$ahead == 0) {
    return(character(0))
  }

  # get latest pushed remote commit
  remote_commit <- local_commits[1 + ahead_behind_status$ahead]

  # get set of unpushed commits
  unpushed_commits <- local_commits[1:(which(local_commits == remote_commit) - 1)]

  changed_files <- unique(unlist(lapply(unpushed_commits, function(commit) {
    diff <- gert::git_diff(commit)
    c(diff$old, diff$new)
  })))

  # filter changed files
  changed_files <- changed_files[!is.na(changed_files) & changed_files != ""]

  return(changed_files)
} # get_files_changed_in_unpushed_local_commits

get_files_with_uncommitted_local_changes <- function() {
  status <- gert::git_status()
  changed_files <- unique(status$file)

  changed_files <- changed_files[!is.na(changed_files) & changed_files != ""]

  return(changed_files)
}

get_local_commit_log <- function() {
  local_log_output <- system("git log --pretty=format:'%H|%an|%ae|%ad|%s'  --date=format:'%Y-%m-%d %H:%M:%S'", intern = TRUE)
  local_commit_log <- read.csv(text = local_log_output, sep = "|", header = FALSE, stringsAsFactors = FALSE)
  names(local_commit_log) <- c("commit", "author_name", "author_email", "time", "message")
  debug(.le$logger, glue::glue("Retrieved local commit log"))

  return(local_commit_log)
}

get_remote_commit_log <- function(remote_name, current_branch) {
  remote_log_output <- system(glue::glue("git log {remote_name}/{current_branch} --pretty=format:'%H|%an|%ae|%ad|%s'  --date=format:'%Y-%m-%d %H:%M:%S'"), , intern = TRUE)
  remote_commit_log <- read.csv(text = remote_log_output, sep = "|", header = FALSE, stringsAsFactors = FALSE)
  names(remote_commit_log) <- c("commit", "author_name", "author_email", "time", "message")
  debug(.le$logger, glue::glue("Retrieved remote commit log"))

  return(remote_commit_log)
}
