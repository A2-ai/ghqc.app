check_if_qc_file_untracked <- function(qc_file_path) {
  status <- processx::run("git", c("status", "-u"))$stdout
  lines <- strsplit(status, "\n")[[1]]
  untracked_start <- grep("Untracked files:", lines)

  if (length(untracked_start) > 0) {
    # lines in "Untracked files" section
    untracked_lines <- lines[(untracked_start + 1):length(lines)]

    # check if the qc_file_path is in untracked_lines
    return(any(grepl(qc_file_path, untracked_lines)))
  }

  # if "Untracked files" not found, return false
  return(FALSE)
}

check_if_git_initialized <- function(path = getwd()) {
  while (TRUE) {
    if (file.exists(file.path(path, ".git"))) {
      return(TRUE)
    } else {
      parent_path <- dirname(path)
      if (identical(parent_path, path)) {
        return(FALSE)
      }
      path <- parent_path
    }
  }
}

error_if_git_not_initialized <- function(path = getwd()) {
  git_inited <- check_if_git_initialized(path)
  if (!git_inited) {
    error(.le$logger, "Git not initialized; push repo to Github first.")
    rlang::abort(message = "Git not initialized; push repo to Github first.")
  }
}


# use this for the update app
# this has been added to get_open_milestone_objects
check_that_milestone_is_non_empty <- function(milestone) {
  if (milestone$open_issues == 0 && milestone$closed_issues == 0) {
    return(FALSE)
  }
  else {
    return(TRUE)
  }
}

check_if_updates_since_init <- function(commits_df) {
  tryCatch({
    if (nrow(commits_df) < 2) FALSE else TRUE
  }, error=function(e){
    error(.le$logger, glue::glue("There was an error checking commits for {commits_df}"))
    rlang::abort(e$message)
  })
}
