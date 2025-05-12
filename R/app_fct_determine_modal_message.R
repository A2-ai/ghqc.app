#' @importFrom log4r warn error info debug
generate_html_list <- function(files) {
  paste("<li>", files, "</li>", collapse = "")
}

#' @importFrom log4r warn error info debug
generate_sync_message <- function(git_sync_status, error_icon_html) {
  messages <- c()
  if (git_sync_status$ahead > 0 || git_sync_status$behind > 0) {
    sync_messages <- c()
    if (git_sync_status$ahead > 0) sync_messages <- c(sync_messages, "push changes to the remote repository")
    if (git_sync_status$behind > 0) sync_messages <- c(sync_messages, "pull updates from the remote repository")
    messages <- paste(error_icon_html, "There are repository changes that need to be synchronized. Please", paste(sync_messages, collapse = " and "), "<br>")
  }
  return(messages)
}

#' @importFrom log4r warn error info debug
generate_uncommitted_message <- function(uncommitted_files, error_icon_html, warning_icon_html) {
  messages <- c()
  if (length(uncommitted_files$selected) > 0) {
    messages <- c(messages, sprintf(
      "%s All selected files must have local changes committed before proceeding. The following selected files have local uncommitted changes:<br><br><ul>%s</ul><br>",
      error_icon_html, generate_html_list(uncommitted_files$selected)
    ))
  }
  if (length(uncommitted_files$general) > 0 && length(uncommitted_files$selected) == 0) {
    messages <- c(messages, sprintf(
      "%s The following local files have uncommitted changes, but are not selected:<ul>%s</ul><br>",
      warning_icon_html, generate_html_list(uncommitted_files$general)
    ))
  }
  return(messages)
}

#' @importFrom log4r warn error info debug
generate_existing_issue_message <- function(existing_issues, error_icon_html) {
  messages <- c()
  if (length(existing_issues) > 0) {
    messages <- c(messages, sprintf(
      "%s The following selected files are already associated with Issues in the Milestone:<ul>%s</ul><br>",
      error_icon_html, generate_html_list(existing_issues)
    ))
  }
  return(messages)
}

generate_existing_qc_branch_message <- function(issues_in_existing_milestone, error_icon_html) {
  messages <- c()
  # get the qc branch from the first issue
  first_issue <- rev(issues_in_existing_milestone)[[1]]
  qc_branch <- get_branch_from_issue_body(first_issue$body)
  current_branch <- gert::git_branch()

  if (current_branch != qc_branch) {
    messages <- c(messages, sprintf(
      "%s The existing Milestone is already associated with QC branch <em>%s</em>. The current branch is <em>%s</em>. Switch to QC branch or create a new Milestone.<br>",
      error_icon_html, qc_branch, current_branch
    ))
  }
  return(messages)
}

#' @importFrom log4r warn error info debug
generate_commit_update_message <- function(commit_update_status, error_icon_html) {
  messages <- c()

  if (!commit_update_status) {
    messages <- c(messages, paste(error_icon_html, "There are no new commits since QC initialization.<br>"))
  }

  return(messages)
}

#' Determine Modal Message
#'
#' Generates a message for a modal dialog based on the status of selected files, git synchronization status,
#' and GitHub Issue status.
#'
#' @param selected_files A character vector of selected files.
#' @param uncommitted_git_files A character vector of uncommitted git files.
#' @param untracked_selected_files A character vector of untracked selected files.
#' @param git_sync_status Result from gert::git_ahead_behind().
#' @param commit_update_status A logical indicating whether there is 2 or more commits available for selected Issue. Defaults to TRUE.
#' @param issues_in_milestone A list containing existing Issues already found in a Milestone. Defaults to empty list.
#'
#' @return A list containing:
#' \item{message}{A character string with the generated message, or \code{NULL} if no message is generated.}
#' \item{state}{A character string indicating the state of the message, either "error" or "warning", or \code{NULL} if no state is determined.}
#' @noRd
determine_modal_message <- function(selected_files,
                                    uncommitted_git_files,
                                    untracked_selected_files,
                                    git_sync_status,
                                    commit_update_status = TRUE,
                                    issue_titles = list(),
                                    issues_in_existing_milestone = NULL
                                    ) {
  warning_icon_html <- "<span style='font-size: 24px; vertical-align: middle;'>&#9888;</span>"
  error_icon_html <- "<span style='font-size: 24px; vertical-align: middle;'>&#10071;</span>"

  uncommitted_selected_files <- selected_files[selected_files %in% uncommitted_git_files | selected_files %in% untracked_selected_files]
  uncommitted_files <- list(selected = uncommitted_selected_files, general = uncommitted_git_files)
  existing_issues <- selected_files[selected_files %in% issue_titles]

  messages <- c()

  # Errors
  sync_message <- generate_sync_message(git_sync_status, error_icon_html)
  messages <- c(messages, sync_message)
  if (length(sync_message) == 0) {
    # In the case that there are remote changes, saying that there are no new commits just isn't accurate
    # (there are no new local commits, but there are remote commits in this case)
    # So only give the no new commits message if there aren't any unsynced commits
    messages <- c(messages, generate_commit_update_message(commit_update_status, error_icon_html))
  }
  messages <- c(messages, generate_existing_issue_message(existing_issues, error_icon_html))

  if (!is.null(issues_in_existing_milestone)) {
    messages <- generate_existing_qc_branch_message(issues_in_existing_milestone, error_icon_html)
  }


  # Errors and Warnings
  messages <- c(messages, generate_uncommitted_message(uncommitted_files, error_icon_html, warning_icon_html))


  log_string <- glue::glue("Modal Check Inputs:
    - Selected Files: {glue::glue_collapse(selected_files, sep = ', ')}
    - Uncommitted Git Files: {glue::glue_collapse(uncommitted_git_files, sep = ', ')}
    - Untracked Selected Files: {glue::glue_collapse(untracked_selected_files, sep = ', ')}
    - Git Sync Status: Ahead: {git_sync_status$ahead}, Behind: {git_sync_status$behind}
    - Commit Update Status: {commit_update_status}
    - Issues in Milestone: {glue::glue_collapse(existing_issues, sep = ', ')}
  ")

  log4r::debug(.le$logger, log_string)

  if (length(messages) == 0) {
    return(list(message = NULL, state = NULL))
  } else {
    state <- if (any(grepl(error_icon_html, messages))) "error" else "warning"
    return(list(message = paste(messages, collapse = "\n"), state = state))
  }
}
