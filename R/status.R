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
  check_github_credentials() # TODO: remove after testing

  issues <- get_all_issues_in_milestone(owner, repo, milestone_name)
  browser()
  issues_df <- map_df(issues, function(x) {
    tibble(
      file = x$title,
      url = x$html_url,
      state = x$state,
      QCer = x$assignee$login,
      #directory = basename(x$title)
    )
  })
  # TODO: add rest of repo files, determine where they're assoc files or not

}

get_file_qc_status <- function() {
  # TODO
}

get_file_git_status <- function() {
  # TODO
}

get_file_parent_dirs <- function(file_name) {
  # TODO
}
