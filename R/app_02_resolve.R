#' @title Comment in an Issue to display file changes during QC
#'
#' @description
#' This function allows a user to insert a comment into a ghqc GitHub Issue that displays changes
#' in the version control information for the Issue’s corresponding file. By default, the comment
#' displays both the original and current commits and hashes for the file. These versions are
#' selected by the user. The comment can optionally display the file difference (“diff”) between
#' the current and previous versions. These changes will likely be implementations of QC feedback.
#'
#' @return Starts a Shiny app and does not return any value.
#' @import shiny
#' @importFrom log4r warn error info debug
#' @export
ghqc_resolve_app <- function() {
  if (!exists("info_repo_path", .le)) ghqc_set_info_repo()

  # error handling before starting app
  remote <- check_github_credentials()
  org <- get_org_errors()
  repo <- get_repo_errors(remote)
  milestone_list <- get_open_milestone_list_errors(org = org, repo = repo)

  if (length(milestone_list) == 0) {
    error(.le$logger, glue::glue("There were no open Milestones found in {org}/{repo}"))
    rlang::abort("No open Milestones found")
  }

  # error if no open milestones
  if (length(milestone_list) == 0) {
    error(.le$logger, glue::glue("There were no open Milestones found in {org}/{repo}. Please use the Assign app before using the Resolve app."))
    rlang::abort("There were no open Milestones found.")
  }

  app <- shinyApp(
    ui = ghqc_resolve_ui(
      id = "ghqc_resolve_app"
    ),
    server = function(input, output, session) {
      ghqc_resolve_server(
        id = "ghqc_resolve_app",
        remote = remote,
        org = org,
        repo = repo,
        milestone_list = milestone_list
      )
    }
  )
  port <- as.numeric(Sys.getenv("GHQC_SHINY_PORT", 5454))
  runApp(app, port = port)
}
