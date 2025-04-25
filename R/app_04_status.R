#' @param milestones The default set of one or more milestones to render in the QC Status table
#'
#' @title Status QC files
#'
#' @description
#' This function provides an interface to status QC files
#'
#' @return Starts a Shiny app and does not return any value.
#' @import shiny
#' @importFrom log4r info debug warn error
#' @export
ghqc_status_app <- function(milestones = NULL) {
  inputted_milestones <- milestones

  if (!exists("config_repo_path", .le)) ghqc_set_config_repo()
  get_options()
  git_fetch(prune = TRUE)

  # error handling before starting app
  rproj_root_dir()
  creds <- check_github_credentials()

  all_ghqc_milestones <- list_ghqc_milestones()

  # error if no ghqc milestones
  if (length(all_ghqc_milestones) == 0) {
    error(.le$logger, glue::glue("There were no ghqc Milestones found in {.le$org}/{.le$repo}. Create ghqc Milestones using `ghqc_assign_app()`"))
    rlang::abort("There were no open Milestones found.")
  }

  all_ghqc_milestone_names <- purrr::map_chr(all_ghqc_milestones, "title")

  all_inputted_milestones_valid <- all(inputted_milestones %in% all_ghqc_milestone_names)
  if (!(all_inputted_milestones_valid)) {
    info(.le$logger, "Not all inputted Milestones exist. Rendering table with most recent Milestone")
  }

  default_milestones <- {
    if (!is.null(inputted_milestones) && all_inputted_milestones_valid) {
      inputted_milestones
    } else {
      get_most_recent_milestone(all_ghqc_milestones)
    }
  }

  current_branch <- gert::git_branch()
  local_commits <- get_local_commits()
  remote_commits <- get_remote_commits(current_branch)

  ahead_behind_status <- check_ahead_behind()
  # get files with remote changes
  files_changed_in_remote_commits <- get_files_changed_in_remote_commits(remote_commits, ahead_behind_status)

  # get files with local unpushed commits
  files_changed_in_unpushed_local_commits <- get_files_changed_in_unpushed_local_commits(local_commits, ahead_behind_status)

  # get files with local uncommitted file changes
  files_with_uncommitted_local_changes <- get_files_with_uncommitted_local_changes()

  app <- shinyApp(
    ui = ghqc_status_ui(
      id = "ghqc_status_app"
    ),
    server = function(input, output, session) {
      ghqc_status_server(
        id = "ghqc_status_app",
        all_ghqc_milestone_names = all_ghqc_milestone_names,
        default_milestones = default_milestones,
        local_commits,
        remote_commits,
        current_branch,
        ahead_behind_status,
        files_changed_in_remote_commits,
        files_changed_in_unpushed_local_commits,
        files_with_uncommitted_local_changes
      )
    }
  )
  port <- as.numeric(Sys.getenv("GHQC_SHINY_PORT", 5257))
  runApp(app, port = port)
}
