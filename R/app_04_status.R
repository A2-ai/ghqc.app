#' @title Status QC file(s)
#'
#' @description
#' This function provides an interface to status QC files
#'
#' @return Starts a Shiny app and does not return any value.
#' @import shiny
#' @importFrom log4r info debug warn error
#' @export
ghqc_status_app <- function(milestones = NULL) {
  if (!exists("config_repo_path", .le)) ghqc_set_config_repo()
  get_options()
  git_fetch(prune = TRUE)

  # error handling before starting app
  rproj_root_dir()
  creds <- check_github_credentials()
  remote <- creds$remote
  remote_name <- remote$name
  org <- get_org_errors(remote)
  repo <- get_repo_errors(remote)

  all_ghqc_milestones <- list_ghqc_milestones(org, repo)
  all_ghqc_milestone_names <- purrr::map_chr(all_ghqc_milestones, "title")

  all_inputted_milestones_valid <- all(milestones %in% all_ghqc_milestone_names)
  if (!(all_inputted_milestones_valid)) {
    info(.le$logger, "Not all inputted Milestones exist. Rendering table with most recent Milestone")
  }

  default_milestones <- {
    if (!is.null(milestones) && all_inputted_milestones_valid) {
    milestones
    } else {
      get_most_recent_milestone(all_ghqc_milestones)
    }
  }

  current_branch <- gert::git_branch()
  local_commits <- get_local_commits()
  remote_commits <- get_remote_commits(remote_name, current_branch)

  app <- shinyApp(
    ui = ghqc_status_ui(
      id = "ghqc_status_app"
    ),
    server = function(input, output, session) {
      ghqc_status_server(
        id = "ghqc_status_app",
        all_ghqc_milestone_names = all_ghqc_milestone_names,
        default_milestones = default_milestones,
        org,
        repo,
        local_commits,
        remote_commits,
        current_branch
      )
    }
  )
  port <- as.numeric(Sys.getenv("GHQC_SHINY_PORT", 5257))
  runApp(app, port = port)
}
