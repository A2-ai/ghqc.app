#' @param milestones The default set of one or more milestones to render in the QC Status table
#'
#' @title Status QC file(s)
#'
#' @description
#' View the status of one or more ghqc Issues and post QC Notification(s) and QC Approval(s)
#'
#' @return Starts a Shiny app and does not return any value.
#' @import shiny
#' @importFrom log4r info debug warn error
#' @export
ghqc_status_app <- function(milestones = NULL) {
  inputted_milestones <- milestones

  if (!exists("config_repo_path", .le)) ghqc_set_config_repo()
  get_options()
  gert::git_fetch(prune = TRUE)

  # error handling before starting app
  rproj_root_dir()
  creds <- check_github_credentials()

  open_milestone_objects <- get_open_non_empty_milestone_objects()

  # error if no non-empty ghqc milestones
  # if (length(open_milestone_objects) == 0) { # TODO: handle/test this case
  #   error(.le$logger, glue::glue("There were no non-empty ghqc Milestones found in {.le$org}/{.le$repo}. Create ghqc Milestones using `ghqc_assign_app()`"))
  #   rlang::abort("There were no open Milestones found.")
  # }

  open_milestones_by_branch <- group_milestone_objects_by_branch(open_milestone_objects)
  open_milestone_names <- get_grouped_milestone_names(open_milestones_by_branch)

  #all_open_ghqc_milestones

  all_inputted_milestones_valid <- all(inputted_milestones %in% open_milestones_by_branch)
  if (!(all_inputted_milestones_valid)) {
    info(.le$logger, "Not all inputted Milestones exist. Rendering table with most recent Milestone")
  }

  current_branch <- gert::git_branch()

  default_milestones <- {
    if (!is.null(inputted_milestones) && all_inputted_milestones_valid) {
      inputted_milestones
    }
    else {
      # get all open milestones on qc_branch
      # TODO: test case for 1 milestone on the branch versus a few
      open_milestones_on_current_branch <- open_milestones_by_branch[[current_branch]]
      open_milestone_names_on_current_branch <- get_milestone_names_from_milestone_objects(open_milestones_on_current_branch)
      if (length(open_milestone_names_on_current_branch) > 0) {
        open_milestone_names_on_current_branch
      }
      else {
        get_most_recent_milestone(open_milestone_objects)
      }

    }
  }


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
        open_milestone_names = open_milestone_names,
        open_milestone_objects = open_milestone_objects,
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
