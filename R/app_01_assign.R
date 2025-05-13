#' @title Assign QC file(s)
#'
#' @description
#' This function provides an interface to assign one or more files for QC in the form of a GitHub Issue(s) within a
#' GitHub Milestone, with options to assign a repository collaborator as the QCer and/or generate a checklist
#' of suggested review tasks during QC.
#'
#' @return Starts a Shiny app and does not return any value.
#' @import shiny
#' @export
ghqc_assign_app <- function() {
  if (!exists("config_repo_path", .le)) ghqc_set_config_repo()
  get_options()

  # error handling before starting app
  root_dir <- rproj_root_dir()
  check_github_credentials()
  checklists <- get_valid_checklists()
  members <- get_members_errors()

  all_milestone_objects <- get_all_non_empty_milestone_objects()
  open_milestone_objects <- get_open_milestone_objects_from_all_milestone_objects(all_milestone_objects)
  open_milestone_names <- get_milestone_names_from_milestone_objects(open_milestone_objects)

  app <- shinyApp(
    ui = ghqc_assign_ui(
      id = "ghqc_assign_app"
    ),
    server = function(input, output, session) {
      ghqc_assign_server(
        id = "ghqc_assign_app",
        root_dir = root_dir,
        checklists = checklists,
        members = members,
        open_milestone_names = open_milestone_names
      )
    }
  )
  port <- as.numeric(Sys.getenv("GHQC_SHINY_PORT", 5254))
  runApp(app, port = port)
}
