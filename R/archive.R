#' @title Archive QC file(s)
#'
#' @description
#' This function provides an interface to assign one or more files for QC in the form of a GitHub Issue(s) within a
#' GitHub Milestone, with options to assign a repository collaborator as the QCer and/or generate a checklist
#' of suggested review tasks during QC.
#'
#' @return Starts a Shiny app and does not return any value.
#' @import shiny
#' @export
ghqc_archive_app <- function() {
  if (!exists("config_repo_path", .le)) ghqc_set_config_repo()
  get_options()

  # error handling before starting app
  root_dir <- rproj_root_dir()
  check_github_credentials()

  all_milestones <- get_all_non_empty_milestone_objects()
  all_milestone_names <- get_milestone_names_from_milestone_objects(all_milestones)
  open_milestone_objects <- get_open_non_empty_milestone_objects()
  open_milestone_names <- get_milestone_names_from_milestone_objects(open_milestone_objects)

  app <- shinyApp(
    ui = ghqc_archive_ui(
      id = "ghqc_archive_app"
    ),
    server = function(input, output, session) {
      ghqc_archive_server(
        id = "ghqc_archive_app",
        root_dir = root_dir,
        all_milestone_names = all_milestone_names,
        open_milestone_names = open_milestone_names
      )
    }
  )
  port <- as.numeric(Sys.getenv("GHQC_SHINY_PORT", 5254))
  runApp(app, port = port)
}
