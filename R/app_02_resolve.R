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
ghqc_notify_app <- function() {
  if (!exists("config_repo_path", .le)) ghqc_set_config_repo()
  get_options()

  # error handling before starting app
  check_github_credentials()
  all_milestone_objects <- get_all_non_empty_milestone_objects()
  open_milestone_objects <- get_open_milestone_objects_from_all_milestone_objects(all_milestone_objects)
  open_milestone_names <- get_milestone_names_from_milestone_objects(open_milestone_objects)

  # error if no open ghqc milestones
  if (length(open_milestone_objects) == 0) {
    error(.le$logger, glue::glue("There were no open ghqc Milestones found in {.le$org}/{.le$repo}. Create ghqc Milestones using `ghqc_assign_app()`"))
    rlang::abort("There were no open ghqc Milestones found.")
  }

  gert::git_fetch()

  app <- shinyApp(
    ui = ghqc_notify_ui(
      id = "ghqc_notify_app"
    ),
    server = function(input, output, session) {
      ghqc_notify_server(
        id = "ghqc_notify_app",
        open_milestone_names = open_milestone_names
      )
    }
  )
  port <- as.numeric(Sys.getenv("GHQC_SHINY_PORT", 5454))
  runApp(app, port = port)
}
