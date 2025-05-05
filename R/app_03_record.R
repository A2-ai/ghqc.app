#' @title Generate a QC Record for one or more Milestones
#'
#' @description
#' This function allows the user to generate a QC Record for one or more Milestones created with the assign app.
#'
#' @return Starts a Shiny app and does not return any value.
#' @import shiny
#' @export
ghqc_record_app <- function() {
  if (!exists("config_repo_path", .le)) ghqc_set_config_repo()
  get_options()

  # error handling before starting app
  check_github_credentials()
  all_milestones <- get_all_non_empty_ghqc_milestone_objects()
  closed_milestones <- get_closed_milestone_objects_from_all_milestone_objects(all_milestones)
  all_milestone_names <- get_milestone_names_from_milestone_objects(all_milestones)
  all_closed_milestone_names <- get_milestone_names_from_milestone_objects(closed_milestones)


  if (length(all_milestones) == 0 || is.null(all_milestones)) {
    error(.le$logger, glue::glue("There were no Milestones found in {.le$org}/{.le$repo}. Create a Milestone by using the Assign app."))
    rlang::abort("No Milestones found")
  }

  app <- shinyApp(
    ui = ghqc_record_ui(
      id = "ghqc_record_app"
    ),
    server = function(input, output, session) {
      ghqc_record_server(
        id = "ghqc_record_app",
        all_milestones_in = all_milestones,
        closed_milestones_in = closed_milestones,
        all_milestone_names_in = all_milestone_names,
        all_closed_milestone_names_in = all_closed_milestone_names
      )
    }
  )
  port <- as.numeric(Sys.getenv("GHQC_SHINY_PORT", 5256))
  runApp(app, port = port)
}
