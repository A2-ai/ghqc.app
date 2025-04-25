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
  all_milestones <- get_all_milestone_list_errors()

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
        all_milestones = all_milestones
      )
    }
  )
  port <- as.numeric(Sys.getenv("GHQC_SHINY_PORT", 5256))
  runApp(app, port = port)
}
