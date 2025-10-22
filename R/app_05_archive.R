#' @title Archive Files
#'
#' @description
#' This function starts a Shiny application for archiving files.
#'
#' @return Starts a Shiny app and does not return any value.
#' @import shiny
#' @importFrom gert git_branch
#' @importFrom purrr map_dfr
#' @importFrom tibble tibble
#' @export
ghqc_archive_app <- function() {
  if (!exists("config_repo_path", .le)) {
    ghqc_set_config_repo()
  }
  get_options()

  root_dir <- rproj_root_dir()
  check_github_credentials()

  branch <- gert::git_branch()

  milestone_df <- get_all_non_empty_milestone_objects() |>
    purrr::map_dfr(function(milestone) {
      tibble::tibble(
        name = milestone$title,
        number = milestone$number,
        open = identical(milestone$state, "open"),
      )
    })

  app <- shiny::shinyApp(
    ui = ghqc_archive_ui(
      id = "ghqc_archive_app"
    ),
    server = function(input, output, session) {
      ghqc_archive_server(
        id = "ghqc_archive_app",
        root_dir = root_dir,
        milestone_df = milestone_df,
        local_branch = branch
      )
    }
  )

  port <- as.numeric(Sys.getenv("GHQC_SHINY_PORT", 5254))
  shiny::runApp(app, port = port)
}
