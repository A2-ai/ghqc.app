#' @title Status QC file(s)
#'
#' @description
#' This function provides an interface to status QC files
#'
#' @return Starts a Shiny app and does not return any value.
#' @import shiny
#' @export
ghqc_status_app <- function(milestones = NULL) {
  if (!exists("config_repo_path", .le)) ghqc_set_config_repo()
  get_options()
  git_fetch(prune = TRUE)

  # error handling before starting app
  root_dir <- rproj_root_dir()
  creds <- check_github_credentials()
  remote <- creds$remote
  remote_name <- remote$name
  org <- get_org_errors(remote)
  repo <- get_repo_errors(remote)

  local_log_output <- system("git log --pretty=format:'%H|%an|%ae|%ad|%s'  --date=format:'%Y-%m-%d %H:%M:%S'", intern = TRUE)
  local_commit_log <- read.csv(text = local_log_output, sep = "|", header = FALSE, stringsAsFactors = FALSE)
  names(local_commit_log) <- c("commit", "author_name", "author_email", "time", "message")

  current_branch <- gert::git_branch()

  all_ghqc_milestones <- list_ghqc_milestones(org, repo)
  all_ghqc_milestone_names <- purrr::map_chr(all_ghqc_milestones, "title")

  default_milestones <- {
    if (is.null(milestones)) get_most_recent_milestone(all_ghqc_milestones)
    else milestones
  }

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
        root_dir,
        remote_name,
        local_commit_log,
        current_branch
      )
    }
  )
  port <- as.numeric(Sys.getenv("GHQC_SHINY_PORT", 5257))
  runApp(app, port = port)
}
