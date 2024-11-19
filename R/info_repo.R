#' set the repo that stores the ghqc info
#' @param repo_path path to the git repo storing ghqc information
#' @export
ghqc_set_config_repo <- function(repo_path = file.path("~/.local/share/ghqc", config_repo_name())) {
  not_files <- NULL
  if (!file.exists(file.path(repo_path, "checklists"))) not_files <- append(not_files, "Checklists directory")
  if (!file.exists(file.path(repo_path, "logo.png"))) not_files <- append(not_files, "logo.png")
  if (!is.null(not_files)) config_repo_files_not_found(not_files, repo_path)
  assign("config_repo_path", repo_path, envir = .le)
}

config_repo_files_not_found <- function(not_files, repo_path) {
  error(.le$logger, glue::glue("{paste(not_files, collapse = ' and ')} not found in {repo_path}. Please ensure file(s) are present before continuing"))
  rlang::abort(glue::glue("{paste(not_files, collapse = ' and ')} not found in {repo_path}. Please ensure file(s) are present before continuing"))
}

check_ghqc_config_repo_exists <- function() {
  if (!file.exists("~/.Renviron")) config_repo_not_found()
  readRenviron("~/.Renviron")
  config_repo <- Sys.getenv("GHQC_CONFIG_REPO")
  if (config_repo == "") config_repo_not_found()
  if (substr(config_repo, 1, 8) != "https://") {
    error(.le$logger, glue::glue("GHQC_CONFIG_REPO ({config_repo}) does not start with 'https://'"))
    rlang::abort(sprintf("GHQC_CONFIG_REPO (%s) does not start with 'https://'"), config_repo)
  }
}

config_repo_name <- function() {
  gsub(".git", "", basename(config_repo_url()))
}

config_repo_url <- function() {
  check_ghqc_config_repo_exists()
  Sys.getenv("GHQC_CONFIG_REPO")
}

config_repo_not_found <- function() {
  error(.le$logger, "GHQC_CONFIG_REPO not found. Please set in ~/.Renviron")
  rlang::abort(message = "GHQC_CONFIG_REPO not found. Please set in ~/.Renviron")
}


