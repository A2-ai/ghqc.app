#' set the repo that stores the ghqc info
#' @param repo_path path to the git repo storing ghqc information
#' @export
ghqc_set_info_repo <- function(repo_path = file.path("~/.local/share/ghqc", info_repo_name())) {
  not_files <- NULL
  if (!file.exists(file.path(repo_path, "checklists"))) not_files <- append(not_files, "Checklists directory")
  if (!file.exists(file.path(repo_path, "logo.png"))) not_files <- append(not_files, "logo.png")
  if (!is.null(not_files)) info_repo_files_not_found(not_files, repo_path)
  assign("info_repo_path", repo_path, envir = .le)
}

info_repo_files_not_found <- function(not_files, repo_path) {
  error(.le$logger, glue::glue("{paste(not_files, collapse = ' and ')} are not found in {repo_path}. Please ensure files are present before continuing"))
  rlang::abort(glue::glue("{paste(not_files, collapse = ' and ')} are not found in {repo_path}. Please ensure files are present before continuing"))
}

check_ghqc_info_repo_exists <- function() {
  if (!file.exists("~/.Renviron")) info_repo_not_found()
  readRenviron("~/.Renviron")
  info_repo <- Sys.getenv("GHQC_INFO_REPO")
  if (info_repo == "") info_repo_not_found()
  if (substr(info_repo, 1, 8) != "https://") {
    error(.le$logger, glue::glue("GHQC_INFO_REPO ({info_repo}) does not start with 'https://'"))
    rlang::abort(sprintf("GHQC_INFO_REPO (%s) does not start with 'https://'"), info_repo)
  }
}

info_repo_name <- function() {
  gsub(".git", "", basename(info_repo_url()))
}

info_repo_url <- function() {
  check_ghqc_info_repo_exists()
  Sys.getenv("GHQC_INFO_REPO")
}

info_repo_not_found <- function() {
  error(.le$logger, "GHQC_INFO_REPO not found. Please set in ~/.Renviron")
  rlang::abort(message = "GHQC_INFO_REPO not found. Please set in ~/.Renviron")
}
