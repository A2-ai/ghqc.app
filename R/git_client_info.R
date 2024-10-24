.lci <- new.env()

#' @importFrom log4r warn error info debug
get_client_git_url <- function() {
  git_url <- Sys.getenv("GIT_CLIENT_URL")

  # error if CLIENT_INFO_URL not set
  if (length(git_url) == 0){
    error(.le$logger, "No client github url found. Please set GIT_CLIENT_URL environmental variable, likely in your ~/.Renviron file.")
    rlang::abort(message = "No client github url found. Please set GIT_CLIENT_URL environmental variable, likely in your ~/.Renviron file.")
  }

  # error if not https
  url_starts_with_https <- stringr::str_starts(git_url, "https://")
  if (!url_starts_with_https) {
    error(.le$logger, glue::glue("Retrieved GIT_CLIENT_URL: {git_url} does not start with https"))
    rlang::abort(message = glue::glue("Retrieved GIT_CLIENT_URL: {git_url} does not start with https"))
  }
  git_url
}

#' @importFrom log4r warn error info debug
check_client_local <- function(git_url) {
  client_repo_name <- get_remote_name(git_url)
  client_repo_path <- file.path("~",client_repo_name)

  if (!file.exists(client_repo_path)) {
    # Case 1: Repo not in home dir, cloning down
    debug(.le$logger, glue::glue("{client_repo_name} not found in home directory. Attempting to clone {git_url} to {client_repo_path}..."))
    tryCatch(
      {
        gert::git_clone(git_url, path = client_repo_path)
        info(.le$logger, glue::glue("Successfully cloned {client_repo_name}"))
      }, error = function(e) {
        error(.le$logger, glue::glue("Clone of {client_repo_name} was not successful"))
        rlang::abort(message = e$message)
      }
    )
  } # if client repo not cloned
  else {
    remote_updates <- remote_repo_updates(client_repo_path)
    local_updates <- local_repo_updates(client_repo_path)

    # if local changes
    if (local_updates) {
      stash <-  gert::git_stash_save(repo = client_repo_path)
      info(.le$logger, glue::glue("Stashed local changes to {client_repo_path}"))
    }

    # if remote changes
    if (remote_updates) {
      # Case 2: Repo in home dir, but local or remote changes
      debug(.le$logger, "Most recent remote commit ({remote_commit_id}) does not match local commit ({local_commit_id}). Attempting to pull down update to {client_repo_path}...")
      tryCatch(
        {
          gert::git_pull(repo = client_repo_path)
          info(.le$logger, glue::glue("Update has been successfully pulled down to {client_repo_path}"))
        }, error = function(e) {
          error(.le$logger, glue::glue("Update was unsuccessfully pulled down. Attempted to pull {client_repo_name} remote commit id {remote_commit_id} to {client_repo_path}"))
          rlang::abort(.le$logger, message = e$message)
        }
      )
    }

    if (!remote_updates && !local_updates) {
      info(.le$logger, "No local or remote updates to ghqc storage repo")
    }

  } # else, client dir has already been cloned

  return(client_repo_path)
}

#' @importFrom log4r warn error info debug
local_repo_updates <- function(client_repo_path) {
  status <- gert::git_status(repo = client_repo_path)
  local_repo_updates <- "modified" %in% status$status
  if (local_repo_updates) {
    info(.le$logger, glue::glue("Detected local changes to {client_repo_path}"))
  }
  return(local_repo_updates)
}

#' @importFrom log4r warn error info debug
remote_repo_updates <- function(client_repo_path) {
  remote_commit_id <- gert::git_remote_ls(repo = client_repo_path)$oid[1]
  local_commit_id <- gert::git_info(repo = client_repo_path)$commit
  remote_repo_updates <- remote_commit_id != local_commit_id
  return(remote_repo_updates)
}

#' @importFrom log4r warn error info debug
load_client_info <- function(){
  if (file.exists("~/.Renviron")) readRenviron("~/.Renviron")

  # get client url from ~./Renviron
  git_url <- get_client_git_url()

  # check if client local is cloned and most up to date commit
  client_repo_path <- check_client_local(git_url)
  assign("client_repo_path", client_repo_path, envir = .lci)
}
