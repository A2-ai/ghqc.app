#' @importFrom log4r warn error info debug
check_git_inited <- function() {
  tryCatch(
    {
      repo <- gert::git_find()
    },
    error = function(e) {
      error(.le$logger, "There was no local Git repository found.")
      rlang::abort("There was no local Git repository found.")
    }
  )
}

#' @importFrom log4r warn error info debug
check_remote_set <- function() {
  remotes <- gert::git_remote_list()

  if (nrow(remotes) == 0) {
    error(.le$logger, "There is no GitHub remote URL set.")
    rlang::abort("There is no GitHub remote URL set.")
  }
}

#' @importFrom log4r warn error info debug
check_upstream_set <- function() {
  repo <- get_simple_path()

  current_branch <- gert::git_branch()

  if (is.null(current_branch)){
    error(.le$logger, glue::glue("There were no branches found for the existing repository: {.le$repo} \n",
                                 "To create a branch, use one of the below for you default branch name: \n",
                                 "  git branch -M main \n",
                                 "  git branch -M master \n",
                                 "Push the branch to the remote repository using: \n",
                                 "  git push -u {.le$remote_name} main \n",
                                 "  git push -u {.le$remote_name} master"))
    rlang::abort(glue::glue("There were no branches found for the existing repo: {.le$repo}"))
  }

  col_names <- c("name", "upstream") # doing this to pass rcmdcheck: check_upstream_set: no visible binding for global variable ‘upstream’
  branch_list <- gert::git_branch_list()
  tracking_branch <- branch_list[branch_list$name == current_branch & branch_list$upstream != "", ]$upstream

  if (length(tracking_branch) == 0 || is.na(tracking_branch)) {
    error(.le$logger, glue::glue(
      "The current branch '{current_branch}' has no tracking information.  \n",
      "If you are planning on basing your work on an upstream branch that already exists at the remote, retrieve them with: \n",
      "  git fetch {.le$remote_name} \n",
      "If you wish to set tracking information for this branch you can do so with: \n",
      "  git branch --set-upstream-to={.le$remote_name}/{current_branch} {current_branch}"
    ))
    rlang::abort(glue::glue(
      "The current branch '{current_branch}' has no tracking information.
      Please set upstream and restart the app."
    ))
  }
}

#' @importFrom log4r warn error info debug
get_env_url <- function() {
  env_url <- Sys.getenv("GITHUB_API_URL")
  env_url <- gsub("/$", "", env_url)
  env_url <- stringr::str_remove(env_url, "/api/v3$")
  if (!stringr::str_starts(env_url, "https://")) env_url <- paste0("https://", env_url)
  env_url
}

#' @importFrom log4r warn error info debug
check_remote_matches_env_url <- function(remote_url) {
  env_url <- get_env_url()
  if (remote_url != env_url && env_url != "https://") {
    info(.le$logger, glue::glue("GITHUB_API_URL environment variable: \"{env_url}\" does not match remote URL: \"{remote_url}\". No action necessary"))
  }
}

#' @importFrom log4r warn error info debug
get_gh_api_url <- function(remote_url) {
  tryCatch(
    {
      glue::glue("{remote_url}/api/v3")
    }, error = function(e) {
      rlang::abort(message = conditionMessage(e))
    }
  )
}

#' @importFrom log4r warn error info debug
get_gh_token <- function(url) {
  tryCatch({
    pat <- gitcreds::gitcreds_get(url = get_gh_api_url(url))$password
  }, error = function(e) {
    error(.le$logger, message = glue::glue("Could not find GitHub PAT for {url} due to: {conditionMessage(e)}. Set your GitHub credentials before continuing"))
    rlang::abort(message = glue::glue("Could not find GitHub PAT for {url}. Set your GitHub credentials before continuing"), parent = e$parent)
  })

  if (nchar(pat) != 40) {
    error(.le$logger, glue::glue("Retrieved GitHub PAT is not 40 characters. Reconfigure your Git Credentials for {url} before continuing"))
    rlang::abort(message = glue::glue("Retrieved GitHub PAT is not 40 characters. Reconfigure your Git Credentials for {url} before continuing"))
  }
  info(.le$logger, glue::glue("Retrieved GitHub PAT successfully: {paste0(substr(pat, 1, 4), strrep('*', nchar(pat)-4))}"))
  pat
}

#' @importFrom log4r warn error info debug
try_api_call <- function(url, token) {
  tryCatch({
    debug(.le$logger, glue::glue("Attempting test api call..."))
    gh::gh("GET /user", .api_url = get_gh_api_url(url)) # , .token = token
    info(.le$logger, glue::glue("Successful test api call to {get_gh_api_url(url)}"))
  }, error = function(e) {
    pat_substr <- paste0(substr(token, 1, 4), strrep("*", nchar(token)-4))
    error(.le$logger, message = glue::glue("{url} could not be accessed using {pat_substr} due to: {conditionMessage(e)}. Ensure your GitHub credentials are correct before continuing"))
    rlang::abort(message = glue::glue("{url} could not be accessed using {pat_substr}. Ensure your GitHub credentials are correct before continuing", parent = e$parent))
  })
}

#' @importFrom log4r warn error info debug
check_github_credentials <- function() {
  # Check errors
  check_git_inited()
  check_remote_set()

  remote <- get_remote()
  remote_name <- remote$name
  assign("remote_name", remote_name, envir = .le)

  remote_git_url <- remote$url
  gh_info <- parse_remote_url(remote_git_url)

  base_git_url <- gh_info$base_url
  assign("base_git_url", base_git_url, envir = .le)

  full_repo_url <- gh_info$full_url
  assign("full_repo_url", full_repo_url, envir = .le)

  org <- gh_info$org
  assign("org", org, envir = .le)

  repo <- gh_info$repo
  assign("repo", repo, envir = .le)

  github_api_url <- get_gh_api_url(base_git_url)
  assign("github_api_url", github_api_url, envir = .le)


  check_upstream_set()
  token <- get_gh_token(base_git_url)
  assign("token", token, envir = .le)

  try_api_call(base_git_url, token)
  check_remote_matches_env_url(base_git_url)


}

