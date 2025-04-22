#' @importFrom log4r warn error info debug
get_names_and_usernames <- function(username) {
  user <- gh::gh("GET /users/{username}", .api_url = .le$github_api_url, username = username)
  return(list(
    username = user$login,
    name = user$name
  ))
}

#' @importFrom log4r warn error info debug
get_repos <- function(org) {
  debug(.le$logger, glue::glue("Retrieving repos in org {org}..."))
  repos <- tryCatch(
    {
      gh::gh("GET /orgs/:org/repos", .api_url = .le$github_api_url, org = org, .limit = Inf)
    },
    error = function(e) {
      error(.le$logger, glue::glue("Failed to get repos in org {org}. {conditionMessage(e)}"))
    }
  )

  purrr::map_chr(repos, "name")
}


filter_for_non_empty_milestones <- function(milestones) {
  non_empty_milestones <- lapply(milestones, function(milestone) {
    if (check_that_milestone_is_non_empty(milestone)) {
      milestone
    }
    else NULL
  })
  # delete NULLs from list
  non_empty_milestones <- Filter(Negate(is.null), non_empty_milestones)
  return(non_empty_milestones)
}

filter_for_ghqc_milestones <- function(owner, repo, milestones) {
  if (length(milestones) == 0) return(milestones)
  labels <- sapply(milestones, function(x) gh::gh("GET /repos/:owner/:repo/milestones/:milestone_number/labels", owner = owner, repo = repo, milestone_number = x$number, .api_url = .le$github_api_url))
  milestones[sapply(labels, function(x) "ghqc" %in% sapply(x, function(y) y))]
}

#' @importFrom log4r warn error info debug
get_open_milestone_objects <- function(owner, repo) {
  debug(.le$logger, glue::glue("Retrieving open Milestone(s) in organization {owner}, repo {repo}..."))

  milestones <- gh::gh("GET /repos/:owner/:repo/milestones", .api_url = .le$github_api_url, owner = owner, repo = repo, state = "open", .limit = Inf)
  debug(.le$logger, glue::glue("Retrieved {length(milestones)} open Milestone(s) in repo {repo}"))
  ghqc_milestones <- filter_for_ghqc_milestones(owner, repo, milestones)
  info(.le$logger, glue::glue("Retrieved {length(ghqc_milestones)} open ghqc Milestone(s) in repo {repo}"))
  non_empty_milestones <- filter_for_non_empty_milestones(ghqc_milestones)
}

#' @importFrom log4r warn error info debug
get_closed_milestone_objects <- function(owner, repo) {
  debug(.le$logger, glue::glue("Retrieving closed Milestone(s) in organization {owner}, repo {repo}..."))

  milestones <- gh::gh("GET /repos/:owner/:repo/milestones", .api_url = .le$github_api_url, owner = owner, repo = repo, state = "closed", .limit = Inf)
  debug(.le$logger, glue::glue("Retrieved {length(milestones)} closed Milestone(s) in repo {repo}"))
  ghqc_milestones <- filter_for_ghqc_milestones(owner, repo, milestones)
  info(.le$logger, glue::glue("Retrieved {length(ghqc_milestones)} closed ghqc Milestone(s) in repo {repo}"))
  non_empty_milestones <- filter_for_non_empty_milestones(ghqc_milestones)
}

#' @importFrom log4r warn error info debug
get_all_milestone_objects <- function(owner, repo) {
  gh::gh("GET /repos/:owner/:repo/milestones", owner = owner, repo = repo, .api_url = .le$github_api_url, state = "all", .limit = Inf)
}

#' @importFrom log4r warn error info debug
get_open_milestone_names <- function(org, repo) {

  tryCatch({
  milestones <- get_open_milestone_objects(org, repo)
  purrr::map_chr(milestones, "title")
  }, error = function(e) {
    error(.le$logger, glue::glue("Failed to retrieve open Milestone name(s) for organization {org} and {repo}."))
    rlang::abort(conditionMessage(e))
  })
}

#' @importFrom log4r warn error info debug
get_closed_milestone_names <- function(org, repo) {
  tryCatch({
    milestones <- get_closed_milestone_objects(org, repo)
    purrr::map_chr(milestones, "title")
  }, error = function(e) {
    error(.le$logger, glue::glue("Failed to retrieve closed Milestone name(s) for organization {org} and {repo}."))
    rlang::abort(conditionMessage(e))
  })
}

#' @importFrom log4r warn error info debug
list_ghqc_milestones <- function(org, repo) {
  debug(.le$logger, glue::glue("Retrieving Milestone(s) in organization {org}, repo {repo}..."))
  milestones <- get_all_milestone_objects(org, repo)
  info(.le$logger, glue::glue("Retrieved {length(milestones)} total Milestone(s) in repo {repo}"))
  ghqc_milestones <- filter_for_ghqc_milestones(org, repo, milestones)
  info(.le$logger, glue::glue("Retrieved {length(ghqc_milestones)} ghqc Milestone(s) in repo {repo}"))
  non_empty_milestones <- filter_for_non_empty_milestones(ghqc_milestones)
  info(.le$logger, glue::glue("Retrieved {length(non_empty_milestones)} non-empty ghqc Milestone(s) in repo {repo}"))
  return(non_empty_milestones)
}

list_ghqc_milestone_names <- function(org, repo) {
  ghqc_milestones <- list_ghqc_milestones(org, repo)
  res <- purrr::map_chr(ghqc_milestones, "title")
  return(res)
}

get_remote_name <- function(remote_url) {
  # remove .git and extract name
  remote_repo_name <- stringr::str_remove(basename(remote_url), ".git")
  info(.le$logger, glue::glue("Retrieved remote repository name: {remote_repo_name}"))
  return(remote_repo_name)
}

#' @importFrom log4r info debug
parse_remote_url <- function(remote_url) {
  api_url <- {
    # if ssh
    if (stringr::str_detect(remote_url, "@")) {
      debug(.le$logger, glue::glue("Detected ssh remote URL: {remote_url}"))
      url <- stringr::str_extract(remote_url, "(?<=@)[^:]+")
      glue::glue("https://{url}")
    }
    # else if https
    else if (stringr::str_detect(remote_url, "^https://")) {
      debug(.le$logger, glue::glue("Detected https remote URL: {remote_url}"))
      dirname(dirname(remote_url))
    }
    else {
      error(.le$logger, glue::glue("Remote url: {remote_url} doesn't match ssh or https formats"))
      rlang::abort(glue::glue("Remote url: {remote_url} doesn't match ssh or https formats"))
    }
  }

  info(.le$logger, glue::glue("Connected to remote repository url: {api_url}"))
  api_url
}

#' @importFrom log4r warn error info debug
get_remote <- function() {

  debug(.le$logger, glue::glue("Retrieving local repo path..."))
  repo_path <- gert::git_find()
  debug(.le$logger, glue::glue("Retrieved local repo path: {repo_path}"))

  debug(.le$logger, glue::glue("Retrieving list of remotes..."))
  remote_list <- gert::git_remote_list(repo = repo_path)

  debug_remote_list <- apply(remote_list, 1, function(row) {
    glue::glue("name: {row['name']}, url: {row['url']}")
  })
  debug_remote_list_str <- glue::glue_collapse(debug_remote_list, sep = "\n")
  debug(.le$logger, glue::glue("Retrieved list of remotes: \n{debug_remote_list_str}"))

  debug(.le$logger, glue::glue("Selecting remote from list..."))
  remote <- {
    ### FIRST: check if there's a single remote,
    num_remotes <- nrow(remote_list)
    if (num_remotes == 1) {
      debug(.le$logger, glue::glue("Single remote: Selected only remote in list"))
      remote_list[1, ]
    } # FIRST

    ### SECOND: check if env var set
    else if (Sys.getenv("GHQC_REMOTE_NAME") != "") {
      # else, there are multiple or zero remotes
      info(.le$logger, "Multiple remote names detected")

      remote_env_var <- Sys.getenv("GHQC_REMOTE_NAME")

      info(.le$logger, glue::glue("Retrieving remote name from GHQC_REMOTE_NAME environment variable: {remote_env_var}"))
      # if in list of remotes
      if (remote_env_var %in% remote_list$name) {
        remote_list[remote_list$name == remote_env_var, ][1, ]
      }
      else {
        error(.le$logger, glue::glue("{remote_env_var} not in list of remotes"))
        rlang::abort(glue::glue("{remote_env_var} not in list of remotes"))
      }
    } # SECOND

    ### THIRD: check if origin exists in remote list
    else if ("origin" %in% remote_list$name) {
      info(.le$logger, "Multiple remote names detected")
      info(.le$logger, "No GHQC_REMOTE_NAME environment variable found. Using \"origin\" from list of remotes.")
      remote_list[remote_list$name == "origin", ][1, ]
    } # THIRD

    ### LAST: try to get first remote
    else {
      tryCatch({
        info(.le$logger, "Multiple remote names detected")
        info(.le$logger, glue::glue("No GHQC_REMOTE_NAME environment variable found. Using first remote from list of remotes: {remote_list$name[1]}"))
        remote_list[1, ]
        # error if no remote urls
      }, error = function(e) {
        error(.le$logger, glue::glue("No remote repository found"))
        rlang::abort(conditionMessage(e))
      })
    } # LAST
  } # remote

  return(remote)
}

#' @importFrom log4r warn error info debug
get_current_repo <- function(remote) {
  tryCatch({
  debug(.le$logger, glue::glue("Connecting to repository..."))

  }, error = function(e) {
    error(.le$logger, glue::glue("No local git repository found."))
    rlang::abort(conditionMessage(e))
  })
  remote_repo_name <- get_remote_name(remote$url)
}

#' @importFrom log4r warn error info debug
get_organization_name_from_url <- function(remote_url) {
  # https url
  matches <- {
    if (grepl("https://", remote_url)) {
      regmatches(remote_url, regexec("https://[^/]+/([^/]+)", remote_url)) #/[^/]+
    }
    # ssh url
    else if (grepl("git@", remote_url)) {
      regmatches(remote_url, regexec("git@[^:]+:([^/]+)/[^/]+", remote_url))
    }
    else {
      NULL
      stop("Unknown remote url format")
    }
  } # matches

  # if could match org
  if (length(matches[[1]]) < 2) {
    stop("Unable to parse organization from url")
  }
  else {
    return(matches[[1]][2])
  }
} # get_organization_name_from_url

#' @importFrom log4r warn error info debug
get_organization <- function(remote) {
  tryCatch({
  debug(.le$logger, glue::glue("Connecting to organization..."))

  # remote url
  debug(.le$logger, glue::glue("Retrieving remote url..."))
  remote_url <- dirname(remote$url)
  debug(.le$logger, glue::glue("Retrieved remote url: {remote_url}"))

  # org name
  debug(.le$logger, glue::glue("Retrieving organization name from remote url..."))

  org_name <- get_organization_name_from_url(remote$url)

  info(.le$logger, glue::glue("Connected to organization: {org_name}"))

  return(org_name)
  }, error = function(e) {
    error(.le$logger, "Failed to connect to organization. Ensure the repository exists and that remotes are correctly configured.")
    rlang::abort(conditionMessage(e))
  })
}

#' @importFrom log4r warn error info debug
get_issue <- function(owner, repo, issue_number) {
  gh::gh("GET /repos/:owner/:repo/issues/:issue_number", .api_url = .le$github_api_url,
         owner = owner, repo = repo, issue_number = issue_number)
} # get_issue

#' @importFrom log4r warn error info debug
get_issue_comments <- function(owner, repo, issue_number, token) {
  api_url <- .le$github_api_url
  base_url <- ifelse(api_url == "https://github.com/api/v3", "https://api.github.com", api_url)
  url <- glue::glue("{base_url}/repos/{owner}/{repo}/issues/{issue_number}/comments")

  req <- httr2::request(url) %>%
    httr2::req_headers(
      "Accept" = "application/vnd.github.v3+json",
      "Authorization" = paste("token", token),
      "Accept" = "application/vnd.github.full+json",
    ) %>%
    httr2::req_perform()

  res <- httr2::resp_body_json(req, simplifyVector = TRUE)

  return(res)
} # get_issue_comments

#' @importFrom log4r warn error info debug
get_issue_events <- function(owner, repo, issue_number) {
  gh::gh("GET /repos/:owner/:repo/issues/:issue_number/events", .api_url = .le$github_api_url,
         owner = owner, repo = repo, issue_number = issue_number)
} # get_issue_events

#' @importFrom log4r warn error info debug
get_issue_timeline <- function(owner, repo, issue_number) {
  gh::gh("GET /repos/:owner/:repo/issues/:issue_number/timeline", .api_url = .le$github_api_url,
         owner = owner, repo = repo, issue_number = issue_number)
}

#' @importFrom log4r warn error info debug
get_issues <- function(owner, repo, milestone) {
  params <- c(owner, repo)
  gh::gh("GET /repos/:owner/:repo/issues", .api_url = .le$github_api_url,
         owner = owner, repo = repo, milestone = milestone, state = "all")
}

#' @importFrom log4r warn error info debug
get_all_issues_in_repo <- function(owner, repo) {
  debug(.le$logger, glue::glue("Retrieving all Issue(s) from repo: {repo}..."))
  open_issues <- list()
  page <- 1

  repeat {
    res <- gh::gh("GET /repos/:owner/:repo/issues", .api_url = .le$github_api_url,
                  owner = owner,
                  repo = repo,
                  state = "open",
                  per_page = 100,
                  page = page)

    # break if no more issues
    if (length(res) == 0) break

    # append to list
    open_issues <- c(open_issues, res)

    # next page
    page <- page + 1
  }

  # closed issues
  closed_issues <- list()
  page <- 1

  repeat {
    res <- gh::gh("GET /repos/:owner/:repo/issues", .api_url = .le$github_api_url,
                  owner = owner,
                  repo = repo,
                  state = "closed",
                  per_page = 100,
                  page = page)

    # break if no more issues
    if (length(res) == 0) break

    # append to list
    closed_issues <- c(closed_issues, res)

    # next page
    page <- page + 1
  }

  issues <- get_only_ghqc_issues(c(open_issues, closed_issues))
  num_issues <- length(issues)
  info(.le$logger, glue::glue("Retrieved {num_issues} Issue(s) from repo: {repo}"))
  return(issues)

}

get_only_ghqc_issues <- function(issues) {
  if (length(issues) == 0) return(issues)
  labels <- sapply(issues, function(x) x$labels)
  if (length(labels) == 0) return(c(list(), list()))
  issues[sapply(labels, function(x) "ghqc" %in% sapply(x, function(y) y))]
}

# sort by open/closed
#' @importFrom log4r warn error info debug
get_all_issues_in_milestone <- function(owner, repo, milestone_name) {
  debug(.le$logger, glue::glue("Retrieving all Issue(s) from Milestone: {milestone_name}..."))
  # get milestone number from name
  milestone_number <- look_up_existing_milestone_number(list(owner = owner, repo = repo, title = milestone_name))

  # if the Milestone dne, there are no Issues in the Milestone, return an empty vector
  if (is.null(milestone_number)) {
    info(.le$logger, glue::glue("Milestone: {milestone_name} doesn't yet exist"))
    return(c())
  }

  open_issues <- list()
  page <- 1

  repeat {
    res <- gh::gh("GET /repos/:owner/:repo/issues", .api_url = .le$github_api_url,
                  owner = owner,
                  repo = repo,
                  milestone = milestone_number,
                  state = "open",
                  per_page = 100,
                  page = page)

    # break if no more issues
    if (length(res) == 0) break

    # append to list
    open_issues <- c(open_issues, res)

    # next page
    page <- page + 1
  }

  # closed issues
  closed_issues <- list()
  page <- 1

  repeat {
    res <- gh::gh("GET /repos/:owner/:repo/issues", .api_url = .le$github_api_url,
                  owner = owner,
                  repo = repo,
                  milestone = milestone_number,
                  state = "closed",
                  per_page = 100,
                  page = page)

    # break if no more issues
    if (length(res) == 0) break

    # append to list
    closed_issues <- c(closed_issues, res)

    # next page
    page <- page + 1
  }

  issues <- get_only_ghqc_issues(c(open_issues, closed_issues))
  info(.le$logger, glue::glue("Retrieved {length(issues)} ghqc Issue(s) from Milestone: {milestone_name}"))
  return(issues)
}

#' @importFrom log4r warn error info debug
get_milestone_url <- function(owner, repo, milestone_name) {
  milestone_number <- get_milestone_number(list(owner = owner, repo = repo, title = milestone_name))
  milestone <- gh::gh(
    "GET /repos/{owner}/{repo}/milestones/{milestone_number}", .api_url = .le$github_api_url,
    owner = owner,
    repo = repo,
    milestone_number = milestone_number
  )

  milestone$html_url
}

#' @importFrom log4r warn error info debug
get_milestone_list_url <- function(org, repo) {
  remote_url <- parse_remote_url(get_remote()$url)
  # will look something like:
  # https://ghe-experiments.dev.a2-ai.cloud/gsk-cpmsprojects/test_ghqc_9005/milestones
  milestones_url <- file.path(remote_url, org, repo, "milestones")
}

#' @importFrom log4r warn error info debug
get_collaborators <- function(owner, repo) {
  tryCatch({
    query <- gh::gh("GET /repos/{owner}/{repo}/collaborators", .api_url = .le$github_api_url, .limit = Inf, owner = owner, repo = repo)
    members_list <- purrr::map(query, ~ get_names_and_usernames(.x$login))
    members_df <- purrr::map_df(members_list, ~ as.data.frame(t(.x), stringsAsFactors = FALSE))
    return(members_df)
  }, error = function(e) {
    error(.le$logger, glue::glue("No collaborators found in {owner}/{repo}"))
    rlang::abort(conditionMessage(e))
  })
}

get_user <- function() {
  user <- gh::gh("GET /user", .api_url = .le$github_api_url)
  return(user$login)
}

get_branch_from_metadata <- function(owner, repo, issue_number) {
  issue <- get_issue(owner, repo, issue_number)
  get_branch_from_issue_body(issue$body)
}

get_branch_from_issue_body <- function(issue_body) {
  tryCatch({
    text <- get_issue_body_metadata(issue_body)$`git branch`
    branch <- stringr::str_match(text, "\\[(.*?)\\]")[, 2]

    if (length(branch) == 0) {
      shiny::stopApp()
      rlang::abort(glue::glue("git branch not present in metadata of Issue #{issue_number} body"))
    }

    return(branch)
  }, error = function(e) {
    shiny::stopApp()
    rlang::abort(conditionMessage(e))
  })
}

close_issue <- function(owner, repo, issue_number) {
  gh::gh(
    "PATCH /repos/:owner/:repo/issues/:issue_number",
    owner = owner,
    repo = repo,
    issue_number = issue_number,
    .api_url = .le$github_api_url,
    state = "closed"
  )
}
