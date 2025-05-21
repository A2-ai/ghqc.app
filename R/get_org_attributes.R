#' @importFrom log4r warn error info debug
get_names_and_usernames <- function(username) {
  user <- gh::gh("GET /users/{username}", .api_url = .le$github_api_url, username = username)
  return(list(
    username = user$login,
    name = user$name
  ))
}


#' @importFrom log4r warn error info debug
get_all_milestone_objects <- function() {
  gh::gh("GET /repos/:org/:repo/milestones", org = .le$org, repo = .le$repo, .api_url = .le$github_api_url, state = "all", .limit = Inf)
}

#' @importFrom log4r warn error info debug
get_open_milestone_objects <- function() {
  gh::gh("GET /repos/:org/:repo/milestones", org = .le$org, repo = .le$repo, .api_url = .le$github_api_url, state = "open", .limit = Inf)
}

#' @importFrom log4r warn error info debug
get_closed_milestone_objects <- function() {
  gh::gh("GET /repos/:org/:repo/milestones", org = .le$org, repo = .le$repo, .api_url = .le$github_api_url, state = "closed", .limit = Inf)
}

get_non_empty_milestone_objects_from_milestone_objects <- function(milestones) {
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

#' @importFrom log4r warn error info debug
get_closed_non_empty_milestone_objects <- function() {
  debug(.le$logger, glue::glue("Retrieving Milestone(s) in organization {.le$org}, repo {.le$repo}..."))
  closed_milestone_objects <- get_closed_milestone_objects()

  info(.le$logger, glue::glue("Retrieved {length(closed_milestone_objects)} closed Milestone(s) in repo {.le$repo}"))
  closed_non_empty_milestone_objects <- get_non_empty_milestone_objects_from_milestone_objects(closed_milestone_objects)

  info(.le$logger, glue::glue("Retrieved {length(closed_non_empty_milestone_objects)} closed non-empty Milestone(s) in repo {.le$repo}"))
  return(rev(closed_non_empty_milestone_objects))
}

#' @importFrom log4r warn error info debug
get_open_non_empty_milestone_objects <- function() {
  debug(.le$logger, glue::glue("Retrieving Milestone(s) in organization {.le$org}, repo {.le$repo}..."))
  open_milestone_objects <- get_open_milestone_objects()

  info(.le$logger, glue::glue("Retrieved {length(open_milestone_objects)} open Milestone(s) in repo {.le$repo}"))
  open_non_empty_milestone_objects <- get_non_empty_milestone_objects_from_milestone_objects(open_milestone_objects)

  info(.le$logger, glue::glue("Retrieved {length(open_non_empty_milestone_objects)} open non-empty Milestone(s) in repo {.le$repo}"))
  return(rev(open_non_empty_milestone_objects))
}

#' @importFrom log4r warn error info debug
get_all_non_empty_milestone_objects <- function() {
  debug(.le$logger, glue::glue("Retrieving Milestone(s) in organization {.le$org}, repo {.le$repo}..."))
  all_milestone_objects <- get_all_milestone_objects()

  info(.le$logger, glue::glue("Retrieved {length(all_milestone_objects)} Milestone(s) in repo {.le$repo}"))
  all_non_empty_milestone_objects <- get_non_empty_milestone_objects_from_milestone_objects(all_milestone_objects)

  info(.le$logger, glue::glue("Retrieved {length(all_non_empty_milestone_objects)} non-empty Milestone(s) in repo {.le$repo}"))
  return(rev(all_non_empty_milestone_objects))
}

get_closed_milestone_objects_from_all_milestone_objects <- function(milestone_objects) {
  purrr::keep(milestone_objects, ~ .x$state == "closed")
}

get_open_milestone_objects_from_all_milestone_objects <- function(milestone_objects) {
  purrr::keep(milestone_objects, ~ .x$state == "open")
}

get_milestone_object_from_milestone_name <- function(milestone_name, milestone_objects) {
  titles <- purrr::map_chr(milestone_objects, "title")
  match_index <- which(titles == milestone_name)

  if (length(match_index) == 0) {
    stop(glue::glue("Milestone title '{milestone_name}' not found."))
  }

  milestone_object <- milestone_objects[[match_index]]
  return(milestone_object)
}

get_milestone_names_from_milestone_objects <- function(milestone_objects) {
  purrr::map_chr(milestone_objects, "title")
}

group_milestone_objects_by_branch <- function(milestone_objects) {
  grouped_milestones <- list()

  for (m in milestone_objects) {
    labels <- gh::gh(m$labels_url)

    # Get the QC branch label
    qc_label <- Filter(
      function(l) isTRUE(l$description == "QC branch"),
      labels
    )

    group_name <- if (length(qc_label) > 0) qc_label[[1]]$name else " "

    # Initialize group if needed
    if (!group_name %in% names(grouped_milestones)) {
      grouped_milestones[[group_name]] <- list()
    }

    # Name the milestone in the group by its title
    grouped_milestones[[group_name]][[m$title]] <- m
  }

  return(grouped_milestones)
}

get_grouped_milestone_names <- function(grouped_milestones, closed_titles = character()) {
  lapply(grouped_milestones, function(group) {
    milestone_titles <- names(group)

    # gray out closed milestones
    display_titles <- ifelse(
      milestone_titles %in% closed_titles,
      sprintf("<span style='color: gray;'>%s</span>", milestone_titles),
      milestone_titles
    )

    stats::setNames(milestone_titles, display_titles)
  })
}



#' @importFrom log4r info debug error warn
parse_remote_url <- function(remote_url) {
  # ssh remote
  if (stringr::str_detect(remote_url, "@")) {
    debug(.le$logger, glue::glue("Detected ssh remote URL: {remote_url}"))
    # Convert ssh to https format: git@github.com:org/repo.git → https://github.com/org/repo.git
    remote_url <- stringr::str_replace(remote_url, "^git@([^:]+):", "https://\\1/")
  } else if (stringr::str_detect(remote_url, "^https://")) {
    debug(.le$logger, glue::glue("Detected https remote URL: {remote_url}"))
  } else {
    error(.le$logger, glue::glue("Remote url: {remote_url} doesn't match ssh or https formats"))
    rlang::abort(glue::glue("Remote url: {remote_url} doesn't match ssh or https formats"))
  }

  # Remove trailing `.git` if present
  remote_url <- stringr::str_remove(remote_url, "\\.git$")

  # Parse parts
  base_url <- dirname(dirname(remote_url))               # https://github.com
  org <- basename(dirname(remote_url))                   # e.g., A2-ai
  repo <- basename(remote_url)                           # e.g., ghqc_status_project2
  full_url <- glue::glue("{base_url}/{org}/{repo}")      # Full remote without .git

  info(.le$logger, glue::glue("Parsed remote URL: {remote_url}"))
  list(
    base_url = base_url,
    full_url = full_url,
    org = org,
    repo = repo
  )
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
get_issue <- function(issue_number) {
  gh::gh("GET /repos/:org/:repo/issues/:issue_number", .api_url = .le$github_api_url,
         org = .le$org, repo = .le$repo, issue_number = issue_number)
} # get_issue

#' @importFrom log4r warn error info debug
get_issue_comments <- function(issue_number) {
  api_url <- .le$github_api_url
  base_url <- ifelse(api_url == "https://github.com/api/v3", "https://api.github.com", api_url)
  url <- file.path(base_url, "repos", .le$org, .le$repo, "issues", issue_number, "comments")
  token <- .le$token

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
get_issue_events <- function(issue_number) {
  gh::gh("GET /repos/:org/:repo/issues/:issue_number/events", .api_url = .le$github_api_url,
         org = .le$org, repo = .le$repo, issue_number = issue_number)
} # get_issue_events

#' @importFrom log4r warn error info debug
get_issue_timeline <- function(issue_number) {
  gh::gh("GET /repos/:org/:repo/issues/:issue_number/timeline", .api_url = .le$github_api_url,
         org = .le$org, repo = .le$repo, issue_number = issue_number)
}

#' @importFrom log4r warn error info debug
get_all_issues_in_repo <- function() {
  debug(.le$logger, glue::glue("Retrieving all Issue(s) from repo: {.le$repo}..."))
  open_issues <- list()
  page <- 1

  repeat {
    res <- gh::gh("GET /repos/:org/:repo/issues",
                  .api_url = .le$github_api_url,
                  org = .le$org,
                  repo = .le$repo,
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
    res <- gh::gh("GET /repos/:org/:repo/issues", .api_url = .le$github_api_url,
                  org = .le$org,
                  repo = .le$repo,
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
  info(.le$logger, glue::glue("Retrieved {num_issues} Issue(s) from repo: {.le$repo}"))
  return(issues)

}

get_only_ghqc_issues <- function(issues) {
  if (length(issues) == 0) return(issues)
  labels <- sapply(issues, function(x) x$labels)
  if (length(labels) == 0) return(c(list(), list()))
  issues[sapply(labels, function(x) "ghqc" %in% sapply(x, function(y) y))]
}

get_all_issues_in_milestone_from_milestone_number <- function(milestone_number, milestone_name) {
  # if the Milestone dne, there are no Issues in the Milestone, return an empty vector
  if (is.null(milestone_number)) {
    info(.le$logger, glue::glue("Milestone: {milestone_name} doesn't exist"))
    return(c())
  }

  issues <- list()
  page <- 1

  repeat {
    res <- gh::gh("GET /repos/:org/:repo/issues",
                  .api_url = .le$github_api_url,
                  org = .le$org,
                  repo = .le$repo,
                  per_page = 100,
                  page = page,
                  state = "all",
                  milestone = milestone_number,
                  labels = "ghqc"
                  )

    # break if no more issues
    if (length(res) == 0) break

    # append to list
    issues <- c(issues, res)

    # next page
    page <- page + 1
  }

  info(.le$logger, glue::glue("Retrieved {length(issues)} ghqc Issue(s) from Milestone: {milestone_name}"))
  return(issues)
}


#' @importFrom log4r warn error info debug
get_milestone_url <- function(milestone_name) {
  milestone_number <- get_milestone_number(list(org = .le$org, repo = .le$repo, title = milestone_name))
  milestone <- gh::gh(
    "GET /repos/:org/:repo/milestones/:milestone_number",
    .api_url = .le$github_api_url,
    org = .le$org,
    repo = .le$repo,
    milestone_number = milestone_number
  )

  milestone$html_url
}

#' @importFrom log4r warn error info debug
get_milestone_list_url <- function() {
  milestones_url <- file.path(.le$full_repo_url, "milestones")
}

#' @importFrom log4r warn error info debug
get_collaborators <- function() {
  tryCatch({
    query <- gh::gh("GET /repos/:org/:repo/collaborators", org = .le$org, repo = .le$repo, .api_url = .le$github_api_url, .limit = Inf)
    members_list <- purrr::map(query, ~ get_names_and_usernames(.x$login))
    members_df <- purrr::map_df(members_list, ~ as.data.frame(t(.x), stringsAsFactors = FALSE))
    return(members_df)
  }, error = function(e) {
    error(.le$logger, glue::glue("No collaborators found in {.le$org}/{.le$repo}"))
    rlang::abort(conditionMessage(e))
  })
}

get_user <- function() {
  user <- gh::gh("GET /user", .api_url = .le$github_api_url)
  return(user$login)
}

get_branch_from_metadata <- function(issue_number) {
  issue <- get_issue(issue_number)
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

close_issue <- function(issue_number) {
  gh::gh(
    "PATCH /repos/:org/:repo/issues/:issue_number",
    org = .le$org,
    repo = .le$repo,
    issue_number = issue_number,
    .api_url = .le$github_api_url,
    state = "closed"
  )
}

open_issue <- function(issue_number) {
  gh::gh(
    "PATCH /repos/:org/:repo/issues/:issue_number",
    org = .le$org,
    repo = .le$repo,
    issue_number = issue_number,
    .api_url = .le$github_api_url,
    state = "open"
  )
}

get_remote_ref_for_branch <- function(branch_name) {
  branches <- gert::git_branch_list(local = FALSE)

  # Find matching remote branch by leaf name
  match_idx <- which(basename(branches$name) == branch_name)

  if (length(match_idx) == 0) {
    rlang::abort(paste("Branch", branch_name, "not found in remote branches."))
  }

  # Return the full remote name, e.g., "origin/qc_branch_1"
  return(branches$name[match_idx[1]])
}
