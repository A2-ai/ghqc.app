validate_owner <- function(owner, dry = FALSE) {
  err_if_not_in_yaml(owner)
  err_if_not_char(owner)
  err_if_len_gt_one(owner)

  # helper fxn
  organization_exists <- function(org_name) {
    tryCatch(
      {
        # try to get org
        org_info <- gh::gh("/orgs/{org}", org = org_name)
      },
      # else, organization does not exist
      error = function(e) {
        rlang::abort(message = glue::glue("organization {owner} not found"),
                     class = "input_doesnt_exist",
                     x = owner)
      }
    )
  }
  if (!dry) {organization_exists(owner)}
} # validate_owner


validate_repo <- function(owner, repo, dry = FALSE) {
  err_if_not_in_yaml(repo)
  err_if_not_char(repo)
  err_if_len_gt_one(repo)

  # helper fxn
  repo_exists <- function(owner, repo) {
    tryCatch(
      {
        # try to get repo
        repo_info <- gh::gh("/repos/{owner}/{repo}", owner = owner, repo = repo)
      },
      error = function(e) {
        rlang::abort(message = glue::glue("repo {repo} not found"),
                     class = "input_doesnt_exist",
                     x = repo)
      }
    )
  }
  if (!dry) {repo_exists(owner, repo)}
} # validate_repo

validate_milestone <- function(milestone) {
  err_if_not_char(milestone)
  err_if_len_gt_one(milestone)
} # validate milestone

validate_description <- function(description) {
  err_if_not_char(description)
  err_if_len_gt_one(description)
} # validate description

validate_checklist_type <- function(checklist_type, file_name) {
  #err_if_invalid_checklist_type(checklist_type, file_name)
}

validate_items <- function(items) {
  err_if_not_in_yaml(items)
  # don't need to check items individually, just that it's a vec of chars
  err_if_not_char(items)
} # validate_items

validate_assignee <- function(assignee, owner, dry = FALSE) {
  assignee_exists <- function(username, org_name) {
    # try to find user in organization
    tryCatch({
      response <- gh::gh(
        "GET /orgs/:org/members/:username", .api_url = .le$github_api_url,
        org = org_name,
        username = username,
        .send_headers = c("Accept" = "application/vnd.github.v3+json")
      )
    },
    error = function(e) {
      rlang::abort(message = glue::glue("assignee {assignee} not found"),
                   class = "input_doesnt_exist",
                   x = assignee)
    })
  } # assignee_exists
  if (!dry) {assignee_exists(assignee, owner)}
} # validate_assignee

validate_assignees <- function(assignees, owner, dry) {
  err_if_not_char(assignees)
  lapply(assignees, validate_assignee, owner, dry)
} # validate_assignees

validate_name <- function(name) {
  err_if_not_in_yaml(name)
  err_if_not_char(name)
  err_if_len_gt_one(name)
} # validate_name

validate_file <- function(file, owner, dry) {
  err_if_file_attr_missing(file)
  #err_if_not_list_of_2_or_3(file)
  validate_name(file$name)
  if (!is.null(file$assignees)) {
    validate_assignees(file$assignees, owner, dry)
  }
  validate_checklist_type(file$checklist_type, file$name)
  #validate_items(file$items)
} # validate_file

validate_files <- function(files, owner, dry) {
  #err_if_not_in_yaml(files)
  err_if_not_list_of_lists(files)
  lapply(files, validate_file, owner, dry)
} # validate_files


