# create an issue for each file given in a yaml file
# - organize issues associated with a set of files with milestones
# - assign a different user to each issue for a given file

create_issue <- function(file, issue_params) {
  # issue title is the name of the file
  issue_params$title <- file$name
  # body is checklist
  issue_params$body <- format_issue_body(file$checklist_type, file_path = file$name)
  # if file has assignees item, add to issue_params
  if (!is.null(file$assignees)) {
    issue_params$assignees <- I(file$assignees)
  }

  issue_params$.api_url <- .le$github_api_url

  # create the issue
  debug(.le$logger, glue::glue("Creating Issue... {issue_params$title}"))
  issue <- do.call(gh::gh, c("POST /repos/{owner}/{repo}/issues", issue_params))
  debug(.le$logger, glue::glue("Created Issue {issue_params$title}"))

  # return the issue number
  list(number = issue$number, assignees = issue_params$assignees)
} # create_issue

#' @importFrom log4r warn error info debug
create_issues <- function(data) {
  # create list of issue_params to input to api call -
  # will build up in pieces because some are optional
  issue_params <- list(
    owner = data$owner,
    repo = data$repo
  )

  # if milestone is in data struct
  if (!is.null(data$milestone)) {
    # create milestone_params
    milestone_params <- list(
      owner = data$owner,
      repo = data$repo,
      title = data$milestone
    )

    # if a decription was given, add it to the milestone_params
    if (!is.null(data$description)) {
      milestone_params$description <- data$description
    }
    debug(.le$logger, glue::glue("Adding Milestone characteristics: {milestone_params}"))

    # add milestone to the issue_params
    issue_params$milestone <- get_milestone_number(milestone_params)
  }


  file_names <- glue::glue_collapse(purrr::map(data$files, "name"), sep = ", ", last = " and ")
  debug(.le$logger, glue::glue("Creating checklists for files: {file_names}"))

  # create an issue for each file
  lapply(data$files, function(file) {
    issue <- create_issue(file, issue_params)
    debug(.le$logger, glue::glue("Created checklist for file: {file$name}"))
    if (!is.null(data$milestone)) {
      debug(.le$logger, glue::glue("Milestone: {data$milestone}"))
    }
    if (!is.null(issue$assignees)) {
      debug(.le$logger, glue::glue("Assignee: ", glue::glue_collapse(issue$assignees, sep = ", ")))
    }
    debug(.le$logger, glue::glue("Issue number: {issue$number}"))
    return(issue)
  })
  info(.le$logger, glue::glue("Created checklist(s) for file(s): {file_names}"))
} # create_issues


# test with "test_yamls/checklist.yaml"
#' @importFrom log4r warn error info debug
create_checklists <- function(yaml_path) {
  data <- read_and_validate_yaml(yaml_path)
  create_issues(data)
}


