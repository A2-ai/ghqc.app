# create an issue for each file given in a yaml file
# - organize issues associated with a set of files with milestones
# - assign a different user to each issue for a given file

create_issue <- function(file, issue_params, remote, file_names) {
  issue_params$org <- .le$org
  issue_params$repo <- .le$repo

  # issue title is the name of the file
  issue_params$title <- file$name
  # body is checklist
  issue_params$body <- format_issue_body(checklist_type = file$checklist_type,
                                         file_path = file$name,
                                         relevant_files = file$relevant_files,
                                         file_names = file_names)
  # if file has assignees item, add to issue_params
  if (!is.null(file$assignees)) {
    issue_params$assignees <- I(file$assignees)
  }

  issue_params$.api_url <- .le$github_api_url

  # create the issue
  debug(.le$logger, glue::glue("Creating Issue... {issue_params$title}"))
  issue <- do.call(gh::gh, c("POST /repos/{.le$org}/{.le$repo}/issues", issue_params))
  debug(.le$logger, glue::glue("Created Issue {issue_params$title}"))

  debug(.le$logger, glue::glue("Adding 'ghqc' label to {issue_params$title}"))
  label_params <- list(org = issue_params$org,
                       repo = issue_params$repo,
                       issue_number = issue$number,
                       labels = array("ghqc"),
                       .api_url = .le$github_api_url)
  label <- do.call(gh::gh, c("POST /repos/{.le$org}/{.le$repo}/issues/{issue_number}/labels", label_params))
  debug(.le$logger, glue::glue("Label 'ghqc' added to {issue_params$title}"))

  # return the issue number
  list(number = issue$number, assignees = issue_params$assignees)
} # create_issue

#' @importFrom log4r warn error info debug
create_issues <- function() {
  # create list of issue_params to input to api call -
  # will build up in pieces because some are optional
  issue_params <- list(
    org = .le$org,
    repo = .le$repo
  )

  # if milestone is in data struct
  if (!is.null(data$milestone)) {
    # create milestone_params
    milestone_params <- list(
      org = .le$org,
      repo = .le$repo,
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

  file_names <- purrr::map(data$files, "name")
  file_names_col <- glue::glue_collapse(file_names, sep = ", ", last = " and ")
  debug(.le$logger, glue::glue("Creating {get_checklist_display_name_var(plural = TRUE)} for files: {file_names_col}"))

  # create an issue for each file
  lapply(data$files, function(file) {
    issue <- create_issue(file, issue_params, remote, file_names)
    debug(.le$logger, glue::glue("Created {get_checklist_display_name_var()} for file: {file$name}"))
    if (!is.null(data$milestone)) {
      debug(.le$logger, glue::glue("Milestone: {data$milestone}"))
    }
    if (!is.null(issue$assignees)) {
      debug(.le$logger, glue::glue("Assignee: ", glue::glue_collapse(issue$assignees, sep = ", ")))
    }
    debug(.le$logger, glue::glue("Issue number: {issue$number}"))
    return(issue)
  })
  info(.le$logger, glue::glue("Created {get_checklist_display_name_var()}(s) for file(s): {file_names_col}"))
} # create_issues

#' @importFrom gh gh
ghqc_label_exists <- function() {
  labels <- do.call(gh::gh, c("GET /repos/{.le$org}/{.le$repo}/labels",
                              list(org = .le$org,
                                   repo = .le$repo,
                                   .api_url = .le$github_api_url
                                   )))
  "ghqc" %in% sapply(labels, function(x) x$name)
}

#' @importFrom gh gh
create_ghqc_label <- function() {
  issue_params <- list(
    org = .le$org,
    repo = .le$repo,
    name = "ghqc",
    color = "FFCB05",
    description = "Issue created by the ghqc package",
    .api_url = .le$github_api_url
  )
  do.call(gh::gh, c("POST /repos/{.le$org}/{.le$repo}/labels", issue_params))
}


# test with "test_yamls/checklist.yaml"
#' @importFrom log4r warn error info debug
create_checklists <- function(yaml_path) {
  data <- yaml::yaml.load_file(yaml_path)
  if (!ghqc_label_exists()) labels <- create_ghqc_label()
   create_issues(data)
}


