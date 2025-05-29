get_init_qc_commit <- function(issue_number) {
  issue <- get_issue(issue_number)
  get_init_qc_commit_from_issue_body(issue$body)

}

get_init_qc_commit_from_issue_body <- function(issue_body) {
  init_commit <- get_issue_body_metadata(issue_body)$`initial qc commit`
  if (is.null(init_commit)) {
    init_commit <- get_issue_body_metadata(issue_body)$`git sha`
  }
  if (is.null(init_commit)) {
    init_commit <- get_issue_body_metadata(issue_body)$`git_sha`
  }
  return(init_commit)
}





create_notify_list <- function(assignees, issue_creator) {
  user_login <- get_user()
  assignee_logins <- sapply(assignees, function(assignee) assignee$login)

  users_to_notify <- {
    # if user is in the list of assignees, @ the issue creator
    # or, if there are no assignees, and the user is not the issue creator, @ the issue creator - the user is probably an unassigned QCer
    if (user_login %in% assignee_logins || (length(assignee_logins) == 0 && user_login != issue_creator)) {
      glue::glue("@{issue_creator}")
    }
    # else, @ the assignees
    else {
      sapply(assignee_logins, function(assignee_login) glue::glue("@{assignee_login}"))
    }
  }

  if (length(users_to_notify) == 0) {
    return("")
  }
  else {
    list <- glue::glue_collapse(users_to_notify, sep = "\n")
    return(glue::glue("{list}\n\n\n"))
  }
} # create_notify_list


create_message_body <- function(message) {
  if (is.null(message)) ""
  else glue::glue("{message}\n\n\n")
}

get_commit_comparison_url <- function(reference_commit, comparator_commit) {
  file.path(.le$full_repo_url, "compare", glue::glue("{reference_commit}..{comparator_commit}"))
}

get_commit_comparison_html <- function(reference_commit, comparator_commit) {
  url <- get_commit_comparison_url(reference_commit, comparator_commit)
  glue::glue("<a href=\"{url}\" target=\"_blank\">commit comparison</a>")
}

create_comment_metadata_body <- function(reference_commit,
                                         comparator_commit,
                                         commit_comparison
                                         ) {

  metadata <- glue::glue("## Metadata\n",
             "* current commit: {comparator_commit}\n",
             "* previous commit: {reference_commit}\n",
             "* {commit_comparison}\n\n\n",
             .trim = FALSE
             )

}

create_previous_qc_metadata_body <- function(reference_commit,
                                             comparator_commit,
                                             commit_comparison,
                                             previous_issue_number) {

  metadata <- glue::glue("## Metadata\n",
                         "* current commit: {comparator_commit}\n",
                         "* previous commit: {reference_commit}\n",
                         "* {commit_comparison}\n",
                         "* {previous_issue_number}\n\n\n",
                         .trim = FALSE
  )

}

create_diff_body <- function(diff, reference_commit, reference_script, comparator_commit, comparator_script) {
  if (!diff) return("")

    diff_formatted <- format_diff(reference_script = reference_script, comparator_script = comparator_script)

    if (diff_formatted >= 65450) {
      info(.le$logger, "File difference too long to render")
      diff_formatted <- glue::glue("File difference too long to render.")
    }

    glue::glue("## File Difference\n\n",
               "{diff_formatted}")
}


create_previous_qc_comment_body <- function(diff,
                                            reference_file_path,
                                            comparator_file_path,
                                            reference_commit,
                                            comparator_commit,
                                            previous_issue_number
                                            ) {

  ## check if file exists locally # TODO test this

  # if is a binary file, don't display git diff
  if (stringr::str_detect(reference_file_path, exclude_patterns()) || stringr::str_detect(comparator_file_path, exclude_patterns())) {
    diff <- FALSE
  }

  debug(.le$logger, glue::glue("Creating Previous QC comment body for {comparator_file_path} in {.le$org}/{.le$repo}"))

  debug(.le$logger, glue::glue("Getting script contents..."))

  script_contents <- get_script_contents(reference_file_path = reference_file_path,
                                         comparator_file_path = comparator_file_path,
                                         reference_commit = reference_commit,
                                         comparator_commit = comparator_commit
                                         )
  reference_script <- script_contents$reference_script
  comparator_script <- script_contents$comparator_script
  debug(.le$logger, glue::glue("Got script contents"))

  debug(.le$logger, glue::glue("Getting file difference body..."))
  diff_body <- create_diff_body(diff = diff,
                                reference_commit = reference_commit,
                                reference_script = reference_script,
                                comparator_commit = comparator_commit,
                                comparator_script = comparator_script)
  debug(.le$logger, glue::glue("Got file difference body"))

  debug(.le$logger, glue::glue("Getting metadata body..."))
  commit_comparison <- get_commit_comparison_html(reference_commit = reference_commit,
                                                  comparator_commit = comparator_commit
                                                  )

  metadata_body <- create_previous_qc_metadata_body(reference_commit = reference_commit,
                                                    comparator_commit = comparator_commit,
                                                    commit_comparison = commit_comparison,
                                                    previous_issue_number = previous_issue_number
                                                    )
  debug(.le$logger, glue::glue("Got metadata body"))

  comment_body_first <- as.character(glue::glue("# Previous QC\n\n",
                                                .trim = FALSE))

  comment_body_second <- as.character(glue::glue("{metadata_body}",
                                                 "{diff_body}",
                                                 .trim = FALSE))

  return(c(comment_body_first, comment_body_second))
} # create_previous_qc_comment_body

create_notify_comment_body <- function(issue,
                                message = NULL,
                                diff = FALSE,
                                reference_commit = "original",
                                comparator_commit = "current"
                                ) {

  ## check if file exists locally
  if (!fs::file_exists(issue$title)) {
    log4r::warn(.le$logger, glue::glue("{issue$title} does not exist in local project repo. You may want to change your branch to one in which the file exists."))
  }

  # if is a binary file, don't display git diff
  if (stringr::str_detect(issue$title, exclude_patterns())) {
    diff <- FALSE
  }

  # log
  debug(.le$logger, glue::glue("Creating comment body for Issue #{issue$number}:{issue$title} in {.le$org}/{.le$repo}"))

  debug(.le$logger, glue::glue("Creating notified users body..."))
  notified_users_body <- create_notify_list(assignees = issue$assignees,
                                            issue_creator = issue$user$login
                                            )
  debug(.le$logger, glue::glue("Created notified users body"))

  debug(.le$logger, glue::glue("Creating message body..."))
  message_body <- create_message_body(message)
  debug(.le$logger, glue::glue("Created message body"))


  # get reference and comparator scripts if default
  if (reference_commit == "original" && comparator_commit == "current") {
    # reference = oldest
    debug(.le$logger, glue::glue("Getting reference commit..."))
    reference_commit <- get_init_qc_commit_from_issue_body(issue$body)
    debug(.le$logger, glue::glue("Got reference commit: {reference_commit}"))

    # comparator = newest
    debug(.le$logger, glue::glue("Getting comparator commit..."))
    comparator_commit <- get_commits_df(issue_number = issue$number)$commit[1]
    debug(.le$logger, glue::glue("Got comparator commit: {comparator_commit}"))
  }

  debug(.le$logger, glue::glue("Getting script contents..."))
  script_contents <- get_script_contents(reference_file_path = issue$title,
                                         comparator_file_path = issue$title,
                                         reference_commit = reference_commit,
                                         comparator_commit = comparator_commit
                                         )
  reference_script <- script_contents$reference_script
  comparator_script <- script_contents$comparator_script
  debug(.le$logger, glue::glue("Got script contents"))

  debug(.le$logger, glue::glue("Getting file difference body..."))
  diff_body <- create_diff_body(diff = diff,
                           reference_commit = reference_commit,
                           reference_script = reference_script,
                           comparator_commit = comparator_commit,
                           comparator_script = comparator_script)
  debug(.le$logger, glue::glue("Got file difference body"))

  debug(.le$logger, glue::glue("Getting metadata body..."))
  commit_comparison <- get_commit_comparison_html(reference_commit = reference_commit,
                                                 comparator_commit = comparator_commit
                                                 )

  metadata_body <- create_comment_metadata_body(reference_commit = reference_commit,
                                                comparator_commit = comparator_commit,
                                                commit_comparison = commit_comparison
                                                )
  debug(.le$logger, glue::glue("Got metadata body"))

  comment_body_first <- as.character(glue::glue("# QC Notification\n\n",
                                                "{notified_users_body}",
                                                "{message_body}",
                                                .trim = FALSE))

  comment_body_second <- as.character(glue::glue("{metadata_body}",
                                   "{diff_body}",
                                   .trim = FALSE))

  return(c(comment_body_first, comment_body_second))
}



create_hyperlink <- function(display_text, url) {
  glue::glue("<a href=\"{url}\" target=\"_blank\">{display_text}</a>")
}


create_approve_comment_body <- function(file_path, initial_qc_commit, approved_qc_commit, issue) {
  file_contents_url <- get_file_contents_url(file_path = file_path,
                                             git_sha = approved_qc_commit)

  file_contents_html <- create_hyperlink(display_text = "file contents at approved qc commit",
                                         url = file_contents_url
                                         )

  init_vs_approved_commit_diff_url <- get_commit_comparison_url(reference_commit = initial_qc_commit,
                                                                comparator_commit = approved_qc_commit
                                                                )

  init_vs_approved_commit_diff_html <- create_hyperlink(display_text = "initial qc commit vs. approved qc commit",
                                                        url = init_vs_approved_commit_diff_url
                                                        )

  metadata_body <- glue::glue("## Metadata\n",
                         "* approved qc commit: {approved_qc_commit}\n",
                         "* {file_contents_html}\n",
                         "* {init_vs_approved_commit_diff_html}\n",
                         .trim = FALSE
  )

  notified_users_body <- create_notify_list(assignees = issue$assignees,
                                            issue_creator = issue$user$login
                                            )

  comment_body_first <- as.character(glue::glue("# QC Approved\n\n",
                                                "{notified_users_body}\n\n",
                                                .trim = FALSE))

  comment_body_second <- as.character(glue::glue("{metadata_body}",
                                                 .trim = FALSE))

  return(c(comment_body_first, comment_body_second))
}

approve <- function(issue_number, body) {
  # step 1: post comment
  post_comment(issue_number, body)

  # step 2: close issue
  close_issue(issue_number)
}

unapprove <- function(issue_number, unapprove_comment_body, approve_comment) {
  # step 1: post unapprove comment
  post_comment(issue_number, unapprove_comment_body)

  # step 2: edit approve comment
  edit_approve_comment(approve_comment)

  # step 3: reopen issue
  open_issue(issue_number)
}


create_unapprove_comment_body <- function(approve_comment, issue) {
  approve_comment_url <- approve_comment$html_url
  unapproved_comment <- create_hyperlink(display_text = "previous approval comment",
                                         url = approve_comment_url)

  metadata_body <- glue::glue("## Metadata\n",
                              "* {unapproved_comment}\n",
                              .trim = FALSE)

  notified_users_body <- create_notify_list(assignees = issue$assignees,
                                            issue_creator = issue$user$login)

  comment_body_first <- as.character(glue::glue("# QC Unapproved\n\n",
                                                "{notified_users_body}\n\n",
                                                .trim = FALSE))

  comment_body_second <- as.character(glue::glue("{metadata_body}",
                                                 .trim = FALSE))

  return(c(comment_body_first, comment_body_second))
} # create_unapprove_comment_body

edit_approve_comment <- function(approve_comment) {
  comment_id <- approve_comment$id
  comment_body <- approve_comment$body

  # strikeout "# QC Approved" header
  new_comment_body <- stringr::str_replace(comment_body, "^# QC Approved", "# ~~QC Approved~~")

  # strikeout each metadata line starting with "* "
  new_comment_body <- stringr::str_replace_all(new_comment_body, "(?m)^\\* (.*)$", "* ~~\\1~~")

  gh::gh(
    "PATCH /repos/:org/:repo/issues/comments/:comment_id",
    org = .le$org,
    repo = .le$repo,
    comment_id = comment_id,
    body = new_comment_body
  )
} # edit_approve_comment


post_comment <- function(issue_number, body) {
  debug(.le$logger, glue::glue("Posting comment to Issue #{issue_number} in {.le$org}/{.le$repo}..."))

  comment <- gh::gh("POST /repos/:org/:repo/issues/:issue_number/comments",
                    .api_url = .le$github_api_url,
                    org = .le$org,
                    repo = .le$repo,
                    issue_number = issue_number,
                    body = body
  )

  info(.le$logger, glue::glue("Posted comment to Issue #{issue_number} in {.le$org}/{.le$repo}"))
}


