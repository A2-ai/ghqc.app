get_init_qc_commit <- function(owner, repo, issue_number) {
  issue <- get_issue(owner, repo, issue_number)
  init_commit <- get_metadata(issue$body)$`initial qc commit`
  if (is.null(init_commit)) {
    init_commit <- get_metadata(issue$body)$`git sha`
  }
  if (is.null(init_commit)) {
    init_commit <- get_metadata(issue$body)$`git_sha`
  }
  return(init_commit)
}

get_branch_from_metadata <- function(owner, repo, issue_number) {
  tryCatch({
    issue <- get_issue(owner, repo, issue_number)
    text <- get_metadata(issue$body)$`git branch`
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

create_assignees_list <- function(assignees) {
  sapply(assignees, function(assignee) glue::glue("@{assignee$login}"))
}

create_assignees_body <- function(assignees_list) {
  if (length(assignees_list) == 0) ""
  else {
    list <- glue::glue_collapse(assignees_list, sep = "\n")
    glue::glue("{list}\n\n\n")
  }
}

create_message_body <- function(message) {
  if (is.null(message)) ""
  else glue::glue("{message}\n\n\n")
}


create_comment_metadata_body <- function(file_path,
                                 reference_commit,
                                 reference_hash,
                                 comparator_commit,
                                 comparator_hash,
                                 owner,
                                 repo,
                                 remote_url) {


  ref_url <- get_file_contents_url(file_path = file_path,
                                   git_sha = reference_commit,
                                   owner = owner,
                                   repo = repo,
                                   remote_url = remote_url)

  comp_url <- get_file_contents_url(file_path = file_path,
                                    git_sha = comparator_commit,
                                    owner = owner,
                                    repo = repo,
                                    remote_url = remote_url)

  glue::glue("## Metadata\n",
             "* current commit: {comparator_commit}\n",
             "* current script md5 checksum: {comparator_hash}\n",
             "* file contents at current commit: {comp_url}\n&nbsp;\n",
             "* previous commit: {reference_commit}\n",
             "* previous script md5 checksum: {reference_hash}\n",
             "* file contents at previous commit: {ref_url}\n\n\n")
}

create_diff_body <- function(diff, reference_commit, reference_script, comparator_commit, comparator_script) {
  if (!diff) return("")

    diff_formatted <- format_diff(reference_script = reference_script, comparator_script = comparator_script)
    glue::glue("## File Difference\n\n",
               "{diff_formatted}\n\n")
}

create_comment_body <- function(owner,
                                repo,
                                issue_number,
                                message = NULL,
                                diff = FALSE,
                                reference_commit = "original",
                                comparator_commit = "current",
                                remote) {

  remote_url <- parse_remote_url(remote$url)

  issue <- get_issue(owner, repo, issue_number)
  ## check if file exists locally
  if (!fs::file_exists(issue$title)) {
    log4r::warn(.le$logger, glue::glue("{issue$title} does not exist in local project repo. You may want to change your branch to one in which the file exists."))
  }

  # log
  debug(.le$logger, glue::glue("Creating comment body for Issue #{issue_number}:{issue$title} in {owner}/{repo}"))

  debug(.le$logger, glue::glue("Creating assignees body..."))
  assignees_list <- create_assignees_list(issue$assignees)
  assignees_body <- create_assignees_body(assignees_list)
  debug(.le$logger, glue::glue("Created assignees body"))

  debug(.le$logger, glue::glue("Creating message body..."))
  message_body <- create_message_body(message)
  debug(.le$logger, glue::glue("Created message body"))


  # get reference and comparator scripts if default
  if (reference_commit == "original" && comparator_commit == "current") {
    # reference = oldest
    debug(.le$logger, glue::glue("Getting reference commit..."))
    reference_commit <- get_init_qc_commit(owner, repo, issue_number)
    debug(.le$logger, glue::glue("Got reference commit: {reference_commit}"))

    # comparator = newest
    debug(.le$logger, glue::glue("Getting comparator commit..."))
    comparator_commit <- get_commits_df(issue_number = issue_number, owner = owner, repo = repo, remote = remote)$commit[1]
    debug(.le$logger, glue::glue("Got comparator commit: {comparator_commit}"))
  }

  debug(.le$logger, glue::glue("Getting script contents..."))
  script_contents <- get_script_contents(issue$title, reference = reference_commit, comparator = comparator_commit)
  reference_script <- script_contents$reference_script
  comparator_script <- script_contents$comparator_script
  debug(.le$logger, glue::glue("Got script contents"))

  reference_hash <- script_contents$hash_at_reference
  comparator_hash <- script_contents$hash_at_comparator

  debug(.le$logger, glue::glue("Getting file difference body..."))
  diff_body <- create_diff_body(diff = diff,
                           reference_commit = reference_commit,
                           reference_script = reference_script,
                           comparator_commit = comparator_commit,
                           comparator_script = comparator_script)
  debug(.le$logger, glue::glue("Got file difference body"))

  debug(.le$logger, glue::glue("Getting metadata body..."))
  metadata_body <- create_comment_metadata_body(file_path = issue$title,
                                        reference_commit = reference_commit,
                                        reference_hash = reference_hash,
                                        comparator_commit = comparator_commit,
                                        comparator_hash = comparator_hash,
                                        owner = owner,
                                        repo = repo,
                                        remote_url = remote_url)
  debug(.le$logger, glue::glue("Got metadata body"))

  comment_body <- glue::glue("{assignees_body}",
                             "{message_body}",
                             "{metadata_body}",
                             "{diff_body}",
                             .trim = FALSE)

  # log
  log_assignees <- if (length(assignees_list) == 0) "None" else paste(assignees_list, collapse = ', ')

  info(.le$logger, glue::glue("Created comment body for issue #{issue_number} in {owner}/{repo} with
                              Assignee(s):     {log_assignees}
                              Previous commit: {reference_commit}
                              Original commit: {comparator_commit}"))

  as.character(comment_body)
}


post_resolve_comment <- function(owner, repo, issue_number, body) {
  debug(.le$logger, glue::glue("Posting comment to issue #{issue_number} in {owner}/{repo}..."))

  comment <- gh::gh("POST /repos/:owner/:repo/issues/:issue_number/comments",
                    .api_url = .le$github_api_url,
                    owner = owner,
                    repo = repo,
                    issue_number = issue_number,
                    body = body
  )

  info(.le$logger, glue::glue("Posted comment to Issue #{issue_number} in {owner}/{repo}"))
}


