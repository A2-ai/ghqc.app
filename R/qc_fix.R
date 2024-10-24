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

get_script_hash <- function(script) {
  collapsed_script <- glue::glue_collapse(script, "\n")
  digest::digest(collapsed_script, algo = "md5")
}

create_metadata_body <- function(reference_commit,
                                 reference_script,
                                 comparator_commit,
                                 comparator_script) {

  # get script hashes
  reference_script_hash <- get_script_hash(reference_script)
  comparator_script_hash <- get_script_hash(comparator_script)

  glue::glue("## Metadata\n",
             "* current commit: {comparator_commit}\n",
             "* current script md5 checksum: {comparator_script_hash}\n",
             "* previous commit: {reference_commit}\n",
             "* previous script md5 checksum: {reference_script_hash}\n\n\n")
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
                                comparator_commit = "current") {

  issue <- get_issue(owner, repo, issue_number)

  # log
  debug(.le$logger, glue::glue("Creating comment body for Issue #{issue_number} in {owner}/{repo}"))

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
    comparator_commit <- gert::git_log(max = 1)$commit
    debug(.le$logger, glue::glue("Got comparator commit: {comparator_commit}"))
  }


  debug(.le$logger, glue::glue("Getting script contents..."))
  script_contents <- get_script_contents(issue$title, reference = reference_commit, comparator = comparator_commit)
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
  metadata_body <- create_metadata_body(reference_commit = reference_commit,
                                        reference_script = reference_script,
                                        comparator_commit = comparator_commit,
                                        comparator_script = comparator_script)
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


post_comment <- function(owner, repo, issue_number, body) {
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

add_fix_comment <- function(owner,
                            repo,
                            issue_number,
                            message = NULL,
                            diff = FALSE,
                            reference_commit = "original",
                            comparator_commit = "current") {

  body <- create_comment_body(owner,
                              repo,
                              issue_number,
                              message,
                              diff,
                              reference_commit = reference_commit,
                              comparator_commit = comparator_commit)

  post_comment(owner, repo, issue_number, body)
}
