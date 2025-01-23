# get list of all shas
# make sure sorted chronologically
# get up to point of sha at initial QC commit
# get commit messages
# get dates
# add numbering to df for easy comparison of chronologicallness
# add "second most recent commit", "most recent commit" and "original qc commit" identifiers
# format in table

#' @importFrom rlang .data
get_commits_df <- function(issue_number, owner, repo, remote) {

  init_qc_commit <- get_init_qc_commit(owner, repo, issue_number)
  branch <- get_branch_from_metadata(owner, repo, issue_number)

  # get commits on branch
  local_log <- gert::git_log(glue::glue("{branch}"))
  remote_log <- gert::git_log(glue::glue("{remote$name}/{branch}"))
  all_commits <- dplyr::bind_rows(local_log, remote_log) %>%
    dplyr::distinct(.data$commit, .keep_all = TRUE) %>%
    dplyr::arrange(dplyr::desc(.data$time))

  if (!init_qc_commit %in% all_commits$commit) {
    rlang::abort(glue::glue("git log does not contain initial qc commit ({init_qc_commit}) given in issue #{issue_number}"))
  }

  cutoff_position <- which(all_commits$commit == init_qc_commit)

  commit_log <- all_commits[1:cutoff_position, ]

  commit_log <- commit_log %>%
    dplyr::mutate(
      date = stringr::str_extract(.data$time, "^[\\w'-]+"),
      message = stringr::str_remove_all(.data$message, "\n"),  # remove /n from message
      short_sha = stringr::str_extract(.data$commit, "^.{1,7}")
    )

  commit_log <- commit_log %>% dplyr::mutate(
      display = glue::glue("{message} | {short_sha}")
    )

  commit_log <- commit_log %>%
    dplyr::select(.data$date, .data$commit, .data$display)

  commit_log
}

get_reference_df <- function(commits_df) {
  # remove first row, the most recent commit (because there's nothing older to compare it to)
  ref_df <- commits_df[-1, ]
  # label new first row as recond most recent commit
   # ref_df$display[1] <- glue::glue("{ref_df$display[1]}") # \n(second most recent commit)

  ref_df
}

get_comparator_df <- function(commits_df, selected_reference_commit) {
  # selected_reference_display <- stringr::str_remove(selected_reference_display, "\\\n\\(second most recent commit\\)")
  # commits_df$display[1] <- glue::glue("{commits_df$display[1]}") # \n(most recent commit)
  # next need to cut off at selected reference commits
  cutoff_position <- which(commits_df$commit == selected_reference_commit)

  # - 1 to not include the selected commit
  comp_df <- commits_df[1:cutoff_position - 1, ]

  comp_df
}
