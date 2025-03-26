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
  metadata_branch <- get_branch_from_metadata(owner, repo, issue_number)
  remote_name <- remote$name

  remote_log_output <- system(glue::glue("git log {remote_name}/{metadata_branch} --pretty=format:'%H|%an|%ae|%ad|%s'  --date=format:'%Y-%m-%d %H:%M:%S'"), , intern = TRUE)
  remote_commit_log <- utils::read.csv(text = remote_log_output, sep = "|", header = FALSE, stringsAsFactors = FALSE)
  names(remote_commit_log) <- c("commit", "author_name", "author_email", "time", "message")

  remote_commits <- remote_commit_log %>%
    dplyr::distinct(.data$commit, .keep_all = TRUE) %>%
    dplyr::arrange(dplyr::desc(.data$time))

  if (!init_qc_commit %in% remote_commits$commit) {
    rlang::abort(glue::glue("git log does not contain initial qc commit ({init_qc_commit}) given in issue #{issue_number}"))
  }

  cutoff_position <- which(remote_commits$commit == init_qc_commit)

  commit_log <- remote_commits[1:cutoff_position, ]

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
