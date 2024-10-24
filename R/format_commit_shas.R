# get list of all shas
# make sure sorted chronologically
# get up to point of sha at initial QC commit
# get commit messages
# get dates
# add numbering to df for easy comparison of chronologicallness
# add "second most recent commit", "most recent commit" and "original qc commit" identifiers
# format in table

get_commits_df <- function(issue_number, owner = get_organization(), repo = get_current_repo()) {
  init_qc_commit <- get_init_qc_commit(owner, repo, issue_number)

  all_commits <- gert::git_log()

  cutoff_position <- which(all_commits$commit == init_qc_commit)

  commit_log <- all_commits[1:cutoff_position, ]

  columns <- c("commit", "message", "date", "time", "short_sha", "display")
  commit_log <- commit_log %>%
    dplyr::mutate(
      date = stringr::str_extract(commit_log[[columns[[4]]]], "^[\\w'-]+"),
      message = stringr::str_remove_all(commit_log[[columns[[2]]]], "\n"),  # remove /n from message
      short_sha = stringr::str_extract(commit_log[[columns[[1]]]], "^.{1,7}"))

  commit_log <- commit_log %>% dplyr::mutate(
      display = glue::glue("{message} | {short_sha}")
    )

  commit_log <- commit_log %>%
    dplyr::select(columns[[3]], columns[[1]], columns[[6]])


  #last_row <- nrow(commit_log)
  # commit_log$display[last_row] <- glue::glue("{commit_log$display[last_row]}") # \n(initial qc commit)
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
