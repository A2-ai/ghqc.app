#' @import dplyr purrr
#' @importFrom log4r info debug warn error
#' @importFrom rlang .data
ghqc_status <- function(milestone_objects,
                        current_branch,
                        local_commits,
                        remote_commits,
                        ahead_behind_status,
                        files_changed_in_remote_commits,
                        files_changed_in_unpushed_local_commits,
                        files_with_uncommitted_local_changes
                        ) {
  total_start_time <- Sys.time()

  all_relevant_files <- list()

  issue_objects <- list()

  status_df <- map_df(c(milestone_objects), function(milestone_object) {
    milestone_name <- milestone_object$title
    milestone_number <- milestone_object$number
    issues <- get_all_issues_in_milestone_from_milestone_number(milestone_number, milestone_name)

    if (length(issues) == 0) {
      return(empty_tibble())
    }

    files <- purrr::map_chr(issues, "title")
    debug(.le$logger, glue::glue("Retrieving all git statuses..."))
    start_time_git <- Sys.time()
    git_statuses <- get_git_statuses(files = files,
                                     local_commits = local_commits,
                                     remote_commits = remote_commits,
                                     ahead_behind_status = ahead_behind_status,
                                     files_changed_in_remote_commits = files_changed_in_remote_commits,
                                     files_changed_in_unpushed_local_commits = files_changed_in_unpushed_local_commits,
                                     files_with_uncommitted_local_changes = files_with_uncommitted_local_changes
                                     )
    end_time_git <- Sys.time()
    elapsed_git <- round(as.numeric(difftime(end_time_git, start_time_git, units = "secs")), 3)
    debug(.le$logger, glue::glue("Retrieved all git statuses in {elapsed_git} seconds"))

    milestone_url <- issues[[1]]$milestone$html_url
    milestone_with_url <- glue::glue('<a href="{milestone_url}" target="_blank">{milestone_name}</a>')

    issues_df <- map_df(issues, function(issue) {
      issue_objects[[milestone_name]][[issue$title]] <<- issue

      # get column values for file
      file_name <- issue$title
      issue_number <- issue$number
      issue_state <- ifelse(issue$state == "open", "Open", "Closed") # capitalize Open and Closed
      qcer <- ifelse(!is.null(issue$assignee$login), issue$assignee$login, "No QCer")
      file_url <- issue$html_url
      file_with_url <- glue::glue('<a href="{file_url}" target="_blank">{file_name}</a>')
      issue_body <- issue$body

      # latest_qc_commit is the most recent commented commit in file's issue
      initial_qc_commit <- get_init_qc_commit_from_issue_body(issue_body)
      qc_commit_info <- get_qc_commit_info(file_name = file_name,
                                                         issue_body = issue_body,
                                                         num_comments = issue$comments,
                                                         comments_url = issue$comments_url,
                                                         initial_qc_commit = initial_qc_commit
                                                         )


      latest_qc_commit <- qc_commit_info$latest_qc_commit
      previous_qc_commit <- qc_commit_info$previous_qc_commit
      approve_comment <- qc_commit_info$approve_comment
      qc_approved <- qc_commit_info$qc_approved
      debug(.le$logger, glue::glue("Retrieved last QC commit for {file_name}: {latest_qc_commit}"))

      # branch from metadata might be different from current branch
      qc_branch <- get_branch_from_issue_body(issue_body)
      # if it is different, don't get git status
      if (qc_branch != current_branch) {

        # QC BRANCH NOT DELETED
        if (!check_remote_branch_deleted(qc_branch)) {
          git_status <- glue::glue("View on QC branch: <em>{qc_branch}</em>") # if not on the qc branch, then getting the git status doesn't make sense
          remote_ref <- get_remote_ref_for_branch(qc_branch)
          remote_commits_on_qc_branch <- get_remote_commits_full_name(remote_ref)
          qc_status_info <- get_file_qc_status_non_local_qc_branch(file_name,
                                                                   issue_state,
                                                                   remote_commits_on_qc_branch,
                                                                   latest_qc_commit,
                                                                   qc_approved)

          qc_status <- qc_status_info$qc_status
          diagnostics <- qc_status_info$diagnostics
        } # remote branch not deleted

        else { # else remote branch has been deleted
          merged_into <- find_merged_into(initial_qc_commit) #  needs to be initial qc commit in case current qc commit is from the merged_in branch
          if (!is.null(merged_into)) {
            git_status <- glue::glue("Deleted QC branch: <em>{qc_branch}</em>{vspace()}
                                     Merged into: <em>{merged_into}</em>"
                                     )
          }
          else {
            git_status <- glue::glue("Deleted QC branch: <em>{qc_branch}</em>")
          }

          latest_qc_commit_short <- get_hyperlinked_commit(latest_qc_commit, file_name)

          if (qc_approved) {
            qc_status <- "Approved"
            diagnostics <- format_diagnostics_list(list(glue::glue("Approved QC commit: {latest_qc_commit_short}")))

          }
          else {
            qc_status <- "QC branch deleted before approval"
            diagnostics <- format_diagnostics_list(list(glue::glue("Last posted commit: {latest_qc_commit_short}")))
          }
        } # else remote branch has been deleted

        # must be on the QC branch to perform operations
        comparator_commit <- NA_character_

        action <- get_action_column(qc_status, diagnostics, git_status, latest_qc_commit, comparator_commit, initial_qc_commit)

        return(
          dplyr::tibble(
            milestone_name = milestone_name,
            milestone_with_url = milestone_with_url,
            file_name = file_name,
            file_with_url = file_with_url,
            issue_state = issue_state,
            qc_status = qc_status,
            git_status = git_status,
            diagnostics = diagnostics,
            issue_number = issue_number,
            initial_qc_commit = initial_qc_commit,
            latest_qc_commit = latest_qc_commit,
            previous_qc_commit = previous_qc_commit,
            comparator_commit = comparator_commit,
            approve_comment = approve_comment,
            issue_url = file_url,
            action = action,
            qcer = qcer,
          )
        )
      } # qc_branch != current_branch

      # git status
      debug(.le$logger, glue::glue("Retrieving git status for {file_name}..."))
      start_time <-  Sys.time()

      git_status <- git_statuses[which(git_statuses$file_name == file_name), ]$git_status

      end_time <- Sys.time()
      elapsed <- round(as.numeric(difftime(end_time, start_time, units = "secs")), 3)
      debug(.le$logger, glue::glue("Retrieved git status for {file_name} in {elapsed} seconds"))

      # qc status
      qc_status_info <- tryCatch({
        debug(.le$logger, glue::glue("Retrieving QC status info for {file_name}..."))
        start_time <-  Sys.time()
        qc_status_res <- get_file_qc_status(file = file_name,
                                  issue_state = issue_state,
                                  git_status = git_status,
                                  local_commits = local_commits,
                                  remote_commits = remote_commits,
                                  latest_qc_commit = latest_qc_commit,
                                  initial_qc_commit = initial_qc_commit,
                                  qc_approved = qc_approved
                                  )

        end_time <- Sys.time()
        elapsed <- round(as.numeric(difftime(end_time, start_time, units = "secs")), 3)
        debug(.le$logger, glue::glue("Retrieved QC status info for {file_name} in {elapsed} seconds"))

        qc_status_res
      }, error = function(e) {
        list(
          qc_status = "Error",
          diagnostics = conditionMessage(e)
        )
      }) # qc status

      qc_status <- qc_status_info$qc_status
      diagnostics <- qc_status_info$diagnostics

      # update relevant files list
      relevant_files_in_issue <- get_relevant_files(issue, milestone_name)
      all_relevant_files <<- dplyr::bind_rows(all_relevant_files, relevant_files_in_issue)
      debug(.le$logger, glue::glue("Updated relevant files list"))

      # comparator commit is the last remote commit
      # this is safer than just giving the last commit in which the file changed -
      # why not just get the whole repo at its present state?
      comparator_commit <- remote_commits[1]

      action <- get_action_column(qc_status, diagnostics, git_status, latest_qc_commit, comparator_commit, initial_qc_commit)

      # return res
      res <- dplyr::tibble(
        milestone_name = milestone_name,
        milestone_with_url = milestone_with_url,
        file_name = file_name,
        file_with_url = file_with_url,
        issue_state = issue_state,
        qc_status = qc_status,
        git_status = git_status,
        diagnostics = diagnostics,
        issue_number = issue_number,
        initial_qc_commit = initial_qc_commit,
        latest_qc_commit = latest_qc_commit,
        previous_qc_commit = previous_qc_commit,
        comparator_commit = comparator_commit,
        approve_comment = approve_comment,
        issue_url = file_url,
        action = action,
        qcer = qcer,
      ) # tibble

      info(.le$logger, glue::glue("Retrieved QC status for {file_name}"))

      return(res)
    }) # issues_df
  }) # status_df

  # rename columns
  colnames(status_df) <- c("milestone_name",
                           "Milestone",
                           "file_name",
                           "File",
                           "Issue State",
                           "QC Status",
                           "Git Status",
                           "Diagnostics",
                           "issue_number",
                           "initial_qc_commit",
                           "latest_qc_commit",
                           "previous_qc_commit",
                           "comparator_commit",
                           "approve_comment",
                           "issue_url",
                           "Action",
                           "QCer")
  # make factors
  status_df <- status_df %>%
    dplyr::mutate(across(
      c(.data$`Issue State`, .data$`QC Status`, .data$`Git Status`, .data$QCer),
      as.factor
    ))

  res <- list(
    status = status_df,
    relevant_files = all_relevant_files,
    issue_objects = issue_objects
  )

  total_time <- difftime(Sys.time(), total_start_time, units = "secs")
  debug(.le$logger, glue::glue("Total ghqc_status time: {total_time} seconds ({total_time/60} minutes)"))
  return(res)
} # ghqc_status


create_relevant_files_df <- function(all_relevant_files,
                                     local_commits,
                                     remote_commits,
                                     ahead_behind_status,
                                     files_changed_in_remote_commits,
                                     files_changed_in_unpushed_local_commits,
                                     files_with_uncommitted_local_changes) {

  unique_files <- unique(all_relevant_files$relevant_file_name)
  if (length(unique_files) == 0) return(dplyr::tibble())

  git_statuses <- get_git_statuses(files = unique_files,
                                   local_commits = local_commits,
                                   remote_commits = remote_commits,
                                   ahead_behind_status = ahead_behind_status,
                                   files_changed_in_remote_commits = files_changed_in_remote_commits,
                                   files_changed_in_unpushed_local_commits = files_changed_in_unpushed_local_commits,
                                   files_with_uncommitted_local_changes = files_with_uncommitted_local_changes
  )

  map_df(unique_files, function(file) {
    instances <- all_relevant_files[all_relevant_files$relevant_file_name == file, ]
    diagnostics <- format_diagnostics_list(
      lapply(seq_len(nrow(instances)), function(i) {
        row <- instances[i, ]
        glue::glue("#{row$issue_number}: {row$qc_file_name} ({row$milestone_name})")
      })
    )

    git_status <- git_statuses[git_statuses$file_name == file, ]$git_status

    dplyr::tibble(
      milestone_name = "No Milestone",
      Milestone = "No Milestone",
      file_name = file,
      File = file,
      `Issue State` = "No Issue",
      `QC Status` = "Relevant file",
      `Git Status` = git_status,
      Diagnostics = glue::glue("Relevant file in Issues:<br>{diagnostics}"),
      issue_number = NA_character_,
      initial_qc_commit = NA_character_,
      latest_qc_commit = NA_character_,
      previous_qc_commit = NA_character_,
      comparator_commit = NA_character_,
      approve_comment = NA_character_,
      issue_url = NA_character_,
      Action = list(options = character(0)),
      QCer = NA_character_
    )
  })
} # create_relevant_files_df

create_non_issue_repo_files_df <- function(files_with_issues,
                                           all_relevant_files,
                                           selected_dirs,
                                           local_commits,
                                           remote_commits,
                                           ahead_behind_status,
                                           files_changed_in_remote_commits,
                                           files_changed_in_unpushed_local_commits,
                                           files_with_uncommitted_local_changes) {

  files_with_issues <- unique(files_with_issues)
  relevant_files <- unique(all_relevant_files$relevant_file_name)

  repo_files <- gert::git_ls()$path
  files_without_issues_or_relevance <- setdiff(repo_files, union(files_with_issues, relevant_files))

  # Filter to selected directories
  files_in_dirs <- files_without_issues_or_relevance[dirname(files_without_issues_or_relevance) %in% selected_dirs]

  if (length(files_in_dirs) == 0) return(dplyr::tibble())

  git_statuses <- get_git_statuses(files = files_in_dirs,
                                  local_commits = local_commits,
                                  remote_commits = remote_commits,
                                  ahead_behind_status = ahead_behind_status,
                                  files_changed_in_remote_commits = files_changed_in_remote_commits,
                                  files_changed_in_unpushed_local_commits = files_changed_in_unpushed_local_commits,
                                  files_with_uncommitted_local_changes = files_with_uncommitted_local_changes)

  map_df(files_in_dirs, function(file) {
    git_status <- git_statuses[git_statuses$file_name == file, ]$git_status

    dplyr::tibble(
      milestone_name = "No Milestone",
      Milestone = "No Milestone",
      file_name = file,
      File = file,
      `Issue State` = "No Issue",
      `QC Status` = NA_character_,
      `Git Status` = git_status,
      Diagnostics = NA_character_,
      issue_number = NA_character_,
      initial_qc_commit = NA_character_,
      latest_qc_commit = NA_character_,
      previous_qc_commit = NA_character_,
      comparator_commit = NA_character_,
      approve_comment = NA_character_,
      issue_url = NA_character_,
      Action = list(options = character(0)),
      QCer = NA_character_
    )
  })
}

