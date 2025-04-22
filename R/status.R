#' @import dplyr purrr
#' @importFrom log4r info debug warn error
#' @importFrom rlang .data
ghqc_status <- function(milestone_names,
                        org,
                        repo,
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

  status_df <- map_df(milestone_names, function(milestone_name) {
    issues <- get_all_issues_in_milestone(org, repo, milestone_name)
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
      # get column values for file
      file_name <- issue$title
      issue_number <- issue$number
      issue_state <- ifelse(issue$state == "open", "Open", "Closed") # capitalize Open and Closed
      qcer <- ifelse(!is.null(issue$assignee$login), issue$assignee$login, "No QCer")
      file_url <- issue$html_url
      file_with_url <- glue::glue('<a href="{file_url}" target="_blank">{file_name}</a>')
      repo_url <- stringr::str_extract(file_url, ".*(?=/issues)")
      issue_body <- issue$body

      # latest_qc_commit is the most recent commented commit in file's issue
      init_qc_commit <- get_init_qc_commit_from_issue_body(issue_body)
      latest_qc_commit_info <- get_latest_qc_commit(file_name = file_name,
                                               issue_body = issue_body,
                                               num_comments = issue$comments,
                                               comments_url = issue$comments_url,
                                               init_qc_commit = init_qc_commit
                                               )
      latest_qc_commit <- latest_qc_commit_info$latest_qc_commit
      qc_approved <- latest_qc_commit_info$qc_approved
      debug(.le$logger, glue::glue("Retrieved last QC commit for {file_name}: {latest_qc_commit}"))

      # branch from metadata might be different from current branch
      metadata_branch <- get_branch_from_issue_body(issue_body)
      # if it is different, don't get git status
      if (metadata_branch != current_branch) {
        # if not on the qc branch, then getting the git status doesn't make sense
        git_status <- NA_character_

        # QC BRANCH NOT DELETED
        if (!check_remote_branch_deleted(metadata_branch)) {
          qc_status <- "QC Status not available"
          comparator_commit <- NA_character_
          notify <- FALSE
          approve <- FALSE

          diagnostics_list <- format_diagnostics_list(list(
            glue::glue("Current branch: {current_branch}"),
            glue::glue("QC branch: {metadata_branch}")
          ))
          diagnostics <- glue::glue("Switch to QC branch to view status.<br>{diagnostics_list}")

        } # remote branch not deleted

        else { # else remote branch has been deleted
          latest_qc_commit_short <- get_hyperlinked_commit(latest_qc_commit, file_name, repo_url)
          diagnostics_items <- list(
            glue::glue("QC branch: {metadata_branch}"),
            glue::glue("Approved QC commit: {latest_qc_commit_short}") # TODO
          )

          merged_into <- find_merged_into(init_qc_commit) #  needs to be initial qc commit in case current qc commit is from the merged_in branch

          # QC BRANCH MERGED AND DELETED
          if (!is.null(merged_into)) {
            qc_status <- glue::glue("QC branch merged to {merged_into}")
            comparator_commit <- get_remote_commits_full_name(merged_into)[1] # comparator commit will be the latest remote commit on the merged_into branch


            # see if file changed after latest qc commit
            last_commit_that_changed_file <- last_commit_that_changed_file_after_latest_qc_commit(file_name, latest_qc_commit, comparator_commit)$last_commit_that_changed_file
            if (!is.null(last_commit_that_changed_file)) {
              last_commit_that_changed_file_short <- get_hyperlinked_commit(last_commit_that_changed_file, file_name, repo_url)
              commit_diff_url <- get_hyperlinked_commit_diff(repo_url,
                                                             old_commit = latest_qc_commit,
                                                             new_commit = last_commit_that_changed_file)
              diagnostics_items <- append(diagnostics_items,
                                          c(glue::glue("Last file change: {last_commit_that_changed_file_short}"),
                                            commit_diff_url)
                                          )
              notify <- get_notify_column(qc_status = "File changes since QC branch merged and deleted",
                                          git_status,
                                          latest_qc_commit,
                                          comparator_commit)
            } # if file changed after latest qc commit
            else {
              notify <- get_notify_column(qc_status = "No file changes since QC branch merged and deleted",
                                          git_status,
                                          latest_qc_commit,
                                          comparator_commit)
            }
          } # if merged_into

          # QC BRANCH NOT MERGED AND DELETED
          else {
            qc_status <- "QC branch deleted"
            comparator_commit <- NA_character_
            notify <- FALSE
          }

          diagnostics <- format_diagnostics_list(diagnostics_items)
        } # else remote branch has been deleted

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
            latest_qc_commit = latest_qc_commit,
            comparator_commit = comparator_commit,
            issue_url = file_url,
            notify = notify,
            approve = approve,
            qcer = qcer,
          )
        )

      } # metadata_branch != current_branch

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
                                  repo_url = repo_url,
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
      notify <- get_notify_column(qc_status, git_status, latest_qc_commit, comparator_commit)
      approve <- get_approve_column(qc_status, git_status)

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
        latest_qc_commit = latest_qc_commit,
        comparator_commit = comparator_commit,
        issue_url = file_url,
        notify = notify,
        approve = approve,
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
                           "latest_qc_commit",
                           "comparator_commit",
                           "issue_url",
                           "Notify",
                           "Approve",
                           "QCer")
  # make factors
  status_df <- status_df %>%
    dplyr::mutate(across(
      c(.data$`Issue State`, .data$`QC Status`, .data$`Git Status`, .data$QCer),
      as.factor
    ))

  res <- list(
    status = status_df,
    relevant_files = all_relevant_files
  )

  total_time <- difftime(Sys.time(), total_start_time, units = "secs")
  debug(.le$logger, glue::glue("Total ghqc_status time: {total_time} seconds ({total_time/60} minutes)"))
  return(res)
} # ghqc_status



create_non_issue_repo_files_df <- function(files_with_issues,
                                           local_commits,
                                           remote_commits,
                                           all_relevant_files,
                                           selected_dirs,
                                           ahead_behind_status,
                                           files_changed_in_remote_commits,
                                           files_changed_in_unpushed_local_commits,
                                           files_with_uncommitted_local_changes) {

  files_with_issues <- unique(files_with_issues)

  # add rest of repo files, determine whether they're relevant files or not
  git_files <- gert::git_ls()$path
  #files_in_repo <- git_files[!stringr::str_detect(git_files, "^\\.") & !stringr::str_detect(git_files, "\\.Rproj$")]
  files_without_issues <- git_files[!git_files %in% files_with_issues]
  files_in_selected_dirs <- files_without_issues[dirname(files_without_issues) %in% selected_dirs]
  if (length(files_in_selected_dirs) == 0) {
    return(character(0))
  }

  git_statuses <- get_git_statuses(files = files_in_selected_dirs,
                                   local_commits = local_commits,
                                   remote_commits = remote_commits,
                                   ahead_behind_status = ahead_behind_status,
                                   files_changed_in_remote_commits = files_changed_in_remote_commits,
                                   files_changed_in_unpushed_local_commits = files_changed_in_unpushed_local_commits,
                                   files_with_uncommitted_local_changes = files_with_uncommitted_local_changes
  )


  repo_files_df <- map_df(files_in_selected_dirs, function(file) {
    debug(.le$logger, glue::glue("Retrieving git status for {file}..."))
    start_time <- Sys.time()
    git_status <- git_statuses[which(git_statuses$file_name == file), ]$git_status
    debug(.le$logger, glue::glue("Retrieved git status for {file} in {difftime(Sys.time(), start_time)} seconds"))

    qc_status_info <- {
      if (file %in% all_relevant_files$relevant_file_name) {
        # get rows in which file is the relevant file
        relevant_file_instances <- all_relevant_files[which(all_relevant_files$relevant_file_name == file), ]
        # loop over rows
        qc_file_strings <- lapply(split(relevant_file_instances, seq_len(nrow(relevant_file_instances))), function(relevant_file_instance) {
          qc_file <- relevant_file_instance$qc_file_name
          issue_number <- relevant_file_instance$issue_number
          milestone_name <- relevant_file_instance$milestone_name
          qc_file_string <- glue::glue("#{issue_number}: {qc_file} ({milestone_name})")
          return(qc_file_string)
        })

        qc_status_string <- format_diagnostics_list(qc_file_strings)

        list(qc_status = "Relevant file",
             diagnostics = glue::glue("Relevant file in Issues:<br>
                                      {qc_status_string}")
        )
      }
      else {
        list(qc_status = NA_character_,
             diagnostics = NA_character_)
      }
    } # qc_status_info

    dplyr::tibble(
      milestone_name = "No Milestone",
      Milestone = "No Milestone",
      file_name = file,
      File = file,
      `Issue State` = "No Issue",
      `QC Status` = qc_status_info$qc_status,
      `Git Status` = git_status,
      Diagnostics = qc_status_info$diagnostics,
      issue_number = NA_character_,
      latest_qc_commit = NA_character_,
      comparator_commit = NA_character_,
      issue_url = NA_character_,
      Notify = FALSE,
      `Approve` = FALSE,
      QCer = NA_character_,
    )
  })
} # create_non_issue_repo_files_df
