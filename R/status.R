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
      latest_qc_commit <- get_latest_qc_commit(issue_body = issue_body,
                                               num_comments = issue$comments,
                                               comments_url = issue$comments_url,
                                               init_qc_commit = init_qc_commit
                                               )
      debug(.le$logger, glue::glue("Retrieved current QC commit for {file_name}: {latest_qc_commit}"))

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
          okay_to_comment <- FALSE

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
            glue::glue("Final QC commit: {latest_qc_commit_short}")
          )

          merged_into <- find_merged_into(init_qc_commit) #  needs to be initial qc commit in case current qc commit is from the merged_in branch

          # QC BRANCH MERGED AND DELETED
          if (!is.null(merged_into)) {
            qc_status <- glue::glue("QC branch merged to {merged_into}")
            comparator_commit <- get_remote_commits_full_name(merged_into)[1] # comparator commit will be the latest remote commit on the merged_into branch
            okay_to_comment <- get_okay_to_comment_column(qc_status, git_status, latest_qc_commit, comparator_commit)

            # see if file changed after latest qc commit
            last_commit_that_changed_file <- last_commit_that_changed_file_after_latest_qc_commit(file_name, latest_qc_commit, comparator_commit)$last_commit_that_changed_file
            if (!is.null(last_commit_that_changed_file)) {
              last_commit_that_changed_file_short <- get_hyperlinked_commit(last_commit_that_changed_file, file_name, repo_url)
              commit_diff_url <- get_hyperlinked_commit_diff(repo_url, latest_qc_commit, last_commit_that_changed_file)
              diagnostics_items <- append(diagnostics_items,
                                          c(glue::glue("Last file change: {last_commit_that_changed_file_short}"),
                                            commit_diff_url)
                                          )
            } # if file changed after latest qc commit
          } # if merged_into

          # QC BRANCH NOT MERGED AND DELETED
          else {
            qc_status <- "QC branch deleted"
            comparator_commit <- NA_character_
            okay_to_comment <- FALSE
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
            okay_to_comment = okay_to_comment,
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
                                  issue_closed_at = issue$closed_at,
                                  repo_url = repo_url
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
      okay_to_comment <- get_okay_to_comment_column(qc_status, git_status, latest_qc_commit, comparator_commit)

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
        okay_to_comment = okay_to_comment,
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
      QCer = NA_character_,
    )
  })
} # create_non_issue_repo_files_df




get_imageless_comments <- function(comments_url) {
  comments <- gh::gh(comments_url, .api_url = .le$github_api_url)
  comments_df <- do.call(rbind, lapply(comments, function(x) as.data.frame(t(unlist(x)), stringsAsFactors = FALSE)))
}

get_latest_qc_commit <- function(issue_body, num_comments, comments_url, init_qc_commit) {
  # start by initializing latest_qc_commit as init qc commit
  latest_qc_commit <- init_qc_commit

  if (num_comments == 0) {
    return(latest_qc_commit)
  }

  comments <- get_imageless_comments(comments_url)$body

  # start from latest comment, check if a resolve comment (i.e. if it has metadata)
  # if it does, get the current commit, then break
  for (comment in rev(comments)) {
    comment_metadata <- get_comment_metadata(comment)
    if (length(comment_metadata) > 0) {
      latest_qc_commit <- comment_metadata$`current commit`
      break
    }
  }

  return(latest_qc_commit)
}

get_comment_metadata <- function(body) {
  metadata_section <- stringr::str_match(body, "(?s)## Metadata(.*)")[2]
  metadata_lines <- stringr::str_trim(unlist(strsplit(metadata_section, "\n")))

  metadata <- list()

  if (!is.na(metadata_section)) {
    for (line in metadata_lines) {
      if (stringr::str_detect(line, "^[*-]")) {
        key_value <- stringr::str_match(line, "[*-]\\s*(.*?):\\s*(.*)")[2:3]
        metadata[[key_value[1]]] <- key_value[2]
      }
    }
  } # if any metadata
  return(metadata)
}

get_hyperlinked_commit <- function(long_commit, file, repo_url) {
  short_commit <- substr(long_commit, 1, 7)
  sha_url <- file.path(repo_url, "blob", long_commit, file)
  hyperlinked_commit <- glue::glue('<a href="{sha_url}" target="_blank">{short_commit}</a>')
  return(hyperlinked_commit)
}

get_hyperlinked_commit_diff <- function(repo_url, old_commit, new_commit) {
  commit_diff_url <- file.path(repo_url, "compare", glue::glue("{old_commit}..{new_commit}"))
  hyperlinked_commit_diff <- glue::glue('<a href="{commit_diff_url}" target="_blank">Commit difference</a>')
  return(hyperlinked_commit_diff)
}

get_file_qc_status <- function(file,
                               issue_state,
                               git_status,
                               local_commits,
                               remote_commits,
                               latest_qc_commit,
                               issue_closed_at,
                               repo_url) {

  latest_qc_commit_short <- get_hyperlinked_commit(latest_qc_commit, file, repo_url)

  ## For open issues
  if (issue_state == "Open") {

    ### QC notification posted - this is not the same as git_status == "Remote file changes"
    if (!latest_qc_commit %in% local_commits) {  # if local commit is older than the latest_qc_commit (even if it didn't change the file - "No file difference" is possible)
      latest_local_commit <- local_commits[1]
      local_commit_pushed <- latest_local_commit %in% remote_commits

      # give hyperlink if latest local commit is pushed to remote
      local_commit_short <- ifelse(local_commit_pushed,
                                   get_hyperlinked_commit(latest_local_commit, file, repo_url),
                                   substr(latest_local_commit, 1, 7))


      diagnostics_items <- list(
        glue::glue("Last posted commit: {latest_qc_commit_short}"),
        glue::glue("Local commit: {local_commit_short}")
      )

      if (local_commit_pushed) {
        # only give commit diff if local commit is pushed
        commit_diff_url <- get_hyperlinked_commit_diff(repo_url, latest_qc_commit, latest_local_commit)
        diagnostics_items <- append(diagnostics_items, commit_diff_url)
      }

      diagnostics <- format_diagnostics_list(diagnostics_items)

      return(list(qc_status = "QC notification posted",
                  diagnostics = diagnostics
                  ))
    } # QC notification posted

    ### File changes since last posted commit
    last_remote_commit_that_changed_file <- last_commit_that_changed_file_after_latest_qc_commit(file,
                                                                                            latest_qc_commit,
                                                                                            head_commit = remote_commits[1])$last_commit_that_changed_file
    if (!is.null(last_remote_commit_that_changed_file)) {
      last_commit_that_changed_file_short <- get_hyperlinked_commit(last_remote_commit_that_changed_file, file, repo_url)
      commit_diff_url <- get_hyperlinked_commit_diff(repo_url, latest_qc_commit, last_remote_commit_that_changed_file)

      diagnostics <- format_diagnostics_list(list(
        glue::glue("Last posted commit: {latest_qc_commit_short}"),
        glue::glue("Last file change: {last_commit_that_changed_file_short}"),
        commit_diff_url
      ))

      return(list(qc_status = "File changes since last posted commit",
                  diagnostics = diagnostics
                  ))
    } # File changes since last posted commit

    ### QC in progress
    return(list(qc_status = "QC in progress",
                diagnostics = format_diagnostics_list(list(glue::glue("Last posted commit: {latest_qc_commit_short}")))
                ))
  } # open

  ## For closed issues
  else if (issue_state == "Closed") {
    ### Local uncommitted file changes after Issue closure
    if (git_status == "Local uncommitted file changes") {
      return(list(qc_status = "Local uncommitted file changes after Issue closure",
                  diagnostics = format_diagnostics_list(list(glue::glue("Final QC commit: {latest_qc_commit_short}")))
                  ))
    }

    ### Local unpushed commits with file changes after Issue closure
    if (git_status == "Local unpushed commits with file changes") {
      last_local_commit_that_changed_file <- last_commit_that_changed_file_after_latest_qc_commit(file,
                                                                                            latest_qc_commit,
                                                                                            head_commit = local_commits[1])$last_commit_that_changed_file
      last_local_commit_that_changed_file_short <-  substr(last_local_commit_that_changed_file, 1, 7)

      diganostics <- format_diagnostics_list(list(
        glue::glue("Final QC commit: {latest_qc_commit_short}"),
        glue::glue("Last local file change: {last_local_commit_that_changed_file_short}")
      ))

      return(list(qc_status = "Local unpushed commits with file changes after Issue closure",
                  diagnostics = diganostics
      ))
    } # Local unpushed commits with file changes after Issue closure

    ### Pushed file changes
    # if there exists a remote commit that changed the file after the latest qc commit,
    file_change_info <- last_commit_that_changed_file_after_latest_qc_commit(file,
                                                                             latest_qc_commit,
                                                                             head_commit = remote_commits[1])
    last_remote_commit_that_changed_file <- file_change_info$last_commit_that_changed_file

    if (!is.null(last_remote_commit_that_changed_file)) {
      # was the commit before or after Issue closure?
      commit_time <- as.POSIXct(file_change_info$commit_time)
      issue_close_time <- as.POSIXct(issue_closed_at, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

      last_commit_that_changed_file_short <- get_hyperlinked_commit(last_remote_commit_that_changed_file, file, repo_url)
      commit_diff_url <- get_hyperlinked_commit_diff(repo_url, latest_qc_commit, last_remote_commit_that_changed_file)

      diagnostics <- format_diagnostics_list(list(
        glue::glue("Final QC commit: {latest_qc_commit_short}"),
        glue::glue("Last file change: {last_commit_that_changed_file_short}"),
        commit_diff_url
      ))

      if (commit_time < issue_close_time) {
        return(list(qc_status = "Uncommented pushed file changes before Issue closure",
                    diagnostics = diagnostics
                    ))
      }
      else {
        return(list(qc_status = "Pushed file changes after Issue closure",
                    diagnostics = diagnostics
        ))
      }

    } # if file changed

   return(list(qc_status = "QC complete",
               diagnostics = format_diagnostics_list(list(glue::glue("Final QC commit: {latest_qc_commit_short}")))
               ))
  } # closed

  ## For non-issue files
  else if (issue_state == "no issue") {
    return(list(qc_status = NA_character_,
                diagnostics = NA_character_
                ))
  }

  else {
    rlang::abort(glue::glue("unrecognized issue state {issue_state}"))
  }

} # get_file_qc_status

get_relevant_files <- function(issue, milestone_name) {
  # parse issue body for associated files
  issue_body <- issue$body

  if (!stringr::str_detect(issue_body, "## Relevant files")) { # if no associated relevant files
    return( # return an empty data frame
      dplyr::tibble(
        relevant_file_name = character(),
        qc_file_name = character(),
        relevant_file_url = character(),
        relevant_file_note = character(),
        milestone_name = character(),
        issue_number = integer()
      )
    )
  } # if no associated relevant files

  relevant_files_section <- stringr::str_extract(
    issue_body,
    "## Relevant files[\\s\\S]*?(?=\\n#{1,6} )"
  )

  file_pattern <- "- \\*\\*(.*?)\\*\\*\\s*- \\[`(.*?)`\\]\\((.*?)\\)(?:\\s*>\\s*(.*?))?(?:\\n|$)"

  matches <- stringr::str_match_all(relevant_files_section, file_pattern)[[1]]

  relevant_files_df <- data.frame(
    relevant_file_name = matches[,3],
    qc_file_name = issue$title,
    relevant_file_url = matches[,4],
    relevant_file_note = stringr::str_trim(matches[,5]),
    milestone_name = milestone_name,
    issue_number = issue$number,
    stringsAsFactors = FALSE
  )

  return(relevant_files_df)
} # get_relevant_files


format_diagnostics_list <- function(items) {
  list_items <- glue::glue('<li style="margin-bottom: 3px;">{items}</li>')
  glue::glue('
    <ul style="list-style-type: disc; margin: 0; padding-left: 1em;">
      {glue::glue_collapse(list_items, sep = "\n")}
    </ul>
  ')
}
