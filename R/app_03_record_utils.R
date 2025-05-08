#' @importFrom log4r warn error info debug
generate_html_list_with_hyperlink <- function(items) {
  html <- paste("<li><a href='", items$url, "' target='_blank'>", items$title, "</a></li>", collapse = "")
  html
}

generate_tiered_html_list_with_hyperlink <- function(grouped_issues) {
  milestone_sections <- purrr::imap_chr(grouped_issues, function(issue_list, milestone_name) {
    if (length(issue_list) == 0) return("")

    # Get milestone URL from the first issue (if available)
    milestone_url <- issue_list[[1]]$milestone$html_url
    milestone_heading <- glue::glue("<a href='{milestone_url}' target='_blank'>{milestone_name}</a>:")

    milestone_list_items <- purrr::map_chr(issue_list, function(item) {
      glue::glue("<li><a href='{item$html_url}' target='_blank'>{item$title}</a></li>")
    }) |> paste(collapse = "")

    glue::glue("<li>{milestone_heading}<ul>{milestone_list_items}</ul></li>")
  })

  glue::glue_collapse(milestone_sections, sep = "\n")
}

# generate_tiered_html_list_with_hyperlink <- function(items) {
#   # Extract milestone titles and URLs (or use fallbacks inline)
#   milestone_titles <- purrr::map_chr(items, function(x) {
#     if (!is.null(x$milestone)) x$milestone$title else "No Milestone"
#   })
#
#   milestone_urls <- purrr::map_chr(items, function(x) {
#     if (!is.null(x$milestone)) x$milestone$html_url else "#"
#   })
#
#   # Use milestone titles as split keys (names)
#   names(items) <- milestone_titles
#   milestone_groups <- split(items, names(items))
#
#   milestone_sections <- lapply(names(milestone_groups), function(milestone_name) {
#     milestone_items <- milestone_groups[[milestone_name]]
#     milestone_url <- if (!is.null(milestone_items[[1]]$milestone)) milestone_items[[1]]$milestone$html_url else "#"
#
#     milestone_heading <- glue::glue("<a href='{milestone_url}' target='_blank'>{milestone_name}</a>:")
#
#     milestone_list_items <- purrr::map_chr(milestone_items, function(item) {
#       glue::glue("<li><a href='{item$html_url}' target='_blank'>{item$title}</a></li>")
#     }) |> paste(collapse = "")
#
#     glue::glue("<li>{milestone_heading}<ul>{milestone_list_items}</ul></li>")
#   })
#
#   glue::glue_collapse(milestone_sections, sep = "\n")
# }

#' @importFrom log4r warn error info debug
generate_open_milestone_message <- function(open_milestones, warning_icon_html) {
  messages <- c()
  if (length(open_milestones) > 0) {
    messages <- c(messages, sprintf(
      "%s Open Milestones:<ul>%s</ul><br>",
      warning_icon_html, generate_html_list_with_hyperlink(open_milestones)
    ))
  }
  return(messages)
}

generate_unapproved_statuses_message <- function(issues_with_unapproved_statuses, warning_icon_html) {
  messages <- c()
  if (length(issues_with_unapproved_statuses) > 0) {
    messages <- c(messages, sprintf(glue::glue(
      "%s Unapproved Issues:<ul>%s</ul>"),
      warning_icon_html, generate_tiered_html_list_with_hyperlink(issues_with_unapproved_statuses)
    ))
  }
  return(messages)
}


#' @importFrom log4r warn error info debug
generate_open_checklist_message <- function(issues_with_open_checklists, warning_icon_html) {
  messages <- c()
  if (length(issues_with_open_checklists) > 0) {
    messages <- c(messages, sprintf(glue::glue(
      "%s Issues with incomplete {get_checklist_display_name_var(plural = TRUE)}:<ul>%s</ul><br>"),
      warning_icon_html, generate_tiered_html_list_with_hyperlink(issues_with_open_checklists)
    ))
  }
  return(messages)
}

#' @importFrom log4r warn error info debug
determine_modal_message_report <- function(milestone_objects) {
  # TODO make this only check for approved Issues
  warning_icon_html <- "<span style='font-size: 24px; vertical-align: middle;'>&#9888;</span>"

  res <- check_for_unapproved_statuses(milestone_objects)
  issue_objects <- res$issue_objects
  statuses <- res$statuses
  issues_with_unapproved_statuses <- res$issues_with_unapproved_statuses

  issues_with_open_checklists <- check_for_open_checklists(issue_objects)
  open_milestones <- check_for_open_milestones(milestone_objects)

  messages <- c()
  messages <- c(messages, generate_open_milestone_message(open_milestones, warning_icon_html))
  messages <- c(messages, generate_open_checklist_message(issues_with_open_checklists, warning_icon_html))
  messages <- c(messages, generate_unapproved_statuses_message(issues_with_unapproved_statuses, warning_icon_html))

  if (length(messages) == 0) {
    return(list(message = NULL,
                state = NULL,
                issue_objects = issue_objects,
                statuses = statuses,
                issues_with_unapproved_statuses = issues_with_unapproved_statuses,
                issues_with_open_checklists = issues_with_open_checklists
                )
           )
  }
  else {
    state <- if (any(grepl(warning_icon_html, messages))) "warning" else "error"
    return(list(message = paste(messages, collapse = "\n"),
                state = state,
                issue_objects = issue_objects,
                statuses = statuses,
                issues_with_unapproved_statuses = issues_with_unapproved_statuses,
                issues_with_open_checklists = issues_with_open_checklists
                )
           )
  }
}

#' @importFrom log4r warn error info debug
check_for_open_milestones <- function(milestone_objects) {
  open_milestones <- purrr::map_dfr(milestone_objects, function(milestone) {
    if (milestone$state == "open") {
      data.frame(title = milestone$title, url = milestone$html_url)
    }
    else NULL
  })
}



#' @importFrom log4r warn error info debug
check_for_open_checklists <- function(issue_objects) {
  purrr::map(issue_objects, function(milestone_issues) {
    purrr::keep(milestone_issues, ~ unchecked_items_in_issue(.x$body))
  })
}


check_for_unapproved_statuses <- function(milestone_objects) {
  current_branch <- gert::git_branch()
  local_commits <- get_local_commits()
  remote_commits <- get_remote_commits(current_branch)

  ahead_behind_status <- check_ahead_behind()
  # get files with remote changes
  files_changed_in_remote_commits <- get_files_changed_in_remote_commits(remote_commits, ahead_behind_status)

  # get files with local unpushed commits
  files_changed_in_unpushed_local_commits <- get_files_changed_in_unpushed_local_commits(local_commits, ahead_behind_status)

  # get files with local uncommitted file changes
  files_with_uncommitted_local_changes <- get_files_with_uncommitted_local_changes()

  status_res <- ghqc_status(milestone_objects = milestone_objects,
                            current_branch = current_branch,
                            local_commits = local_commits,
                            remote_commits = remote_commits,
                            ahead_behind_status = ahead_behind_status,
                            files_changed_in_remote_commits = files_changed_in_remote_commits,
                            files_changed_in_unpushed_local_commits = files_changed_in_unpushed_local_commits,
                            files_with_uncommitted_local_changes = files_with_uncommitted_local_changes
  )

  issue_objects <- status_res$issue_objects
  status_df <- status_res$status

  statuses <- status_df |>
    split(~ milestone_name) |>
    purrr::map(~ split(.x, ~ .x$file_name))

  unapproved_statuses <- dplyr::filter(status_df, `QC Status` != "Approved")

  unapproved_keys <- paste(unapproved_statuses$milestone_name, unapproved_statuses$file_name, sep = "::")

  filtered_issue_objects <- purrr::imap(issue_objects, function(issue_list, milestone_name) {
    purrr::keep(issue_list, function(issue) {
      key <- paste(issue$milestone$title, issue$title, sep = "::")
      key %in% unapproved_keys
    })
  })

  issues_with_unapproved_statuses <- purrr::discard(filtered_issue_objects, ~ length(.x) == 0)

  return(list(
    issues_with_unapproved_statuses = issues_with_unapproved_statuses,
    issue_objects = issue_objects,
    statuses = statuses
  ))
}




