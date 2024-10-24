#' @importFrom log4r warn error info debug
generate_html_list_with_hyperlink <- function(items) {
  paste("<li><a href='", items$url, "' target='_blank'>", items$title, "</a></li>", collapse = "")
}

generate_tiered_html_list_with_hyperlink <- function(items) {
  milestone_dfs <- split(items, items$milestone)

  milestone_sections <- lapply(names(milestone_dfs), function(milestone_name) {
    milestone_url <- milestone_dfs[[milestone_name]]$milestone_url[1]
    milestone_heading <- glue::glue("<a href='{milestone_url}'>{milestone_name}</a>:")

    milestone_list_items <- paste(
      "<li><a href='",
      milestone_dfs[[milestone_name]]$url,
      "' target='_blank'>",
      milestone_dfs[[milestone_name]]$title,
      "</a></li>",
      collapse = ""
    )
    milestone_section <- glue::glue("<li>{milestone_heading}<ul>{milestone_list_items}</ul></li>")
    return(milestone_section)
  })

  list <- glue::glue_collapse(milestone_sections, sep = "\n")
  return(list)
}

#' @importFrom log4r warn error info debug
generate_open_milestone_message <- function(open_milestones, warning_icon_html) {
  messages <- c()
  if (length(open_milestones) > 0) {
    messages <- c(messages, sprintf(
      "%s The following selected Milestones are open:<ul>%s</ul><br>",
      warning_icon_html, generate_html_list_with_hyperlink(open_milestones)
    ))
  }
  return(messages)
}

#' @importFrom log4r warn error info debug
generate_open_issue_message <- function(open_issues, warning_icon_html) {
  messages <- c()
  if (length(open_issues) > 0) {
    messages <- c(messages, sprintf(
      "%s The selected Milestones contain the following open Issues:<ul>%s</ul><br>",
      warning_icon_html, generate_tiered_html_list_with_hyperlink(open_issues)
    ))
  }
  return(messages)
}

#' @importFrom log4r warn error info debug
generate_open_checklist_message <- function(issues_with_open_checklists, warning_icon_html) {
  messages <- c()
  if (length(issues_with_open_checklists) > 0) {
    messages <- c(messages, sprintf(
      "%s The selected Milestones contain the following Issues with open checklist items:<ul>%s</ul><br>",
      warning_icon_html, generate_tiered_html_list_with_hyperlink(issues_with_open_checklists)
    ))
  }
  return(messages)
}

#' @importFrom log4r warn error info debug
determine_modal_message_report <- function(owner, repo, milestone_names) {
  warning_icon_html <- "<span style='font-size: 24px; vertical-align: middle;'>&#9888;</span>"

  open_milestones <- check_for_open_milestones(owner, repo, milestone_names)
  open_issues <- check_for_open_issues(owner, repo, milestone_names)
  open_checklists <- check_for_open_checklists(owner, repo, milestone_names)

  messages <- c()
  messages <- c(messages, generate_open_milestone_message(open_milestones, warning_icon_html))
  messages <- c(messages, generate_open_issue_message(open_issues, warning_icon_html))
  messages <- c(messages, generate_open_checklist_message(open_checklists, warning_icon_html))

  log_string <- glue::glue("Modal Check Inputs:
    - Open Milestones: {glue::glue_collapse(open_milestones, sep = ', ')}
    - Open Issues: {glue::glue_collapse(open_issues, sep = ', ')}
    - Issues with unchecked checklist items: {glue::glue_collapse(open_checklists, sep = ', ')}
  ")

  log4r::debug(.le$logger, log_string)

  if (length(messages) == 0) {
    return(list(message = NULL, state = NULL))
  } else {
    state <- if (any(grepl(warning_icon_html, messages))) "warning" else "error"
    return(list(message = paste(messages, collapse = "\n"), state = state))
  }
}

#' @importFrom log4r warn error info debug
check_for_open_milestones <- function(owner, repo, milestone_names) {
  milestones <- purrr::map(milestone_names, function(milestone_name) {
    tryCatch(
      {
        get_milestone_from_name(owner, repo, milestone_name)
      },
      error = function(e) {
        debug(.le$logger, glue::glue("Error retrieving Milestones: {e$message}"))
        rlang::abort(e$message)
      }
    )
  })

  open_milestones <- purrr::map_dfr(milestones, function(milestone) {
    if (milestone$state == "open") {
      data.frame(title = milestone$title, url = milestone$html_url)
    }
    else NULL
  })
}

#' @importFrom log4r warn error info debug
check_for_open_issues <- function(owner, repo, milestone_names) {
  open_issues <- purrr::map_dfr(milestone_names, function(milestone_name) {
    issues <- tryCatch(
      {
        get_all_issues_in_milestone(owner, repo, milestone_name)
      },
      error = function(e) {
        debug(.le$logger, glue::glue("Error retrieving Issues from Milestone: {milestone_name}: {e$message}"))
        rlang::abort(e$message)
      }
    )

    purrr::map_dfr(issues, function(issue) {
      if (issue$state == "open") {
        data.frame(title = issue$title,
                   url = issue$html_url,
                   milestone = issue$milestone$title,
                   milestone_url = issue$milestone$html_url)
      }
      else NULL
    })
  })
}

#' @importFrom log4r warn error info debug
check_for_open_checklists <- function(owner, repo, milestone_names) {
  issues_with_open_checklists <- purrr::map_dfr(milestone_names, function(milestone_name) {
    issues <- tryCatch(
      {
        get_all_issues_in_milestone(owner, repo, milestone_name)
      },
      error = function(e) {
        debug(.le$logger, glue::glue("Error retrieving Issues from {milestone_name}: {e$message}"))
        rlang::abort(e$message)
      }
    )

    purrr::map_dfr(issues, function(issue) {
      if (unchecked_items_in_issue(issue)) {
        data.frame(title = issue$title,
                   url = issue$html_url,
                   milestone = issue$milestone$title,
                   milestone_url = issue$milestone$html_url)
      } else NULL
    })
  })
}


