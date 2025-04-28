approve_warnings <- function(issue, row) {
  warnings <- c()

  user <- get_user()
  qcer <- row$QCer

  # 1: is there a QCer?
  if (qcer == "No QCer") {
    link <- glue::glue("<a href=\"{row$issue_url}\" target=\"_blank\">assign yourself on GitHub</a>")
    warning <- glue::glue("There is no QCer assigned for the Issue. If you are the QCer, {link} before approving QC.")
    warnings <- c(warnings, warning)
  }
  # 2: If there is a QCer, is the user the QCer?
  else if (user != qcer) {
    warning <- "It is highly recommended that the QCer ({qcer}) approves the final state of the QC file."
    warnings <- c(warnings, warning)
  }


  # 3: Are there QC comments in-line in the script still?


  # 4: Are there unchecked checklist items?
  if (unchecked_items_in_issue(issue$body)) {
    link <- glue::glue("<a href=\"{row$issue_url}\" target=\"_blank\">Issue</a>")
    warning <- glue::glue("There are unchecked checklist items in the {link}")
    warnings <- c(warnings, warning)
  }

  return(warnings)
} # approve_warnings


