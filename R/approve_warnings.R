approve_warnings <- function(issue, row) {
  warnings <- c()

  user <- get_user()
  qcer <- as.character(row$QCer)

  # 1: is there a QCer?
  if (qcer == "No QCer") {
    link <- glue::glue("<a href=\"{row$issue_url}\" target=\"_blank\">assign yourself on GitHub</a>")
    warning <- glue::glue("There is no QCer assigned for the Issue. If you are the QCer, {link} before approving QC")
    warnings <- c(warnings, warning)
  }
  # 2: If there is a QCer, is the user the QCer?
  else if (user != qcer) {
    warning <- glue::glue("It is highly recommended that the QCer ({qcer}) approves the final state of the QC file")
    warnings <- c(warnings, warning)
  }

  # 3: Are there unchecked checklist items?
  if (unchecked_items_in_issue(issue$body)) {
    link <- glue::glue("<a href=\"{row$issue_url}\" target=\"_blank\">Issue</a>")
    warning <- glue::glue("There are unchecked checklist items in the {link}")
    warnings <- c(warnings, warning)
  }

  # 4: Are there QC comments in-line in the script still?
  qc_comments <- get_qc_comments(row$file_name)
  if (length(qc_comments$comment_lines) > 0) {
    escape_html <- function(text) {
      text <- gsub("&", "&amp;", text)
      text <- gsub("<", "&lt;", text)
      text <- gsub(">", "&gt;", text)
      text
    }

    list_items <- paste0(
      "<li style=\"margin:0;\">line ",
      qc_comments$comment_lines,
      ": ",
      escape_html(qc_comments$comment_values),
      "</li>"
    )

    warning <- paste0(
      "There are still QC comments in the code:\n<ul style=\"padding-left: 20px;\">\n",
      paste(list_items, collapse = "\n"),
      "\n</ul>"
    )

    warnings <- c(warnings, warning)
  }

  return(warnings)
} # approve_warnings

get_qc_comments <- function(file_path) {
  is_binary_file <- stringr::str_detect(file_path, exclude_patterns())
  if (is_binary_file) {
    return(integer(0))
  }

  file_content <- readLines(file_path)
  lines_with_qc_comments <- stringr::str_which(file_content, stringr::regex("#\\s?qc", ignore_case = TRUE))
  return(
    list(
      comment_lines = lines_with_qc_comments,
      comment_values = file_content[lines_with_qc_comments]
    )
  )
}

