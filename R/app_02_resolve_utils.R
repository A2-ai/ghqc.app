#' Convert Issue Data Frame Format
#'
#' This function converts a list of Issue data frames into a structured format for further processing.
#' It extracts Issue number, title, and state, then organizes them into a named list split by their state
#' for shiny input.
#'
#' @param issue_df A list of Issue data frames, where each data frame contains the columns: number, title, and state.
#'
#' @return A named list where the names are the Issue states ("Open Issues" or "Closed Issues")
#' and the values are named vectors with Issue details formatted as "Issues <number>: <title>".
#' @importFrom dplyr %>% tibble mutate case_when
#' @importFrom purrr map_df
#' @examples
#' issues <- list(
#'   list(number = 1, title = "Issue 1", state = "open"),
#'   list(number = 2, title = "Issue 2", state = "closed")
#' )
#' convert_issue_df_format(issues)
#' @noRd
convert_issue_df_format <- function(issues, org, repo) {
  log4r::debug(.le$logger, "Parsing Issue data frame format")

  issues_df <- map_df(issues, function(.x) {
    tibble(
      number = .x$number,
      title = .x$title,
      state = .x$state,
      milestone = .x$milestone$title
    )
  })

  latest_commit_date <- lapply(issues_df$title, function(file_path) {
    # commits for the specific file
    commits <- gh::gh(
      "GET /repos/{org}/{repo}/commits",
      .api_url = .le$github_api_url,
      org = org,
      repo = repo,
      path = file_path
    )

    # get most recent commit date
    if (length(commits) > 0) {
      return(as.POSIXct(commits[[1]]$commit$committer$date, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
    } else {
      return(NA)
    }
  })

  latest_commit_date <- do.call(c, latest_commit_date)
  issues_df <- dplyr::mutate(issues_df, latest_commit_date)

  issues_df <- issues_df %>%
    dplyr::arrange(desc(latest_commit_date))

  # display <- issues_df %>%
  #   mutate(state = case_when(
  #     state == "open" ~ "Open Issues",
  #     state == "closed" ~ "Closed Issues"
  #   ))

  # display <- display %>%
  #   split(display$state) %>%
  #   rev() %>%
  #   lapply(function(x) {
  #     stats::setNames(nm = paste0("Issue ", x$number, ": ", x$title))
  #   })


  issues_df <- issues_df %>%
    mutate(display = paste0("Issue ", number, ": ", title),
           clean_date = format(latest_commit_date, "%Y-%m-%d")
           )

  #issues_df <- issues_df %>% split(issues_df$clean_date)


  log4r::info(.le$logger, "Parsed Issue data frame format")

  return(issues_df)
}


#' Convert Commits Data Frame Format
#'
#' This function converts a data frame of commits into a named list for further processing.
#' It splits the commits by their date, then organizes them into a named list for shiny input.
#'
#' @param commit_df A data frame of commits, where each row contains the columns: date, commit, and display.
#'
#' @return A named list where the names are the commit display names and the values are the corresponding commit hashes.
#' @importFrom dplyr %>%
#' @examples
#' commits <- tibble(
#'   date = as.Date(c("2023-01-01", "2023-01-02")),
#'   commit = c("abc123", "def456"),
#'   display = c("Commit 1", "Commit 2")
#' )
#' convert_commits_df_format(commits)
#' @noRd
convert_commits_df_format <- function(commit_df) {
  debug(.le$logger, "Converting commits data frame format to named list")

  commits <- commit_df %>%
    split(commit_df$date) %>%
    rev() %>%
    lapply(function(x) {
      stats::setNames(
        object = x$commit,
        nm = x$display
      )
    })

  debug(.le$logger, "Successfully created commits list")

  return(commits)
}


#' Split Issue Parts
#'
#' This function splits an Issue string into its components: Issue number and Issue title for functions that take one or the other.
#'
#' @param issue A character string representing an Issue in the format "Issue <number>: <title>".
#'
#' @return A list containing the issue number and issue title.
#' @examples
#' split_issue_parts("Issue 1: Issue Title")
#' @noRd
split_issue_parts <- function(issue) {
  tryCatch(
    {
      debug(.le$logger, glue::glue("Splitting Issue parts for: {issue}"))

      issue_parts <- strsplit(sub("Issue ", "", issue), ": ")[[1]]
      issue_number <- as.numeric(issue_parts[1])
      issue_title <- as.character(issue_parts[2])

      debug(.le$logger, glue::glue("Issue parts split: number = {issue_number}, title = {issue_title}"))

      list(issue_number = issue_number, issue_title = issue_title)
    },
    warning = function(w) {
      debug(.le$logger, glue::glue("Error in split_issue_parts: {w$message}"))
      rlang::abort(w$message)
    }
  )
}
