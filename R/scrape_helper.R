create_section <- function(section_title, contents) {
  glue::glue("## {section_title}\n{contents}\n\n", .trim = FALSE)
} # create_section

humanize_time <- function(time_string) {
  time_object <- strptime(time_string, format="%Y-%m-%dT%H:%M:%SZ", tz="UTC")
  format(time_object, "%Y-%m-%d %H:%M:%S")
} # humanize_time

get_events_list <- function(events) {
  sapply(events, function(event) {
    event_type <- event$event
    user <- if (!is.null(event$actor)) event$actor$login else "Unknown"
    time <- humanize_time(event$created_at)
    if (event_type == "assigned") {
      return(glue::glue("- assigned to {event$assignee$login} by {event$assigner$login} at {time}\n", .trim = FALSE))
    }

    if (event_type == "unassigned") {
      return(glue::glue("- unassigned {event$assignee$login} by {event$assigner$login} at {time}\n", .trim = FALSE))
    }

    else if (event_type == "milestoned") {
      return(glue::glue("- milestone set to {event$milestone$title} by {user} at {time}\n", .trim = FALSE))
    }

    else {
      return(glue::glue("- {event_type} by {user} at {time}\n", .trim = FALSE))
    }
  })
} # get_events_list

get_events_df <- function(issue_events) {
  data <- sapply(issue_events, function(event) {
    event_type <- event$event
    created_at <- humanize_time(event$created_at)
    by_user <- if (!is.null(event$assigner)) event$assigner$login else event$actor$login
    to_user <- if (!is.null(event$assignee)) event$assignee$login else NA
    title <- if (!is.null(event$milestone)) event$milestone$title else NA
    c(event_type, created_at, by_user, to_user, title)
  })

  data <- t(data)
  colnames(data) <- c("event", "created at", "by user", "to user", "title")
  data <- as.data.frame(data, stringsAsFactors = FALSE)

  return(data)
}

get_timeline_list <- function(timeline_events) {
  sapply(timeline_events, function(event) {
    event_type <- event$event
    user <- if (!is.null(event$actor)) event$actor$login else "Unknown"
    time <- humanize_time(event$created_at)
    if (event_type == "assigned") {
      return(glue::glue("- assigned to {event$assignee$login} by {user} at {time}\n", .trim = FALSE))
    }

    if (event_type == "unassigned") {
      return(glue::glue("- unassigned {event$assignee$login} by {user} at {time}\n", .trim = FALSE))
    }

    else if (event_type == "milestoned") {
      return(glue::glue("- milestone set to {event$milestone$title} by {user} at {time}\n", .trim = FALSE))
    }

    else {
      return(glue::glue("- {event_type} by {user} at {time}\n", .trim = FALSE))
    }
  })
}

download_image <- function(url) {
  is_amz_redirect <- function(resp) {
    # if the server is Amazon and there's a value for x-amz-request-id, it's an amz error
    bool <- httr2::resp_header(resp, "Server") == "AmazonS3" && nzchar(httr2::resp_header(resp, "x-amz-request-id", default = ""))
    bool
  }

  is_ghe_redirect <- function(resp) {
    # if the server is Github and there's a value for x-github-request-id, it's a ghe error
    bool <- httr2::resp_header(resp, "Server") == "GitHub.com" && nzchar(httr2::resp_header(resp, "x-github-request-id", default = ""))
    bool
  }

  token <- get_gh_token(.le$github_api_url)

  req <- httr2::request(url)

  req <- httr2::req_headers(req, "Accept" = "application/vnd.github.v3.raw")
  req <- httr2::req_auth_bearer_token(req, token)
  # for the error, tis not really an error if its an amazon redirect, then we can go in and get the
  # url code
  req <- httr2::req_error(req, is_error = function(resp) {
    # it's only considered an error if it's not an amz error and it's not a ghe error
    !is_amz_redirect(resp) && !is_ghe_redirect(resp)
  })
  req <- httr2::req_perform(req, verbosity = 1)

  asset_url <- httr2::last_response() |> httr2::resp_url()

  path <- tempfile(fileext = ".png")

  httr2::request(asset_url) |>
    httr2::req_perform(path = path)

  path
}

process_comments <- function(comments) {
  sapply(comments, function(comment) {
    text <- comment$body

    # detect markdown images
    pattern_md <- "!\\[.*?\\]\\((.*?)\\)"
    matches_md <- gregexpr(pattern_md, text, perl = TRUE)
    links_md <- regmatches(text, matches_md)

    # detect html images
    pattern_html <- "<img[^>]+src=\"(https://[^\"]+)\"[^>]*>"
    matches_html <- gregexpr(pattern_html, text, perl = TRUE)
    links_html <- regmatches(text, matches_html)

    all_links <- c(unlist(links_md), unlist(links_html))

    if (length(all_links) > 0) {
      for (link in all_links) {
        url <- sub("!\\[.*?\\]\\((.*?)\\)", "\\1", link)
        url <- sub(".*src=\"(https://[^\"]+)\".*", "\\1", url)

        if (startsWith(url, "http")) {
          local_path <- download_image(url)
          text <- gsub(link, paste0("\n![](", local_path, ")\n"), text, fixed = TRUE)
        }
        else {
          # replace with plain text link
          text <- gsub(link, url, text, fixed = TRUE)
        }

      }
    }
    time <- humanize_time(comment$created_at)
    glue::glue("**Comment by {comment$user$login} at {time}:**\n\n{text}\n\n", .trim = FALSE)
  })
} # process_comments

get_metadata <- function(body) {
  metadata_section <- stringr::str_match(body, "(?s)## Metadata(.*)")[2]
  metadata_lines <- stringr::str_trim(unlist(strsplit(metadata_section, "\n")))

  metadata <- list()

  for (line in metadata_lines) {
    if (stringr::str_detect(line, "^\\*")) {
      key_value <- stringr::str_match(line, "\\*\\s*(.*?):\\s*(.*)")[2:3]
      metadata[[key_value[1]]] <- key_value[2]
    }
  }
  metadata
} # get_metadata


get_close_info <- function(issue) {
  issue_events <- gh::gh(issue$events_url)
  events_list <- get_events_list(issue_events)
  closures <- events_list[grep("closed", events_list)]

  status <- {
    # if still open
    if (length(closures) == 0) {
      return(list(
        closer = NA,
        closed_at = NA
      ))
    }
    else {
      # get the last time it was closed, which is the current status
      last_closure <- closures[length(closures)]
      gsub("- ", "", last_closure)
    }
  }
  pattern <- "closed by (.+?) at (\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2})"
  matches <- stringr::str_match(status, pattern)

  result <- list(
    closer = matches[2],
    closed_at = matches[3]
  )
}
