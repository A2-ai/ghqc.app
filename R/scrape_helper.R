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
    # if the server is GitHub and there's a value for x-github-request-id, it's a ghe error
    bool <- httr2::resp_header(resp, "Server") == "GitHub.com" && nzchar(httr2::resp_header(resp, "x-github-request-id", default = ""))
    bool
  }

  req <- httr2::request(url)

  req <- httr2::req_headers(req, "Accept" = "application/vnd.github.v3.raw")
  req <- httr2::req_headers(req, "Accept" = "application/vnd.github.full+json")
  req <- httr2::req_error(req, is_error = function(resp) {
    # it's only considered an error if it's not an amz error and it's not a ghe error and not status 200
    !is_amz_redirect(resp) && !is_ghe_redirect(resp) && httr2::resp_status(resp) != 200
  })

  req <- httr2::req_perform(req, verbosity = 1)

  asset_url <- httr2::last_response() |> httr2::resp_url()
  path <- tempfile(fileext = ".png")

  httr2::request(asset_url) |>
    httr2::req_perform(path = path)

  path
}

process_comments <- function(comments) {
  if (length(comments) == 0) {
    return("")
  }
  comments_list <- as.list(split(as.data.frame(comments), seq_len(nrow(comments))))
  lapply(comments_list, function(comment) {
    comment <- as.list(comment)
    text_html <- comment$body_html
    text_md <- comment$body

    # get markdown images in html text body
    pattern_md <- "!\\[.*?\\]\\((.*?)\\)"
    matches_md <- gregexpr(pattern_md, text_html, perl = TRUE)
    md_links_in_html_body <- regmatches(text_html, matches_md)

    # get html images in html text body
    pattern_html <- "<img[^>]+src=\"(https://[^\"]+)\"[^>]*>"
    matches_html <- gregexpr(pattern_html, text_html, perl = TRUE)
    html_links_in_html_body <- regmatches(text_html, matches_html)

    # links in html body
    all_links_in_html_body <- c(unlist(md_links_in_html_body), unlist(html_links_in_html_body))

    # get markdown images in md body
    matches_md <- gregexpr(pattern_md, text_md, perl = TRUE)
    md_links_in_md_body <- regmatches(text_md, matches_md)

    # get html images in md body
    pattern_html <- "<img[^>]+src=\"(https://[^\"]+)\"[^>]*>"
    matches_html <- gregexpr(pattern_html, text_md, perl = TRUE)
    html_links_in_md_body <- regmatches(text_md, matches_html)

    # links in html body
    all_links_in_md_body <- c(unlist(md_links_in_md_body), unlist(html_links_in_md_body))

    # text to appear in QC Record
    text <- text_md

    if (length(all_links_in_html_body) > 0) {
      mapply(function(link_in_html_body, link_in_md_body) {
        html_url <- sub("!\\[.*?\\]\\((.*?)\\)", "\\1", link_in_html_body)
        html_url <- sub(".*src=\"(https://[^\"]+)\".*", "\\1", link_in_html_body)

        if (startsWith(html_url, "http")) {
          local_path <- download_image(html_url)
          new_local_image <- paste0("\n\n![](", local_path, ")\n")
          text <<- gsub(link_in_md_body, new_local_image, text, fixed = TRUE)
        }
        else {
          # replace with plain text link
          text <<- gsub(link_in_html_body, html_url, text, fixed = TRUE)
        }
      },
      all_links_in_html_body, all_links_in_md_body)
    } # if any links

    time <- humanize_time(comment$created_at)
    glue::glue("**Comment by {comment$user$login} at {time}:**\n\n{text}\n\n", .trim = FALSE)
  })
} # process_comments

get_issue_body_metadata <- function(body) {
  metadata_section <- stringr::str_match(body, "(?s)## Metadata(.*)")[2]
  metadata_lines <- stringr::str_trim(unlist(strsplit(metadata_section, "\n")))

  metadata <- list()

  for (line in metadata_lines) {
    if (stringr::str_detect(line, "^[*-]")) {
      key_value <- stringr::str_match(line, "[*-]\\s*(.*?):\\s*(.*)")[2:3]
      metadata[[key_value[1]]] <- key_value[2]
    }
  }
  metadata
} # get_issue_body_metadata


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
