archive_render_selected_list <- function(input, ns, items = NULL, depth = 0, output) {
  tryCatch(
    {
      debug(.le$logger, glue::glue(
        "Rendering selected list with items: {paste(items, collapse = ', ')}"
      ))

      ul <- div(class = paste("grid-container", "depth", depth, sep = "-"))

      for (name in items) {
        modified_name <- gsub("([^a-zA-Z0-9])", "\\1<wbr>", htmltools::htmlEscape(name))
        milestone_input_id <- generate_input_id("milestone", name)
        commit_input_id <- generate_input_id("commit", name)

        item_input <- div(
          class = "form-control",
          HTML(modified_name)
        )
         milestone_input <- selectizeInput(
           ns(milestone_input_id),
           label = NULL,
           choices = NULL,
           multiple = FALSE,
           width = "100%",
           options = list(
             closeAfterSelect = TRUE
           )
         )
         commit_input <- selectizeInput(
           ns(commit_input_id),
           label = NULL,
           choices = NULL,
           multiple = FALSE,
           width = "100%",
           options = list(
             closeAfterSelect = TRUE
           )
         )

        ul <- tagAppendChild(
          ul,
          div(
            class = "grid-items",
            div(class = "item-a", item_input),
            div(class = "item-b", milestone_input),
            div(class = "item-c", commit_input)
          )
        )
      }

      debug(.le$logger, "Rendered selected list successfully")
      ul
    },
    error = function(e) {
      items <- glue::glue_collapse(items, sep = ", ")
      error_message <- glue::glue(
        "Error rendering selected {items}: {conditionMessage(e)}"
      )
      log4r::error(.le$logger, error_message)
      stopApp()
      rlang::abort(error_message)
    }
  )
}

#parsing for all commits change where this is in the app
parse_local_log <- function(lines, keep_status = FALSE) {
  lines <- as.character(lines)
  lines <- trimws(lines)
  lines <- lines[nzchar(lines)]

  is_commit <- grepl("^[0-9a-f]{40}\\|", lines)
  commits   <- ifelse(is_commit, sub("^([0-9a-f]{40}).*$", "\\1", lines), NA_character_)

  for (i in seq_along(commits)) {
    if (is.na(commits[i]) && i > 1) commits[i] <- commits[i-1]
  }

  is_change <- grepl("^([A-Z](?:\\d{1,3})?)\\t", lines, perl = TRUE)

  lines   <- lines[is_change]
  commits <- commits[is_change]

  status <- sub("^([A-Z](?:\\d{1,3})?)\\t.*$", "\\1", lines, perl = TRUE)


  split2 <- strsplit(lines, "\t", fixed = TRUE)

  files <- vapply(seq_along(split2), function(i) {
    s <- status[i]
    p <- split2[[i]]
    if (length(p) >= 3 && grepl("^[RC]\\d{0,3}$", s)) {
      p[[3]]
    } else if (length(p) >= 2) {
      p[[2]]
    } else {
      NA_character_
    }
  }, character(1))

  out <- data.frame(commit = commits, title = files, stringsAsFactors = FALSE)
  if (keep_status) out$status <- status
  rownames(out) <- NULL
  out
}

archive_isolate_rendered_list <- function(input, session, items, local_commit_df) {
  for (name in items) {
    debug(.le$logger, glue::glue("Updating selectize inputs for item: {name}"))

    milestone_input_id <- generate_input_id("milestone", name)
    commit_input_id    <- generate_input_id("commit", name)

    df_item <- local_commit_df %>%
      dplyr::filter(.data$title == !!name)

    milestone_choices <- df_item %>%
      dplyr::pull(.data$milestone_title) %>%
      purrr::discard(is.na) %>%
      unique() %>%
      sort()

    commit_choices <- df_item %>%
      dplyr::pull(.data$commit) %>%
      purrr::discard(is.na) %>%
      unique()

    updateSelectizeInput(
      session,
      milestone_input_id,
      choices  = milestone_choices,
      server   = TRUE
    )

    updateSelectizeInput(
      session,
      commit_input_id,
      choices  = commit_choices,
      server   = TRUE
    )
  }
}
