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


milestone_archive_render <- function(input, ns, items, milestone_commit_df, session, depth = 0, output) {
  tryCatch({
    debug(.le$logger, glue::glue(
      "Rendering selected list with items: {paste(items, collapse = ', ')}"
    ))

    # Create a container div for the grid layout with flex properties for wrapping
    ul <- div(
      class = paste("grid-container", "depth", depth, sep = "-"),
      style = "display: grid; grid-template-columns: 1fr 1.2fr 1.2fr; gap: 10px 12px; align-items: start; word-wrap: break-word; height: auto;"
    )

    # Header row for files, milestones, and commits
    ul <- tagAppendChild(ul, div(tags$strong("Files")))
    ul <- tagAppendChild(ul, div(tags$strong("Milestones")))
    ul <- tagAppendChild(ul, div(tags$strong("Commits")))



    milestone_df <- milestone_commit_df()
    # Loop through each item to add rows dynamically
    for (name in items) {
      modified_name <- gsub("([^a-zA-Z0-9])", "\\1<wbr>", htmltools::htmlEscape(name))
      milestone_input_id <- generate_input_id("milestone", name)
      commit_input_id    <- generate_input_id("commit", name)

      ul <- tagAppendChild(ul, div(class = "form-control", HTML(modified_name),
                                   style = "word-wrap: break-word; white-space: normal; height: auto;"))

      ul <- tagAppendChild(ul, selectizeInput(
        ns(milestone_input_id), label = NULL, choices = NULL,
        multiple = FALSE, width = "100%", options = list(closeAfterSelect = TRUE)
      ))

      ul <- tagAppendChild(ul, selectizeInput(
        ns(commit_input_id), label = NULL, choices = NULL,
        multiple = FALSE, width = "100%", options = list(closeAfterSelect = TRUE)
      ))


    }
    debug(.le$logger, "Rendered selected list successfully")
    ul
  },

  error = function(e) {
    items <- glue::glue_collapse(items, sep = ", ")
    error_message <- glue::glue("Error rendering selected {items}: {conditionMessage(e)}")
    log4r::error(.le$logger, error_message)
    stopApp()
    rlang::abort(error_message)
  })
}


create_single_item_ui <- function(name, ns) {
  milestone_raw <- generate_input_id("milestone", name)
  commit_raw    <- generate_input_id("commit", name)

  shiny::tags$div(
    id = ns(generate_input_id("item_row", name)),
    style = "display: contents;",  # children participate in parent grid

    # Files cell
    shiny::tags$div(
      class = "form-control",
      HTML(htmltools::htmlEscape(name)),
      style = "word-wrap: break-word; white-space: normal;"
    ),

    # Milestones cell (NOTE: inputId is namespaced!)
    shiny::selectizeInput(
      inputId = ns(milestone_raw),
      label   = NULL,
      choices = character(0),
      multiple = FALSE,
      width = "100%",
      options = list(closeAfterSelect = TRUE)
    ),

    # Commits cell (NOTE: inputId is namespaced!)
    shiny::selectizeInput(
      inputId = ns(commit_raw),
      label   = NULL,
      choices = character(0),
      multiple = FALSE,
      width = "100%",
      options = list(closeAfterSelect = TRUE)
    )
  )
}


milestone_archive_isolate_rendered_list <- function(input, session, items, milestone_commit_df) {
  milestone_df <- milestone_commit_df()
  if (is.null(df) || nrow(df) == 0) return(invisible(NULL))

  selected_milestones <- input$milestone_existing

  for (name in items) {
    debug(.le$logger, glue::glue("Updating selectize inputs for item: {name}"))

    milestone_input_id <- generate_input_id("milestone", name)
    commit_input_id    <- generate_input_id("commit", name)

    milestone_df_item <- milestone_df %>%
      dplyr::filter(.data$title == name)


    milestone_choices <-  milestone_df_item %>%
      dplyr::mutate(milestone_title = ifelse(is.na(.data$milestone_title), "N/A", .data$milestone_title)) %>%
      dplyr::pull(.data$milestone_title) %>%
      unique() %>%
      sort()

    commit_choices <-  milestone_df_item %>%
      dplyr::pull(.data$commit) %>%
      unique()

    if (length(matching_milestones) == 0){
      updateSelectizeInput(
        session,
        milestone_input_id,
        choices  = c("N/A", milestone_choices),
        selected = "N/A",
        server   = TRUE
      )
    }else if (length(matching_milestones) == 1) {

      updateSelectizeInput(
        session,
        milestone_input_id,
        choices  = matching_milestones,
        selected = matching_milestones[1],
        server   = TRUE
      )

    }else {
      # Check for matching milestone greater than 1 done in modal pop up
    }
    observeEvent(input[[milestone_input_id]], {

      selected_milestone_input <- input[[milestone_input_id]]
      if (selected_milestone_input == "N/A" || selected_milestone_input == "") {
        commit_choices <- df_item %>%
          dplyr::pull(.data$commit) %>%
          unique()

        updateSelectizeInput(
          session,
          commit_input_id,
          choices  = commit_choices,  # Show all commits for the item
          selected = NULL,  # Allow free selection
          server   = TRUE
        )
      } else {
        commit_choices <- df_item %>%
          dplyr::filter(.data$milestone_title == selected_milestone_input) %>%
          dplyr::pull(.data$commit) %>%
          unique()


        updateSelectizeInput(
          session,
          commit_input_id,
          choices  = commit_choices,
          selected = commit_choices,
          server   = TRUE
        )
      }
    })







#     if (length(matching_milestones) > 0) {
#
#       updateSelectizeInput(
#         session,
#         milestone_input_id,
#         choices  = matching_milestones,
#         selected = matching_milestones[1],
#         server   = TRUE
#       )
#
#
#       commit_choices <- df_item %>%
#         dplyr::filter(.data$milestone_title == matching_milestones[1]) %>%
#         dplyr::pull(.data$commit) %>%
#         unique()
#
#
#       forced_commit <- if (length(commit_choices) > 0) commit_choices[[1]]
#
#
#       updateSelectizeInput(
#         session,
#         commit_input_id,
#         choices  = commit_choices,
#         selected = forced_commit,
#         server   = TRUE
#       )
#     } else {
#
#       updateSelectizeInput(
#         session,
#         milestone_input_id,
#         choices  = c("N/A", milestone_choices),
#         selected = "N/A",
#         server   = TRUE
#       )
#
#
#       updateSelectizeInput(
#         session,
#         commit_input_id,
#         choices  = df_item %>% dplyr::pull(.data$commit) %>% unique(),
#         selected = NULL,
#         server   = TRUE
#       )
#
#
#       observeEvent(input[[milestone_input_id]], {
#         selected_milestone_input <- input[[milestone_input_id]]
#         if (selected_milestone_input != "N/A") {
#           commit_choices <- df_item %>%
#             dplyr::filter(.data$milestone_title == selected_milestone_input) %>%
#             dplyr::pull(.data$commit) %>%
#             unique()
#
#
#           updateSelectizeInput(
#             session,
#             commit_input_id,
#             choices  = commit_choices,
#             selected = commit_choices,
#             server   = TRUE
#           )
#         } else {
#           commit_choices <- df_item %>%
#             dplyr::pull(.data$commit) %>%
#             unique()
#
#           updateSelectizeInput(
#             session,
#             commit_input_id,
#             choices  = commit_choices,  # Show all commits for the item
#             selected = NULL,  # Allow free selection
#             server   = TRUE
#           )
#         }
#       })
#     }
   }

   invisible(NULL)
 }


archive_selected_items <- function(input, session, items, archive_name, flatten = FALSE, milestone_commit_df = NULL) {
  ts <- format(Sys.time(), "%Y%m%d_%H%M%S")

  base_dir <- "archive"
  if (!dir.exists(base_dir)) {
    dir.create(base_dir)
  }

  milestone_items <- character(0)
  if (!is.null(milestone_commit_df)) {
    df <- if (is.function(milestone_commit_df)) milestone_commit_df() else milestone_commit_df
    if (!is.null(df) && nrow(df) > 0 && "title" %in% names(df)) {
      milestone_items <- sort(unique(stats::na.omit(df$title)))
    }
  }

  items_all <- unique(c(items %||% character(0), milestone_items))

  temp_files <- character(0)

  for (item in items_all) {
    commit_input_id <- generate_input_id("commit", item)
    sel_commit      <- input[[commit_input_id]]
    if (is.null(sel_commit) || identical(sel_commit, "")) next

    script_contents <- get_script_contents(item, sel_commit)

    file_path <- if (isTRUE(flatten)) {
      file.path(tempdir(), basename(item))
    } else {
      file.path(tempdir(), item)
    }

    temp_files <- c(temp_files, file_path)
    writeLines(script_contents, file_path, useBytes = TRUE)
  }

  zip_file <- file.path(base_dir, paste0(archive_name, "_", ts, ".zip"))
  zip(zip_file, files = temp_files)

  unlink(temp_files, recursive = TRUE)

  showNotification(paste("Archived and zipped to:", zip_file), type = "message")
}
