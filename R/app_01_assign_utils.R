#' @import shiny
#' @importFrom fs dir_ls
NULL

#' Generate Input ID
#'
#' This function generates a input ID from the given name
#' and optionally adding a prefix. For creating shiny input IDs.
#'
#' @param prefix An optional character string to prepend to the generated ID.
#' @param name A character string representing the name to be sanitized.
#'
#' @return A character string representing the generated input ID.
#' @noRd
generate_input_id <- function(prefix = NULL, name) {
  clean_name <- gsub("[^a-zA-Z0-9_.-]", "_", name)
  if (is.null(prefix)) {
    return(name)
  } else {
    return(paste0(prefix, "_", clean_name))
  }
}

#' Render a Selected List with Inputs
#'
#' This function generates a nested list of selected items with associated input fields
#' for checklists and assignees. It creates a hierarchical representation of items with
#' file extensions, providing selectize input fields for each file.
#'
#' @param input A named list from Shiny server function containing inputs from the Shiny application.
#' @param ns A namespace function used for handling Shiny modules.
#' @param items A list representing the selected items.
#' @param checklist_choices A vector of checklist choices for the selectize input fields.
#'
#' @noRd
render_selected_list <- function(input, ns, items = NULL, checklist_choices = NULL, depth = 0, relevant_files = NULL, previous_qc = NULL, output) {
  tryCatch(
    {
      debug(.le$logger, glue::glue("Rendering selected list with items: {paste(items, collapse = ', ')}"))

      checklist_choices <- stats::setNames(names(checklist_choices), names(checklist_choices))
      ul <- div(class = paste("grid-container", "depth", depth, sep = "-")) # if i remove depth, it won't take styles anymore
      for (name in items) {
        # no css only way to set line breaks on certain chr; used <wbr> to designate non-alphanumeric values as wbr (https://stackoverflow.com/a/24489931)
        modified_name <- gsub("([^a-zA-Z0-9])", "\\1<wbr>", generate_input_id(name = name))

        checklist_input_id <- generate_input_id("checklist", name)
        assignee_input_id <- generate_input_id("assignee", name)
        file_preview_id <- generate_input_id("button", name)
        preview_input_id <- generate_input_id("preview", name)
        history_input_id <- generate_input_id("history", name)
        associate_relevant_files_id <- generate_input_id("associate_relevant_files", name)

        assignee_input <- selectizeInput(
          ns(assignee_input_id),
          label = NULL,
          choices = NULL,
          multiple = FALSE,
          width = "100%",
          options = list(
            closeAfterSelect = TRUE
          )
        )

        checklist_input <- selectizeInput(
          ns(checklist_input_id),
          label = NULL,
          choices = c("", checklist_choices),
          width = "100%",
          selected = NULL,
          options = list(placeholder = get_checklist_display_name_var(capitalized = TRUE))
        )

        preview_input <- actionButton(
          ns(preview_input_id),
          label = HTML(glue::glue("<span>Preview<br>{get_checklist_display_name_var()}</span>")),
          style = "height: 34px !important; font-size: 12px !important; padding: 2px 2px 2px 2px !important; color: #5f5f5f !important; line-height: 1.2em",
          class = "checklist-preview-button"
        )

        file_preview <- actionButton(
          ns(file_preview_id),
          label = HTML(modified_name),
          style = "font-size: 12px !important; font-weight: bold; padding: 2px 2px 2px 2px !important; color: #5f5f5f !important; line-height: 1.2em",
          class = "file-preview-button"
        )

        associate_relevant_files <- actionButton(
          ns(associate_relevant_files_id),
          label = HTML("<span>Associate<br>relevant files</span>"),
          style = "height: 34px !important;font-size: 12px !important; padding: 2px 2px 2px 2px !important; color: #5f5f5f !important; line-height: 1.2em",
          class = "associate-relevant-files-button"
        )

        associate_previous_qc <- actionButton(
          ns(history_input_id),
          label = HTML(glue::glue("<span>Associate<br>previous QC</span>")),
          style = "height: 34px !important; font-size: 12px !important; padding: 2px 2px 2px 2px !important; color: #5f5f5f !important; line-height: 1.2em",
          class = "checklist-preview-button"
        )



        ul <- tagAppendChild(ul, div(class = "item-a", file_preview, style = "padding-bottom: 5px;"))

        ul <- tagAppendChild(ul,
                             div(
                               class = "grid-items",
                               div(class = "item-c", checklist_input),
                               div(class = "item-d", preview_input),
                               div(class = "item-b", assignee_input),
                               div(class = "item-d", associate_relevant_files),
                               div(class = "item-d", associate_previous_qc)
                             )
        )

        # container to hold both sections side by side
        side_by_side_container <- div(
          style = "display: flex; gap: 20px; flex-wrap: wrap; padding-bottom: 15px;",

          # relevant files section
          if (!is.null(relevant_files) && length(relevant_files[[name]]) > 0) {
            div(
              class = "relevant-files-section",
              style = "flex: 1; min-width: 200px;",
              tags$strong("Relevant files:"),
              tags$ul(
                lapply(relevant_files[[name]], function(file) {
                  tags$li(file, style = "font-size: 12px; color: #333; padding: 2px 0;")
                })
              )
            )
          },

          # previous qc section
          if (!is.null(previous_qc) && length(previous_qc[[name]]) > 0) {
            div(
              class = "previous-qc-section",
              style = "flex: 1; min-width: 200px;",
              tags$strong("Previous QC:"),
              previous_qc[[name]]$issue
            )
          }
        )

        # append side-by-side section to main container
        ul <- tagAppendChild(ul, side_by_side_container)


      } # for

      debug(.le$logger, "Rendered selected list successfully")
      ul
    },
    error = function(e) {
      items <- glue::glue_collapse(items, sep = ", ")

      error_message <- glue::glue("Error rendering selected {items}: {conditionMessage(e)}")
      log4r::error(.le$logger, error_message)
      stopApp()
      rlang::abort(error_message)
    }
  )
}


#' Update Selectize Inputs for Checklists and Assignees
#'
#' This function updates selectize inputs for both checklist and assignee
#' selections within a Shiny application, handling conditions where assignee
#' options may vary in number. It specifically preserves the current user
#' selections for checklists, and automatically selects the sole assignee if
#' only one is available, or maintains the current selection when more are added.
#'
#' @param input A reactive list of inputs from a Shiny session.
#' @param session A server-side representation of a Shiny session.
#' @param items A list representing the selected items.
#'
#' @return None. The function performs operations on UI elements and does not return
#'   any value.
#' @noRd
isolate_rendered_list <- function(input, session, items, members) {
  for (name in items) {
    debug(.le$logger, glue::glue("Updating selectize inputs for item: {name}"))

    assignee_input_id <- generate_input_id("assignee", name)
    checklist_input_id <- generate_input_id("checklist", name)

    # add option to not assign QCer
    no_assigned_qcer <- data.frame(username = "No assigned QCer", name = NA_character_, stringsAsFactors = FALSE)
    members <- rbind(
      no_assigned_qcer,
      members
    )

    updateSelectizeInput(
      session,
      assignee_input_id,
      server = TRUE,
      choices = members,
      selected = isolate(input[[assignee_input_id]]),
      options = list(
        placeholder = "QCer (Optional)",
        valueField = "username",
        labelField = "username",
        searchField = c("username", paste0("name")),
        render = I(
          '{ option: function(item, escape) {
if (item.name !== null) {
return "<div><strong>" + escape(item.username) + "</strong> (" + escape(item.name) +") </div>" } else {
return "<div><strong>" + escape(item.username) + "</div>"
}
}
}'
        )
      ) # list
    ) # updateSelectizeInput

    updateSelectizeInput(
      session,
      checklist_input_id,
      selected = isolate(input[[checklist_input_id]])
    )

  }
}

#' Extract File Data from Selected Items
#'
#' This function extracts file data from a hierarchical structure of selected items.
#' It collects the checklist and assignee information for each file and returns a structured list.
#'
#' @param input A list containing input parameters, specifically the values of checklist and assignee inputs.
#' @param items A list representing the selected items, typically structured hierarchically.
#' @return A list of structured data for each file, including the file name, assignees, and checklist type.
#'
#' @noRd
extract_file_data <- function(input, items, relevant_files_list, previous_qc) {
  tryCatch(
    {
      debug(.le$logger, glue::glue("Extracting file data for items: {paste(items, collapse = ', ')}"))

      file_data <- list()
      for (name in items) {

        checklist_input_id <- generate_input_id("checklist", name)
        assignee_input_id <- generate_input_id("assignee", name)

        checklist_input_value <- input[[checklist_input_id]]
        assignee_input_value <- input[[assignee_input_id]]
        if (!isTruthy(assignee_input_value) || assignee_input_value == "No assigned QCer") {
          assignee_input_value <- NULL
        }

        # requires the widget and input to be available before proceeding
        if (!isTruthy(checklist_input_value) || checklist_input_value == "") {
          return(NULL)
        }

        relevant_files <- relevant_files_list[[name]]
        if (length(relevant_files) > 0) {
          relevant_file_data <- lapply(relevant_files, function(file) {
            name_input_id <- paste0("name_", file)
            note_input_id <- paste0("note_", file)

            file_name <- if (!is.null(input[[name_input_id]])) input[[name_input_id]] else basename(file)
            file_note <- if (!is.null(input[[note_input_id]])) input[[note_input_id]] else ""

            list(
              file_path = file,
              name = file_name,
              note = file_note
            )
          })
        }
        else {
           relevant_file_data <- NULL
        }

        previous_qc <- previous_qc[[name]]
        if (length(previous_qc) == 4) {
          previous_qc_data <- glue::glue_collapse(c(previous_qc$comment_body_parts[1], previous_qc$message, "\n\n", previous_qc$comment_body_parts[2]))
        }
        else {
          previous_qc_data <- NULL
        }

        file_data <- append(file_data,
                            list(create_file_data_structure(
                              file_name = generate_input_id(name = name),
                              assignees = assignee_input_value,
                              checklist_type = checklist_input_value,
                              previous_qc = previous_qc_data,
                              relevant_files =  relevant_file_data
                            ))
                      )
      } #for
      debug(.le$logger, "Extracted file data successfully")
      return(file_data)
    },
    error = function(e) {
      log4r::error(glue::glue("Error extracting data from selected {items}: {conditionMessage(e)}"))
      rlang::abort(conditionMessage(e))
    }
  )
}


#' Flatten a List to UI Elements
#'
#' This function takes a list with named elements and flattens it into a series of UI elements
#' suitable for rendering in a Shiny application. It converts each item in the list
#' to a bullet point (`<li>` tag) and adds a bold title (`<b>` tag) for each named
#' sub-list. It also adds a line break (`<br>` tag) before all but the first named
#' sub-list.
#'
#' @param checklists A list containing the items to be converted to UI elements.
#' @param parent_name A character string representing the name of the current element.
#' @param is_first A logical value indicating whether the current list is the first
#'   in the hierarchy. If value is first, there is no additional line break. Aesthetic only.
#'
#' @return A list of UI elements created from the checklists.
#' @noRd
convert_list_to_ui <- function(checklists, parent_name = NULL, is_first = TRUE) {
  ui_elements <- list()
  debug(.le$logger, glue::glue("Converting list to UI with parent name: {parent_name}, is first: {is_first}"))

  if (!is.null(parent_name)) {
    if (!is_first) {
      ui_elements <- list(ui_elements, tags$br(), tags$b(parent_name))
    } else {
      ui_elements <- list(ui_elements, tags$b(parent_name))
    }
  }

  if (is.character(checklists)) {
    ui_elements <- c(ui_elements, lapply(checklists, tags$li))
  } else if (is.list(checklists)) {
    first_child <- TRUE
    for (name in names(checklists)) {
      ui_elements <- c(ui_elements, convert_list_to_ui(checklists[[name]], name, is_first && first_child))
      first_child <- FALSE
    }
  } else {
    error(.le$logger, glue::glue("{get_checklist_display_name_var(capitalized = TRUE)} not supported: {checklists}"))
    rlang::abort(glue::glue("Unsupported type of {get_checklist_display_name_var()}"))
  }
  debug(.le$logger, "Converted list to UI successfully")
  return(ui_elements)
}

#' Create Button Preview Event
#'
#' This function creates an event observer for the preview buttons created in `render_selected_list()`.
#' When the button is clicked, it displays a modal dialog with the content of the specified file.
#'
#' @param input A reactive list of inputs from a Shiny session.
#' @param name A character string representing the name of the item associated with the button.
#'
#' @return None. The function creates an observer event and does not return any value.
#' @noRd
create_button_preview_event <- function(input, name) {
  tryCatch(
    {
      file_preview_id <- generate_input_id("button", name)
      clean_name <- generate_input_id(name = name)
      if (stringr::str_detect(clean_name, exclude_patterns())) {
        print <- "File preview not available for binary files."

      }
      else {
        print <- renderPrint(cat(readLines(clean_name), sep = "\n"))
      }

      observeEvent(input[[file_preview_id]],
        {
          showModal(
            modalDialog(
              title = tags$div(tags$span(glue::glue("File Preview"), style = "float: left; font-weight: bold; font-size: 20px; margin-top: 5px;"),
                               modalButton("Dismiss"),
                               style = "text-align: right;"
              ),
              footer = NULL,
              easyClose = TRUE,
              renderUI({
                print
              })
            )
          )
        },
        ignoreInit = TRUE
      )
      debug(.le$logger, glue::glue("Created button preview event for item: {name} successfully"))
    },
    error = function(e) {
      log4r::error(glue::glue("Error creating observe event for item {name}: {conditionMessage(e)}"))
      rlang::abort(conditionMessage(e))
    }
  )
}


render_markdown_html <- function(md_string) {
  md_file <- tempfile(fileext = ".md")
  writeLines(md_string, md_file)

  html_file <- tempfile(fileext = ".html")
  suppressMessages(suppressWarnings(
    rmarkdown::pandoc_convert(
      input = md_file,
      to = "html5",
      from = "gfm",
      output = html_file,
      options = c("--standalone", "--metadata", "title= ")
    )
  ))

  html <- paste(readLines(html_file, warn = FALSE), collapse = "\n")
  HTML(html)
}

#' @import shiny
#' @importFrom glue glue
#' @importFrom log4r warn error info debug
#' @importFrom shinyjs enable disable addClass removeClass delay
create_checklist_preview_event <- function(input, name, checklists) {
  tryCatch(
    {
      preview_input_id <- generate_input_id("preview", name)
      checklist_input_id <- generate_input_id("checklist", name)

      observeEvent(input[[preview_input_id]], {
        selected_checklist <- input[[checklist_input_id]]

        if (selected_checklist == "") {
          showModal(
            modalDialog(
              title = tags$div(tags$span(glue::glue("{get_checklist_display_name_var(capital = TRUE)} Preview"), style = "float: left; font-weight: bold; font-size: 20px; margin-top: 5px;"),
                               modalButton("Dismiss"),
                               style = "text-align: right;"
              ),
              footer = NULL,
              easyClose = TRUE,
              renderUI({
                glue::glue("Select a {get_checklist_display_name_var()} to preview in the {get_checklist_display_name_var(capitalized = TRUE)} dropdown.")
              })
            )
          )
        }
        else { # else a checklist is selected in the dropdown
          if (selected_checklist %in% names(checklists$yaml)) {
            info <- checklists$yaml[[selected_checklist]]

            log_string <- glue::glue_collapse(info, sep = "\n")
            debug(.le$logger, glue::glue("Items found in the {get_checklist_display_name_var()}: \n{log_string}"))

            list <- convert_list_to_ui(info) # checklists needs additional formatting for list of named elements
            checklist_rendered <- tags$ul(list)
          }
          else {
            info <- checklists$txt[[selected_checklist]]
            checklist_rendered <- render_markdown_html(info)
          }

          showModal(
            modalDialog(
              title = tags$div(tags$span(glue::glue("{get_checklist_display_name_var(capital = TRUE)} Preview"), style = "float: left; font-weight: bold; font-size: 20px; margin-top: 5px;"),
                               modalButton("Dismiss"),
                               style = "text-align: right;"
              ),
              footer = NULL,
              easyClose = TRUE,
              renderUI({
                header <- tags$h1(selected_checklist, style = "margin-top: 0; margin-bottom: 0.5em;")
                tagList(
                  header,
                  checklist_rendered
                )
              })
            )
          )
        }

      },
      ignoreInit = TRUE)

      debug(.le$logger, glue::glue("Created button preview event for item: {name} successfully"))
    },
    error = function(e) {
      log4r::error(glue::glue("Error creating observe event for item {name}: {conditionMessage(e)}"))
      rlang::abort(conditionMessage(e))
    }
  )
}




#' Associate Relevant Files Event
#'
#'
#' @param input A reactive list of inputs from a Shiny session.
#' @param name A character string representing the name of the item associated with the button.
#'
#' @return A list of files selected with name, path, and note
#' @noRd
associate_relevant_files_button_event <- function(input, output, name, ns, root_dir, relevant_files) {
  tryCatch(
    {
      # init metadata
      file_meta <- reactiveVal(list())

      associate_relevant_files_id <- generate_input_id("associate_relevant_files", name)
      filtered_file_selector_id <- generate_input_id("filtered_file_selector", name)
      clean_name <- generate_input_id(name = name)

      # make paths relative, remove renv, remove selected file itself
      filter_files <- function(dir) {
        all_files <- list.files(dir, full.names = TRUE, recursive = TRUE)
        filtered_files <- all_files[!grepl("renv", all_files)]
        relative_paths <- fs::path_rel(filtered_files, root_dir)
        files_without_qc_file <- relative_paths[relative_paths != clean_name]
        return(files_without_qc_file)
      }

      filtered_files <- reactive({
        filter_files(root_dir)
      })

      # when a relevant file is added/subtracted
      observeEvent(input[[filtered_file_selector_id]], {
        req(input[[filtered_file_selector_id]])
        selected_files <- if (!is.null(input[[filtered_file_selector_id]])) input[[filtered_file_selector_id]] else character(0)

        # saving metadata stops name and note from disappearing when relevant files are added/subtracted

        lapply(selected_files, function(file) {
          current_meta <- file_meta()
          current_meta[[file]]$name <- input[[paste0("name_", file)]]
          current_meta[[file]]$note <- input[[paste0("note_", file)]]
          file_meta(current_meta)
        })

      })

      # click "Associate files"
      observeEvent(input$add_files, ignoreInit = FALSE, {
        selected_files <- input[[filtered_file_selector_id]]

        # update current metadata
        current_meta <- file_meta()
        current_meta <- lapply(selected_files, function(file) {
          current_meta[[file]]$name <- input[[paste0("name_", file)]]
          current_meta[[file]]$note <- input[[paste0("note_", file)]]
          file_meta(current_meta)
          current_meta[[file]]
        })
        names(current_meta) <- selected_files
        current_meta <- current_meta[selected_files] # remove metadata for unselected files
        file_meta(current_meta)

        valid_files <- intersect(selected_files, filtered_files())
        updated_relevant_files <- relevant_files()

        if (is.null(valid_files)) {
          updated_relevant_files[[name]] <- NULL
        }
        else {
          updated_relevant_files[[name]] <- selected_files
        }
        relevant_files(updated_relevant_files)

        debug(.le$logger, glue::glue("Files associated for {name}: {paste({selected_files}, collapse = ', ')}"))
        removeModal()
      })

      observeEvent(input[[associate_relevant_files_id]], {
        removeModal() # avoid modal stuttering

        # left pane
        output[[filtered_file_selector_id]] <- renderUI({
          current_files <- relevant_files()[[name]]

          available_files <- filtered_files()
          choices <- c("None" = "", available_files)

          valid_selected_files <- intersect(current_files, available_files)

          updated_relevant_files <- relevant_files()
          updated_relevant_files[[name]] <- valid_selected_files
          relevant_files(updated_relevant_files)


          selectInput(
            ns(filtered_file_selector_id),
            label = tags$div(
              style = "word-wrap: break-word; word-break: break-word; white-space: normal;",
              glue::glue("Associate relevant files to {name}:")
            ),
            choices = choices,
            selected = if (length(valid_selected_files) == 0) "" else valid_selected_files,
            multiple = TRUE,
          )
        }) # left pane

        # right pane
        output[[paste0(filtered_file_selector_id, "_selected")]] <- renderUI({
          selected_files <- if (!is.null(input[[filtered_file_selector_id]])) input[[filtered_file_selector_id]] else character(0)
          current_meta <- file_meta()

          if (length(selected_files) == 0) {
            return(tags$div("No files selected.", style = "font-size: 14px; color: #999;"))
          }

          # input boxes
          ui_elements <- lapply(selected_files, function(file) {
            # pre-fill with meta if set previously
            # if meta is null, make name the basename and note blank
            meta <- if (!is.null(current_meta[[file]])) current_meta[[file]] else list(name = basename(file), note = "")

            tags$div(
              style = "margin-bottom: 15px; padding: 10px; border: 1px solid #ccc; border-radius: 5px;",
              tags$div(
                style = "padding-bottom: 7px; font-weight: bold; word-wrap: break-word; word-break: break-word; white-space: normal; overflow-wrap: break-word;",
                file
              ),
              textInput(
                ns(paste0("name_", file)),
                label = "Name",
                value = meta$name,
                placeholder = "(Optional)"
              ),
              textAreaInput(
                ns(paste0("note_", file)),
                label = "Note",
                value = meta$note,
                rows = 2,
                placeholder = "(Optional)"
                )
            )
          }) # ui_elements
          do.call(tagList, ui_elements)
        }) # right pane

        showModal(
          modalDialog(
            title = tags$div(
              tags$span("Associate relevant files",
                        style = "float: left; font-weight: bold; font-size: 20px; margin-top: 5px;"),
              tags$div(
                style = "text-align: right;",
                actionButton(ns("return"), "Cancel", style = "color: #a94442;"),
                actionButton(ns("add_files"), "Associate")
              )
            ),
            footer = NULL,
            easyClose = TRUE,
            fluidRow(
              # context message about relevant files
              column(
                width = 12,
                div(
                  "Select files related to the QC file that the QCer may want to reference during review: e.g. data, spec, or helper function files.",
                  style = "margin-bottom: 10px; color: #333;"
                )
              ),
              # left pane
              column(6, uiOutput(ns(filtered_file_selector_id))),
              # right pane
              column(6, uiOutput(ns(paste0(filtered_file_selector_id, "_selected"))))
            )
          )
        )
      }, ignoreInit = TRUE)

      debug(.le$logger, glue::glue("Created associate relevant files event for item: {name} successfully"))
    },
    error = function(e) {
      log4r::error(glue::glue("Error creating associate relevant files event for item {name}: {conditionMessage(e)}"))
      rlang::abort(conditionMessage(e))
    }
  )
}



post_qc_history_button_event <- function(input, output, name, ns, all_milestone_objects, previous_qc_rv) { # TODO
  tryCatch({

    issues_rv <- reactiveVal(list())

    milestone_id <- generate_input_id("milestone_selector", name)
    issue_id <- generate_input_id("issue_selector", name)
    message_id <- generate_input_id("message_input", name)
    history_input_id <- generate_input_id("history", name)

    observeEvent(input[[history_input_id]], {
    removeModal()
    all_milestone_names <- get_milestone_names_from_milestone_objects(all_milestone_objects)

    output[[milestone_id]] <- renderUI({
      selectInput(
        ns(milestone_id),
        "Milestone",
        choices = c("(Required)" = "", all_milestone_names)
      )
    })


    output[[issue_id]] <- renderUI({
      milestone_name <- input[[milestone_id]]
      req(milestone_name)
      milestone_number <- get_milestone_number_from_all_milestones(milestone_name = milestone_name,
                                                                   milestone_objects = all_milestone_objects)

      issues <- get_all_issues_in_milestone_from_milestone_number(milestone_number, milestone_name)
      issues_rv(issues)
      issue_choices <- convert_issue_df_format(issues)

      selectInput(
        ns(issue_id),
        "Issue",
        choices = c("(Required)" = "", issue_choices)
      )
    })


      output[[message_id]] <- renderUI({
        textAreaInput(
          ns(message_id),
          "Message",
          value = "",
          rows = 3,
          placeholder = "Describe changes since the previous QC"
        )
      })

      output[[paste0(name, "_diff_preview")]] <- renderUI({
        issue_display <- input[[issue_id]]
        req(issue_display)
        issue_parts <- split_issue_parts(issue_display)
        previous_issue_number <- issue_parts$issue_number
        issue <- get_issue_from_all_issues(issue_parts$issue_title, issues_rv())
        remote_commits <- get_remote_commits(current_branch = gert::git_branch())
        last_remote_commit <- remote_commits[1]
        comparator_file_path <- generate_input_id(name = name)

        initial_qc_commit <- get_init_qc_commit_from_issue_body(issue$body)
        qc_commit_info <- get_qc_commit_info(file_name = issue$title,
                                             issue_body = issue$body,
                                             num_comments = issue$comments,
                                             comments_url = issue$comments_url,
                                             initial_qc_commit = initial_qc_commit
                                             )
        latest_qc_commit <- qc_commit_info$latest_qc_commit

        comment_body_parts <- create_previous_qc_comment_body(diff = TRUE,
                                                              reference_file_path = issue$title,
                                                              comparator_file_path = comparator_file_path,
                                                              reference_commit = latest_qc_commit,
                                                              comparator_commit = last_remote_commit,
                                                              previous_issue_number = previous_issue_number)
        # save to reactiveVal
        meta <- previous_qc_rv()
        if (is.null(meta[[name]])) meta[[name]] <- list()
        meta[[name]]$comment_body_parts <- comment_body_parts
        previous_qc_rv(meta)

        comment_body <- glue::glue_collapse(comment_body_parts)
        html_file_path <- create_gfm_file(comment_body)
        custom_html <- readLines(html_file_path, warn = FALSE) %>% paste(collapse = "\n")

        div(
          HTML(custom_html)
        )
      })

      showModal(
        modalDialog(
          title = tags$div(
            tags$span("Associate previous QC", style = "float: left; font-weight: bold; font-size: 20px; margin-top: 5px;"),
            tags$div(
              style = "text-align: right;",
              actionButton(ns("return"), "Cancel", style = "color: #a94442;"),
              actionButton(ns("post_qc_history"), "Associate")
            )
          ),
          footer = NULL,
          easyClose = TRUE,
           uiOutput(ns(milestone_id)),
           uiOutput(ns(issue_id)),
           uiOutput(ns(message_id)),
           uiOutput(ns(paste0(name, "_diff_preview")))
        )
      )
    })

      observeEvent(input$post_qc_history, {
        req(input[[issue_id]], input[[milestone_id]])

        # Save metadata
        meta <- previous_qc_rv()
        if (is.null(meta[[name]])) meta[[name]] <- list()

        meta[[name]]$milestone <- input[[milestone_id]]
        meta[[name]]$issue <- input[[issue_id]]
        meta[[name]]$message <- input[[message_id]]
        # comment_body_parts is already saved by the preview step

        previous_qc_rv(meta)

        removeModal()
      })

    debug(.le$logger, glue::glue("Created associate previous QC event for item: {name} successfully"))
  },
  error = function(e) {
    log4r::error(glue::glue("Error creating associate previous QC event for item {name}: {conditionMessage(e)}"))
    rlang::abort(conditionMessage(e))
  })
}
