


arcive_render_selected_list <- function(input, ns, items = NULL, checklist_choices = NULL, depth = 0, relevant_files = NULL, output) {
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

        preview_input <- actionButton(
          ns(preview_input_id),
          label = HTML(glue::glue("<span>Preview<br>{get_checklist_display_name_var()}</span>")),
          style = "height: 34px !important; font-size: 12px !important; padding: 2px 2px 2px 2px !important; color: #5f5f5f !important; line-height: 1.2em",
          class = "checklist-preview-button"
        )

        ul <- tagAppendChild(ul, div(class = "item-a", file_preview, style = "padding-bottom: 5px;"))

        ul <- tagAppendChild(ul,
                             div(
                               class = "grid-items",
                               div(class = "item-a", associate_relevant_files),
                               div(class = "item-b", assignee_input),
                               div(class = "item-c", checklist_input),
                               div(class = "item-d", preview_input)
                             )
        )


        # relevant files section
        if (!is.null(relevant_files) && length(relevant_files[[name]]) > 0) {
          relevant_files_list <- tags$ul(
            lapply(relevant_files[[name]], function(file) {
              tags$li(file, style = "font-size: 12px; color: #333; padding: 2px 0;")
            })
          )

          relevant_files_section <- div(
            class = "relevant-files-section",
            style = "padding-bottom: 15px;",
            tags$strong("Relevant files:"),
            relevant_files_list
          )

          ul <- tagAppendChild(ul, relevant_files_section)
        } # if relevant files
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
extract_file_data <- function(input, items, relevant_files_list) {
  tryCatch(
    {
      debug(.le$logger, glue::glue("Extracting file data for items: {paste(items, collapse = ', ')}"))

      file_data <- list()
      for (name in items) {
        checklist_input_id <- generate_input_id("checklist", name)
        assignee_input_id <- generate_input_id("assignee", name)
        preview_input_id <- generate_input_id("preview", name)
        filtered_file_selector_id <- generate_input_id("filtered_file_selector", name)

        checklist_input_value <- input[[checklist_input_id]]
        assignee_input_value <- input[[assignee_input_id]]
        preview_input_value <- input[[preview_input_id]]
        # passing in reactive instead to preserve order of selection
        #filtered_file_selector_value <- input[[filtered_file_selector_id]]

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

        file_data <- append(file_data,
                            list(create_file_data_structure(
                              file_name = generate_input_id(name = name),
                              assignees = assignee_input_value,
                              checklist_type = checklist_input_value,
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
