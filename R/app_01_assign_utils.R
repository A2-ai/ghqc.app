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
render_selected_list <- function(input, ns, items = NULL, checklist_choices = NULL, depth = 0, relevant_files = NULL, output) {
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

      error_message <- glue::glue("Error rendering selected {items}: {e$message}")
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
        placeholder = "QCer (optional)",
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

        #relevant_files <- input[[filtered_file_selector_id]] %||% character(0)
        relevant_files <- relevant_files_list[[name]]

        if (length(relevant_files) > 0) {
          relevant_file_data <- lapply(relevant_files, function(file) {
            name_input_id <- paste0("name_", file)
            note_input_id <- paste0("note_", file)

            file_name <- input[[name_input_id]] %||% basename(file)
            file_note <- input[[note_input_id]] %||% ""

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
      log4r::error(glue::glue("Error extracting data from selected {items}: {e$message}"))
      rlang::abort(e$message)
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

      observeEvent(input[[file_preview_id]], # input[[checklist_id_specific to file]]
        {
          showModal(
            modalDialog(
              title = tags$div(modalButton("Dismiss"), style = "text-align: right;"),
              footer = NULL,
              easyClose = TRUE,
              renderUI({
                renderPrint(cat(readLines(clean_name), sep = "\n")) # checklist specfic
              })
            )
          )
        },
        ignoreInit = TRUE
      )
      debug(.le$logger, glue::glue("Created button preview event for item: {name} successfully"))
    },
    error = function(e) {
      log4r::error(glue::glue("Error creating observe event for item {name}: {e$message}"))
      rlang::abort(e$message)
    }
  )
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
              title = tags$div(modalButton("Dismiss"), style = "text-align: right;"),
              footer = NULL,
              easyClose = TRUE,
              renderUI({
                glue::glue("Select a {get_checklist_display_name_var()} to preview in the {get_checklist_display_name_var(capitalized = TRUE)} dropdown.")
              })
            )
          )
        }
        else {
          info <- checklists[[selected_checklist]]
          showModal(
            modalDialog(
              title = tags$div(modalButton("Dismiss"), style = "text-align: right;"),
              footer = NULL,
              easyClose = TRUE,
              renderUI({
                header <- tags$h3(selected_checklist)
                list <- convert_list_to_ui(info) # checklists needs additional formatting for list of named elements
                tagList(
                  header,
                  tags$ul(list)
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
      log4r::error(glue::glue("Error creating observe event for item {name}: {e$message}"))
      rlang::abort(e$message)
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
        selected_files <- input[[filtered_file_selector_id]] %||% character(0)

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
          selected_files <- input[[filtered_file_selector_id]] %||% character(0)
          current_meta <- file_meta()

          if (length(selected_files) == 0) {
            return(tags$div("No files selected.", style = "font-size: 14px; color: #999;"))
          }

          # input boxes
          ui_elements <- lapply(selected_files, function(file) {
            # pre-fill with meta if set previously
            # if meta is null, make name the basename and note blank
            meta <- current_meta[[file]] %||% list(name = basename(file), note = "")

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
                placeholder = "(optional)"
              ),
              textAreaInput(
                ns(paste0("note_", file)),
                label = "Note",
                value = meta$note,
                rows = 2,
                placeholder = "(optional)"
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
                actionButton(ns("add_files"), "Associate files", style = "margin-right: 10px;"),
                modalButton("Dismiss")
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
      log4r::error(glue::glue("Error creating associate relevant files event for item {name}: {e$message}"))
      rlang::abort(e$message)
    }
  )
}
