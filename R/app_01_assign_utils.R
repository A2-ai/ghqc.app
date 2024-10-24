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
  # clean_name <- gsub("[^a-zA-Z0-9/_.-]", "", name)
  if (is.null(prefix)) {
    return(name)
  } else {
    return(paste0(prefix, "_", name))
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
render_selected_list <- function(input, ns, iv, items = NULL, checklist_choices = NULL, depth = 0) {
  tryCatch(
    {
      debug(.le$logger, glue::glue("Rendering selected list with items: {paste(items, collapse = ', ')}"))
      checklist_choices <- stats::setNames(names(checklist_choices), names(checklist_choices))
      ul <- div(class = paste("grid-container", "depth", depth, sep = "-")) # if i remove depth, it won't take styles anymore

      for (name in items) {
        checklist_input_id <- generate_input_id("checklist", name)
        assignee_input_id <- generate_input_id("assignee", name)
        button_input_id <- generate_input_id("button", name)
        preview_input_id <- generate_input_id("preview", name)

        assignee_input <- selectizeInput(
          ns(assignee_input_id),
          label = NULL,
          choices = c("No assignee", input$assignees),
          width = "100%",
          options = list(placeholder = "No assignee")
        )

        checklist_input <- selectizeInput(
          ns(checklist_input_id),
          label = NULL,
          choices = c("", checklist_choices), #select checklist (required)
          width = "100%",
          selected = NULL,  # Ensures no default selection
          options = list(placeholder = "Checklist")
        )

        button_input <- actionButton(
          ns(button_input_id),
          label = HTML("<span>Preview file<br>contents</span>"),
          style = "height: 34px !important; font-size: 12px !important; padding: 2px 2px 2px 2px !important; color: #5f5f5f !important; line-height: 1.2em",
          #style = "min-width: auto; display: inline-block; text-align: center; line-height: 2em; height: 2em;",
          class = "file-preview-button"
        )

        preview_input <- actionButton(
          ns(preview_input_id),
          label = HTML("<span>Preview<br>checklist</span>"),
          style = "height: 34px !important; font-size: 12px !important; padding: 2px 2px 2px 2px !important; color: #5f5f5f !important; line-height: 1.2em",
          #style = "min-width: auto; display: inline-block; text-align: center; line-height: 2em; height: 2em;",
          #class = "checklist-preview-button"
        )

        # no css only way to set line breaks on certain chr; used <wbr> to designate non-alphanumeric values as wbr (https://stackoverflow.com/a/24489931)
        modified_name <- gsub("([^a-zA-Z0-9])", "\\1<wbr>", generate_input_id(name = name))

        ul <- tagAppendChild(ul, div(HTML(modified_name), style = "padding-bottom: 5px;"))

        ul <- tagAppendChild(ul,
                             div(
                               class = "grid-items",
                               div(class = "item-a", button_input),
                               div(class = "item-b", assignee_input),
                               div(class = "item-c", checklist_input),
                               div(class = "item-d", preview_input)
                             )
        )
      }
      debug(.le$logger, "Rendered selected list successfully")
      ul
    },
    error = function(e) {
      log4r::error(glue::glue("Error rendering selected {items}: {e$message}"))
      rlang::abort(e$message)
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
isolate_rendered_list <- function(input, session, items) {
  for (name in items) {
    debug(.le$logger, glue::glue("Updating selectize inputs for item: {name}"))

    assignee_input_id <- generate_input_id("assignee", name)

    checklist_input_id <- generate_input_id("checklist", name)

    updateSelectizeInput(
      session,
      assignee_input_id,
      choices = c("No assignee", input$assignees),
      selected = isolate(input[[assignee_input_id]])
    )

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
extract_file_data <- function(input, items) {
  tryCatch(
    {
      debug(.le$logger, glue::glue("Extracting file data for items: {paste(items, collapse = ', ')}"))

      file_data <- list()
      for (name in items) {
        checklist_input_id <- generate_input_id("checklist", name)
        assignee_input_id <- generate_input_id("assignee", name)
        preview_input_id <- generate_input_id("preview", name)

        checklist_input_value <- input[[checklist_input_id]]
        assignee_input_value <- input[[assignee_input_id]]
        preview_input_value <- input[[preview_input_id]]

        if (!isTruthy(assignee_input_value) || assignee_input_value == "No assignee") {
          assignee_input_value <- NULL
        }
        # requires the widget and input to be available before proceeding
        if (!isTruthy(checklist_input_value) || checklist_input_value == "") {
          return(NULL)
        }



        file_data <- append(file_data, list(create_file_data_structure(file_name = generate_input_id(name = name), assignees = assignee_input_value, checklist_type = checklist_input_value)))
      }
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
    error(.le$logger, glue::glue("Checklist not supported: {checklists}"))
    rlang::abort("Unsupported type of checklist")
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
      button_input_id <- generate_input_id("button", name)
      clean_name <- generate_input_id(name = name)

      observeEvent(input[[button_input_id]], # input[[checklist_id_specific to file]]
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
create_checklist_preview_event <- function(input, iv, ns, name, checklists) {


  tryCatch(
    {
      preview_input_id <- generate_input_id("preview", name)
      checklist_input_id <- generate_input_id("checklist", name)

      if (is.null(input[[checklist_input_id]])) {
        iv$add_rule(checklist_input_id, shinyvalidate::sv_required())
      }

      observeEvent(input[[checklist_input_id]], {
        checklist_input <- input[[checklist_input_id]]
        if (checklist_input == "") {
          addClass(checklist_input_id, "input-error")
        } else {
          removeClass(checklist_input_id, "input-error")
        }
      })

      observeEvent(input[[preview_input_id]], {
        selected_checklist <- input[[checklist_input_id]]

        if (selected_checklist == "") {
          showModal(
            modalDialog(
              title = tags$div(modalButton("Dismiss"), style = "text-align: right;"),
              footer = NULL,
              easyClose = TRUE,
              renderUI({
                "Select a checklist to preview in the Checklist dropdown."
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
