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
