#' Create a Waiter Overlay
#'
#' This function creates a Waiter overlay with a spinner and a message.
#' It is used for load in messages that cover the entire application.
#'
#' @param ns A namespace function used for handling Shiny modules.
#' @param message A character string representing the message to be displayed in the overlay.
#'
#' @importFrom waiter Waiter spin_1
#' @noRd
create_waiter <- function(ns, message) {
  Waiter$new(
    id = ns("main_container"),
    html = tagList(
      spin_1(),
      h4(sprintf("%s", message), style = "color: white;")
    ),
    color = "darkgrey"
  )
}
