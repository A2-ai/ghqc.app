#' @import shiny
#' @importFrom miniUI miniPage gadgetTitleBar miniContentPanel miniButtonBlock
#' @importFrom shinyjs useShinyjs
#' @importFrom waiter use_waiter waiter_show_on_load spin_1
NULL

ghqc_notify_ui <- function(id) {
  ns <- NS(id)
  ui <- miniPage(
    use_waiter(),
    useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "ghqc.app/css/styles.css"),
    ),
    waiter_show_on_load(
      html = tagList(
        spin_1(),
        h4("Loading in...", style = "color: white;")
      ),
      color = "darkgrey"
    ),
    div(
      id = ns("main_container"),
      gadgetTitleBar(
        title = div(
          style = "display: inline-flex",
          div(
            style = "position: relative; flex-shrink: 0; width: 50px; height: 50px;",
            tags$img(src = "ghqc.app/ghqc_hex.png", class = "logo-img", style = "height: 46px; !important;") # this is important to ensure style priority so logo is the correct size
          ),
          div("Post QC notification(s)", style = "white-space: nowrap;")
        ),
        left = actionButton(ns("close"), "Close", class = "btn-sm"),
        right = actionButton(ns("reset"), "Reset", class = "btn-sm")
      ),
      miniContentPanel(
        div(
          id = ns("center_content"),
          selectInput(ns("select_milestone"), "Filter Issues by Milestone", choices = "", multiple = FALSE),
          selectInput(ns("select_issue"), "Select Issue", choices = "", multiple = FALSE),
          textAreaInput(ns("message"), "Message", "", placeholder = "(Optional)"),
          div(
            id = ns("show_diff_wrap"),
            checkboxInput(ns("show_diff"), "Show file difference", TRUE)
          ),
          radioButtons(ns("compare"), "Compare file versions:",
            inline = TRUE,
            choices = c(
              "Original vs. Current" = "init",
              "Previous vs. Current" = "comparators"
            )
          ),
          conditionalPanel(
            condition = "input.compare === 'comparators'", ns = ns,
            div(
              class = "inline-selectize",
              selectizeInput(ns("ref_commits"), "Previous",
                choices = "",
                multiple = FALSE,
                options = list(
                  placeholder = "No commits since QC initialization."
                )
              ),
              selectizeInput(ns("comp_commits"), "Current",
                choices = "",
                multiple = FALSE,
                options = list(
                  placeholder = "No commits since previous commit."
                )
              )
            )
          )
        )
      ),
      div(
        class = "button_block",
        miniButtonBlock(
          actionButton(ns("post"), "Preview")
        )
      )
    )
  )
  return(ui)
}
