#' @import shiny
#' @importFrom miniUI miniPage gadgetTitleBar miniContentPanel miniButtonBlock
#' @importFrom shinyjs useShinyjs
#' @importFrom waiter use_waiter waiter_show_on_load spin_1
NULL

ghqc_resolve_ui <- function(id) {
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
        h4("Loading in ...", style = "color: white;")
      ),
      color = "darkgrey"
    ),
    div(
      id = ns("main_container"),
      gadgetTitleBar(
        title = div(
          style = "display: inline-flex; align-items: center; justify-content: center; width: 100%; height: 100%;",
          div(
            style = "position: relative;",  # Keep this div centered
            tags$img(src = "ghqc.app/ghqc_hex.png", height = 46, class = "logo-img", style = "position: relative; left: -18px; margin-right: 10px; top: -2px;")
          ),
          div("Resolve QC finding(s)", style = "white-space: nowrap;")
        ),
        left = actionButton(ns("close"), "Close", class = "btn-sm"),
        right = actionButton(ns("reset"), "Reset", class = "btn-sm")
      ),
      miniContentPanel(
        div(
          id = ns("center_content"),
          selectInput(ns("select_milestone"), "Filter Issues by Milestone", choices = "", multiple = FALSE),
          selectInput(ns("select_issue"), "Select Issue", choices = "", multiple = FALSE),
          textAreaInput(ns("message"), "Message", "", placeholder = "(optional)"),
          checkboxInput(ns("show_diff"), "Show file difference", TRUE),
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
          actionButton(ns("preview"), "Preview Comment"),
          actionButton(ns("post"), "Post Comment")
        )
      )
    )
  )
  return(ui)
}
