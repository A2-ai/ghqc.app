#' @import shiny
#' @importFrom miniUI miniPage gadgetTitleBar miniContentPanel miniButtonBlock
#' @importFrom shinyjs useShinyjs
#' @importFrom waiter use_waiter waiter_show_on_load spin_1
NULL

ghqc_record_ui <- function(id) {
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
          div("Generate QC Record", style = "white-space: nowrap;")
        ),
        left = actionButton(ns("close"), "Close", class = "btn-sm"),
        right = actionButton(ns("reset"), "Reset", class = "btn-sm")
      ),
      miniContentPanel(
        div(
          id = ns("center_content"),
          checkboxInput(ns("closed_only"), "Closed Milestones only", TRUE),
          selectizeInput(ns("select_milestone"), "Select Milestones", choices = "", multiple = TRUE),
          textAreaInput(ns("pdf_name"), "PDF Name", placeholder = "(optional)"),
          textAreaInput(ns("pdf_location"), "PDF Location", value = get_simple_path()),
          checkboxInput(ns("just_tables"), "Just tables", FALSE)
        )
      ),
      div(
        class = "button_block",
        miniButtonBlock(
          actionButton(ns("generate_report"), "Generate QC Record")
        )
      )
    )
  )
  return(ui)
}


