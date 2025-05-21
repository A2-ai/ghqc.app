#' @import shiny
#' @importFrom miniUI miniPage gadgetTitleBar miniContentPanel miniButtonBlock
#' @importFrom shinyjs useShinyjs
#' @importFrom waiter use_waiter waiter_show_on_load spin_1
NULL

ghqc_status_ui <- function(id) {
  ns <- NS(id)
  ui <- miniPage(
    use_waiter(),
    useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "ghqc.app/css/styles.css"),
      tags$script(HTML(glue::glue("var ns_prefix = '{ns(\"\")}';"))),
      tags$script(src = "ghqc.app/js/status.js"),
      tags$script(type = "module", src = "ghqc.app/js/toggle_sidebar.js"),
      tags$style(HTML((
        ".gadget-absfill {
          top: 0 !important;
          bottom: 0 !important;
        }"
      )))
    ), # tags$head
    waiter_show_on_load(
      html = tagList(
        spin_1(),
        h4("Loading in...", style = "color: white;")
      ),
      color = "darkgrey"
    ),
    div(
      id = ns("main_container"),
      gadgetTitleBar(title = div(
        style = "display: inline-flex; align-items: center; justify-content: center; width: 100%; height: 100%;",
        div(
          style = "position: relative; flex-shrink: 0; width: 50px; height: 50px;",
          tags$img(src = "ghqc.app/ghqc_hex.png", class = "logo-img", style = "height: 46px; !important;") # this is important for style priority so logo is the correct size
        ),
        div("Status QC file(s)", style = "white-space: nowrap;")
      ),
      left = actionButton(ns("close"), "Close", class = "btn-sm"),
      right = actionButton(ns("reset"), "Reset", class = "btn-sm")
      ),
      div(
        id = ns("content"),
        uiOutput(ns("sidebar")),
        div(
          id = ns("divider"),
          actionButton(ns("toggle_sidebar"), "", icon = icon("angle-double-left"), class = "toggle-sidebar-btn")
        ),
        miniContentPanel(
          uiOutput(ns("main_panel_dynamic"))
        )
      )
    )
  ) # miniPage

  return(ui)
}
