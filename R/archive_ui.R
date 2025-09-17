#' @import shiny
#' @importFrom miniUI miniPage gadgetTitleBar miniContentPanel miniButtonBlock
#' @importFrom shinyjs useShinyjs
#' @importFrom waiter use_waiter waiter_show_on_load spin_1
NULL

ghqc_archive_ui <- function(id) {
  ns <- NS(id)
  ui <- miniPage(
    use_waiter(),
    useShinyjs(),
    # tags$head(tags$style(HTML(brio::read_file(system.file("css/styles.css", package = "ghqc.app"))))),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "ghqc.app/css/styles.css"),
      tags$script(type = "module", src = "ghqc.app/js/adjust_grid.js"),
      tags$script(type = "module", src = "ghqc.app/js/toggle_sidebar.js"),
      tags$style(HTML("
    ::placeholder {
      color: #8e8e8e; /* match colors of placeholders */
    }
  "))
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
      gadgetTitleBar(title = div(
        style = "display: inline-flex; align-items: center; justify-content: center; width: 100%; height: 100%;",
        div(
          style = "position: relative; flex-shrink: 0; width: 50px; height: 50px;",
          tags$img(src = "ghqc.app/ghqc_hex.png", class = "logo-img", style = "height: 46px; !important;") # this is important to ensure style priority so logo is the correct size
        ),
        div("Archive QC file(s)", style = "white-space: nowrap;")
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
        miniContentPanel(tagList(
          uiOutput(ns("main_panel_static")),  # Static button
          uiOutput(ns("main_panel_dynamic"))  # Reactive content
        ))
      ),
      div(
        class = "button_block",
        miniButtonBlock(
          actionButton(ns("create_qc_items"), "Archive file(s)")
        )
      )
    )
  )

  return(ui)
}
