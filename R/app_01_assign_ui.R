#' @import shiny
#' @importFrom miniUI miniPage gadgetTitleBar miniContentPanel miniButtonBlock
#' @importFrom shinyjs useShinyjs
#' @importFrom waiter use_waiter waiter_show_on_load spin_1
NULL

ghqc_assign_ui <- function(id) {
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
        h4("Loading in ...", style = "color: white;")
      ),
      color = "darkgrey"
    ),
    div(
      id = ns("main_container"),
      gadgetTitleBar(title = div(
          style = "display: inline-flex; align-items: center; justify-content: center; width: 100%; height: 100%;",
          div(
            style = "position: relative;",  # Keep this div centered
            tags$img(src = "ghqc.app/ghqc_hex.png", height = 46, class = "logo-img", style = "position: relative; left: -18px; margin-right: 10px; top: -2px;")
          ),
          div("Assign file(s) for QC", style = "white-space: nowrap;")
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
          actionButton(ns("create_qc_items"), "Assign file(s) for QC")
        )
      )
    )
  )

  return(ui)
}
