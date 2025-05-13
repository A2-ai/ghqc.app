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
      tags$script(type = "module", src = "ghqc.app/js/adjust_grid.js"),
      tags$script(type = "module", src = "ghqc.app/js/toggle_sidebar.js"),
      tags$style(HTML("
    ::placeholder {
      color: #8e8e8e; /* match colors of placeholders */
    }
  ")),
      tags$style(HTML("
  .checkbox label {
    display: flex;
    justify-content: space-between;
    align-items: center;
    width: 100%;
    font-weight: 600;
    font-size: 13px;
    color: #333;
  }
")),
      tags$script(HTML(glue::glue("
  Shiny.onInputChange('{ns('show_qcer')}', document.getElementById('{ns('show_qcer')}').checked);
  document.getElementById('{ns('show_qcer')}').addEventListener('change', function() {{
    Shiny.setInputValue('{ns('show_qcer')}', this.checked);
  }});
"))),
      tags$style(HTML("
div[id$='-sidebar'] {
    width: 200px;
    min-width: 200px;
    max-width: 200px;
    flex: 0 0 0;
    overflow-y: auto;
    transition: all 0.3s ease;
    display: flex;
    flex-direction: column;
  }

  .sidebar-collapsed div[id$='-sidebar'] {
    width: 0 !important;
    min-width: 0 !important;
    max-width: 0 !important;
    padding: 0 !important;
    overflow: hidden !important;
    flex: 0 0 0 !important;
  }

  .sidebar-collapsed div[id$='-sidebar'] * {
    opacity: 0;
    transition: opacity 0.2s ease;
  }

  div[id$='-sidebar'] * {
    opacity: 1;
    transition: opacity 0.2s ease;
  }
")),
      tags$script(HTML("
  $(document).on('click', 'button[id^=\"ghqc_status_app-modal_btn_\"]', function() {
    Shiny.setInputValue(this.id, Math.random());
  });
")),
      tags$style(HTML("
  table.dataTable td, table.dataTable th {
    border-left: none !important;
    border-right: none !important;
  }
")),
      tags$style(HTML("
  .dataTables_wrapper .dataTables_scrollBody {
    height: auto !important;
    max-height: none !important;
    min-height: 20px !important;
  }
")),
      tags$style(HTML("
  .gadget-absfill {
    top: 0 !important;
    bottom: 0 !important;
  }
")),
      tags$style(HTML("
  .dataTables_wrapper .dataTables_info {
    text-align: left !important;
    float: left !important;
    padding-left: 0 !important;
    margin-left: 0 !important;
    padding: 1px !important;
    margin: 0 !important;
  }
"))
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
