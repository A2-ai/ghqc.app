strip_variant <- function(label) {
  sub("\\s*\\(.*\\)$", "", label)
}

#' @importFrom rlang .data
get_most_recent_milestone <- function(all_ghqc_milestones) {
  milestone_df <- all_ghqc_milestones |>
    purrr::map_dfr(~ dplyr::tibble(
      title = .x$title,
      created_at = as.POSIXct(.x$created_at, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
      )) |>
    dplyr::arrange(dplyr::desc(.data$created_at))

  latest_milestone <- milestone_df[1, ]$title
}

dropdown_split_button_html <- function(id, default, options) {
  button_items <- lapply(options, function(option) {
    btn_class <- switch(option,
                        "Approve" = "btn-success",
                        "Notify file changes" = "btn-info",
                        "Notify last remote commit" = "btn-plum",
                        "Repost last QC notification" = "btn-plum",
                        "Unapprove (danger)" = "btn-danger",
                        "Unapprove (light)" = "btn-light",
                        "btn-default"
    )
    display_label <- strip_variant(option)

    glue::glue(
      '<li><a class="btn {btn_class} btn-sm btn-block dropdown-btn" href="#"
      onclick="triggerDefaultAction(\'{id}\', \'{option}\')">{display_label}</a></li>'
    )
  })

  dropdown_menu <- paste(button_items, collapse = "\n")

  default_class <- switch(default,
                          "Approve" = "btn-success",
                          "Notify file changes" = "btn-info",
                          "Notify last remote commit" = "btn-plum",
                          "Repost last QC notification" = "btn-plum",
                          "Unapprove" = "btn-danger"
  )

  glue::glue('
      <div class="btn-group">
        <button id="main-btn-{id}" type="button" class="btn btn-sm {default_class}"
          onclick="triggerDefaultAction(\'{id}\', \'{default}\')">
          {default}
        </button>
        <button id="caret-btn-{id}" type="button" class="btn btn-sm {default_class} dropdown-toggle"
          data-toggle="dropdown" aria-expanded="false">
          <span class="caret"></span>
          <span class="sr-only">Toggle Dropdown</span>
        </button>
        <ul class="dropdown-menu custom-dropdown" role="menu">
          {dropdown_menu}
        </ul>
      </div>
    ')
}

generate_action_ui <- function(id, options, message = NULL) {
  if (length(options) == 0) {
    glue::glue('{message %||% \"\"}')
  } else if (length(options) == 1) {
    label <- options[1]
    display_label <- strip_variant(label)
    btn_class <- switch(label,
                        "Approve" = "btn-success",
                        "Notify file changes" = "btn-info",
                        "Notify last remote commit" = "btn-plum",
                        "Repost last QC notification" = "btn-plum",
                        "Unapprove (danger)" = "btn-danger",
                        "Unapprove (light)" = "btn-light",
                        "btn-default"
    )
    glue::glue('
  <button id="main-btn-{id}" type="button" class="btn btn-sm {btn_class}"
    onclick="triggerDefaultAction(\'{id}\', \'{label}\')">
    {display_label}
  </button>
')
  }
  else {
    default <- options[1]
    dropdown_split_button_html(id, default, options)
  }
}
