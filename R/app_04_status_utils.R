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
