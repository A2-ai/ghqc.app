get_checklist_display_name_var <- function(capitalized = FALSE, plural = FALSE) {
  checklist_display_name_var <- .le$checklist_display_name_var

  if (is.null(checklist_display_name_var)) {
    checklist_display_name_var <- "checklist"
  }

  if (plural) {
    checklist_display_name_var <- pluralize(checklist_display_name_var)
  }

  if (capitalized) {
    checklist_display_name_var <- capitalize(checklist_display_name_var)
  }

  return(checklist_display_name_var)
}

get_prepended_checklist_note <- function() {
  prepended_checklist_note <- .le$prepended_checklist_note

  # if not given, make the empty string
  if (is.null(prepended_checklist_note)) {
    prepended_checklist_note <- ""
  }

  # if given, append with a newline
  else {
    prepended_checklist_note <- paste0(prepended_checklist_note, "\n")
  }

  return(prepended_checklist_note)
}

get_options <- function() {
  # put in tryCatch because file may not exist
  tryCatch({
    options_yaml <- file.path(.le$config_repo_path, "options.yaml")
    options <- yaml::read_yaml(options_yaml)

    lapply(names(options), function(option_key) {
      option_value <- options[[option_key]]
      assign(option_key, option_value, envir = .le)
    })
  }, error = function(e) {
    return()
  })

}

capitalize <- function(word) {
  first_letter <- toupper(substring(word, 1, 1))
  rest_of_word <- substring(word, 2)
  glue::glue("{first_letter}{rest_of_word}")
}

pluralize <- function(word) {
  glue::glue("{word}s")
}
