get_checklist_display_name_var <- function(capitalized = FALSE, plural = FALSE) {
  #browser()
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

# get_dne_var <- function(capitalized = FALSE) {
#   dne_var <- .le$dne_var
#
# }


get_custom_options <- function() {
  custom_options_yaml <- file.path(.le$info_repo_path, "custom_options.yaml")
  custom_options <- yaml::read_yaml(custom_options_yaml)
  custom_options

  lapply(names(custom_options), function(option_key) {
    option_value <- custom_options[[option_key]]
    assign(option_key, option_value, envir = .le)
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
