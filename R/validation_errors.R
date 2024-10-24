err_if_not_in_yaml <- function(x) {
  # get var name as character
  attr <- deparse(substitute(x))

  if (is.null(x)) {
    # error
    rlang::abort(message = glue::glue("{attr} not inputted"),
                 class = "required_input_not_found",
                 x = x)
  }
}

err_if_len_gt_one <- function(x) {
  # get var name as character
  attr <- deparse(substitute(x))

  if (length(x) != 1) {
    # error
    rlang::abort(message = glue::glue("{attr} is not a single character input"),
                 class = "input_error",
                 x = x)
  }
}

err_if_not_char <- function(x) {
  # get var name as character
  attr <- deparse(substitute(x))

  if (!is.character(x)) {
    # error
    rlang::abort(message = glue::glue("{attr} not a character; {attr} is a {class(attr)}"),
                 class = "input_type_error",
                 x = x)
  }
}

err_if_not_list <- function(x) {
  # get var name as character
  attr <- deparse(substitute(x))

  if (!is.list(x)) {
    rlang::abort(message = glue::glue("{attr} not a list; {attr} is a {class(attr)}"),
                 class = "input_type_error",
                 x = x)
  }
}

err_if_not_list_of_2_or_3 <- function(x) {
  # get var name as character
  attr <- deparse(substitute(x))

  if (length(x) != 3 || length(x) != 2) {
    # error
    rlang::abort(message = glue::glue("{attr} not a list of 2 or 3"),
                 class = "input_error",
                 x = x)
  }
}

err_if_file_attr_missing <- function(file) {
  if (is.null(file$name)) {
    rlang::abort(message = glue::glue("file missing name attribute"),
                 class = "input_error",
                 x = file)
  } # name

  # if (is.null(file$items)) {
  #   rlang::abort(message = glue::glue("file {file$name} missing items attribute"),
  #                class = "input_error",
  #                x = file)
 # } # items
}

err_if_not_list_of_lists <- function(files) {
  if (!is.list(files)) {
    rlang::abort(message = glue::glue("files not a list; files is a {class(files)}"),
                 class = "input_error",
                 x = files)
  }

  all_elements_are_lists <- all(sapply(files, is.list))
  if (!all_elements_are_lists) {
    rlang::abort(message = glue::glue("files not a list of lists; files is a {class(files)}"),
                 class = "input_error",
                 x = files)
  }
}

err_if_invalid_checklist_type <- function(checklist_type, file_name) {
  valid_checklist_types <- c("R", "mod", "cpp", "vpc")

  if (!checklist_type %in% valid_checklist_types) {
    rlang::abort(message = glue::glue("{checklist_type} not a valid checklist type for file {file_name}"),
                 class = "input_error",
                 x = checklist_type)
  }
}

