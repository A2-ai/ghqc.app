get_checklists <- function() {
  checklists_path <- file.path(.le$config_repo_path, "checklists")
  yaml_checklists <- list.files(checklists_path, pattern = "\\.ya?ml$", full.names = TRUE)
  txt_checklists <- list.files(checklists_path, pattern = "\\.txt$", full.names = TRUE)
  custom_checklist <- system.file("default_checklist", "custom.yaml", package = "ghqc.app")
  yaml_checklists <- c(yaml_checklists, custom_checklist)

  invalid_search <- grepl("INVALID - ", yaml_checklists)
  if (any(invalid_search)) {
    invalid_checklists <- yaml_checklists[which(invalid_search)]
    invalid_checklist_names <- lapply(invalid_checklists, function(invalid_checklist) {
      basename(invalid_checklist) %>% stringr::str_remove("\\.ya?ml$") %>% stringr::str_remove("INVALID - ")
    })
    invalid_checklist_names_col <- glue::glue_collapse(invalid_checklist_names, sep = ", ", last = " and ")
    warn(.le$logger, glue::glue("The following {get_checklist_display_name_var()}(s) are invalid and will therefore not be selectable in the app: {invalid_checklist_names_col}. Run check_ghqc_configuration() for guidance."))

    # remove bad checklists
    yaml_checklists <- yaml_checklists[!invalid_search]
  }

  checklists_data <- list()

  # Legacy yaml checklists
  checklists_data$yaml <- sapply(yaml_checklists, function(yaml_checklist) {
    yaml::read_yaml(yaml_checklist)
  }, USE.NAMES = FALSE)

  # Verbatim txt checklists
  checklists_data$txt <- setNames(
    lapply(txt_checklists, function(path) {
      paste(readLines(path, warn = FALSE), collapse = "\n")
    }),
    nm = txt_checklists %>%
      basename() %>%
      tools::file_path_sans_ext() %>%
      stringr::str_remove_all("`")
  )

  return(checklists_data)
}

create_file_data_structure <- function(file_name, assignees = NULL, checklist_type, relevant_files) {

  file_data <- list(
    name = file_name,
    checklist_type = checklist_type
  )

  if (!is.null(assignees)) {
    file_data$assignees <- assignees
  }

  if (!is.null(relevant_files)) {
    file_data$relevant_files <- relevant_files
  }

  file_data
}

# files should be a list of lists like this:
# list(
#   list(
#     name = name1,
#     assignees = assignees1,
#.    checklist_type = type1,
#     items = items1
#   ),
#   list(
#     name = name2,
#     assignees = assignees2,
#.    checklist_type = type2,
#     items = items2
#   ),
#   list(
#     name = name3,
#     assignees = assignees3,
#.    checklist_type = type3,
#     items = items3
#   ),
#  ...
# )

#### Example output


# files:
# - name: file1.cpp
#   checklist_type: cpp
#   assignees: jenna-a2ai
#   items:
#   - code is easily readable
#   - code follows style guidelines
#   - comments explain non-standard coding conventions
# - name: file1.R
#   checklist_type: vpc
#   assignees: jenna-a2ai
#   items:
#   - confidence interval is 95%
#   - 500 simulations given
#   - generated figures match
create_yaml <- function(files, # files must be a list of lists not a vector of lists
                        milestone = NULL,
                        description = NULL
                        ) {

  data <- list(
    files = files # files must be a list of lists not a vector of lists
  )

  if (!is.null(milestone) && milestone != "") {data$milestone = milestone}
  if (!is.null(description) && description != "") {data$description = description}

  # make into yaml string
  yaml_string <- yaml::as.yaml(data)
  # make path
  yaml_path <- tempfile(fileext = ".yaml")
  withr::defer_parent(fs::file_delete(yaml_path))
  # create yaml
  write(yaml_string, file = yaml_path)

  yaml_path
}
