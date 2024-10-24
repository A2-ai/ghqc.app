validate_yaml_contents <- function(contents, dry = FALSE) {
  # validate technical requirements
  validate_owner(contents$owner, dry)
  validate_repo(contents$owner, contents$repo, dry)

  # business requirements

  # if milestone was inputted
  if (!is.null(contents$milestone)) {
    validate_milestone(contents$milestone)
  }

  # if description was inputted
  if (!is.null(contents$description)) {
    validate_description(contents$description)
  }

  validate_files(contents$files, contents$owner, dry)
}

read_and_validate_yaml <- function(yaml, dry = FALSE) {
  contents <- yaml::yaml.load_file(yaml)
  #validate_yaml_contents(contents, dry)
  contents
}

read_and_validate_yaml_2 <- function(yaml, dry = FALSE) {
  contents <- yaml::yaml.load_file(yaml)
  validate_yaml_contents(contents, dry)
  contents
}
