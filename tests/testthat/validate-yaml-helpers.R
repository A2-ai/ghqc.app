create_test_yaml <- function(name, env,
      owner = "owner",
      repo = "repo",
      title = "title",
      milestone = "milestone",
      description = "description",
      files_bool = TRUE,
      files = NULL,
      name1 = "file1.R",
      assignees1 = c("user1.1", "user1.2"),
      items1 = c("item1.1", "item1.2", "item1.3"),
      name2 = "file2.R",
      assignees2 = c("user2.1", "user2.2"),
      items2 = c("item2.1", "item2.2", "item2.3")
) {

  files <- {
    if (files_bool) {
      list(
        list(
          name = name1,
          assignees = assignees1,
          items = items1
        ),
        list(
          name = name2,
          assignees = assignees2,
          items = items2
        )
      )
    }
    else {
      files
    }
  }

  data <- list()
  if (!is.null(owner)) {data$owner = owner}
  if (!is.null(repo)) {data$repo = repo}
  if (!is.null(title)) {data$title = title}
  if (!is.null(milestone)) {data$milestone = milestone}
  if (!is.null(description)) {data$description = description}
  if (!is.null(files)) {data$files = files}

  yaml_string <- yaml::as.yaml(data)
  yaml_path <- file.path(tempdir(), paste0(name, ".yaml"))
  write(yaml_string, file = yaml_path)
  withr::defer(fs::file_delete(yaml_path), envir = env)
  #withr::defer(fs::dir_delete(tempdir()), envir = env)
  yaml_path
}

check_test_yaml <- function(yaml,
                owner = "owner",
                repo = "repo",
                title = "title",
                milestone = "milestone",
                description = "description",
                files_bool = TRUE,
                files = NULL,
                name1 = "file1.R",
                assignees1 = c("user1.1", "user1.2"),
                items1 = c("item1.1", "item1.2", "item1.3"),
                name2 = "file2.R",
                assignees2 = c("user2.1", "user2.2"),
                items2 = c("item2.1", "item2.2", "item2.3")
) {
  # read yaml
  data <- yaml::yaml.load_file(yaml)
  files <- {
    if (files_bool) {
      list(
        list(
          name = name1,
          assignees = assignees1,
          items = items1
        ),
        list(
          name = name2,
          assignees = assignees2,
          items = items2
        )
      )
    }
    else {
     files
    }
  }

  expect_equal(data$owner, owner)
  expect_equal(data$repo, repo)
  expect_equal(data$title, title)
  expect_equal(data$milestone, milestone)
  expect_equal(data$description, description)
  expect_equal(data$files, files)
}
