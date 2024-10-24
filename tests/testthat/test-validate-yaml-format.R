source("validate-yaml-helpers.R")

test_that("validate_works when expected DRY", {
  yaml_path <- create_test_yaml("test", parent.frame())
  expect_true(file.exists(yaml_path))
  read_and_validate_yaml_2(yaml_path, dry = TRUE)
  check_test_yaml(yaml_path)
})

# validate existence of relevant fields
test_that("error occurs if owner not in yaml", {
  yaml_path <- create_test_yaml("test", parent.frame(), owner = NULL)
  expect_true(file.exists(yaml_path))
  expect_error(read_and_validate_yaml_2(yaml_path, dry = TRUE), "owner not inputted")
})

test_that("error occurs if repo not in yaml", {
  yaml_path <- create_test_yaml("test", parent.frame(), repo = NULL)
  expect_true(file.exists(yaml_path))
  expect_error(read_and_validate_yaml_2(yaml_path, dry = TRUE), "repo not inputted")
})

test_that("error doesn't occur if milestone not in yaml", {
  yaml_path <- create_test_yaml("test", parent.frame(), milestone = NULL)
  expect_true(file.exists(yaml_path))
  read_and_validate_yaml_2(yaml_path, dry = TRUE)
  check_test_yaml(yaml_path, milestone = NULL)
})

test_that("error doesn't occur if description not in yaml", {
  yaml_path <- create_test_yaml("test", parent.frame(), description = NULL)
  expect_true(file.exists(yaml_path))
  read_and_validate_yaml_2(yaml_path, dry = TRUE)
  check_test_yaml(yaml_path, description = NULL)
})

# test_that("error occurs if files not in yaml", {
#   yaml_path <- create_test_yaml("test", parent.frame(), files_bool = FALSE)
#   expect_true(file.exists(yaml_path))
#   expect_error(read_and_validate_yaml_2(yaml_path, dry = TRUE), "files not inputted")
# })

test_that("error occurs if name not in first file", {
  yaml_path <- create_test_yaml("test", parent.frame(), name1 = NULL)
  expect_true(file.exists(yaml_path))
  expect_error(read_and_validate_yaml_2(yaml_path, dry = TRUE), "file missing name attribute")
})

test_that("error occurs if name not in second file", {
  yaml_path <- create_test_yaml("test", parent.frame(), name2 = NULL)
  expect_true(file.exists(yaml_path))
  expect_error(read_and_validate_yaml_2(yaml_path, dry = TRUE), "file missing name attribute")
})

test_that("error doesn't occur if assignees not in first file", {
  yaml_path <- create_test_yaml("test", parent.frame(), assignees1 = NULL)
  expect_true(file.exists(yaml_path))
  read_and_validate_yaml_2(yaml_path, dry = TRUE)
  check_test_yaml(yaml_path, assignees1 = NULL)
})

# test_that("error doesn't occur if assignees not in second file", {
#   yaml_path <- create_test_yaml("test", parent.frame(), assignees2 = NULL)
#   expect_true(file.exists(yaml_path))
#   read_and_validate_yaml_2(yaml_path, dry = TRUE)
#   check_test_yaml(yaml_path, assignees2 = NULL)
# })


# test_that("error occurs if items not in first file", {
#   yaml_path <- create_test_yaml("test", parent.frame(), items1 = NULL)
#   expect_true(file.exists(yaml_path))
#   expect_error(read_and_validate_yaml_2(yaml_path, dry = TRUE), "file file1.R missing items attribute")
# })

# test_that("error occurs if items not in second file", {
#   yaml_path <- create_test_yaml("test", parent.frame(), items2 = NULL)
#   expect_true(file.exists(yaml_path))
#   expect_error(read_and_validate_yaml_2(yaml_path, dry = TRUE), "file file2.R missing items attribute")
# })

# validate types of relevant fields
test_that("error occurs if owner not character", {
  yaml_path <- create_test_yaml("test", parent.frame(), owner = 1)
  expect_true(file.exists(yaml_path))
  expect_error(read_and_validate_yaml_2(yaml_path, dry = TRUE), "owner not a character")
})

test_that("error occurs if repo not character", {
  yaml_path <- create_test_yaml("test", parent.frame(), repo = TRUE)
  expect_true(file.exists(yaml_path))
  expect_error(read_and_validate_yaml_2(yaml_path, dry = TRUE), "repo not a character")
})

test_that("error occurs if milestone not character", {
  yaml_path <- create_test_yaml("test", parent.frame(), milestone = list(a = "1"))
  expect_true(file.exists(yaml_path))
  expect_error(read_and_validate_yaml_2(yaml_path, dry = TRUE), "milestone not a character")
})

test_that("error occurs if description not character", {
  yaml_path <- create_test_yaml("test", parent.frame(), description = list("1", 1, FALSE))
  expect_true(file.exists(yaml_path))
  expect_error(read_and_validate_yaml_2(yaml_path, dry = TRUE), "description not a character")
})

test_that("error occurs if files not list", {
  yaml_path <- create_test_yaml("test", parent.frame(), files_bool = FALSE, files = "char")
  expect_true(file.exists(yaml_path))
  expect_error(read_and_validate_yaml_2(yaml_path, dry = TRUE))
})

test_that("error occurs if files not list of lists", {
  yaml_path <- create_test_yaml("test", parent.frame(), files_bool = FALSE, files = c(list(a = "a", b = "b"), list(c = "c", d = "d")))
  expect_true(file.exists(yaml_path))
  expect_error(read_and_validate_yaml_2(yaml_path, dry = TRUE), "files not a list of lists")
})

# validate lengths of relevant fields
test_that("error occurs if owner not char of 1", {
  yaml_path <- create_test_yaml("test", parent.frame(), owner = c("a", "b", "c"))
  expect_true(file.exists(yaml_path))
  expect_error(read_and_validate_yaml_2(yaml_path, dry = TRUE), "owner is not a single character input")
})

test_that("error occurs if repo not char of 1", {
  yaml_path <- create_test_yaml("test", parent.frame(), repo = c("d", "e", "f"))
  expect_true(file.exists(yaml_path))
  expect_error(read_and_validate_yaml_2(yaml_path, dry = TRUE), "repo is not a single character input")
})

test_that("error occurs if milestone not char of 1", {
  yaml_path <- create_test_yaml("test", parent.frame(), milestone = list("i", "j", "k", "l"))
  expect_true(file.exists(yaml_path))
  expect_error(read_and_validate_yaml_2(yaml_path, dry = TRUE), "milestone is not a single character input")
})

test_that("error occurs if description not char of 1", {
  yaml_path <- create_test_yaml("test", parent.frame(), description = c("m", "n", "o", "p"))
  expect_true(file.exists(yaml_path))
  expect_error(read_and_validate_yaml_2(yaml_path, dry = TRUE), "description is not a single character input")
})

