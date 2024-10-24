# doesn't cover indexing further into directories
# TODO: shiny tests for that
test_that("list_files_and_dirs handles empty directory correctly", {
  path <- tempfile()
  dir.create(path)

  result <- list_files_and_dirs(path, pattern = ".*", all.files = TRUE)
  expect_true(result$empty)
})

test_that("list_files_and_dirs filters out binary files and renv directory", {
  path <- tempfile()
  dir.create(path)

  # Create some files and directories
  file.create(file.path(path, "file1.txt"))
  file.create(file.path(path, "file2.png"))
  dir.create(file.path(path, "renv"))

  result <- list_files_and_dirs(path, pattern = exclude_patterns(), all.files = FALSE)
  expect_false(result$empty)
  expect_equal(basename(result$files), "file1.txt")
})

test_that("list_files_and_dirs returns all files if no match with pattern", {
  path <- tempfile()
  dir.create(path)

  # Create some files
  file.create(file.path(path, "file1.txt"))
  file.create(file.path(path, "file2.txt"))

  result <- list_files_and_dirs(path, pattern = exclude_patterns(), all.files = FALSE)
  expect_false(result$empty)
  expect_equal(basename(result$files), c("file1.txt", "file2.txt"))
})

test_that("list_files_and_dirs includes non-empty directories and excludes empty ones", {
  path <- tempfile()
  dir.create(path)

  # Create some directories and files
  dir.create(file.path(path, "dir1"))
  dir.create(file.path(path, "dir2"))
  file.create(file.path(path, "dir2/file2.txt"))

  result <- list_files_and_dirs(path, pattern = exclude_patterns(), all.files = FALSE)
  expect_false(result$empty)
  expect_equal(basename(result$files), c("dir2"))
})
