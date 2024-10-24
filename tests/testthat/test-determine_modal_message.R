test_that("determine_modal_message works correctly if there are messages", {
  selected_files <- c("file1.txt", "file2.txt")
  uncommitted_git_files <- c("file1.txt")
  untracked_selected_files <- c("file2.txt")
  git_sync_status <- list(ahead = 1, behind = 1)
  commit_update_status <- FALSE
  issues_in_milestone <- list(list(title = "file1.txt"))

  result <- determine_modal_message(
    selected_files,
    uncommitted_git_files,
    untracked_selected_files,
    git_sync_status,
    commit_update_status,
    issues_in_milestone
  )

  expect_true(grepl("push changes to the remote repository and pull updates from the remote repository", result$message))
  expect_true(grepl("file1.txt", result$message))
  expect_true(grepl("file2.txt", result$message))
  expect_true(grepl("file1.txt", result$message))
  expect_true(grepl("There are no update commits on the QC item", result$message))
  expect_equal(result$state, "error")
})

test_that("determine_modal_message works correctly if there are no messages", {
  selected_files <- c("file3.txt")
  uncommitted_git_files <- character(0)
  untracked_selected_files <- character(0)
  git_sync_status <- list(ahead = 0, behind = 0)
  commit_update_status <- TRUE
  issues_in_milestone <- list()

  result <- determine_modal_message(
    selected_files,
    uncommitted_git_files,
    untracked_selected_files,
    git_sync_status,
    commit_update_status,
    issues_in_milestone
  )

  expect_null(result$message)
  expect_null(result$state)
})

test_that("generate_uncommitted_message works correctly for uncommitted general files only", {
  uncommitted_files <- list(
    selected = character(0),
    general = c("file4.txt", "file5.txt")
  )
  error_icon_html <- "<error>"
  warning_icon_html <- "<warning>"

  result <- generate_uncommitted_message(uncommitted_files, error_icon_html, warning_icon_html)

  expect_true(length(result) == 1)
  expect_true(grepl("There are local files, which are not in the selected QC items, that have uncommitted changes", result))
  expect_true(grepl("file4.txt", result))
  expect_true(grepl("file5.txt", result))
  expect_true(grepl("<warning>", result))
})

