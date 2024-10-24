test_that("convert_issue_df_format works correctly", {
  issues <- list(
    list(number = 1, title = "Issue 1", state = "open"),
    list(number = 2, title = "Issue 2", state = "closed"),
    list(number = 3, title = "Issue 3", state = "open"),
    list(number = 4, title = "Issue 4", state = "closed")
  )

  result <- convert_issue_df_format(issues)

  expect_true(is.list(result))
  expect_named(result, c("Open Issues", "Closed Issues"))
  expect_equal(length(result[["Open Issues"]]), 2)
  expect_equal(length(result[["Closed Issues"]]), 2)
  expect_equal(result[["Open Issues"]][[1]], "Issue 1: Issue 1")
  expect_equal(result[["Closed Issues"]][[1]], "Issue 2: Issue 2")
})

test_that("convert_commits_df_format works correctly", {
  commits <- tibble(
    date = as.Date(c('2023-01-01', '2023-01-02')),
    commit = c('abc123', 'def456'),
    display = c('Commit 1', 'Commit 2')
  )

  result <- convert_commits_df_format(commits)

  expect_true(is.list(result))
  expect_equal(length(result), 2)
  expect_named(result[[1]], "Commit 2")
  expect_equal(result[[1]][["Commit 2"]], "def456")
})

test_that("split_issue_parts works correctly", {
  issue <- "Issue 1: Issue Title"
  result <- split_issue_parts(issue)

  expect_true(is.list(result))
  expect_equal(result$issue_number, 1)
  expect_equal(result$issue_title, "Issue Title")
})

test_that("split_issue_parts handles invalid input", {
  issue <- "Invalid Format Issue"
  result <- tryCatch(
    split_issue_parts(issue),
    error = function(e) e
  )
  expect_true(inherits(result, "error"))
})
