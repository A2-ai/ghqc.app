test_that("generate_excluded_file_message works correctly", {
  excluded_files <- c("path/to/file1.txt", "path/to/file2.pdf")
  result <- generate_excluded_file_message(excluded_files)
  error_icon_html <- "<span style='font-size: 24px; vertical-align: middle;'>&#10071;</span>"
  expected_message <- sprintf("%s The selected directory contains only the following files which are not selectable QC items:<ul>%s</ul><br>",
                              error_icon_html, paste0("<li>", basename(excluded_files), "</li>", collapse = ""))
  expect_equal(result, expected_message)

  excluded_files <- character(0)
  result <- generate_excluded_file_message(excluded_files)
  expect_equal(result, NULL)
})
