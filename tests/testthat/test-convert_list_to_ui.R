test_that("convert_list_to_ui handles unsupported types correctly", {
  unsupported_input <- 42
  expect_error(convert_list_to_ui(unsupported_input), "Unsupported type of checklist")

  unsupported_input <- TRUE
  expect_error(convert_list_to_ui(unsupported_input), "Unsupported type of checklist")

  unsupported_input <- 3.14
  expect_error(convert_list_to_ui(unsupported_input), "Unsupported type of checklist")
})
