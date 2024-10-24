test_that("generate_input_id works correctly without prefix", {
  result <- generate_input_id(name = "testName")
  expect_equal(result, "testName")

  result <- generate_input_id(name = "test name with spaces")
  expect_equal(result, "test name with spaces")
})

test_that("generate_input_id works correctly with prefix", {
  result <- generate_input_id(prefix = "prefix", name = "testName")
  expect_equal(result, "prefix_testName")
})
