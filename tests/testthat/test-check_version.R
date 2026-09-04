library('testthat')

test_that('check_version runs without error and returns the expected structure', {
  result <- NULL
  expect_no_error(result <- suppressMessages(check_version(print_message = FALSE)))

  expect_type(result, 'list')
  expect_named(result, c('is_current', 'local_version', 'remote_version', 'status', 'message'),
               ignore.order = TRUE)
  expect_type(result$status, 'character')
  expect_type(result$message, 'character')
})

test_that('check_version respects print_message = FALSE (no console message)', {
  expect_no_message(check_version(print_message = FALSE))
})
