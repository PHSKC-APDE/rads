library('testthat')

test_that('Returns TRUE for NaN, NA, NULL, and empty string', {
  expect_true(check_nan(NaN))
  expect_true(check_nan(NA))
  expect_true(check_nan(NULL))
  expect_true(check_nan(''))
})

test_that('Returns FALSE for non-blank values', {
  expect_false(check_nan('x'))
  expect_false(check_nan(10))
  expect_false(check_nan(FALSE))
})

test_that('Errors when given a vector of length > 1 (not NA/NULL)', {
  expect_error(check_nan(c('x', 'y')))
})
