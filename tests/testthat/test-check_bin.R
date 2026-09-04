library('testthat')

test_that('Detects a simple bin with square and round brackets', {
  expect_true(check_bin('[0,5)'))
  expect_true(check_bin('(0,5]'))
  expect_true(check_bin('[0,5]'))
  expect_true(check_bin('(0,5)'))
})

test_that('Rejects strings without both bracket ends', {
  expect_false(check_bin('[0,5'))
  expect_false(check_bin('0,5)'))
})

test_that('Rejects strings without a comma', {
  expect_false(check_bin('[05]'))
})

test_that('Rejects plain non-binning strings', {
  expect_false(check_bin('male'))
  expect_false(check_bin('5'))
})

test_that('Warns when it detects a hyphen instead of a comma', {
  expect_warning(check_bin('[0-5]'))
})

test_that('Errors when x is not length one', {
  expect_error(check_bin(c('[0,5)', '[5,10)')))
})
