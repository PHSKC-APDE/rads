library('data.table')
library('testthat')

# create test data
set.seed(98104)
dt <- data.table::data.table(
  chi_year = rep(2000:2019),
  chi_sex = factor(sample(c("Male", "Female"), 20, rep = TRUE, prob = c(0.5, 0.5))),
  result = rnorm(20, .75, 0.025),
  se = rnorm(20, 0.0258787654654, 0.00001)
)
dt[, lower_bound := result - (1.96 * se)]
dt[, upper_bound := result + (1.96 * se)]
dt[, rse := se / result]


# test defaults
dt1 <- digits(digit_data = copy(dt))
test_that('Check that defaults work as expected',{
  expect_equal( nchar(dt1[1]$result), nchar("0.123"))
  expect_equal( nchar(dt1[1]$lower_bound), nchar("0.123"))
  expect_equal( nchar(dt1[1]$upper_bound), nchar("0.123"))
  expect_equal( nchar(dt1[1]$rse), nchar("0.123"))
  expect_equal( nchar(dt1[1]$se), nchar("0.1234"))
})


# test custom settings
dt1 <- digits(digit_data = copy(dt),
              vars_1 = c("lower_bound", "upper_bound"),
              digits_1 = 5,
              vars_2 = c("result", "se", "rse"),
              digits_2 = 2)
test_that('Check that custom settings work as expected',{
  expect_equal( nchar(dt1[1]$lower_bound), nchar("0.12345"))
  expect_equal( nchar(dt1[1]$upper_bound), nchar("0.12345"))
  expect_equal( nchar(dt1[1]$result), nchar("0.12"))
  expect_equal( nchar(dt1[1]$se), nchar("0.12"))
  expect_equal( nchar(dt1[1]$rse), nchar("0.12"))
})


# test setting vars_2 to NULL
dt1 <- digits(digit_data = copy(dt),
              vars_1 = c("lower_bound", "upper_bound"),
              digits_1 = 5,
              vars_2 = NULL,
              digits_2 = 2)
test_that('Check that custom settings work as expected',{
  expect_equal( nchar(dt1[1]$lower_bound), nchar("0.12345"))
  expect_equal( nchar(dt1[1]$upper_bound), nchar("0.12345"))
  expect_equal( nchar(dt1[1]$result), nchar(dt[1]$result))
  expect_equal( nchar(dt1[1]$se), nchar(dt[1]$se))
  expect_equal( nchar(dt1[1]$rse), nchar(dt[1]$rse))
})


