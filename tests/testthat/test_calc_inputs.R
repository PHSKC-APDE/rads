library('testthat')
library('rads')
library('data.table')
library('dtsurvey')
library('survey')


a = data.frame(1:10)
b = data.table(1:10)

data(api)

# stratified sample
c <- apistrat %>%
  as_survey(strata = stype, weights = pw)

# Examples from ?survey::svrepdesign
data(scd)
# use BRR replicate weights from Levy and Lemeshow
scd$rep1 <- 2 * c(1, 0, 1, 0, 1, 0)
scd$rep2 <- 2 * c(1, 0, 0, 1, 0, 1)
scd$rep3 <- 2 * c(0, 1, 1, 0, 0, 1)
scd$rep4 <- 2 * c(0, 1, 0, 1, 1, 0)

d <- scd %>%
  as_survey(type = "BRR", repweights = starts_with("rep"),
            combined_weights = FALSE)

testthat::test_that('Invalid inputs to calc show an error',{
  expect_error(calc(a, 'V1'), 'no longer')
  expect_error(calc(b, 'V1'), 'no longer')
  expect_error(calc(c, 'api00'), 'no longer')
  expect_error(calc(d, 'alive'), 'no longer')
})
