library('rads')
library('dtsurvey')
library('survey')
library('data.table')
library('testthat')

data(api) #from the survey package
setDT(apisrs)
apisrs[stype != 'E', bytest := stype]
#apisrs[, c('w', 'tw')] #potentially reserved words for calc

#DO NOT EDIT APISRS AFTER THIS STEP
sur = dtsurvey(apisrs)
apisrs = dtadmin(apisrs)
set.seed(98104)

test_that('Both versions of calc deal with missing in the by var the same',{

  r1 = calc(sur, what = 'api00', by = 'bytest', metrics = c('mean', 'numerator', 'denominator'))
  r2 = calc(apisrs, what = 'api00', by = 'bytest', metrics = c('mean', 'numerator', 'denominator'))

  setorder(r1, bytest)
  setorder(r2, bytest)

  expect_equal(names(r1),names(r2))
  expect_equal(r1[, .(variable, level, bytest, mean, numerator, denominator)],r2[, .(variable, level, bytest, mean, numerator, denominator)])

})

test_that('Where with quoted and non-quoted wheres',{
  r1 = calc(sur, what = 'api00', where = stype == 'H', metrics = c('mean', 'numerator', 'denominator'))
  expect_warning(r2 <- calc(sur, what = 'api00', where = "stype == 'H'", metrics = c('mean', 'numerator', 'denominator')))

  expect_equal(r1,r2)

})

test_that('Where is improperly specified',{
  expect_error(calc(sur, what = 'api00', where = stype == c('H', 'E'), metrics = c('mean', 'numerator', 'denominator')))
  expect_error(calc(apisrs, what = 'api00', where = stype == c('H', 'E'), metrics = c('mean', 'numerator', 'denominator')))

})
