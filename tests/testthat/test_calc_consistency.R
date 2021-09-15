library('rads')
library('dtsurvey')
library('survey')
library('data.table')
library('testthat')

data(api) #from the survey package
setDT(apisrs)
apisrs[stype != 'E', bytest := stype]

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
