library('rads')
library('srvyr')
library('survey')
library('dplyr')
library('data.table')
library('testthat')

data(api) #from the survey package
setDT(apisrs)
apisrs[stype != 'E', bytest := stype]

#DO NOT EDIT APISRS AFTER THIS STEP
sur = apisrs %>% as_survey_design(ids = 1)
set.seed(98104)

test_that('Both versions of calc deal with missing in the by var the same',{

  r1 = calc(sur, what = 'api00', by = 'bytest', metrics = c('mean', 'numerator', 'denominator'))
  r2 = calc(apisrs, what = 'api00', by = 'bytest', metrics = c('mean', 'numerator', 'denominator'))

  setorder(r1, bytest)
  setorder(r2, bytest)

  expect_equal(names(r1),names(r2))
  expect_equal(r1[, .(variable, level, bytest, mean, numerator, denominator)],r2[, .(variable, level, bytest, mean, numerator, denominator)])

})

test_that('Both versions of calc have approximately the same factor answers',{

  r1 = calc(sur, what = 'stype', by = 'both', metrics = metrics())
  r2 = calc(apisrs, what = 'stype', by = 'both', metrics = metrics())

  expect_equal(names(r1),names(r2))

  #remove the SE ones since those are different
  droppies = c(as.vector(outer(c('mean_','total_', 'rate_'), c('se', 'lower', 'upper'), paste0)), 'rse')
  r1[,  (droppies) := NULL]
  r2[, (droppies) := NULL]
  expect_equal(r1,r2)

})
