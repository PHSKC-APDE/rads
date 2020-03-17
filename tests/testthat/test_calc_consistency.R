library('rads')
library('srvyr')
library('survey')
library('dplyr')
library('data.table')
library('test_that')

data(api) #from the survey package
setDT(apisrs)
apisrs[stype != 'E', bytest := stype]

#DO NOT EDIT APISRS AFTER THIS STEP
sur = apisrs %>% as_survey_design(ids = 1)
set.seed(98104)

test_that('Both versions of calc deal with missing in the by var the same',{

  r1 = calc(sur, what = 'api00', by = 'bytest', metrics = c('mean', 'numerator', 'denominator'))
  r2 = calc(apisrs, what = 'api00', by = 'bytest', metrics = c('mean', 'numerator', 'denominator'))



})
