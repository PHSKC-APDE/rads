library('testthat')
library('rads')
library('data.table')
library('dtsurvey')
library('survey')


a = data.frame(V1 = 1:10)
b = data.table(V1 = 1:10)

data(api)

# stratified sample
c <- svydesign(id=~1,
               strata=~stype,
               weights=~pw,
               data=apistrat)

# Examples from ?survey::svrepdesign
data(scd)
# use BRR replicate weights from Levy and Lemeshow
scd$rep1 <- 2 * c(1, 0, 1, 0, 1, 0)
scd$rep2 <- 2 * c(1, 0, 0, 1, 0, 1)
scd$rep3 <- 2 * c(0, 1, 1, 0, 0, 1)
scd$rep4 <- 2 * c(0, 1, 0, 1, 1, 0)

d <- svydesign(data=scd,
               prob=~1,
               id=~ambulance,
               strata=~ESA,
               nest=TRUE)
d <- as.svrepdesign(d, type="BRR")


testthat::test_that('Multiple types (beyond dtsurvey) work',{

  # data.frames and data.tables
  t1 = calc(a, 'V1')
  t2 = calc(b, 'V1')
  expect_equal(t1,t2)
  expect_equal(t1$mean, mean(1:10))

  # standard survey designs
  t3 = calc(c, 'api00', by = 'awards')
  t4 = calc(as.dtsurvey(c), 'api00', by = 'awards')
  expect_equal(t3,t4)

  t5 = calc(d, 'arrests', by = 'ESA')
  t6 = calc(dtrepsurvey(d), 'arrests', by = 'ESA')
  expect_equal(t5,t6)

  # test srvyr
  if(requireNamespace('srvyr')){
    e = srvyr::as_survey_design(apistrat, strata = stype, weights = pw)
    t4.1 = calc(e, 'api00', by = 'awards')
    expect_equal(t3, t4.1)
  }

})


