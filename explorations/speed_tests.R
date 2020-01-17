library('rads')
library('srvyr')
library('data.table')
library('proffer')

dat = data.table(id = 1:100000000)
dat[, v1 := runif(.N)]
dat[, v2 := sample(1:3, .N, replace = T)]
dat[, v3 := sample(1:3, .N, replace = T)]
dat[, weight := 1]
dat[, time := 10]
#format(object.size(dat), 'Mb')


s0 <- system.time({
  test.results <- calc(dat,
                       what = c("v1", "v2"),
                       v3 !=1,
                       by = c('v3'),
                       metrics = c('mean', 'se'),
                       per = 1,
                       time_var = 'time')
})


s1 <- profvis::profvis(a <- as_survey_design(dat, probs = weight))
s2 <- system.time(res1 <- calc(a,
                                what = c("v1"),
                                v3 !=1,
                                by = c('v3'),
                                metrics = c('mean', 'se'),
                                per = 1,
                                time_var = 'time'))
s3 <- system.time(res2  <- calc(a,
                                what = c("v2"),
                                v3 !=1,
                                by = c('v3'),
                                metrics = c('mean', 'se'),
                                per = 1,
                                time_var = 'time'))
s4 <- system.time(svy.results <- calc(a,
                                      what = c("v1", "v2"),
                                      v3 !=1,
                                      by = c('v3'),
                                      metrics = c('mean', 'se'),
                                      per = 1,
                                      time_var = 'time'))


px2 <- profvis({
  a <- as_survey_design(dat, probs = weight)

  svy.results <- calc(a,
                      what = c("v1", "v2"),
                      v3 !=1,
                      by = c('v3'),
                      metrics = c('mean', 'se'),
                      per = 1,
                      time_var = 'time')
})
