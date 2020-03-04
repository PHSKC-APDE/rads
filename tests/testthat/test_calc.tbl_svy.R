library('srvyr')
library('survey')
library('dplyr')
library('data.table')
library('testthat')

data(api) #from the survey package
sur = apisrs %>% as_survey_design(ids = 1, fpc = fpc)
set.seed(98104)

test_that('Defaults (mostly) work: svy',
          expect_equal(
            calc(sur, what = 'api00', time_var = NULL)$mean,
            sur %>% summarize(mean = survey_mean(api00, na.rm = T)) %>% .$mean
          ))

test_that('Grouping without filtering',
          expect_equal(
            calc(sur, 'api00', by = c('stype'), metrics = 'denominator', time_var = NULL)[, .(stype, denominator)],
            sur %>% group_by(stype) %>% summarize(denominator = unweighted(n())) %>% setDT
          ))
#
test_that('Multi Grouping with filtering',{
          st = calc(sur, 'api00', cname == 'Los Angeles', by = c('stype', 'cname'), metrics = 'denominator', time_var = NULL)[, .(stype, cname, denominator)]
          man = sur %>% filter(cname == 'Los Angeles') %>% group_by(stype, cname) %>% summarize(denominator = unweighted(n())) %>% setDT
          expect_equal(st, man)
})

test_that('Grouping with NAs in the group',{

  sur <- sur %>% mutate(g1 = sample(c(0,1,NA), n(), T), g2 = sample(c(0,1,NA), n(), T))
  expect_equal(
    calc(sur, 'api00', by = c('g1'), metrics = 'mean', time_var = NULL)[, .(g1, mean)],
    sur %>% group_by(g1) %>% summarize(mean = survey_mean(api00)) %>% select(g1, mean) %>% setDT
  )
  expect_equal(
    calc(sur, 'api00', by = c('g1', 'g2'), metrics = 'mean', time_var = NULL)[, .(g1,g2, mean)],
    sur %>% group_by(g1,g2) %>% summarize(mean = survey_mean(api00)) %>% select(g1,g2, mean) %>% setDT
  )

  expect_equal(
    calc(sur, 'api00',!is.na(g2), by = c('g1', 'g2'), metrics = 'mean', time_var = NULL)[, .(g1,g2, mean)],
    sur %>% filter(!is.na(g2)) %>% group_by(g1,g2) %>% summarize(mean = survey_mean(api00)) %>% select(g1,g2, mean) %>% setDT
  )


})

#
test_that('Multi Grouping with filtering and multiple what variables',{
  st = calc(sur, what = c('api00', 'api99'),
                       cname == 'Los Angeles',
                       by = c('stype', 'cname'),
                       metrics = 'mean', time_var = NULL)[, level := NULL]
  man1 = sur %>% filter(cname == 'Los Angeles') %>% group_by(stype, cname) %>% summarize(mean = survey_mean(api00)) %>% setDT
  man2 = sur %>% filter(cname == 'Los Angeles') %>% group_by(stype, cname) %>% summarize(mean = survey_mean(api99)) %>% setDT
  man = rbind(man1, man2)[, .(stype, cname, mean,mean_se, variable = c(rep('api00', 3), rep('api99', 3)))]
  st[, c('mean_lower', 'mean_upper') := NULL]
  setcolorder(man, names(st))
  expect_equal(st, man)
})
#
test_that('Proportion toggle- on',{
  sur = sur %>% mutate(schwide = as.numeric(sch.wide == 'Yes'))
  st = calc(sur, 'schwide', metrics = 'mean', proportion = T, time_var = NULL)
  man = sur %>% summarize(blah = survey_mean(schwide, vartype = 'ci', proportion = T)) %>% setDT

  #without proportion
  expect_equal(st[, mean_lower], man[, blah_low])

})
#
test_that('Proportion toggle changes result',{
  sur = sur %>% mutate(schwide = as.numeric(sch.wide == 'Yes'))
  a = calc(sur, 'schwide', metrics = 'mean', proportion = T, time_var = NULL)[, mean_lower]
  b = calc(sur, 'schwide', metrics = 'mean', proportion = F, time_var = NULL)[, mean_lower]

  expect_true(a != b)

})

#test windowing
test_that('Win(dows) do sensible things',{

  sur <- sur %>% mutate(yyy = sample(1:4, nrow(sur), replace = T))
  a = calc(sur, 'api00', metrics = 'mean', proportion = F, time_var = 'yyy', win = 2)

  b1 = sur %>% filter(yyy %in% c(1,2)) %>% summarize(variable = survey_mean(api00))
  b2 = sur %>% filter(yyy %in% c(2,3)) %>% summarize(variable = survey_mean(api00))
  b3 = sur %>% filter(yyy %in% c(3,4)) %>% summarize(variable = survey_mean(api00))

  b = rbind(b1, b2, b3)

  expect_equal(a$mean, b$variable)

})

#window without time var
test_that('win(dow) without time var',{
  expect_error(calc(sur, 'api00', metrics = 'mean', proportion = F, time_var = NULL, win = 2), 'win(dow)', fixed = T)
})

#time var without window
test_that('Time var without window',{
  sur <- sur %>% mutate(yyy = sample(1:4, nrow(sur), replace = T))
  a = calc(sur, 'api00', metrics = 'mean', proportion = F, time_var = 'yyy', win = NULL)
  b = sur %>% summarize(variable = survey_mean(api00))
  expect_equal(a$mean, b$variable)

})

#
test_that('Invalid input for what',{
  expect_error(calc(sur, 'thisvarnoexists', time_var = NULL), 'thisvarnoexists')
})
#
test_that('Invalid input for ... filters',{
  expect_error(calc(sur, 'api00', bananas>100, api00 < 50, time_var = NULL), 'bananas')
})

test_that('Invalid input for by',{
  expect_error(calc(sur, 'api00', by = 'turtles', time_var = NULL), 'turtles')
})
#
test_that('Invalid input for metric',{
  expect_error(calc(sur, 'api00', metrics = 'turtles', time_var = NULL), 'Invalid metrics detected: turtles')
})
#

#test numerator and denominator calculations
test_that('Numerator and denominator calculations account for NAs',{

  #make a column with NAs
  sur <- sur %>% mutate(blah = sample(c(0,1, NA), dplyr::n(), replace = T))
  dt = data.table::as.data.table(sur$variables)

  a <- calc(sur, 'blah', metrics = c('numerator', 'denominator', 'missing'), proportion = T, time_var = NULL, win = NULL)
  b = dt[, .N, by = blah]

  expect_equal(a[, numerator], b[blah == 1, N])
  expect_equal(a[, denominator], b[blah %in% c(0,1), sum(N)])
  expect_equal(a[, missing], b[is.na(blah), N])

  #now try with groups
  a <- calc(sur, 'blah', metrics = c('numerator', 'denominator', 'missing'), proportion = T, time_var = NULL, win = NULL, by = 'stype')
  b = dt[, .N, by = .(blah, stype)]

  setorder(a, stype)
  setorder(b, stype)

  expect_equal(a[, numerator], b[blah == 1, N])
  expect_equal(a[, denominator], b[blah %in% c(0,1), list(N = sum(N)), by = stype][, N])
  expect_equal(a[, missing], b[is.na(blah), N])

})

test_that('survey design and survey rep design are equal',{

  s1 <- as_survey_design(svydesign(id=~dnum, weights=~pw, data=apiclus1, fpc=~fpc))
  s2 <- as_survey_rep(as.svrepdesign(s1))

  a1 = calc(s1, 'stype', by = 'both', metrics = survey_metrics())
  a2 = calc(s2, 'stype', by = 'both', metrics = survey_metrics())

  #Epect that the means are equal. Se is different depending on method
  expect_equal(a1[, .(mean, total, rate, numerator, missing, ndistinct, unique.time, denominator, missing.prop, obs)],
               a2[, .(mean, total, rate, numerator, missing, ndistinct, unique.time, denominator, missing.prop, obs)])

})

test_that('Multiple by conditions',{

  apiclus1$highenroll = apiclus1$enroll>median(apiclus1$enroll)
  apiclus1$highmeals = apiclus1$meals>median(apiclus1$meals)
  apiclus1$both_bin = apiclus1$both == 'Yes'
  s1 <- as_survey_design(svydesign(id=~dnum, weights=~pw, data=apiclus1, fpc=~fpc))
  s2 <- as_survey_rep(as.svrepdesign(s1))

  a1 = calc(s1, 'both', by = c('highenroll', 'highmeals'), metrics = c('mean', 'numerator', 'denominator'))
  a2 = calc(s2, 'both', by = c('highenroll', 'highmeals'), metrics = c('mean', 'numerator', 'denominator'))

  #confirm survey design and survey rep are the same
  expect_equal(a1[, .(variable, level, highenroll, highmeals, mean, numerator, denominator)],a2[,.(variable, level, highenroll, highmeals, mean, numerator, denominator)])

  r1 = svyby(~both, ~highenroll + highmeals, s1, svymean)
  ci = confint(r1)
  r1 = setDT(as.data.frame(r1))
  r1[, grep('se.', names(r1), fixed = T) := NULL]
  r1 = melt(r1, id.vars = c('highenroll', 'highmeals'), measure.vars = paste0('both',c('No', 'Yes')), variable.factor = F, variable.name = 'level')
  r1[, level := gsub('both', '', level)]
  r1 = cbind(r1, ci)

  r1 = merge(r1, a1, by = c('level', 'highenroll', 'highmeals'), all.x = T)

  expect_equal(r1[,value], r1[, mean])

})

test_that('Finding the mean uses svyciprop', {
  s1 <- as_survey_design(svydesign(id=~dnum, weights=~pw, data=apiclus1, fpc=~fpc))
  s1 <- s1 %>% mutate(level = stype == 'M')
  s1 <- s1 %>% mutate(blah = as.numeric(level))

  #NOTE: confint(svyciprop(~level, s1)) and confint(svyby(~level, 1, s1, svyciprop)) don't give the same answers.
  #not sure why

  a1 = calc(s1, what = c('level', 'blah'), metrics = c('mean', 'numerator', 'denominator'), proportion = T)
  a2 = svyciprop(~as.numeric(level), s1)
  a3 = svyby(~level, 1, s1, svyciprop)

  #the two different (svyby and normal approaches work)
  expect_equal(c(as.numeric(a2), confint(a2)), unname(unlist(a1[variable == 'blah', .(mean, mean_lower, mean_upper)])))
})

test_that('time_var and fancy_time options', {
  s1 <- as_survey_design(svydesign(id=~dnum, weights=~pw, data=apiclus1, fpc=~fpc))
  s1 <- s1 %>% mutate(time = rep(c(1,3, 5), nrow(apiclus1)/3))

  a1 = calc(s1, what = c('api00'), metrics = c('mean', 'numerator', 'denominator'), time_var = 'time', fancy_time = TRUE, proportion = FALSE)
  expect_equal(unique(a1[,time]) , '1, 3, 5')

  a2 = calc(s1, what = c('api00'), metrics = c('mean', 'numerator', 'denominator'), time_var = 'time', fancy_time = FALSE, proportion = FALSE)
  expect_equal(unique(a2[,time]) , '1 - 5')

})


