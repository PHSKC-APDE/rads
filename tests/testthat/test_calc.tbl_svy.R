library('srvyr')
library('survey')
library('dplyr')
library('data.table')

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
#
test_that('Multi Grouping with filtering and multiple what variables',{
  st = calc(sur, what = c('api00', 'api99'),
                       cname == 'Los Angeles',
                       by = c('stype', 'cname'),
                       metrics = 'mean', time_var = NULL)
  man1 = sur %>% filter(cname == 'Los Angeles') %>% group_by(stype, cname) %>% summarize(mean = survey_mean(api00)) %>% setDT
  man2 = sur %>% filter(cname == 'Los Angeles') %>% group_by(stype, cname) %>% summarize(mean = survey_mean(api99)) %>% setDT
  man = rbind(man1, man2)[, .(stype, cname, mean, variable = c(rep('api00', 3), rep('api99', 3)))]
  expect_equal(st, man)
})
#
test_that('Proportion toggle- on',{
  sur = sur %>% mutate(schwide = as.numeric(sch.wide == 'Yes'))
  st = calc(sur, 'schwide', metrics = 'lower', proportion = T, time_var = NULL)
  man = sur %>% summarize(blah = survey_mean(schwide, vartype = 'ci', proportion = T)) %>% setDT

  #without proportion
  expect_equal(st[, lower], man[, blah_low])

})
#
test_that('Proportion toggle changes result',{
  sur = sur %>% mutate(schwide = as.numeric(sch.wide == 'Yes'))
  a = calc(sur, 'schwide', metrics = 'lower', proportion = T, time_var = NULL)[, lower]
  b = calc(sur, 'schwide', metrics = 'lower', proportion = F, time_var = NULL)[, lower]

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
  expect_error(calc(sur, 'api00', metrics = 'turtles', time_var = NULL), 'should be one of')
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


