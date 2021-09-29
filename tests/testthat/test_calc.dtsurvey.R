library('testthat')
library('rads')
library('survey')
library('data.table')
library('dtsurvey')
set.seed(98104)


data(api) #from the survey package
apiclus1$nagrp = sample(c(1,2,NA), nrow(apiclus1), replace = T)
apiclus1$nagrp2 = sample(c(1,2,NA), nrow(apiclus1), replace = T)
apiclus1$ysw = as.numeric(apiclus1$sch.wide == "Yes")
apiclus1$yyy = sample(1:4, nrow(apiclus1), replace = T)
apiclus1$yyy2 = sample(c(1,3,5), nrow(apiclus1), replace = T)
apiclus1$yyy3 = sample(c(1,3,5), nrow(apiclus1), replace = T)
apiclus1$yyy3[apiclus1$stype=='E' & apiclus1$yyy3 %in% c(3,5)] = 1
apiclus1$yyy3[apiclus1$stype!='E' & apiclus1$yyy3 %in% c(3)] = 5
apiclus1$E = as.numeric(apiclus1$stype == 'E')
apiclus1$E[apiclus1$yyy3 == 5] = NA
apiclus1$H = as.numeric(apiclus1$stype == 'H')
apiclus1$M = as.numeric(apiclus1$stype == 'M')
apiclus1$blah = sample(c(NA,0,1), nrow(apiclus1), replace = T)
apiclus1$highenroll = apiclus1$enroll>median(apiclus1$enroll)
apiclus1$highmeals = apiclus1$meals>median(apiclus1$meals)
apiclus1$hm_bin = as.numeric(apiclus1$highmeals)
apiclus1$both_bin = apiclus1$both == 'Yes'
apiclus1$w = 'banana'
apiclus1$awardsYes = as.numeric(apiclus1$awards == 'Yes')
s2 <- dtrepsurvey(as.svrepdesign(svydesign(id=~dnum, weights=~pw, data=apiclus1)))
clus<-svydesign(id=~dnum, weights=~pw, data=apiclus1)
sur = dtsurvey(apiclus1, psu = 'dnum', weight = 'pw')

test_that('Defaults (mostly) work: svy',{
          r1 = calc(sur, what = 'api00', time_var = NULL)$mean
          r2 = sur[, smean(api00)]
          r3 = unname(coef(svymean(~api00, clus)))

          expect_equal(r1,r2)
          expect_equal(r1,r3)

})

test_that('Grouping without filtering',{
  expect_equal(
    calc(sur, 'api00', by = c('stype'), metrics = 'denominator', time_var = NULL)[, denominator],
    sur[, .(denominator = .N), stype][, denominator]
  )
})

test_that('Multi Grouping with filtering',{
          st = calc(sur, 'api00', cname == 'Los Angeles', by = c('stype', 'cname'), metrics = 'denominator', time_var = NULL)[, .(stype, cname, denominator)]
          man = sur[cname == 'Los Angeles', .(denominator = .N), .(stype, cname)]
          expect_equal(st, as.data.table(man))
})

test_that('Grouping with NAs in the group',{

  #one grouping
  r1 = calc(sur, 'api00', by = c('nagrp'), metrics = 'mean') #returns NA group
  r2 = sur[, .(mean = smean(api00)), nagrp] #returns NA group
  r3 = svyby(~api00, ~nagrp, clus, svymean) #does not return NA group, but that is ok
  r3 = data.table(r3)[, .(nagrp, mean = api00)]
  setorder(r1, nagrp)
  setorder(r2, nagrp)

  expect_equal(r1[, mean], r2[, mean])
  expect_equal(r1[!is.na(nagrp), mean], r3[,mean])

  #two by variables, both with NAs in them
  r4 = calc(sur, 'api00', by = c('nagrp', 'nagrp2'), metrics = 'mean') #returns NA group
  r5 = sur[, .(mean = smean(api00)), .(nagrp, nagrp2)] #returns NA group
  r6 = svyby(~api00, ~nagrp+nagrp2, clus, svymean) #does not return NA group, but that is ok
  r6 = data.table(r6)[, .(nagrp,nagrp2, mean = api00)]
  setorder(r4, nagrp, nagrp2)
  setorder(r5, nagrp, nagrp2)
  setorder(r6, nagrp, nagrp2)
  expect_equal(r4[, mean], r5[, mean])
  expect_equal(r4[!is.na(nagrp) & !is.na(nagrp2), mean], r6[,mean])

})

#TODO add a version of this test to dtsurvey?
test_that('Multi Grouping with filtering and multiple what variables',{
  r1 = calc(sur, what = c('api00', 'api99'),
                       cname != 'Los Angeles',
                       by = c('stype', 'sch.wide'),
                       metrics = 'mean')[, .(stype, sch.wide, variable, mean, mean_se)]

  #The syntax here is a bit messy since multiple smeans in the same `[` call gets wonky kind of quickly.
  r2 = sur[cname != 'Los Angeles',
           .(a00 = list(smean(api00, var_type = 'se')),
             a99 = list(smean(api99, var_type = 'se'))),
           .(stype, sch.wide)]
  r2[, c('mean_api00', 'mean_se_api00') := rbindlist(a00)]
  r2[, c('mean_api99', 'mean_se_api99') := rbindlist(a99)]
  r2[, c('a00', 'a99') := NULL]
  r2 = melt(r2, id.vars = c('stype', 'sch.wide'), variable.factor = F,
            measure.vars = list(c('mean_api00', 'mean_api99'), c('mean_se_api00', 'mean_se_api99')),
            value.name = c('mean', 'mean_se'))
  r2[, variable := paste0('api', substr(2001-as.numeric(variable),3,4))]

  r3 = svyby(~api00+api99, ~stype + sch.wide, subset(clus, cname != 'Los Angeles'), svymean)
  r3 = data.table(r3)
  r3 = melt(r3, id.vars = c('stype', 'sch.wide'), variable.factor = F,
            measure.vars = list(c('api00', 'api99'), c('se.api00', 'se.api99')),
            value.name = c('mean', 'mean_se'))
  r3[, variable := paste0('api', substr(2001-as.numeric(variable),3,4))]

  setorder(r1, sch.wide, stype )
  setorder(r2, sch.wide, stype )
  setorder(r3, sch.wide, stype )

  expect_equal(r1, r2)
  expect_equal(r1, r3)

})
#
test_that('Proportion toggle- on',{
  r1 = calc(sur, 'ysw', metrics = 'mean', proportion = T)[, .(result = mean, lower = mean_lower, upper = mean_upper)]
  r2 = sur[, smean(ysw, var_type = 'ci', proportion = T, ci_method = 'xlogit')]
  r3 = svyciprop(~ysw, clus, method = 'xlogit')
  r3 = unname(c(coef(r3), confint(r3)))

  r1 = unname(unlist(r1))
  r2 = unname(unlist(r2))

  #without proportion
  expect_equal(r1, r2)
  expect_equal(r1, r3)

})
#
test_that('Proportion toggle changes result',{
  a = calc(sur, 'ysw', metrics = 'mean', proportion = T, time_var = NULL)[, mean_lower]
  b = calc(sur, 'ysw', metrics = 'mean', proportion = F, time_var = NULL)[, mean_lower]

  expect_true(a != b)

})

#test windowing
test_that('Win(dows) do sensible things',{
  r1 = calc(sur, 'api00', metrics = 'mean', proportion = F, time_var = 'yyy', win = 2)[, .(mean,yyy)]
  r2 = rbind(
    sur[yyy %in% 1:2, .(mean = mean(api00), yyy = paste(unique(sort(yyy)), collapse = '-')) ],
    sur[yyy %in% 2:3, .(mean = mean(api00), yyy = paste(unique(sort(yyy)), collapse = '-')) ],
    sur[yyy %in% 3:4, .(mean = mean(api00), yyy = paste(unique(sort(yyy)), collapse = '-')) ]
  )
  r3 = rbind(
    data.table(yyy = '1-2', mean = svymean(~api00, subset(clus, yyy %in% c(1,2)))),
    data.table(yyy = '2-3', mean = svymean(~api00, subset(clus, yyy %in% c(2,3)))),
    data.table(yyy = '3-4', mean = svymean(~api00, subset(clus, yyy %in% c(3,4))))

  )[, .(mean = as.numeric(mean), yyy)]


  expect_equal(r1,r2)
  expect_equal(r1,r3)

})

#window without time var
test_that('win(dow) without time var',{
  expect_error(calc(sur, 'api00', metrics = 'mean', proportion = F, time_var = NULL, win = 2), 'win(dow)', fixed = T)
})

#time var without window
test_that('Time var without window',{
  r1 = calc(sur, 'api00', metrics = 'mean', proportion = F, time_var = 'yyy', win = NULL)[, .(yyy, mean)]
  r2 = sur[, .(yyy = '1-4', mean = smean(api00))]
  r2 = data.table(r2) #strip the dtsurvey stuff
  r3 = data.table(yyy = '1-4', mean = as.numeric(svymean(~api00, clus)))

  expect_equal(r1,r2)
  expect_equal(r1,r3)

})

#
test_that('Invalid input for what',{
  expect_error(calc(sur, 'thisvarnoexists', time_var = NULL), 'column in `ph.data`')
})
#
test_that('Invalid input for ... filters',{
  expect_error(calc(sur, 'api00', bananas>100 & api00 < 50, time_var = NULL), 'bananas')
})

test_that('Invalid input for by',{
  expect_error(calc(sur, 'api00', by = 'turtles', time_var = NULL), '`by` must be a')
})
#
test_that('Invalid input for metric',{
  expect_error(calc(sur, 'api00', metrics = 'turtles', time_var = NULL), 'Requested invalid')
})
#

#test numerator and denominator calculations
test_that('Numerator and denominator calculations account for NAs',{

  #make a column with NAs
  dt = data.table::as.data.table(sur)

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


  a1 = calc(sur, 'stype', by = 'both', time_var = 'yyy', metrics = setdiff(metrics(), 'median'))
  a2 = calc(s2, 'stype', by = 'both', time_var = 'yyy', metrics = setdiff(metrics(), 'median'))

  #Epect that the means are equal. Se is different depending on method
  expect_equal(a1[, .(mean, total, rate, numerator, missing, unique.time, denominator, missing.prop, obs)],
               a2[, .(mean, total, rate, numerator, missing, unique.time, denominator, missing.prop, obs)])

})

test_that('Multiple by conditions',{


  s1 <- sur


  a1 = calc(s1, 'both', by = c('highenroll', 'highmeals'), metrics = c('mean', 'numerator', 'denominator'))
  a2 = calc(s2, 'both', by = c('highenroll', 'highmeals'), metrics = c('mean', 'numerator', 'denominator'))

  #confirm survey design and survey rep are the same
  expect_equal(a1[, .(variable, level, highenroll, highmeals, mean, numerator, denominator)],a2[,.(variable, level, highenroll, highmeals, mean, numerator, denominator)])

  r1 = svyby(~both, ~highenroll + highmeals, clus, svymean)
  ci = confint(r1)
  r1 = setDT(as.data.frame(r1))
  r1[, grep('se.', names(r1), fixed = T) := NULL]
  r1 = melt(r1, id.vars = c('highenroll', 'highmeals'), measure.vars = paste0('both',c('No', 'Yes')), variable.factor = F, variable.name = 'level')
  r1[, level := gsub('both', '', level)]
  r1 = cbind(r1, ci)

  r1 = merge(r1, a1, by = c('level', 'highenroll', 'highmeals'), all.x = T)

  expect_equal(r1[,value], r1[, mean])

})

test_that('Proportion toggle', {


  #NOTE: confint(svyciprop(~level, s1)) and confint(svyby(~level, 1, s1, svyciprop)) don't give the same answers.
  #not sure why. Note 9/15/21-- I think it might have to do with passing degrees of freedom, but I don't remember.

  a1 = calc(sur, what = c('hm_bin'), metrics = c('mean', 'numerator', 'denominator'), proportion = T)
  a2 = svyciprop(~hm_bin, clus)
  a3 = svyby(~hm_bin, 1, clus, svyciprop)

  #the two different (svyby and normal approaches work)
  expect_equal(c(as.numeric(a2), confint(a2)), unname(unlist(a1[, .(mean, mean_lower, mean_upper)])))
})

test_that('time_var, fancy_time, and missing years because of NAs options', {

  a1 = calc(sur, what = c('api00'), metrics = c('mean', 'numerator', 'denominator'), time_var = 'yyy2', fancy_time = TRUE, proportion = FALSE)
  expect_equal(unique(a1[,yyy2]) , '1, 3, 5')

  a2 = calc(sur, what = c('api00'), metrics = c('mean', 'numerator', 'denominator'), time_var = 'yyy2', fancy_time = FALSE, proportion = FALSE)
  expect_equal(unique(a2[,yyy2]) , '1-5')

  #what happens when its a factor
  #this tests entire year missingness and additional missingness by type & year
  #time calculations are dependant on the variable, not the value of the variable
  a3 = calc(sur, what = 'stype', time_var = 'yyy3', fancy_time = T)
  a4 = calc(sur, what = 'stype', time_var = 'yyy3', fancy_time = F)

  expect_equal(a3[, .(level, yyy3)], data.table(level = c('E', 'H', 'M'), yyy3 = c('1, 5')))
  expect_equal(a4[, .(level, yyy3)], data.table(level = c('E', 'H', 'M'), yyy3 = c('1-5')))

  #several variables passed to what, with different temporal dimensions
  a5 = calc(sur, what = c('E', 'H', 'M'), time_var = 'yyy3', fancy_time = T)
  a6 = calc(sur, what = c('E', 'H', 'M'), time_var = 'yyy3', fancy_time = F)

  expect_equal(a5[, .(variable, yyy3)], data.table(variable = c('E', 'H', 'M'), yyy3 = c('1', '1, 5', '1, 5')))
  expect_equal(a6[, .(variable, yyy3)], data.table(variable = c('E', 'H', 'M'), yyy3 = c('1', '1-5','1-5')))


})

test_that('ci option works', {
  #normal vars
  r1 = calc(sur, what = 'api00', metrics = c('mean'), ci = .95)
  r2 = calc(sur, what = 'api00', metrics = c('mean'), ci = .99)

  expect_gt(r2[, mean_upper], r1[, mean_upper])
  expect_lt(r2[, mean_lower], r1[, mean_lower])

  #factor vars
  r3 = calc(sur, what = 'stype', metrics = c('mean'), ci = .95)
  r4 = calc(sur, what = 'stype', metrics = c('mean'), ci = .99)

  expect_gt(r4[2, mean_upper], r3[2, mean_upper])
  expect_lt(r4[2, mean_lower], r3[2, mean_lower])

})

test_that('proportion cis for factors', {
  #normal vars
  r1 = calc(sur, what = 'awards', metrics = c('mean'), proportion = T, ci = .95)
  r2 = calc(sur, what = 'awards', metrics = c('mean'), proportion = F, ci = .95)
  r3 = svyciprop(~awardsYes, clus, method = 'logit')
  r3 = data.table(t(c(r3, confint(r3))))
  setnames(r3, c('mean', 'mean_lower', 'mean_upper'))

  expect_equal(r1[, mean], r2[, mean])
  expect_false(identical(r1,r2))
  expect_equal(r1[level == 'Yes', .(mean, mean_lower, mean_upper)], r3)

})



