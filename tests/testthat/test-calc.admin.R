library('testthat')
library('rads')
library('data.table')
library('dtsurvey')

# create test data
set.seed(98104)
dt <- data.table::data.table(
  year = rep(2008:2018, 2000),
  sex = factor(sample(c("Male", "Female"), 22000, rep = TRUE, prob = c(0.5, 0.5))),
  age = round(rnorm(22000, 23, 2.5), 0),
  fetal_pres = factor(sample(c("Breech", "Cephalic", "Other", NA), 22000, rep = TRUE, prob = c(0.04, 0.945, 0.01, 0.005))),
  kotelchuck = sample(c(0, 1, NA), 22000, rep = TRUE, prob = c(0.26, 0.64, 0.10)),
  birth_weight_grams = round(rnorm(22000, 3343, 576), 0)
)
dt[rbinom(22000, 1, 0.01) == 1, age := NA] # add missing for age
dt[rbinom(22000, 1, 0.01) == 1, birth_weight_grams := NA] # add missing for birth weight
dt = dtadmin(dt)
## run tests
test_that('Check <what>: class(what) == factor',{
          expect_equal( nrow(calc(dt, year==2008, what = c("fetal_pres"), time_var = "year")),
                        length(unique(dt[year==2008 & !is.na(fetal_pres)]$fetal_pres)))
          expect_equal( round2(calc(dt, year==2008, what = c("fetal_pres"), time_var = "year")[level == "Cephalic"]$mean, 3),
                        round2(nrow(dt[year==2008 & fetal_pres=="Cephalic"]) / nrow(dt[year==2008 & !is.na(fetal_pres)]), 3))
          expect_equal( unique(calc(dt, year==2008, what = c("fetal_pres"), time_var = "year")[level == "Cephalic"]$year),
                        '2008')
})

test_that('Check <what>: class(what) == binary',{
          expect_equal( nrow(calc(dt, year==2008, what = c("kotelchuck"), time_var = "year")),
                        1)
          expect_equal( round2(calc(dt, year==2008, what = c("kotelchuck"), time_var = "year")$mean, 3),
                        round2(mean(dt[year==2008]$kotelchuck, na.rm = T), 3))
          expect_equal( unique(calc(dt, year==2008, what = c("kotelchuck"), time_var = "year")$year),
                        '2008')
})

test_that('Check <what>: class(what) == continuous',{
          expect_equal( nrow(calc(dt, year==2008, what = c("birth_weight_grams"), time_var = "year")),
                        1)
          expect_equal( round2(calc(dt, year==2008, what = c("birth_weight_grams"), time_var = "year")$mean, 3),
                        round2(mean(dt[year==2008]$birth_weight_grams, na.rm = T), 3))
          expect_equal( unique(calc(dt, year==2008, what = c("birth_weight_grams"), time_var = "year")$year),
                        '2008')
})

test_that('Check <what>: multiple classes',{
          expect_equal( nrow(calc(dt, year==2008, what = c("kotelchuck", "fetal_pres", "birth_weight_grams"), time_var = "year")),
                        length(unique(dt[year==2008 & !is.na(fetal_pres)]$fetal_pres)) + 2)

          expect_equal( round2(calc(dt, year==2008, what = c("kotelchuck", "fetal_pres", "birth_weight_grams"), time_var = "year")[level == "Cephalic"]$mean, 3),
                        round2(nrow(dt[year==2008 & fetal_pres=="Cephalic"])/nrow(dt[year==2008 & !is.na(fetal_pres)]), 3))

          expect_equal( round2(calc(dt, year==2008, what = c("kotelchuck", "fetal_pres", "birth_weight_grams"), time_var = "year")[variable=="kotelchuck"]$mean, 3),
                        round2(mean(dt[year==2008]$kotelchuck, na.rm = T), 3))

          expect_equal( round2(calc(dt, year==2008, what = c("kotelchuck", "fetal_pres", "birth_weight_grams"), time_var = "year")[variable=="birth_weight_grams"]$mean, 3),
                        round2(mean(dt[year==2008]$birth_weight_grams, na.rm = T), 3))
})

test_that('Check metrics',{
        # check median
          expect_equal( calc(dt, metrics = c("median"), what = c("birth_weight_grams"), time_var = "year")$median,
                        median(dt$birth_weight_grams, na.rm = T))
        # check total
          expect_equal( calc(dt, metrics = c("total"), what = c("birth_weight_grams"), time_var = "year")$total,
                        sum(dt$birth_weight_grams, na.rm = T))
        # check rate
          expect_equal( round2(calc(dt, metrics = c("rate"), what = c("kotelchuck"), time_var = "year")$rate, 3),  # should be NA when not binary
                        round2(1*mean(dt$kotelchuck, na.rm = T), 3) ) # when select rate, default is per 1
        # check rate_per
          expect_equal( round2(calc(dt, metrics = c("rate"), per = 10, what = c("kotelchuck"), time_var = "year")$rate, 3),
                        round2(10*mean(dt$kotelchuck, na.rm = T), 3) )
          expect_equal( calc(dt, metrics = c("rate"), per = 10, what = c("kotelchuck"), time_var = "year")$rate_per,
                        10 )
        # check rse
          expect_equal( round2(calc(dt, metrics = c("mean", "rse"), what = c("birth_weight_grams"), time_var = "year")$rse, 4),
                        round2( 100*(sd(dt$birth_weight_grams, na.rm = T) / sqrt(nrow(dt[!is.na(birth_weight_grams)]))
                                 / mean(dt$birth_weight_grams, na.rm = T)), 4) )
        # check numerator
          expect_equal( calc(dt, metrics = c("numerator"), what = c("kotelchuck"), time_var = "year")$numerator,
                        nrow(dt[kotelchuck==1]))
        # check denominator
          expect_equal( calc(dt, metrics = c("denominator"), what = c("kotelchuck"), time_var = "year")$denominator,
                        nrow(dt[!is.na(kotelchuck)]))
        # check missing
          expect_equal( calc(dt, metrics = c("missing"), what = c("kotelchuck"), time_var = "year")$missing,
                        nrow(dt[is.na(kotelchuck)]))
        # check missing.prop
          expect_equal( round2(calc(dt, metrics = c("missing.prop"), what = c("kotelchuck"), time_var = "year")$missing.prop, 3),
                        round2(nrow(dt[is.na(kotelchuck)])/nrow(dt), 3) )
        # check obs
          expect_equal( calc(dt, metrics = c("obs"), age<20, what = c("kotelchuck"), time_var = "year")$obs,
                        nrow(dt[age<20]))
})

test_that('Check per',{
  expect_equal( round2(calc(dt, metrics = c("rate"), per = 1000, what = c("kotelchuck"), time_var = "year")$rate, 3),
                round2(1000*mean(dt$kotelchuck, na.rm = T), 3) )
  expect_equal( round2(calc(dt, metrics = c("rate"), per = 10, what = c("kotelchuck"), time_var = "year")$rate, 3),
                round2(10*mean(dt$kotelchuck, na.rm = T), 3) )
  expect_equal( calc(dt, metrics = c("rate"), per = 10, what = c("kotelchuck"), time_var = "year")$rate_per,
                10 )
})

test_that('Check win: rolling averages, sums, etc.',{
  expect_equal( round2(calc(dt, what = c("birth_weight_grams"), win=3, time_var = "year")[year == "2008-2010"]$mean, 3),
                round2(mean(dt[year %in% c(2008:2010)]$birth_weight_grams, na.rm = T), 3))
  expect_equal( round2(calc(dt, what = c("birth_weight_grams"), win=3, time_var = "year")[year == "2013-2015"]$mean, 3),
                round2(mean(dt[year %in% c(2013:2015)]$birth_weight_grams, na.rm = T), 3))
})

test_that('Check ci:',{
  #numeric, large denominator
  r1 = calc(dt, what = 'birth_weight_grams', time_var = 'year', metrics = 'mean', ci = .95)
  r2 = calc(dt, what = 'birth_weight_grams', time_var = 'year', metrics = 'mean', ci = .90)
  expect_lte(r1$mean_lower, r2$mean_lower)
  expect_gte(r1$mean_upper, r2$mean_upper)
  expect_equal(r1$mean, r2$mean)

  #numeric, small denominator
  r3 = calc(dt[1:29,], what = 'birth_weight_grams', time_var = 'year', metrics = 'mean', ci = .95)
  r4 = calc(dt[1:29,], what = 'birth_weight_grams', time_var = 'year', metrics = 'mean', ci = .90)
  expect_lte(r3$mean_lower, r4$mean_lower)
  expect_gte(r3$mean_upper, r4$mean_upper)

  #binary/factor, small denominator
  r5 = calc(dt, what = 'fetal_pres', time_var = 'year', metrics = 'mean', ci = .95)
  r6 = calc(dt, what = 'fetal_pres', time_var = 'year', metrics = 'mean', ci = .90)
  expect_lte(r5$mean_lower[1], r6$mean_lower[1])
  expect_gte(r5$mean_upper[1], r6$mean_upper[1])


})

test_that('Check fancy_time and similar',{
  expect_equal( calc(dt, metrics = c("mean"), year %in% c(2008:2011, 2013, 2015:2018), what = c("kotelchuck"), time_var = "year")$year,
                "2008-2011, 2013, 2015-2018" )
  expect_equal( calc(dt, metrics = c("mean"), year %in% c(2008:2011, 2012, 2013, 2015:2018), what = c("kotelchuck"), time_var = "year", fancy_time = T)$year,
                "2008-2013, 2015-2018" )
  expect_equal( calc(dt, metrics = c("mean"), year %in% c(2008:2011, 2012, 2013, 2015:2018), what = c("kotelchuck"), time_var = "year", fancy_time = F)$year,
                "2008-2018" )

  #factor variable times
  d = copy(dt)
  d[, sss := fetal_pres]
  d[(year == 2017) | (year == 2010 & sss == 'Cephalic'), sss := NA]
  r1 = calc(d, metrics = 'mean', what = 'sss', time_var = 'year', fancy_time = T)

  #record data assumes absence is a true 0, and therefore Cephalic data is "present" in 2010 (e.g. 0 / denominator)
  #this is a divergance from calc.tbl_svy
  expect_equal(r1$year, rep(c('2008-2016, 2018'),3))

  #numeric variable times
  d[year == 2016, birth_weight_grams := NA]
  r2 = calc(d, metrics = 'mean', what = 'birth_weight_grams', time_var = 'year', fancy_time = T)
  expect_equal(r2$year, c('2008-2015, 2017-2018'))

})

test_that('Nonspecified time',{
  r1 = calc(dt, metrics = 'mean', what = 'birth_weight_grams')
  dt[, yyy := -10]
  r2 = calc(dt, metrics = 'mean', time_var = 'yyy', what = 'birth_weight_grams')

  expect_false('time' %in% names(r1))
  expect_equal(r2$yyy, '-10')
  expect_equal(r1$mean, r2$mean)

})

test_that('NA in by is valid.',{

  dt[, fact := fetal_pres]
  dt[is.na(fact), fact := 'Turtle']
  dt[fact == 'Cephalic', fact := NA]
  r1 = calc(dt, 'kotelchuck', metrics = c('mean', 'numerator', 'denominator', 'missing'), by = 'fetal_pres', proportion = TRUE)
  r2 = calc(dt, 'kotelchuck', metrics = c('mean', 'numerator', 'denominator', 'missing'), by = 'fact', proportion = TRUE)

  expect_equal(r1[fetal_pres == 'Cephalic', .(mean, numerator, denominator, missing)], r2[is.na(fact), .(mean, numerator, denominator, missing)])

})

test_that('When a by level is all NA, a result is still provided',{

  dt[, bw := birth_weight_grams]
  dt[, fact := fetal_pres]
  dt[fact == 'Cephalic', bw := NA]
  r1 = calc(dt, 'bw', metrics = c('mean', 'numerator', 'denominator', 'missing'), by = 'fact', proportion = FALSE)


  expect_equal(nrow(r1[fact == 'Cephalic']), 1)
  expect_equal(r1[fact == 'Cephalic', .(mean, numerator, denominator, mean_se)],
               data.table(mean = NaN, numerator = 0, denominator = 0, mean_se = NA_real_))

})

