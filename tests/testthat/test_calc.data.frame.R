library('data.table')
library('testthat')
library('rads')

# create test data
set.seed(98104)
dt <- data.table(
  chi_year = rep(2008:2018, 2000),
  chi_sex = factor(sample(c("Male", "Female"), 22000, rep = TRUE, prob = c(0.5, 0.5))),
  chi_age = round(rnorm(22000, 23, 2.5), 0),
  fetal_pres = factor(sample(c("Breech", "Cephalic", "Other", NA), 22000, rep = TRUE, prob = c(0.04, 0.945, 0.01, 0.005))),
  kotelchuck = sample(c(0, 1, NA), 22000, rep = TRUE, prob = c(0.26, 0.64, 0.10)),
  birth_weight_grams = round(rnorm(22000, 3343, 576), 0)
)
dt[rbinom(22000, 1, 0.01) == 1, chi_age := NA] # add missing for age
dt[rbinom(22000, 1, 0.01) == 1, birth_weight_grams := NA] # add missing for birth weight

## run tests
test_that('Check <what>: class(what) == factor',{
          expect_equal( nrow(calc(dt, chi_year==2008, what = c("fetal_pres"), time_var = "chi_year")),
                        length(unique(dt[chi_year==2008 & !is.na(fetal_pres)]$fetal_pres)))
          expect_equal( round2(calc(dt, chi_year==2008, what = c("fetal_pres"), time_var = "chi_year")[level == "Cephalic"]$mean, 3),
                        round2(nrow(dt[chi_year==2008 & fetal_pres=="Cephalic"]) / nrow(dt[chi_year==2008 & !is.na(fetal_pres)]), 3))
          expect_equal( unique(calc(dt, chi_year==2008, what = c("fetal_pres"), time_var = "chi_year")[level == "Cephalic"]$time),
                        "2008")
})

test_that('Check <what>: class(what) == binary',{
          expect_equal( nrow(calc(dt, chi_year==2008, what = c("kotelchuck"), time_var = "chi_year")),
                        1)
          expect_equal( round2(calc(dt, chi_year==2008, what = c("kotelchuck"), time_var = "chi_year")$mean, 3),
                        round2(mean(dt[chi_year==2008]$kotelchuck, na.rm = T), 3))
          expect_equal( unique(calc(dt, chi_year==2008, what = c("kotelchuck"), time_var = "chi_year")$time),
                        "2008")
})

test_that('Check <what>: class(what) == continuous',{
          expect_equal( nrow(calc(dt, chi_year==2008, what = c("birth_weight_grams"), time_var = "chi_year")),
                        1)
          expect_equal( round2(calc(dt, chi_year==2008, what = c("birth_weight_grams"), time_var = "chi_year")$mean, 3),
                        round2(mean(dt[chi_year==2008]$birth_weight_grams, na.rm = T), 3))
          expect_equal( unique(calc(dt, chi_year==2008, what = c("birth_weight_grams"), time_var = "chi_year")$time),
                        "2008")
})

test_that('Check <what>: multiple classes',{
          expect_equal( nrow(calc(dt, chi_year==2008, what = c("kotelchuck", "fetal_pres", "birth_weight_grams"), time_var = "chi_year")),
                        length(unique(dt[chi_year==2008 & !is.na(fetal_pres)]$fetal_pres)) + 2)
          expect_equal( round2(calc(dt, chi_year==2008, what = c("kotelchuck", "fetal_pres", "birth_weight_grams"), time_var = "chi_year")[level == "Cephalic"]$mean, 3),
                        round2(nrow(dt[chi_year==2008 & fetal_pres=="Cephalic"])/nrow(dt[chi_year==2008 & !is.na(fetal_pres)]), 3))
          expect_equal( round2(calc(dt, chi_year==2008, what = c("kotelchuck", "fetal_pres", "birth_weight_grams"), time_var = "chi_year")[variable=="kotelchuck"]$mean, 3),
                        round2(mean(dt[chi_year==2008]$kotelchuck, na.rm = T), 3))
          expect_equal( round2(calc(dt, chi_year==2008, what = c("kotelchuck", "fetal_pres", "birth_weight_grams"), time_var = "chi_year")[variable=="birth_weight_grams"]$mean, 3),
                        round2(mean(dt[chi_year==2008]$birth_weight_grams, na.rm = T), 3))
})

test_that('Check ... (where)',{
          expect_equal( unique(calc(dt, chi_year==2008, what = c("fetal_pres"), time_var = "chi_year")$time),
                        "2008")
          expect_equal( unique(calc(dt, chi_year%in%c(2008:2012), what = c("fetal_pres"), time_var = "chi_year")$time),
                        "2008-2012")
          expect_equal( unique(calc(dt, chi_sex=="Male", what = c("fetal_pres"), time_var = "chi_year")$denominator),
                        nrow(dt[chi_sex == "Male" & !is.na(fetal_pres)]))
          expect_equal( unique(calc(dt, chi_sex=="Male" & chi_year == 2015, what = c("fetal_pres"), time_var = "chi_year")$denominator),
                        nrow(dt[chi_sex == "Male" & chi_year==2015 & !is.na(fetal_pres)]))
          expect_equal( unique(calc(dt, chi_sex=="Male", chi_year == 2015, what = c("fetal_pres"), time_var = "chi_year")$denominator),
                        nrow(dt[chi_sex == "Male" & chi_year==2015 & !is.na(fetal_pres)])) # assess implicity "&" in ...
})

test_that('Check metrics',{
        # check median
          expect_equal( calc(dt, metrics = c("median"), what = c("birth_weight_grams"), time_var = "chi_year")$median,
                        median(dt$birth_weight_grams, na.rm = T))
        # check total
          expect_equal( calc(dt, metrics = c("total"), what = c("birth_weight_grams"), time_var = "chi_year")$total,
                        sum(dt$birth_weight_grams, na.rm = T))
        # check rate
          expect_equal( calc(dt, metrics = c("rate"), what = c("birth_weight_grams"), time_var = "chi_year")$rate,  # should be NA when not binary
                        as.numeric(NA) )
          expect_equal( round2(calc(dt, metrics = c("rate"), what = c("kotelchuck"), time_var = "chi_year")$rate, 3),  # should be NA when not binary
                        round2(1*mean(dt$kotelchuck, na.rm = T), 3) ) # when select rate, default is per 1
        # check rate_per
          expect_equal( round2(calc(dt, metrics = c("rate"), per = 10, what = c("kotelchuck"), time_var = "chi_year")$rate, 3),
                        round2(10*mean(dt$kotelchuck, na.rm = T), 3) )
          expect_equal( calc(dt, metrics = c("rate"), per = 10, what = c("kotelchuck"), time_var = "chi_year")$rate_per,
                        10 )

        # check rse
          expect_equal( round2(calc(dt, metrics = c("mean", "se", "rse"), what = c("birth_weight_grams"), time_var = "chi_year")$rse, 4),
                        round2( 100*(sd(dt$birth_weight_grams, na.rm = T) / sqrt(nrow(dt[!is.na(birth_weight_grams)]))
                                 / mean(dt$birth_weight_grams, na.rm = T)), 4) )
        # check numerator
          expect_equal( calc(dt, metrics = c("numerator"), what = c("kotelchuck"), time_var = "chi_year")$numerator,
                        nrow(dt[kotelchuck==1]))
        # check denominator
          expect_equal( calc(dt, metrics = c("denominator"), what = c("kotelchuck"), time_var = "chi_year")$denominator,
                        nrow(dt[!is.na(kotelchuck)]))
        # check missing
          expect_equal( calc(dt, metrics = c("missing"), what = c("kotelchuck"), time_var = "chi_year")$missing,
                        nrow(dt[is.na(kotelchuck)]))
        # check missing.prop
          expect_equal( round2(calc(dt, metrics = c("missing.prop"), what = c("kotelchuck"), time_var = "chi_year")$missing.prop, 3),
                        round2(nrow(dt[is.na(kotelchuck)])/nrow(dt), 3) )
        # check obs
          expect_equal( calc(dt, metrics = c("obs"), "chi_age<20", what = c("kotelchuck"), time_var = "chi_year")$obs,
                        nrow(dt[chi_age<20]))
        # check ndistinct
          expect_equal( calc(dt, metrics = c("ndistinct"), what = c("birth_weight_grams"), time_var = "chi_year", by = c("chi_sex"))[chi_sex=="Male"]$ndistinct,
                        length(unique(dt[!is.na(birth_weight_grams) & chi_sex == "Male"]$birth_weight_grams)) )
})

test_that('Check per',{
  expect_equal( round2(calc(dt, metrics = c("rate"), per = 1000, what = c("kotelchuck"), time_var = "chi_year")$rate, 3),
                round2(1000*mean(dt$kotelchuck, na.rm = T), 3) )
  expect_equal( round2(calc(dt, metrics = c("rate"), per = 10, what = c("kotelchuck"), time_var = "chi_year")$rate, 3),
                round2(10*mean(dt$kotelchuck, na.rm = T), 3) )
  expect_equal( calc(dt, metrics = c("rate"), per = 10, what = c("kotelchuck"), time_var = "chi_year")$rate_per,
                10 )
})

test_that('Check win: rolling averages, sums, etc.',{
  expect_equal( round2(calc(dt, what = c("birth_weight_grams"), win=3, time_var = "chi_year")[time == "2008-2010"]$mean, 3),
                round2(mean(dt[chi_year %in% c(2008:2010)]$birth_weight_grams, na.rm = T), 3))
  expect_equal( round2(calc(dt, what = c("birth_weight_grams"), win=3, time_var = "chi_year")[time == "2013-2015"]$mean, 3),
                round2(mean(dt[chi_year %in% c(2013:2015)]$birth_weight_grams, na.rm = T), 3))
})

test_that('Check ci:',{

})

test_that('Check fancy_time and similar',{
  expect_equal( calc(dt, metrics = c("mean"), chi_year %in% c(2008:2011, 2013, 2015:2018), what = c("kotelchuck"), time_var = "chi_year")$time,
                "2008-2011, 2013, 2015-2018" )
  expect_equal( calc(dt, metrics = c("mean"), chi_year %in% c(2008:2011, 2012, 2013, 2015:2018), what = c("kotelchuck"), time_var = "chi_year", fancy_time = T)$time,
                "2008-2013, 2015-2018" )
  expect_equal( calc(dt, metrics = c("mean"), chi_year %in% c(2008:2011, 2012, 2013, 2015:2018), what = c("kotelchuck"), time_var = "chi_year", fancy_time = F)$time,
                "2008-2018" )

  #factor variable times
  d = copy(dt)
  d[, sss := fetal_pres]
  d[(chi_year == 2017) | (chi_year == 2010 & sss == 'Cephalic'), sss := NA]
  r1 = calc(d, metrics = 'mean', what = 'sss', time_var = 'chi_year', fancy_time = T)

  #record data assumes absence is a true 0, and therefore Cephalic data is "present" in 2010 (e.g. 0 / denominator)
  #this is a divergance from calc.tbl_svy
  expect_equal(r1$time, rep(c('2008-2016, 2018'),3))

  #numeric variable times
  d[chi_year == 2016, birth_weight_grams := NA]
  r2 = calc(d, metrics = 'mean', what = 'birth_weight_grams', time_var = 'chi_year', fancy_time = T)
  expect_equal(r2$time, c('2008-2015, 2017-2018'))



})

