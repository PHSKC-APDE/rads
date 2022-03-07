library('data.table')
library('testthat')

# create test data ----
# Test with 1970 CA Abridged Death Data
# Chiang, Chin Long & World Health Organization. (1979).
# Life table and mortality analysis / Chin Long Chiang.
# World Health Organization. https://apps.who.int/iris/handle/10665/62916
dt <- data.table::data.table(
  age_interval = c("0-1", "1-5", "5-10", "10-15", "15-20", "20-25", "25-30", "30-35",
            "35-40", "40-45", "45-50", "50-55", "55-60", "60-65", "65-70",
            "70-75", "75-80", "80-85", "85+"),
  deaths = c(6234, 1049, 723, 735, 2054, 2702, 2071, 1964, 2588, 4114, 6722,
              8948, 11942, 14309, 17088, 19149, 21325, 20129, 22483),
  pop = c(340483, 1302198, 1918117, 1963681, 1817379, 1740966, 1457614,
                  1219389, 1149999, 1208550, 1245903, 1083852, 933244, 770770,
                  620805, 484431, 342097, 210953, 142691),
  ax = c(0.09, 0.41, 0.44, 0.54, 0.59, 0.49, 0.51, 0.52, 0.53, 0.54, 0.53,
             0.53, 0.52, 0.52, 0.51, 0.52, 0.51, 0.50, NA))

# default argument values
test1 <- life_table(mydt = dt,
                   age_interval = "age_interval",
                   mydeaths = "deaths",
                   mypop = "pop",
                   myfrac = "ax",
                   ci = 0.95)

# non-default argument values
dt2 <- copy(dt)
setnames(dt2, paste0(names(dt), "x"))
test2 <- life_table(mydt = dt2,
                   age_interval = "age_intervalx",
                   mydeaths = "deathsx",
                   mypop = "popx",
                   myfrac = "axx",
                   ci = 0.95)
test2[, axx := NULL]

# alternate ci
test1.90 <- life_table(mydt = dt,
                    age_interval = "age_interval",
                    mydeaths = "deaths",
                    mypop = "pop",
                    myfrac = "ax",
                    ci = 0.90)
test1.99 <- life_table(mydt = dt,
                       age_interval = "age_interval",
                       mydeaths = "deaths",
                       mypop = "pop",
                       myfrac = "ax",
                       ci = 0.99)

test_that("Check for errors based on validation failure...", {
  expect_error(life_table()) # need to specify data.frame
  expect_error(life_table(hello)) # non-existant data.frame
  expect_error(life_table(dt, age_interval = "blah"))
  dta<-copy(dt); dta[, age_interval := gsub("-", "_", age_interval)]
  expect_error(life_table(dta)) # interval needs '-'
  dta<-copy(dt); dta[, age_interval := gsub("\\+", "", age_interval)]
  expect_error(life_table(dta)) # final interval needs '+'
  dta<-copy(dt); dta[, pop := as.character(pop)]
  expect_error(life_table(dta)) # pop must be numeric
  dta<-copy(dt); dta[, deaths := as.character(deaths)]
  expect_error(life_table(dta)) # deaths must be numeric
  dta<-rbind(copy(dt), data.table(age_interval = c(NA, NA), deaths = c(1000, 1000)), fill = T)
  expect_error(life_table(dta)) # age_interval can only have one row with NA
  dta<-copy(dt); dta[, ax := as.character(ax)]
  expect_error(life_table(dta)) # my_frac must be numeric
  expect_error(life_table(dt, ci = 1 )) # ci must be between 0.01 & 0.99
  expect_error(life_table(dt, ci = 0 )) # ci must be between 0.01 & 0.99
  expect_error(life_table(dt, ci = -.1 )) # ci must be between 0.01 & 0.99
})


test_that('Confirm output is independent of argument/column names...',{
  expect_equal( test1, test2)
})

test_that('structure and results compared to Chiang 1979...',{
  expect_equal( nrow(test1), 19)
  expect_equal( ncol(test1), 14)
  expect_equal( test1[1]$qx, 0.01801)
  expect_equal( test1[1]$ex, 71.95)
  expect_equal( test1[10]$qx, 0.01689)
  expect_equal( test1[10]$ex, 35.56)
  expect_equal( test1[19]$qx, 1)
  expect_equal( test1[19]$ex, 6.35)
})

test_that('confidence intervals seem logical...',{
  # remember the higher the % confidence, the wider the interval
  expect_gt( test1.99[7]$ex_upper, test1[7]$ex_upper)
  expect_gt( test1[7]$ex_upper, test1.90[7]$ex_upper)
  expect_lt( test1.99[7]$ex_lower, test1[7]$ex_lower)
  expect_lt( test1[7]$ex_lower, test1.90[7]$ex_lower)
})


test_that('check that deaths with an unknown age interval and redistributed...', {
  dtna <- rbind(dt, data.table(deaths = 16000), fill = T)
  dtna_table <- life_table(dtna)
  expect_equal(nrow(dtna_table), nrow(test1))
  expect_lte(sum(dtna_table$deaths), (sum(test1$deaths) + 16000 + 5)) # allow some buffer for rounding
  expect_gte(sum(dtna_table$deaths), (sum(test1$deaths) + 16000 - 5)) # allow some buffer for rounding
})



