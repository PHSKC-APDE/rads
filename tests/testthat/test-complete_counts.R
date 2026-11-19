library(data.table)

# basic completion ----
test_that('adds missing ages with zero counts', {
  dt <- data.table(age = c(20, 20, 22, 22),
                   sex = c('Female', 'Male', 'Female', 'Male'),
                   deaths = c(3, 1, 2, 4))
  res <- suppressMessages(complete_counts(dt, age = 18:24))

  expect_equal(nrow(res), 14) # 7 ages * 2 sexes
  expect_equal(sort(unique(res$age)), 18:24)
  expect_equal(res[age == 21]$deaths, c(0, 0))
  expect_equal(sum(res$deaths), sum(dt$deaths))
  expect_identical(names(res), names(dt))
})

test_that('completes observed combinations when nothing is supplied in ...', {
  dt <- data.table(age = c(20, 20, 22), sex = c('Female', 'Male', 'Male'), deaths = c(3, 1, 4))
  res <- suppressMessages(complete_counts(dt))

  expect_equal(nrow(res), 4)
  expect_equal(res[age == 22 & sex == 'Female']$deaths, 0)
})

test_that('fills every column in counts', {
  dt <- data.table(age = c(20, 22), deaths = c(3, 4), ypll_65 = c(135, 172))
  res <- suppressMessages(complete_counts(dt, counts = c('deaths', 'ypll_65'), age = 20:22))

  expect_equal(res[age == 21]$deaths, 0)
  expect_equal(res[age == 21]$ypll_65, 0)
})

test_that('works with population tables (no brainer!)', {
  pop <- data.table(age = c(90, 92), gender = 'Female', pop = c(10, 5))
  res <- suppressMessages(complete_counts(pop, counts = 'pop', age = 90:92))

  expect_equal(res[age == 91]$pop, 0)
})

test_that('keeps existing NA counts as NA, with a message', {
  dt <- data.table(age = c(20, 22), deaths = c(NA, 4))
  expect_message(res <- complete_counts(dt, age = 20:22), 'left as NA')
  expect_true(is.na(res[age == 20]$deaths))
  expect_equal(res[age == 21]$deaths, 0)
})

test_that('prints a summary of key columns', {
  dt <- data.table(age = c(20, 20, 22), sex = c('Female', 'Male', 'Male'), deaths = c(3, 1, 4))
  expect_message(complete_counts(dt, age = 20:22), 'added 3 rows')
  expect_message(complete_counts(dt, age = 20:22), 'age\\s+3\\s+supplied')
})

# include.na ----
test_that('include.na controls whether NA is completed', {
  dt <- data.table(age = c(20, 22, NA), sex = c('Female', 'Male', 'Female'), deaths = c(1, 2, 3))

  res_false <- suppressMessages(complete_counts(dt))
  expect_equal(nrow(res_false), 5) # 2 ages * 2 sexes + the existing NA row
  expect_equal(res_false[is.na(age)]$deaths, 3)

  res_true <- suppressMessages(complete_counts(dt, include.na = TRUE))
  expect_equal(nrow(res_true), 6) # 3 ages (incl. NA) * 2 sexes
  expect_equal(res_true[is.na(age) & sex == 'Male']$deaths, 0)
})

# column types ----
test_that('handles integer, factor & character key columns', {
  dt <- data.table(age = c(20L, 20L, 22L), sex = factor(c('F', 'M', 'M')),
                   color = c('blue', 'red', 'green'), deaths = c(1, 2, 3))
  res <- suppressMessages(complete_counts(dt, age = 20:22,
                                          sex = c('F', 'M', 'X'),
                                          color = c('blue', 'red', 'green')))

  expect_true(is.integer(res$age))
  expect_true(is.factor(res$sex))
  expect_true(is.character(res$color))
  expect_equal(nrow(res), 27)
})

# errors ----
test_that('errors on bad input', {
  dt <- data.table(age = c(20, 20, 22), sex = c('Female', 'Male', 'Male'), deaths = c(3, 1, 4))

  expect_error(complete_counts(as.list(dt)), 'must be a data.frame')
  expect_error(complete_counts(dt, counts = 'count'), 'do not exist')
  expect_error(complete_counts(dt, counts = 'sex'), 'not numeric')
  expect_error(complete_counts(dt, 'deaths', 18:64), 'must be named')
  expect_error(complete_counts(dt, myage = 18:64), 'do not exist')
  expect_error(complete_counts(dt, age = c('20', '22')), 'must be numeric')
  expect_error(complete_counts(dt, age = 21:25), 'not in the values you supplied: 20')
  expect_error(complete_counts(rbind(dt, dt)), 'more than one row')
  expect_error(complete_counts(dt, include.na = NA), 'must be TRUE or FALSE')
})

# linked columns ----
test_that('warns about linked (1:1) key columns', {
  dt <- data.table(cause.of.death = c('Cancer', 'Stroke'), causeid = c(1, 2),
                   age = c(20, 22), deaths = c(1, 2))
  expect_warning(suppressMessages(complete_counts(dt)), 'cause.of.death & causeid')

  # no warning once the linked column is dropped
  expect_no_warning(suppressMessages(complete_counts(dt[, list(cause.of.death, deaths)])))
})
