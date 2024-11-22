library('testthat')
library(DBI)
library(data.table)

# adjust_direct() ----
test_that('adjust_direct',{

  temp.direct <- adjust_direct(count = c(11, 9), pop = c(500, 500), stdpop = c(640, 720), per = 100, conf.level = 0.95)

  expect_equal(20, temp.direct[["count"]] )

  expect_equal(2, temp.direct[["crude.rate"]] )

  expect_equal(1.2217, round(temp.direct[["crude.lci"]], 4) ) # checked vis-à-vis survival::cipoisson() exact method

  expect_equal(3.0888, round(temp.direct[["crude.uci"]], 4) ) # checked vis-à-vis survival::cipoisson() exact method

  expect_equal(1.9882, round(temp.direct[["adj.rate"]], 4) ) # checked vis-à-vis epitools::ageadjust.direct

  expect_equal(1.2133, round(temp.direct[["adj.lci"]], 4) ) # checked vis-à-vis epitools::ageadjust.direct

  expect_equal(3.0820, round(temp.direct[["adj.uci"]], 4) ) # checked vis-à-vis epitools::ageadjust.direct

})

# age_standardize() ----
test_that('age_standardize ... valid output',{
  temp.dt1 <- data.table(age = c(50:60), count = c(25:35), pop = c(seq(1000, 800, -20)) )
  temp.agestd1 <- suppressWarnings(age_standardize(ph.data = temp.dt1, ref.popname = "2000 U.S. Std Population (18 age groups - Census P25-1130)", collapse = T,
                  my.count = "count", my.pop = "pop", per = 1000, conf.level = 0.95))

  expect_equal(sum(temp.dt1$count), temp.agestd1[["count"]])

  expect_equal(round(1000*sum(temp.dt1$count) / sum(temp.dt1$pop), 2), temp.agestd1[["crude.rate"]])

  expect_equal(29.83, temp.agestd1[["crude.lci"]] ) # checked vis-à-vis survival::cipoisson() exact method

  expect_equal(37.13, temp.agestd1[["crude.uci"]] ) # checked vis-à-vis survival::cipoisson() exact method

  expect_equal(35.10, temp.agestd1[["adj.rate"]] ) # checked vis-à-vis epitools::ageadjust.direct

  expect_equal(30.62, temp.agestd1[["adj.lci"]] ) # checked vis-à-vis epitools::ageadjust.direct

  expect_equal(40.25, temp.agestd1[["adj.uci"]] ) # checked vis-à-vis epitools::ageadjust.direct


  temp.dt2 <- data.table(sex = c(rep("M", 11), rep("F", 11)), age = rep(50:60, 2),
                      count = c(25:35, 26:36), pop = c(seq(1000, 900, -10), seq(1100, 1000, -10)),
                      stdpop = rep(1000, 22))
  temp.agestd2 <- suppressWarnings(age_standardize(ph.data = temp.dt2, ref.popname = "none", collapse = F, my.count = "count",
                  my.pop = "pop", per = 1000, conf.level = 0.95, group_by = "sex"))

  expect_equal(sum(temp.dt2[sex == "M"]$count) , temp.agestd2[sex == "M"]$count)
  expect_equal(sum(temp.dt2[sex == "F"]$count) , temp.agestd2[sex == "F"]$count)

  expect_equal(round(1000*sum(temp.dt2[sex == "M"]$count) / sum(temp.dt2[sex == "M"]$pop), 2) , temp.agestd2[sex == "M"]$crude.rate)
  expect_equal(round(1000*sum(temp.dt2[sex == "F"]$count) / sum(temp.dt2[sex == "F"]$pop), 2) , temp.agestd2[sex == "F"]$crude.rate)

  expect_equal(28.26 , temp.agestd2[sex == "M"]$crude.lci) # checked vis-à-vis survival::cipoisson() exact method
  expect_equal(26.47 , temp.agestd2[sex == "F"]$crude.lci) # checked vis-à-vis survival::cipoisson() exact method

  expect_equal(35.18 , temp.agestd2[sex == "M"]$crude.uci) # checked vis-à-vis survival::cipoisson() exact method
  expect_equal(32.83 , temp.agestd2[sex == "F"]$crude.uci) # checked vis-à-vis survival::cipoisson() exact method

  expect_equal(31.73 , temp.agestd2[sex == "M"]$adj.rate) # checked vis-à-vis epitools::ageadjust.direct
  expect_equal(29.64 , temp.agestd2[sex == "F"]$adj.rate) # checked vis-à-vis epitools::ageadjust.direct

  expect_equal(28.39 , temp.agestd2[sex == "M"]$adj.lci) # checked vis-à-vis epitools::ageadjust.direct
  expect_equal(26.58 , temp.agestd2[sex == "F"]$adj.lci) # checked vis-à-vis epitools::ageadjust.direct

  expect_equal(35.35 , temp.agestd2[sex == "M"]$adj.uci) # checked vis-à-vis epitools::ageadjust.direct
  expect_equal(32.97 , temp.agestd2[sex == "F"]$adj.uci) # checked vis-à-vis epitools::ageadjust.direct
  })

test_that('age_standardize ... errors & warnings',{
  set.seed(98104)
  temp.dt3 <- data.table(age = c(0:100), count = sample(1000:4000, size = 101), pop = sample(10000:40000, size = 101) )

  expect_silent(age_standardize(temp.dt3, my.count = "count", my.pop = "pop")) # no error, no warning

  expect_error(age_standardize(copy(temp.dt3)[1, age := NA], my.count = "count", my.pop = "pop"))
  expect_warning(age_standardize(copy(temp.dt3)[1, age := 103], my.count = "count", my.pop = "pop"))
  expect_error(age_standardize(copy(temp.dt3)[1, age := -1], my.count = "count", my.pop = "pop"))
  expect_warning(age_standardize(copy(temp.dt3)[!2, ], my.count = "count", my.pop = "pop"))
  expect_warning(age_standardize(copy(temp.dt3)[!1], my.count = "count", my.pop = "pop"))

  expect_warning(age_standardize(copy(temp.dt3)[1, count := NA], my.count = "count", my.pop = "pop"))
  expect_error(age_standardize(copy(temp.dt3)[1, count := -1], my.count = "count", my.pop = "pop"))

  expect_error(age_standardize(copy(temp.dt3)[1, pop:= NA], my.count = "count", my.pop = "pop"))
  expect_error(age_standardize(copy(temp.dt3)[1, pop := -1], my.count = "count", my.pop = "pop"))

  expect_warning(age_standardize(copy(temp.dt3)[1, count := pop + 1], my.count = "count", my.pop = "pop"))

  expect_error(age_standardize(copy(temp.dt3)[, agecat := 'myagegroup'], my.count = "count", my.pop = "pop"),
               "Both 'age' and 'agecat' columns are present, but only one is needed")

  expect_error(age_standardize(copy(temp.dt3)[, age := NULL], my.count = "count", my.pop = "pop"),
               "Neither 'age' nor 'agecat' columns are present")

  expect_error(age_standardize(copy(temp.dt3)[, age := age + 0.1], my.count = "count", my.pop = "pop"),
               "The 'age' column is not an integer and cannot be converted to integer without loss of data")

  expect_error(age_standardize(copy(temp.dt3)[, age := NULL][, agecat := 10], my.count = "count", my.pop = "pop"),
               "The 'agecat' column is neither character nor factor")

  # detailed warnings when missing ages in specific groups defined by group_by
  set.seed(98104)
  tempy <- data.table(
    gender = sample(c('F', 'M'), 20000, replace = T),
    height = sample(c('Short', 'Tall'), 20000, replace = T),
    weight = sample(c('Heavy', 'Light'), 20000, replace = T),
    age = sample(0:100, 20000, replace = T),
    disease = sample(0:1, 20000, replace = T))
  tempy <- tempy[, .(pop = .N, disease = sum(disease)), .(gender, height, weight, age)]

  tempy <- tempy[!(gender == 'F' & age %in% 20:25) ]
  tempy <- tempy[!(height == 'Short' & age %in% 30:35) ]
  tempy <- tempy[!(weight == 'Heavy' & age %in% 40:45) ]

  expect_warning(age_standardize(ph.data = tempy,
                                 ref.popname = "2000 U.S. Std Population (11 age groups)",
                                 collapse = T,
                                 my.count = 'disease',
                                 my.pop = 'pop',
                                 per = 100000,
                                 conf.level = 0.95,
                                 group_by = c('height', 'weight', 'gender')),
                 'Group \\(height=Tall, weight=Light, gender=F\\) is missing ages: 20, 21, 22, 23, 24, 25')

  # test if fails when have mismatched aggregated data
  set.seed(98104)
  tempy <- data.table(
    gender = sample(c('F', 'M'), 20000, replace = T),
    age = sample(0:100, 20000, replace = T),
    disease = sample(0:1, 20000, replace = T))
  tempy <- tempy[, .(pop = .N, disease = sum(disease)), .(gender, age)]
  tempy[age == 0, agecat := "0"]
  tempy[age %in% 1:5, agecat := "1-5 years"]
  tempy[age %in% 6:14, agecat := "6-14 years"]
  tempy[age %in% 15:24, agecat := "15-24 years"]
  tempy[age %in% 25:34, agecat := "25-34 years"]
  tempy[age %in% 35:44, agecat := "35-44 years"]
  tempy[age %in% 45:54, agecat := "45-54 years"]
  tempy[age %in% 55:64, agecat := "55-64 years"]
  tempy[age %in% 65:74, agecat := "65-74 years"]
  tempy[age %in% 75:84, agecat := "75-84 years"]
  tempy[age %in% 85:100, agecat := "85+ years"]
  tempy <- tempy[, .(count = sum(disease), pop = sum(pop)), .(agecat, gender)]

  expect_error(age_standardize(ph.data = copy(tempy),
                               ref.popname = "2000 U.S. Std Population (11 age groups)",
                               collapse = F, # because already collapsed
                               my.count = "count",
                               my.pop = "pop",
                               per = 1000,
                               conf.level = 0.95,
                               group_by = "gender"),
               'The agecat values in ph.data must match those in your reference')

})

# calc_age ----
test_that("calc_age gives expected ages", {
  expect_equal(calc_age(from = as.Date('1990-08-02'), to = as.Date('2024-08-01')), 33)
  expect_equal(calc_age(from = as.Date('1990-08-02'), to = as.Date('2024-08-03')), 34)
  expect_equal(calc_age(from = as.Date('2000-02-29'), to = as.Date('2024-02-28')), 23)
  expect_equal(calc_age(from = as.Date('2000-02-29'), to = as.Date('2024-02-29')), 24)
})

# convert_to_date() ----
# Test that common date formats are parsed correctly
test_that("common date formats are parsed correctly", {
  expect_equal(convert_to_date("2024-01-01"), as.Date("2024-01-01"))
  expect_equal(convert_to_date("2024/02/01"), as.Date("2024-02-01"))
  expect_equal(convert_to_date("03-01-2024"), as.Date("2024-03-01"))
  expect_equal(convert_to_date("04/01/2024"), as.Date("2024-04-01"))
  expect_equal(convert_to_date("2024-03-05 12:00:00"), as.Date("2024-03-05"))
  expect_equal(convert_to_date("2024/03/05 12:00:00"), as.Date("2024-03-05"))
  expect_equal(convert_to_date("March 10, 2024"), as.Date("2024-03-10"))
  expect_equal(convert_to_date("10 March 2024"), as.Date("2024-03-10"))
})

# Test that numeric values are converted correctly using different origins
test_that("numeric values are converted correctly using default and custom origins", {
  expect_equal(convert_to_date(0), as.Date("1899-12-30"))
  expect_equal(convert_to_date(1), as.Date("1899-12-31"))
  expect_equal(convert_to_date(43500, origin = "1900-1-1"), as.Date("2019-02-06"))
})

# Test handling of non-date strings
test_that("non-date strings return NA and a warning", {
  expect_warning(out <- convert_to_date(c("dogs", "cats")),
                 "cannot be converted to a date")
  expect_equal(out, c("dogs", "cats"))
})

# Test that origin must be in %Y-%m-%d format
test_that("origin must be in %Y-%m-%d format", {
  expect_error(convert_to_date(43500, origin = "01-01-1900"),
               "Origin date must be in '%Y-%m-%d' format.")
  expect_error(convert_to_date(43500, origin = "1900/1/1"),
               "Origin date must be in '%Y-%m-%d' format.")
})

# Test with real data and mixed valid/invalid dates
test_that("real data with mixed valid and invalid dates handles correctly", {
  mixed_dates <- c("2024-01-01", "invalid date", "2024-12-31", "not a date")
  expected_dates <- as.Date(c("2024-01-01", NA, "2024-12-31", NA))
  result <- convert_to_date(mixed_dates)
  expect_equal(result, expected_dates)
})


# format_time() ----
test_that('format_time',{

  expect_equal('2010', format_time(2010))

  expect_equal('2000, 2014-2016, 3000, 3002-4000', format_time(c(2000, 2014:2016, 3000, 3002:4000)))

  expect_equal('2000, 2014-2016, 3000, 3002-4000', format_time(c(3002:4000, 2000, 2014:2016, 3000)))


})

# get_ref_pop() ----
test_that('get_ref_pop',{

  temp.pop <- get_ref_pop("2000 U.S. Std Population (19 age groups - Census P25-1130)")

  expect_equal(19, nrow(temp.pop))

  expect_equal(5, ncol(temp.pop))

  expect_equal(c("age_end", "age_start", "agecat", "pop", "ref_pop_name"), sort(names(temp.pop)))

})

# list_ref_pop() ----
test_that('list_ref_pop',{

  expect_equal(36, length(list_ref_pop()))

})

# lossless_convert() ----
test_that('lossless_convert', {
  expect_equal(class(lossless_convert(c('1', '2', '3'), 'integer')), 'integer')

  expect_equal(
    expect_message(
      lossless_convert(c('one', '2', '3'), 'integer'),
      'would introduce additional NAs'),
    c('one', '2', '3'))

  expect_equal(
    expect_message(
      lossless_convert(c('1', '2', 'three'), 'integer'),
      'would introduce additional NAs'),
    c('1', '2', 'three'))

  expect_equal(class(lossless_convert(c('2020-01-01', '2021-12-31', '2022-02-22'), 'Date')), 'Date')

  expect_equal(
    expect_message(
      lossless_convert(c('2020-01-01', '2021-12-31', 'z'), 'Date'),
    'would introduce additional NAs'),
  c('2020-01-01', '2021-12-31', 'z'))

  expect_equal(
    expect_message(
      lossless_convert(c('z', '2020-01-01', '2021-12-31'), 'Date'),
    'would introduce additional NAs'),
  c('z', '2020-01-01', '2021-12-31'))

})

# multi_t_test ----
test_that("multi_t_test functions correctly", {
  # Setup
  set.seed(98104)
  means <- c(10, 12, 9, 11, 13)
  ses <- c(0.5, 0.6, 0.4, 0.5, 0.7)
  n <- c(30, 35, 28, 32, 33)
  reference_index <- 2

  # Test 1: Basic functionality
  result <- suppressWarnings(multi_t_test(means, ses, reference_index))
  expect_is(result, "data.table")
  expect_equal(nrow(result), 5)
  expect_equal(ncol(result), 9)
  expect_true(all(c("comparison", "diff_means", "t.statistic", "df", "p.value", "ci_lower", "ci_upper", "significant", "df_method") %in% names(result)))

  # Test 2: Different df_methods
  expect_silent(multi_t_test(means, ses, reference_index, df_method = "conservative"))
  expect_silent(multi_t_test(means, ses, reference_index, df_method = "moderate"))
  expect_silent(multi_t_test(means, ses, reference_index, df_method = "liberal"))
  expect_gt(multi_t_test(means, ses, reference_index, df_method = "conservative")[1]$p.value,
            multi_t_test(means, ses, reference_index, df_method = "moderate")[1]$p.value)
  expect_gt(multi_t_test(means, ses, reference_index, df_method = "moderate")[1]$p.value,
            multi_t_test(means, ses, reference_index, df_method = "liberal")[1]$p.value)

  # Test 3: Different alternative hypotheses
  expect_no_error(suppressWarnings(multi_t_test(means, ses, reference_index, alternative = "less")))
  expect_no_error(suppressWarnings(multi_t_test(means, ses, reference_index, alternative = "greater")))

  # Test 4: Providing sample sizes & confirm output structure
  result_with_n <- suppressWarnings(multi_t_test(means, ses, reference_index, n = n))
  expect_is(result_with_n, "data.table")
  expect_equal(nrow(result_with_n), length(means))
  expect_equal(ncol(result_with_n), 9)

  # Test 5: Different alpha levels
  expect_no_error(suppressWarnings(multi_t_test(means, ses, reference_index, alpha = 0.01)))
  expect_no_error(suppressWarnings(multi_t_test(means, ses, reference_index, alpha = 0.1)))
  expect_lt(suppressWarnings(multi_t_test(means, ses, reference_index, alpha = 0.01))[1]$ci_lower, # expect wider CI
            suppressWarnings(multi_t_test(means, ses, reference_index, alpha = 0.10))[1]$ci_lower) # expect narrower CI

  # Test 6: Error for non-numeric means
  expect_error(multi_t_test(c("a", "b", "c"), ses, reference_index), "must be numeric vectors")

  # Test 7: Error for non-numeric ses
  expect_error(multi_t_test(means, c("a", "b", "c"), reference_index), "must be numeric vectors")

  # Test 8: Error for invalid reference_index
  expect_error(multi_t_test(means, ses, 0), "out of bounds")
  expect_error(multi_t_test(means, ses, 6), "out of bounds")

  # Test 9: Error for invalid n
  expect_error(multi_t_test(means, ses, reference_index, n = c(-1, 2, 3, 4, 5)), "must be a numeric vector of positive values")

  # Test 10: Warning for small sample sizes
  expect_warning(multi_t_test(means, ses, reference_index, n = c(10, 15, 20, 25, 30)), "Some sample sizes are below 30")

  # Test 11: Error for invalid alpha
  expect_error(multi_t_test(means, ses, reference_index, alpha = 1.5), "must be a numeric value between 0 and 1")

  # Test 12: Error for invalid df_method
  expect_error(multi_t_test(means, ses, reference_index, df_method = "invalid"), "Invalid df_method")

  # Test 13: Error for invalid alternative
  expect_error(multi_t_test(means, ses, reference_index, alternative = "invalid"), "Invalid alternative")

  # Test 14: Warning for estimated sample sizes
  expect_warning(multi_t_test(means, ses, reference_index), "Sample sizes are estimated from standard errors")

  # Test 15: Check if reference group has NA values
  result <- suppressWarnings(multi_t_test(means, ses, reference_index))
  expect_true(all(is.na(result[comparison == "Group 2 - Referent", .(t.statistic, df, p.value, ci_lower, ci_upper)])))

  # Test 16: Check if diff_means is calculated correctly
  expected_diff <- means - means[reference_index]
  expect_equal(result$diff_means, expected_diff)

  # Test 17: Check if significance is determined correctly
  expect_equal(result$significant, result$p.value < 0.05)

  # Test 18: Error for less than two elements in means or ses
  expect_error(multi_t_test(10, 0.5, 1), "must have at least two elements")

  # Test 19: Error for non-positive ses
  expect_error(multi_t_test(means, c(0.5, -0.6, 0.4, 0.5, 0.7), reference_index), "All values in 'ses' must be positive")

  # Test 20: Error for null alpha
  expect_error(multi_t_test(means, ses, reference_index, alpha = NULL), "'alpha' must be provided")
})

test_that("multi_t_test 'two.sided' compared to stats::t.test", {
  # check all output when stats::t.test paired == F and var.equal = F
  # create data
  set.seed(98104)
  sample1 <- sample(1000:2000, size = 500, replace = T)
  sample2 <- sample(1200:2200, size = 600, replace = T)

  # Generate rads::multi_t_test estimate
  apde <- multi_t_test(means = c(mean(sample1), mean(sample2)),
                       ses = c(rads::std_error(sample1), rads::std_error(sample2)),
                       reference_index = 2,
                       n = c(length(sample1), length(sample2)),
                       alternative = 'two.sided',
                       df_method = "estimated"
  )[comparison == 'Group 1 vs Reference']

  # Generate stats::t.test estimate
  standard <- stats::t.test(sample1, sample2,
                            var.equal = F, paired = F, # when var.equal = F, use Welch's
                            alternative = 'two.sided')

  standardDT <- data.table(comparison = 'stats::t.test()', # structure results into a data.table
                           diff_means = standard$estimate[1]-standard$estimate[2],
                           ci_lower = standard$conf.int[1],
                           ci_upper = standard$conf.int[2],
                           p.value = standard$p.value,
                           significant = NA_character_,
                           t.statistic = standard$statistic,
                           df = standard$parameter,
                           df_method = 'stats::t_test')

  # Combine rads:: and stats:: output
  combo <- rbind(apde, standardDT)

  # actual tests
  expect_equal(combo[1]$diff_means, combo[2]$diff_means)
  expect_equal(combo[1]$ci_lower, combo[2]$ci_lower)
  expect_equal(combo[1]$ci_upper, combo[2]$ci_upper)
  expect_equal(combo[1]$p.value, combo[2]$p.value)
  expect_equal(combo[1]$t.statistic, combo[2]$t.statistic)
  expect_equal(combo[1]$df, combo[2]$df)

})

test_that("multi_t_test 'greater' compared to stats::t.test", {
  # check all output when stats::t.test paired == F and var.equal = F
  # create data
  set.seed(98104)
  sample1 <- sample(1000:2000, size = 500, replace = T)
  sample2 <- sample(1200:2200, size = 600, replace = T)

  # Generate rads::multi_t_test estimate
  apde <- multi_t_test(means = c(mean(sample1), mean(sample2)),
                       ses = c(rads::std_error(sample1), rads::std_error(sample2)),
                       reference_index = 2,
                       n = c(length(sample1), length(sample2)),
                       alternative = 'greater',
                       df_method = "estimated"
  )[comparison == 'Group 1 vs Reference']

  # Generate stats::t.test estimate
  # Generate stats::t.test estimate
  standard <- stats::t.test(sample1, sample2,
                            var.equal = F, paired = F, # when var.equal = F, use Welch's
                            alternative = 'greater')

  standardDT <- data.table(comparison = 'stats::t.test()', # structure results into a data.table
                           diff_means = standard$estimate[1]-standard$estimate[2],
                           ci_lower = standard$conf.int[1],
                           ci_upper = standard$conf.int[2],
                           p.value = standard$p.value,
                           significant = NA_character_,
                           t.statistic = standard$statistic,
                           df = standard$parameter,
                           df_method = 'stats::t_test')

  # Combine rads:: and stats:: output
  combo <- rbind(apde, standardDT)

  # actual tests
  expect_equal(combo[1]$diff_means, combo[2]$diff_means)
  expect_lt(abs(combo[1]$ci_lower - combo[2]$ci_lower) / combo[1]$ci_lower, 0.005) # allow for up to 0.5% difference in CI
  expect_equal(combo[1]$ci_upper, combo[2]$ci_upper)
  expect_equal(combo[1]$p.value, combo[2]$p.value)
  expect_equal(combo[1]$t.statistic, combo[2]$t.statistic)
  expect_equal(combo[1]$df, combo[2]$df)
})

test_that("multi_t_test 'greater' compared to stats::t.test", {
  # check all output when stats::t.test paired == F and var.equal = F
  # create data
  set.seed(98104)
  sample1 <- sample(1000:2000, size = 500, replace = T)
  sample2 <- sample(1200:2200, size = 600, replace = T)

  # Generate rads::multi_t_test estimate
  apde <- multi_t_test(means = c(mean(sample1), mean(sample2)),
                       ses = c(rads::std_error(sample1), rads::std_error(sample2)),
                       reference_index = 2,
                       n = c(length(sample1), length(sample2)),
                       alternative = 'less',
                       df_method = "estimated"
  )[comparison == 'Group 1 vs Reference']

  # Generate stats::t.test estimate
  # Generate stats::t.test estimate
  standard <- stats::t.test(sample1, sample2,
                            var.equal = F, paired = F, # when var.equal = F, use Welch's
                            alternative = 'less')

  standardDT <- data.table(comparison = 'stats::t.test()', # structure results into a data.table
                           diff_means = standard$estimate[1]-standard$estimate[2],
                           ci_lower = standard$conf.int[1],
                           ci_upper = standard$conf.int[2],
                           p.value = standard$p.value,
                           significant = NA_character_,
                           t.statistic = standard$statistic,
                           df = standard$parameter,
                           df_method = 'stats::t_test')

  # Combine rads:: and stats:: output
  combo <- rbind(apde, standardDT)

  # actual tests
  expect_equal(combo[1]$diff_means, combo[2]$diff_means)
  expect_equal(combo[1]$ci_lower, combo[2]$ci_lower)
  expect_lt(abs(combo[1]$ci_upper - combo[2]$ci_upper) / combo[1]$ci_upper, 0.005) # allow for up to 0.5% difference in CI
  expect_equal(combo[1]$p.value, combo[2]$p.value)
  expect_equal(combo[1]$t.statistic, combo[2]$t.statistic)
  expect_equal(combo[1]$df, combo[2]$df)
})

test_that("multi_t_test handles adjustment methods correctly", {

  # Sample data ----
  means <- c(10, 12, 15, 11)
  ses <- c(1, 1.5, 2, 1.2)
  n <- c(100, 90, 80, 95)

  # Test 1: Invalid adjustment method ----
  expect_error(
    multi_t_test(means, ses, reference_index = 1, n = n, adjust_method = "invalid"),
    "Invalid adjust_method. Choose NULL, 'Holm-Bonferroni', or 'Benjamini-Hochberg'."
  )

  # Test 2: Holm-Bonferroni adjustment ----
  result_holm <- multi_t_test(means, ses, reference_index = 1, n = n, adjust_method = "Holm-Bonferroni")
  unadjusted <- multi_t_test(means, ses, reference_index = 1, n = n)

  # Check if adjusted p-values are >= original p-values
  non_ref_rows <- !is.na(result_holm$p.value)
  expect_true(all(result_holm$p.value[non_ref_rows] >= unadjusted$p.value[non_ref_rows]))

  # Check if order of significance (p.value magnitude) is maintained
  expect_equal(order(result_holm$p.value[non_ref_rows]), order(unadjusted$p.value[non_ref_rows]))

  expect_equal(result_holm$adjust_method[1], "Holm-Bonferroni")

  # Test 3: Benjamini-Hochberg adjustment ----
  result_bh <- multi_t_test(means, ses, reference_index = 1, n = n, adjust_method = "Benjamini-Hochberg")
  unadjusted <- multi_t_test(means, ses, reference_index = 1, n = n)

  # Check if adjusted p-values are >= original p-values
  non_ref_rows <- !is.na(result_bh$p.value)
  expect_true(all(result_bh$p.value[non_ref_rows] >= unadjusted$p.value[non_ref_rows]))

  # Check if order of significance (p.value magnitude) is maintained
  expect_equal(order(result_bh$p.value[non_ref_rows]), order(unadjusted$p.value[non_ref_rows]))

  expect_equal(result_bh$adjust_method[1], "Benjamini-Hochberg")
})

# pool_brfss_weights ----
test_that('pool_brfss_weights data.table', {

  # generate a standard BRFSS table that contains multiple years (and therefore used pool_brfss_weights)
    brfss_table <- get_data_brfss(cols = c('chi_sex'), year = 2019:2023)

  # check that the total new weight is between the total for the earliest and the latest years (because population is growing)
    expect_true(sum(brfss_table$default_wt) > sum(brfss_table[chi_year == 2019]$finalwt1) &
                  sum(brfss_table$default_wt) < sum(brfss_table[chi_year == 2023]$finalwt1))

  # survey set (including creating new weights) for just a subset of the years
    brfss_table2 <- pool_brfss_weights(ph.data = brfss_table, years = 2020:2021, new_wt_var = 'brfss2_wt')

  # check that the total new weight total is between the total weights for 2020 & 20201
    expect_true(sum(brfss_table2$brfss2_wt) > sum(brfss_table2[chi_year == 2020]$finalwt1) &
                  sum(brfss_table2$brfss2_wt) < sum(brfss_table2[chi_year == 2021]$finalwt1))

  # check that new weight is zero outside of the specified years
    expect_equal(sum(brfss_table2[!chi_year %in% c(2020:2021)]$brfss2_wt), 0)

  # check that object is still a dtsurvey / data.table object
    expect_true(inherits(brfss_table2, 'dtsurvey'))
    expect_true(inherits(brfss_table2, 'data.table'))
})

test_that('pool_brfss_weights mitools:imputation_list', {

  # generate a standard BRFSS table that contains multiple years (and therefore used pool_brfss_weights)
  brfss_miList <- get_data_brfss(cols = c('chi_geo_region'), year = 2019:2023)

  # check that the total new weight is between the total for the earliest and the latest years (because population is growing)
  expect_true(sum(brfss_miList$imputations[[1]]$default_wt) > sum(brfss_miList$imputations[[1]][chi_year == 2019]$finalwt1) &
                sum(brfss_miList$imputations[[1]]$default_wt) < sum(brfss_miList$imputations[[1]][chi_year == 2023]$finalwt1))

  # survey set (including creating new weights) for just a subset of the years
  brfss_miList2 <- pool_brfss_weights(ph.data = brfss_miList, years = 2020:2021, new_wt_var = 'brfss2_wt')

  # check that the total new weight total is between the total weights for 2020 & 20201
  expect_true(sum(brfss_miList2$imputations[[1]]$brfss2_wt) > sum(brfss_miList2$imputations[[1]][chi_year == 2020]$finalwt1) &
                sum(brfss_miList2$imputations[[1]]$brfss2_wt) < sum(brfss_miList2$imputations[[1]][chi_year == 2021]$finalwt1))

  # check that new weight is zero outside of the specified years
  expect_equal(sum(brfss_miList2$imputations[[1]][!chi_year %in% c(2020:2021)]$brfss2_wt), 0)

  # check that object is still a dtsurvey / data.table object
  expect_true(inherits(brfss_miList2$imputations[[1]], 'dtsurvey'))
  expect_true(inherits(brfss_miList2$imputations[[1]], 'data.table'))
  expect_true(inherits(brfss_miList2, 'imputationList'))
})

test_that("pool_brfss_weights correctly rescales weights using different methods", {
  # Sample data
  ph.data <- data.table(
    chi_year = c(2019, 2019, 2020, 2020, 2021, 2021),
    finalwt1 = c(100, 100, 150, 150, 250, 250),
    x_ststr = c(1, 1, 2, 2, 3, 3)
  )

  # Test "simple" method
  res_simple <- pool_brfss_weights(ph.data, years = 2019:2021, new_wt_var = "new_wt_simple", wt_method = "simple")
  expect_s3_class(res_simple, "dtsurvey")

  # Test "obs" method
  res_obs <- pool_brfss_weights(ph.data, years = 2019:2021, new_wt_var = "new_wt_obs", wt_method = "obs")
  expect_s3_class(res_obs, "dtsurvey")

  # Test "simple" and "obs" produced the same new weights
  # "simple" should give 1/3 for each because 3 years
  # "obs" should give 1/3 for each because equal number of observations for each of three years
  expect_equal(res_simple$new_wt_simple, res_obs$new_wt_obs)
  expect_equal(res_simple$new_wt_simple, res_simple$finalwt1 / 3)

  # Test "pop" method
  res_pop <- pool_brfss_weights(ph.data, years = 2019:2021, new_wt_var = "new_wt_pop", wt_method = "pop")
  expect_s3_class(res_pop, "dtsurvey")

  # Test that "pop" apportioned according to sumy of year population
  expect_equal(res_pop[chi_year == 2019][1]$new_wt_pop, 100*0.20) # 20% because 2019 pop = 200 out of 1000 total
  expect_equal(res_pop[chi_year == 2020][1]$new_wt_pop, 150*0.30) # 30% because 2020 pop = 300 out of 1000 total
  expect_equal(res_pop[chi_year == 2021][1]$new_wt_pop, 250*0.50) # 50% because 2021 pop = 500 out of 1000 total

})

test_that("pool_brfss_weights validates ph.data correctly", {
  sample_data <- data.table(
    chi_year = c(rep(2020, 10), rep(2021, 10), rep(2022, 10)),
    finalwt1 = c(rep(NA, 10), 1:10, 11:20),
    x_ststr = c(rep(NA, 10), letters[1:10], letters[11:20])
  )

  # Test for missing ph.data
  expect_error(pool_brfss_weights(), "\n\U1F6D1 You must specify a dataset")

  # Test for invalid ph.data types
  expect_error(pool_brfss_weights("not_a_dataframe"), "\n\U1F6D1 'ph.data' must be a data.frame, data.table, or mitools imputationList")

  # Test for missing year_var column
  expect_error(pool_brfss_weights(sample_data[, -c("chi_year")]), "\n\U1F6D1 Column 'chi_year' not found in dataset")

  # Test for missing specified years in ph.data
  expect_error(pool_brfss_weights(sample_data, years = c(2019)), "\n\U1F6D1 The following years are not present in the dataset: 2019")

  # Test for missing old_wt_var column
  expect_error(pool_brfss_weights(sample_data[, -c("finalwt1")], years = 2020), "\n\U1F6D1 Weight variable 'finalwt1' not found in dataset")

  # Test for non-numeric old_wt_var
  expect_error(pool_brfss_weights(copy(sample_data)[, finalwt1 := as.character(finalwt1)], years = 2020), "\n\U1F6D1 Weight variable 'finalwt1' must be numeric")

  # Test for NA values in old_wt_var for specified years
  expect_error(pool_brfss_weights(sample_data, years = 2020), "\n\U1F6D1 Missing values found in weight variable 'finalwt1' for specified years")

  # Test for non-positive values in old_wt_var for specified years
  sample_data[, finalwt1 := ifelse(finalwt1 == 1, -1, finalwt1)]
  expect_error(pool_brfss_weights(sample_data, years = 2021), "\n\U1F6D1 Weight variable 'finalwt1' must contain only positive values")

  # Reset finalwt1 to positive values for further tests
  sample_data[, finalwt1 := c(rep(NA, 10), 1:10, 11:20)]

  # Test for existing new_wt_var column
  expect_error(pool_brfss_weights(sample_data, new_wt_var = "finalwt1", years = 2021), "\n\U1F6D1 Column name 'finalwt1' already exists in dataset")

  # Test for invalid wt_method
  expect_error(pool_brfss_weights(sample_data, new_wt_var = "new_weight", wt_method = "invalid_method", years = 2021), "\n\U1F6D1 'wt_method' must be one of: 'obs', 'pop', or 'simple'")

  # Test for missing strata column
  expect_error(pool_brfss_weights(sample_data[, -c("x_ststr")], new_wt_var = 'new_weight', years = 2021), "'x_ststr' not found in dataset")

  # Test for NA values in strata column for specified years
  expect_error(pool_brfss_weights(sample_data, years = 2020, new_wt_var = 'new_weight'), "\n\U1F6D1 Missing values found in weight variable 'finalwt1' for specified years")

  # Test for missing years
  ss = get_data_brfss(cols = 'chi_year', year = c(2019, 2023), wt_method = 'simple')
  expect_error(pool_brfss_weights(ph.data = ss, years = c(2019:2023), new_wt_var = 'new_weight'), "\n\U1F6D1 The following years are not present in the dataset: 2020-2022")

})

# std_error() ----
test_that('std_error',{
  expect_equal(std_error(c(seq(0, 400, 100), NA)), sd(c(seq(0, 400, 100), NA), na.rm = T) / sqrt(5))
})

test_that('std_error',{
  expect_equal(std_error(c(seq(0, 400, 100), NA)), sd(c(seq(0, 400, 100), NA), na.rm = T) / sqrt(5))
})

# tsql_chunk_loader() ----
# set up tsql_chunk_loader test data
mydt = data.table(col1 = 1:10000L,  # create integer
                  col2 = 1:10000/3) # create float
mydt[, col3 := as.Date(Sys.Date()) - col1] # create date
mydt[, col4 := as.character(col3)] # create string
myconn = validate_hhsaw_key()

test_that("tsql_chunk_loader works correctly and congratulates on success", {
  expect_message(
    tsql_chunk_loader(
      ph.data = mydt,
      db_conn = myconn,
      chunk_size = 3333,
      schema_name = Sys.getenv("USERNAME"),
      table_name = 'justTesting',
      overwrite = TRUE,
      append = FALSE,
      field_types = c(col1 = 'int', col2 = 'float', col3 = 'date', col4 = 'nvarchar(255)'),
      validate_field_types = TRUE,
      validate_upload = TRUE
    ),
    regexp = "Congratulations"
  )
  expect_message(
    tsql_chunk_loader(
      ph.data = mydt,
      db_conn = myconn,
      chunk_size = 3333,
      schema_name = Sys.getenv("USERNAME"),
      table_name = 'justTesting',
      overwrite = FALSE,
      append = TRUE,
      field_types = c(col1 = 'int', col2 = 'float', col3 = 'date', col4 = 'nvarchar(255)'),
      validate_field_types = TRUE,
      validate_upload = TRUE
    ),
    regexp = "Congratulations"
  )
})

test_that("error if ph.data is NULL", {
  expect_error(tsql_chunk_loader(ph.data = NULL), "must specify a dataset")
})

test_that("error if ph.data is not a data.frame or data.table", {
  expect_error(tsql_chunk_loader(ph.data = list()), "must be the name of a data.frame or data.table.")
})

test_that("error if db_conn is NULL", {
  expect_error(tsql_chunk_loader(ph.data = mydt, db_conn = NULL), "must be specified")
})

test_that("error if db_conn is not a MS SQL Server object", {
  expect_error(tsql_chunk_loader(ph.data = mydt, db_conn = mydt), 'not a "Microsoft SQL Server" object')
})

test_that("error if chunk_size is not an integer between 100 and 20,000", {
  expect_error(tsql_chunk_loader(ph.data = mydt, db_conn = myconn, chunk_size = 50), "must be an integer between 100 and 20,000")
})

test_that("error if schema_name is not a single quoted name", {
  expect_error(tsql_chunk_loader(ph.data = mydt, db_conn = myconn, schema_name = c("invalid", "name")), "must be a quoted name of a single schema")
})

test_that("error if table_name is not a single quoted name", {
  expect_error(tsql_chunk_loader(ph.data = mydt, db_conn = myconn, schema = Sys.getenv("USERNAME"),
                                table_name = c("invalid", "name")), "must be a quoted name of a single table")
})

test_that("error if overwrite and append are both TRUE or FALSE", {
  expect_error(tsql_chunk_loader(ph.data = mydt, db_conn = myconn, schema = Sys.getenv("USERNAME"),
                                table_name = c("JustTesting"), overwrite = TRUE, append = TRUE), "cannot both be set to the same value")
})

test_that("error if field_types do not match column names in ph.data", {
  expect_error(tsql_chunk_loader(ph.data = mydt, db_conn = myconn, schema = Sys.getenv("USERNAME"),
                                table_name = c("JustTesting"), overwrite = TRUE, append = FALSE,
                                field_types = c(wrong = 'int')), "must match the column names")
})

test_that("error if field_types are not compatible with classes in ph.data", {
  expect_error(tsql_chunk_loader(ph.data = mydt, db_conn = myconn, schema = Sys.getenv("USERNAME"),
                                 table_name = c("JustTesting"), overwrite = TRUE, append = FALSE,
                                 field_types =  c(col1 = 'date', col2 = 'float', col3 = 'date', col4 = 'nvarchar(255)'),
                                 validate_field_types = TRUE), "did not align with the proposed TSQL field types")
})

test_that("error if validate_upload is not logical", {
  expect_error(tsql_chunk_loader(ph.data = mydt, db_conn = myconn, chunk_size = 180, schema = Sys.getenv("USERNAME"),
                                table_name = c("JustTesting"), overwrite = TRUE, append = FALSE,
                                c(col1 = 'int', col2 = 'float', col3 = 'date', col4 = 'nvarchar(255)'),
                                validate_upload = NULL), "must be specified as a logical")
})

# tsql_validate_field_types ----
test_that("function succeeds with compatible data.table and field types", {
  mydt <- data.table(col1 = 1:10, col2 = runif(10), col3 = sample(c(0L, 1L, NA), 10, replace = T))
  my_field_types <- c(col1 = 'int', col2 = 'float', col3 = 'bit')
  expect_message(tsql_validate_field_types(ph.data = mydt, field_types = my_field_types), "Success")
})

test_that("function is case insensitive", {
  mydt <- data.table(COL1 = 1:10, Col2 = runif(10))
  my_field_types <- c(CoL1 = 'INT', col2 = 'FLOAT')
  expect_message(tsql_validate_field_types(ph.data = mydt, field_types = my_field_types), "Success")
})

test_that("function fails with incompatible field types", {
  mydt <- data.table(col1 = 32758:32767,
                     col2 = as.Date('2023-01-01'),
                     col3 = (99991:100000)/10,
                     col4 = rep('AReallyLongStringOfWordForTestingLengthChecks', 10),
                     col5 = Sys.time(),
                     col6 = 2L)
  my_field_types <- c(col1 = 'smallint',
                      col2 = 'date',
                      col3 = 'numeric',
                      col4 = 'nvarchar(100)',
                      col5 = 'datetime',
                      col6 = 'int')
  expect_message(tsql_validate_field_types(ph.data = mydt, field_types = my_field_types), 'Success')

  bad_field_types <- data.table::copy(my_field_types)
  bad_field_types["col1"] <- "fff" # invalid tsql type
  expect_error(tsql_validate_field_types(ph.data = mydt, field_types = bad_field_types), 'The following TSQL field types are not recognized')

  bad_field_types <- data.table::copy(my_field_types)
  bad_field_types["col1"] <- "tinyint" # tinyint is too small (up to 255)
  expect_error(tsql_validate_field_types(ph.data = mydt, field_types = bad_field_types), 'column: col1')

  bad_field_types <- data.table::copy(my_field_types)
  bad_field_types["col2"] <- "nvarchar(20)" # does not expect moving dates to non dates in TSQL
  expect_error(tsql_validate_field_types(ph.data = mydt, field_types = bad_field_types), 'column: col2')

  bad_field_types <- data.table::copy(my_field_types)
  bad_field_types["col3"] <- "INT" # numeric cannot be changed to integer without loss
  expect_error(tsql_validate_field_types(ph.data = mydt, field_types = bad_field_types), 'column: col3')

  bad_field_types <- data.table::copy(my_field_types)
  bad_field_types["col4"] <- "nvarchar(25)" # not long enough for number of characters
  expect_error(tsql_validate_field_types(ph.data = mydt, field_types = bad_field_types), 'column: col4')

  bad_field_types <- data.table::copy(my_field_types)
  bad_field_types["col5"] <- "date" # R POSIXct should only map to some sort of datetime, not date
  expect_error(tsql_validate_field_types(ph.data = mydt, field_types = bad_field_types), 'column: col5')

  bad_field_types <- data.table::copy(my_field_types)
  bad_field_types["col6"] <- "bit" # bit is limited to logical or integers with only 0|1 values
  expect_error(tsql_validate_field_types(ph.data = mydt, field_types = bad_field_types), 'column: col6')

})

test_that("function handles NULL arguments appropriately", {
  mydt <- data.table(col1 = 1:10, col2 = runif(10))
  my_field_types <- c(col1 = 'int', col2 = 'float')
  expect_error(tsql_validate_field_types(ph.data = mydt, field_types = NULL), 'must specify a named character vector')
  expect_error(tsql_validate_field_types(ph.data = NULL, field_types = my_field_types), 'must specify a dataset')
})

test_that("function converts data.frame to data.table and succeeds", {
  mydf <- data.frame(col1 = 1:10, col2 = runif(10))
  my_field_types <- c(col1 = 'int', col2 = 'float')
  expect_message(tsql_validate_field_types(ph.data = mydf, field_types = my_field_types), "Success")
})

test_that("function fails when field types and data.table column names do not match", {
  mydt <- data.table(col1 = 1:10, col3 = runif(10))
  my_field_types <- c(col1 = 'int', col2 = 'float')
  expect_error(tsql_validate_field_types(ph.data = mydt, field_types = my_field_types), 'necessitates exactly one TSQL datatype per column name')
})

test_that("function handles unsupported R data types gracefully", {
  mydt <- data.table(col1 = 1:10, col2 = list(1:10))
  my_field_types <- c(col1 = 'int', col2 = 'int')
  expect_error(tsql_validate_field_types(ph.data = mydt, field_types = my_field_types),
               "are not recognized")
  mydt <- data.table(col1 = 1:10, col2 = as.character(1:10))
  my_field_types <- c(col1 = 'int', col2 = 'unsupported')
  expect_error(tsql_validate_field_types(ph.data = mydt, field_types = my_field_types),
               "The following TSQL field types are not recognized")
})

test_that("function correctly handles character fields at size limit", {
  mydt <- data.table(col1 = c("abc", "defgh", "ijklm"))
  expect_message(tsql_validate_field_types(ph.data = mydt, field_types = c(col1 = 'varchar(5)')), "Success")
  expect_error(tsql_validate_field_types(ph.data = mydt, field_types = c(col1 = 'varchar(4)')), "Fails constraints")
})

test_that("function correctly handles numeric to integer conversion", {
  mydt <- data.table(col1 = c(1, 2, 3.0), col2 = c(4.0, 5.0, 6.0))
  my_field_types <- c(col1 = 'int', col2 = 'int')
  expect_message(tsql_validate_field_types(ph.data = mydt, field_types = my_field_types), "Success")

  mydt <- data.table(col1 = c(1.1, 2.0, 3.0), col2 = c(4.0, 5.0, 6.0))
  expect_error(tsql_validate_field_types(ph.data = mydt, field_types = my_field_types), "Numeric values cannot be safely converted to integer")
})

test_that("function correctly handles POSIXct types", {
  mydt <- data.table(col1 = as.POSIXct("2023-01-01 12:00:00"))
  expect_message(tsql_validate_field_types(ph.data = mydt, field_types = c(col1 = 'datetime')), "Success")
  expect_message(tsql_validate_field_types(ph.data = mydt, field_types = c(col1 = 'datetime2')), "Success")
  expect_message(tsql_validate_field_types(ph.data = mydt, field_types = c(col1 = 'datetimeoffset')), "Success")
})

test_that("function handles NA and NULL values correctly", {
  mydt <- data.table(col1 = c(1, 2, NA), col2 = c("a", "b", NA), col3 = c(TRUE, FALSE, NA))
  my_field_types <- c(col1 = 'int', col2 = 'varchar(10)', col3 = 'bit')
  expect_message(tsql_validate_field_types(ph.data = mydt, field_types = my_field_types), "Success")

  mydt <- data.table(col1 = c(1, 2, NA), col2 = c("a", "b", NA), col3 = c(NA, NA, NA))
  my_field_types <- c(col1 = 'int', col2 = 'varchar(10)', col3 = 'bit')
  expect_warning(tsql_validate_field_types(ph.data = mydt, field_types = my_field_types))
})
