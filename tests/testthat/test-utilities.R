library(testthat)
library(data.table)

# bin_age() ----
test_that('bin_age with ref.popname matches get_ref_pop bins', {
  ref.pop <- get_ref_pop("2000 U.S. Std Population (11 age groups)")

  expect_equal(
    bin_age(age = ref.pop$age_start, ref.popname = "2000 U.S. Std Population (11 age groups)"),
    ref.pop$agecat
  )

  expect_equal(
    bin_age(age = c(0, 3, 10, 45, 90), ref.popname = "2000 U.S. Std Population (11 age groups)"),
    c("0", "1-4 years", "5-14 years", "45-54 years", "85+ years")
  )
})

test_that('bin_age with custom cuts labels bins like RADS reference populations', {
  expect_equal(
    bin_age(age = c(0, 1, 4, 5, 9, 10, 19, 20), cuts = c(0, 1, 5, 10, 20)),
    c("0", "1-4", "1-4", "5-9", "5-9", "10-19", "10-19", "20+")
  )

  # unsorted cuts are handled the same as sorted cuts
  expect_equal(
    bin_age(age = c(0, 1, 4, 5, 9, 10, 19, 20), cuts = c(20, 0, 10, 1, 5)),
    bin_age(age = c(0, 1, 4, 5, 9, 10, 19, 20), cuts = c(0, 1, 5, 10, 20))
  )
})

test_that('bin_age defaults to the same reference population as age_standardize() when neither ref.popname nor cuts is given', {
  expect_equal(
    bin_age(age = c(0, 3, 10, 45, 90)),
    bin_age(age = c(0, 3, 10, 45, 90), ref.popname = "2000 U.S. Std Population (11 age groups)")
  )

  expect_error(bin_age(age = 0:10, ref.popname = "2000 U.S. Std Population (11 age groups)", cuts = c(0, 18)),
               "Only one of")
})

test_that('bin_age validates age', {
  expect_error(bin_age(age = c(0, 5.5), cuts = c(0, 18)), "whole numbers")
  expect_error(bin_age(age = c(-1, 5), cuts = c(0, 18)), "negative values")
  expect_error(bin_age(age = "5", cuts = c(0, 18)), "numeric vector")
})

test_that('bin_age warns and returns NA for NA ages, rather than erroring', {
  out <- expect_warning(bin_age(age = c(0, NA, 5), cuts = c(0, 18)), "could not be assigned")
  expect_equal(out, c("0-17", NA, "0-17"))
})

test_that('bin_age validates ref.popname', {
  expect_error(bin_age(age = 0:10, ref.popname = "not a real reference population"),
               "not a valid reference population")
})

test_that('bin_age validates cuts', {
  expect_error(bin_age(age = 0:10, cuts = c(0)), "length of at least 2")
  expect_error(bin_age(age = 0:10, cuts = c(0, 5.5)), "non-negative whole numbers")
  expect_error(bin_age(age = 0:10, cuts = c(-5, 5)), "non-negative whole numbers")
  expect_error(bin_age(age = 0:10, cuts = c(0, 5, 5)), "duplicate values")
})

test_that('bin_age warns and returns NA for ages below the youngest bin', {
  out <- expect_warning(bin_age(age = c(0, 5, 20), cuts = c(5, 18)), "could not be assigned")
  expect_equal(out, c(NA, "5-17", "18+"))
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
  expect_equal(convert_to_date("05/15/89"), as.Date("1989-05-15"))
  expect_equal(convert_to_date("05-15-89"), as.Date("1989-05-15"))
  expect_equal(convert_to_date("2024-03-05 12:00:00"), as.Date("2024-03-05"))
  expect_equal(convert_to_date("2024/03/05 12:00:00"), as.Date("2024-03-05"))
  expect_equal(convert_to_date("March 10, 2024"), as.Date("2024-03-10"))
  expect_equal(convert_to_date("10 March 2024"), as.Date("2024-03-10"))
  expect_equal(convert_to_date("10Sep1998"), as.Date("1998-09-10"))
  expect_equal(convert_to_date("10-Sep-1998"), as.Date("1998-09-10"))
  expect_equal(convert_to_date("10 September 1998"), as.Date("1998-09-10"))
  expect_equal(convert_to_date("10 September, 1998"), as.Date("1998-09-10"))
  expect_equal(convert_to_date("29 Feb 2020"), as.Date("2020-02-29")) # leap year
  expect_identical(convert_to_date(as.Date("2020-09-18")), as.Date("2020-09-18"))
})

# Test that numeric values are converted correctly using different origins
test_that("numeric values are converted correctly using default and custom origins", {
  expect_equal(convert_to_date(0), as.Date("1899-12-30"))
  expect_equal(convert_to_date(1), as.Date("1899-12-31"))
  expect_equal(convert_to_date(1, origin = "1970-01-01"), as.Date("1970-01-02"))
})

# Test handling of non-date strings
test_that("non-date strings return NA and a warning", {
  expect_warning(out <- convert_to_date(c("dogs", "cats")),
                 "cannot be converted to a date")
  expect_true(all(is.na(out)))
  expect_s3_class(out, "Date")
  expect_true(is.na(suppressWarnings(convert_to_date(NA_character_))))
  expect_s3_class(expect_warning(convert_to_date(NA_character_)), "Date")
})

# Test that origin must be in %Y-%m-%d format
test_that("origin must be in %Y-%m-%d format", {
  expect_error(convert_to_date(43500, origin = "01-01-1900"),
               "Origin date must be in 'YYYY-MM-DD' format.")
  expect_error(convert_to_date(43500, origin = "1900/1/1"),
               "Origin date must be in 'YYYY-MM-DD' format.")
})

# Test with real data and mixed valid/invalid dates
test_that("real data with mixed valid and invalid dates handles correctly", {
  mixed_dates <- c("2024-01-01", "invalid date", "2024-12-31", "not a date")
  expected_dates <- as.Date(c("2024-01-01", NA, "2024-12-31", NA))
  result <- convert_to_date(mixed_dates)
  expect_equal(result, expected_dates)
})

# Test when have serial dates mixed with character dates in data.table
test_that("mixed column in data.table gives proper results", {
  myvector <- c(
    "15Jan2024",           # %d%b%Y
    "15-Jan-2024",         # %d-%b-%Y
    "15 January, 2024",    # %d %B, %Y
    "15 January 2024",     # %d %B %Y
    "2024-01-15",          # %Y-%m-%d
    "2024/01/15",          # %Y/%m/%d
    "01/15/2024",          # %m/%d/%Y
    "01-15-2024",          # %m-%d-%Y
    "January 15, 2024",    # %B %d, %Y
    "2024-01-15 14:30:25", # %Y-%m-%d %H:%M:%S
    "2024/01/15 14:30:25", # %Y/%m/%d %H:%M:%S
    "01/15/24",            # %m/%d/%y
    "01-15-24",            # %m-%d-%y
    45306                  # Excel serial date that will become a character in data.table
  )
  mydt <- data.table(orig = myvector)
  mydt[, result := convert_to_date(orig)]
  expect_equal(unique(mydt$result), as.Date('2024-01-15'))
})

test_that("YYYYMMDD works when mixed with other formats", {
  vec <- c("20240728", "2024-07-29", "July 30, 2024", "42000")
  res <- convert_to_date(vec)
  expect_equal(res[1], as.Date("2024-07-28"))
  expect_equal(res[2], as.Date("2024-07-29"))
  expect_equal(res[3], as.Date("2024-07-30"))
  expect_equal(res[4], as.Date("2014-12-27"))  # Excel serial 42000
})

# Test 'YYYYMMDD'
test_that("YYYYMMDD format is parsed correctly", {
  expect_equal(convert_to_date("20240728"), as.Date("2024-07-28"))
})

# Test that random spaces are addressed properly
test_that("leading/trailing whitespace does not break conversion", {
  expect_equal(convert_to_date(" 20240102 "), as.Date("2024-01-02"))
})

# Test that gigantic numbers are addressed properly
test_that("large numeric strings are NOT treated as dates", {
  out <- expect_warning(convert_to_date("99999999"))
  expect_true(is.na(out))
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
test_that('lossless_convert misc tests', {
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

test_that("lossless_convert handles Date conversions correctly", {
  # Setup test vectors
  alpha <- c('2022-01-01', '2023-01-01', '2024-01-01', '2025-01-01')
  beta <- c(NA, '2023-01-01', '2024-01-01', '2025-01-01')
  gamma <- c(NA, 'Not a Date', '2024-01-01', '2025-01-01')
  delta <- c('Not a Date', '2023-01-01', '2024-01-01', '2025-01-01')

  # Test successful Date conversion
  expect_true(inherits(lossless_convert(alpha, 'Date'), 'Date'))
  expect_true(inherits(lossless_convert(beta, 'Date'), 'Date'))

  # Test failed Date conversion (preserves original)
  expect_message(result_gamma <- lossless_convert(gamma, 'Date', column_name = "gamma"),
                 "Conversion of 'gamma' to Date would introduce additional NAs")
  expect_true(inherits(result_gamma, 'character'))

  expect_message(result_delta <- lossless_convert(delta, 'Date', column_name = "delta"),
                 "Conversion of 'delta' to Date would introduce additional NAs")
  expect_true(inherits(result_delta, 'character'))
})

test_that("lossless_convert handles numeric and integer conversions correctly", {
  # Setup test vectors
  epsilon <- c('1', '2', '3', NA)
  zeta <- c('One', '2', '3', NA)
  eta <- c('1.1', '2', '3', NA)

  # Test successful integer conversion
  expect_true(inherits(lossless_convert(epsilon, 'integer'), 'integer'))

  # Test failed integer conversion
  expect_message(result_zeta <- lossless_convert(zeta, 'integer', column_name = "zeta"),
                 "Conversion of 'zeta' to integer would introduce additional NAs")
  expect_true(inherits(result_zeta, 'character'))

  expect_message(result_eta <- lossless_convert(eta, 'integer', column_name = "eta"),
                 "Conversion of 'eta' to integer would introduce additional NAs")
  expect_true(inherits(result_eta, 'character'))

  # Test successful numeric conversion
  expect_true(inherits(lossless_convert(epsilon, 'numeric'), 'numeric'))
  expect_true(inherits(lossless_convert(eta, 'numeric'), 'numeric'))
})

test_that("lossless_convert handles POSIXct conversions correctly", {
  # Setup test vector
  tau <- c(NA, '2023-01-01 12:30:45', '2024-12-31 23:59:59', '2025-01-01 11:11:11')

  # Test successful POSIXct conversion
  expect_true(inherits(lossless_convert(tau, 'POSIXct'), 'POSIXct'))
})

test_that("lossless_convert deals with unique issues of raw conversions", {
  expect_message(lossless_convert(NA, class = 'raw'), "Conversion of 'NA' to raw would introduce additional NAs.")
  expect_equal(as.integer(lossless_convert(c(0, "1", "2", 3, 4, 5L, 6L), class = 'raw')), c(0:6))
  expect_equal(lossless_convert(c(1, 2, 3.1), class = 'raw'), c(1, 2, 3.1)) # because limited to [0L:255L]
  expect_message(lossless_convert(c(1, 2, 3.1), class = 'raw'), "Conversion of 'c\\(1, 2, 3\\.1\\)' to raw would introduce additional NAs.")
  expect_message(lossless_convert('test', class = 'raw'), 'Conversion of \'"test"\' to raw would introduce additional NAs.')
})

test_that("lossless_convert works with data.table", {
  # Setup test vectors
  alpha <- c('2022-01-01', '2023-01-01', '2024-01-01', '2025-01-01')
  beta <- c(NA, '2023-01-01', '2024-01-01', '2025-01-01')
  gamma <- c(NA, 'Not a Date', '2024-01-01', '2025-01-01')
  delta <- c('Not a Date', '2023-01-01', '2024-01-01', '2025-01-01')
  epsilon <- c('1', '2', '3', NA)
  zeta <- c('One', '2', '3', NA)
  eta <- c('1.1', '2', '3', NA)

  # Create data.table
  library(data.table)
  mydt <- data.table(alpha, beta, gamma, delta, epsilon, zeta, eta)

  # Test numeric conversion on all columns
  mydt[, (names(mydt)) := lapply(names(mydt), function(col_name) {
    lossless_convert(get(col_name), class = 'numeric', column_name = col_name)
  })]

  # Check that only epsilon and eta were converted to numeric
  expect_equal(names(mydt)[sapply(mydt, is.numeric)], c('epsilon', 'eta'))

  # Test Date conversion on all columns
  mydt[, (names(mydt)) := lapply(names(mydt), function(col_name) {
    lossless_convert(get(col_name), class = 'Date', column_name = col_name)
  })]

  # Check that only alpha and beta were converted to Date
  expect_equal(
    names(mydt)[sapply(mydt, function(x) inherits(x, "Date"))],
    c("alpha", "beta")
  )
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

# std_error() ----
test_that('std_error',{
  expect_equal(std_error(c(seq(0, 400, 100), NA)), sd(c(seq(0, 400, 100), NA), na.rm = T) / sqrt(5))
})

test_that('std_error',{
  expect_equal(std_error(c(seq(0, 400, 100), NA)), sd(c(seq(0, 400, 100), NA), na.rm = T) / sqrt(5))
})

# string_clean() -----
test_that("string_clean validates arguments correctly", {
  expect_error(string_clean(NULL), "must be the name of a data.frame or data.table")

  expect_error(string_clean(c(1, 2, 3)), "must be the name of a data.frame or data.table")

  test_dt <- data.table(text = "test")
  expect_error(string_clean(test_dt, stringsAsFactors = "yes"), "stringsAsFactors must be specified as a logical")

  expect_error(string_clean(test_dt, convert_to_utf8 = "yes"), "convert_to_utf8 must be specified as a logical")
})

test_that("string_clean handles standard whitespace correctly", {
  test_dt <- data.table(
    id = 1:3,
    text = c("  Hello  World  ", # regular spaces
             "Test\t\tString", # tabs
             "  Multiple\n\nSpaces  ") # new lines
  )

  expected <- c("Hello World", "Test String", "Multiple Spaces")

  string_clean(test_dt)

  expect_equal(test_dt$text, expected)
})

test_that("string_clean handles non-standard whitespace correctly", {
  test_dt <- data.table(
    id = 1:13,
    text = c(
      paste0("\u200A", "Hello", "\u200A"),     # Hair space
      paste0("\u2002", "Hello", "\u2002"),     # En space
      paste0("\u00A0", "Hello", "\u00A0"),     # Non breaking space
      paste0("\u2003", "Hello", "\u2003"),     # Em space
      paste0("Hello", "\u2009", "World"),      # Thin space between words
      paste0("\u200A\u2002\u00A0", "Hello", "\u2003\u2009\u200A"),  # Multiple mixed spaces
      paste0("\u2006", "Hello", "\u2006"),     # 6-per-em space
      paste0("\u202F", "Hello", "\u202F"),     # Narrow no-break space
      paste0("\u2005", "Hello", "\u2005"),     # 4-per-em space
      paste0("\u2008", "Hello", "\u2008"),     # Punctuation space
      paste0("\u2004", "Hello", "\u2004"),     # 3-per-em space
      paste0("\u2007", "Hello", "\u2007"),     # Figure space
      paste0("\u2006\u202F\u2005", "Hello", "\u2008\u2004\u2007")   # More mixed spaces
    )
  )

  expected <- c("Hello", "Hello", "Hello", "Hello", "Hello World", "Hello",
                "Hello", "Hello", "Hello", "Hello", "Hello", "Hello", "Hello")

  string_clean(test_dt)

  expect_equal(test_dt$text, expected)
})

test_that("string_clean handles zero-width and invisible characters correctly", {
  test_dt <- data.table(
    id = 1:8,
    text = c(
      paste0("\u200B", "Hello", "\u200B"),             # Zero-width space
      paste0("\u200C", "Hello", "\u200C"),             # Zero-width non-joiner
      paste0("\u200D", "Hello", "\u200D"),             # Zero-width joiner
      paste0("\uFEFF", "Hello", "\uFEFF"),             # Byte-order mark
      paste0("Hel", "\u200B", "lo"),                   # Zero-width space in middle of word
      paste0("Hel", "\u200C\u200D", "lo"),             # Multiple zero-width characters in word
      paste0("\u200B\u200C", "Hello", "\u200D\uFEFF"), # Multiple mixed invisible characters
      paste0("\u200B\u00A0\u200C", "Hello", "\u2003\u200D\uFEFF")  # Mix of invisible and whitespace
    )
  )

  expected <- c("Hello", "Hello", "Hello", "Hello", "Hello", "Hello", "Hello", "Hello")

  string_clean(test_dt)

  expect_equal(test_dt$text, expected)
})

test_that("string_clean handles empty strings and NA values correctly", {
  test_dt <- data.table(
    id = 1:5,
    text = c("Content", "", "   ", NA, "  Content  ")
  )

  expected <- c("Content", NA, NA, NA, "Content")

  string_clean(test_dt)

  expect_equal(test_dt$text, expected)
})

test_that("string_clean correctly handles factor columns", {
  test_dt <- data.table(
    id = 1:3,
    text_factor = factor(c("  Option A  ", "Option   B", "  Option C  "))
  )

  expected <- factor(c("Option A", "Option B", "Option C"))

  string_clean(test_dt)

  expect_equal(as.character(test_dt$text_factor), as.character(expected))

  expect_true(is.factor(test_dt$text_factor))
})

test_that("string_clean correctly converts strings to factors when requested", {
  test_dt <- data.table(
    id = 1:3,
    text = c("  Hello  ", "World", "  Test  ")
  )

  string_clean(test_dt, stringsAsFactors = TRUE)

  expect_true(is.factor(test_dt$text))

  expect_equal(levels(test_dt$text), c("Hello", "Test", "World"))
})

test_that("string_clean preserves non-string columns", {
  test_dt <- data.table(
    id = 1:3,
    numeric = c(1.5, 2.7, 3.9),
    integer = as.integer(c(1, 2, 3)),
    logical = c(TRUE, FALSE, TRUE),
    text = c("  Hello  ", "World", "  Test  ")
  )

  test_copy <- copy(test_dt) # copy b/c want the original for comparison

  string_clean(test_copy)

  expect_equal(test_copy$numeric, test_dt$numeric)
  expect_equal(test_copy$integer, test_dt$integer)
  expect_equal(test_copy$logical, test_dt$logical)

  # Check if the string column is cleaned properly
  expect_equal(test_copy$text, c("Hello", "World", "Test"))
})

