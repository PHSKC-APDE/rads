library(data.table)
library(dtsurvey)

# test internal functions ----
test_that("is_binary_var() correctly classifies data", {
  # factors
  expect_true(is_binary_var(factor(c("Yes", "No", "Yes"), levels = c("Yes", "No"))))
  expect_false(is_binary_var(factor(c("A", "B", "C"), levels = c("A", "B", "C"))))
  expect_false(is_binary_var(factor(c("A"), levels = c("A"))))

  # logicals
  expect_true(is_binary_var(c(TRUE, FALSE, TRUE, NA)))
  expect_true(is_binary_var(c(TRUE, TRUE, TRUE)))

  # numeric
  expect_true(is_binary_var(c(0, 1, 0, 1, NA)))
  expect_true(is_binary_var(c(0L, 1L, 1L)))
  expect_false(is_binary_var(c(0, 1, 2)))
  expect_false(is_binary_var(c(0.5, 1, 0)))
  expect_false(is_binary_var(rep(NA_real_, 5)))
  expect_true(is_binary_var(c(1, 1, 1)))

  # character
  expect_false(is_binary_var(c("0", "1", "0")))
})

test_that("is_proportion_var() correctly classifies data", {
  # factors
  expect_true(is_proportion_var(factor(c("Yes", "No", "Yes"), levels = c("Yes", "No"))))
  expect_true(is_proportion_var(factor(c("A", "B", "C"), levels = c("A", "B", "C"))))
  expect_true(is_proportion_var(factor(c("A"), levels = c("A"))))

  # logicals
  expect_true(is_proportion_var(c(TRUE, FALSE, TRUE, NA)))

  # numerics
  expect_true(is_proportion_var(c(0, 1, 0, 1, NA))) # because binary
  expect_false(is_proportion_var(c(0, 1, 2)))
  expect_false(is_proportion_var(c(0.5, 1, 0)))
  expect_false(is_proportion_var(rep(NA_real_, 5)))

  # character
  expect_false(is_proportion_var(c("0", "1", "0")))
})


# calc() on admin data: binary numeric (0/1) ----
make_admin_data <- function(n = 2000, p = 0.08, seed = 1){
  set.seed(seed)
  dt <- data.table::data.table(
    id = seq_len(n),
    diabetes = rbinom(n, 1, p),          # 0/1 numeric indicator, prevalence ~p
    grp = sample(c("A", "B"), n, replace = TRUE)
  )
  dt[, not_diabetes := 1L - diabetes]     # explicit complement, for symmetry checks
  dtsurvey::dtadmin(dt)
}

test_that("autodetect applies the flipped RSE formula to a 0/1 numeric indicator", {
  ph <- make_admin_data()

  res_default  <- calc(ph, what = "diabetes", metrics = c("mean", "rse"))
  res_explicit <- calc(ph, what = "diabetes", metrics = c("mean", "rse"), proportion = TRUE)

  expect_equal(res_default$rse, res_explicit$rse)

  # Manually verify the formula: 100 * se / min(mean, 1-mean)
  expected_rse <- 100 * res_default$mean_se / pmin(res_default$mean, 1 - res_default$mean)
  expect_equal(res_default$rse, expected_rse)
})

test_that("a 0/1 indicator and its explicit complement get identical RSE under autodetect", {
  ph <- make_admin_data()

  res_x    <- calc(ph, what = "diabetes",     metrics = c("mean", "rse"))
  res_notx <- calc(ph, what = "not_diabetes", metrics = c("mean", "rse"))

  # Estimates should be complementary...
  expect_equal(res_x$mean, 1 - res_notx$mean, tolerance = 1e-8)

  # RSE should be identical
  expect_equal(res_x$rse, res_notx$rse, tolerance = 1e-8)
})

test_that("proportion = FALSE forces the standard (asymmetric) RSE formula", {
  ph <- make_admin_data()

  res_false <- calc(ph, what = "not_diabetes", metrics = c("mean", "rse"), proportion = FALSE)
  expected_rse <- 100 * res_false$mean_se / res_false$mean
  expect_equal(res_false$rse, expected_rse)

  # Should differ from RSE when proportion autodetected
  res_auto <- calc(ph, what = "not_diabetes", metrics = c("mean", "rse"), proportion = 'autodetect')
  expect_false(isTRUE(all.equal(res_false$rse, res_auto$rse)))
})

# calc() on admin data: 2-level factor ----
make_admin_factor_data <- function(n = 2000, p = 0.08, seed = 2){
  set.seed(seed)
  dt <- data.table::data.table(
    id = seq_len(n),
    burden = factor(rbinom(n, 1, p), levels = c(0, 1), labels = c("Not burdened", "Burdened"))
  )
  dtsurvey::dtadmin(dt)
}

test_that("both levels of a 2-level factor receive the same RSE under autodetect", {
  ph <- make_admin_factor_data()
  res <- calc(ph, what = "burden", metrics = c("mean", "rse"))

  # Both rows should carry the identical rse value
  expect_equal(res$rse[1], res$rse[2])

  # Manually test that RSE = 100 * se / min(mean, 1-mean) using EITHER row
  expected_rse1 <- 100 * res$mean_se[1] / min(res$mean[1], 1 - res$mean[1])
  expect_equal(res$rse[2], expected_rse1)

  expected_rse2 <- 100 * res$mean_se[2] / min(res$mean[2], 1 - res$mean[2])
  expect_equal(res$rse[1], expected_rse2)
})

# calc() on admin data: 3+ level factor (RSE should be unaffected) ----
make_admin_multilevel_data <- function(n = 2000, seed = 3){
  set.seed(seed)
  dt <- data.table::data.table(
    id = seq_len(n),
    cat3 = factor(sample(c("Low", "Medium", "High"), n, replace = TRUE, prob = c(0.7, 0.2, 0.1)),
                  levels = c("Low", "Medium", "High"))
  )
  dtsurvey::dtadmin(dt)
}

test_that("3-level factors keep the original (per-level) RSE formula, not the binary flip", {
  ph <- make_admin_multilevel_data()
  res <- calc(ph, what = "cat3", metrics = c("mean", "rse"))

  expected_rse <- 100 * res$mean_se / res$mean
  expect_equal(res$rse, expected_rse)

  # Sanity check: rse should NOT all be identical here (unlike binary cases)
  expect_gt(length(unique(res$rse)), 1)
})

# proportion argument validation ----
test_that("invalid `proportion` values trigger an error", {
  ph <- make_admin_data()
  expect_error(calc(ph, what = "diabetes", metrics = "mean", proportion = "yes"))
  expect_error(calc(ph, what = "diabetes", metrics = "mean", proportion = NA))
  expect_error(calc(ph, what = "diabetes", metrics = "mean", proportion = c(TRUE, FALSE)))
})

test_that("proportion = TRUE on a variable that isn't proportion-like warns but still runs", {
  # A continuous numeric (not a factor, not logical, not 0/1) is the only case that should warn --
  # a 3+ level factor is proportion-like (see tests below) and should NOT warn.
  set.seed(98104)
  dt <- data.table::data.table(weight_kg = rnorm(500, 70, 10))
  ph <- dtsurvey::dtadmin(dt)

  expect_warning(
    calc(ph, what = "weight_kg", metrics = c("mean", "rse"), proportion = TRUE),
    "does not seem to be proportion-like"
  )
})

test_that("proportion = TRUE on a genuinely binary variable does NOT warn", {
  ph <- make_admin_data()
  expect_no_warning(calc(ph, what = "diabetes", metrics = c("mean", "rse"), proportion = TRUE))
})

test_that("proportion = TRUE on a 3-level factor does NOT warn (it's proportion-like, just not binary)", {
  ph <- make_admin_multilevel_data()
  expect_no_warning(calc(ph, what = "cat3", metrics = c("mean", "rse"), proportion = TRUE))
})

# logical variables ----
test_that("logical `what` variables are autodetected as binary and get the flipped RSE", {
  set.seed(98104)
  dt <- data.table::data.table(flag = sample(c(TRUE, FALSE), 2000, replace = TRUE, prob = c(0.1, 0.9)))
  ph <- dtsurvey::dtadmin(dt)

  res <- calc(ph, what = "flag", metrics = c("mean", "rse"))
  expected_rse <- 100 * res$mean_se / min(res$mean, 1 - res$mean)
  expect_equal(res$rse, expected_rse)
})

# detection happens pre-`where`-filter ----
# a real 0|1 variable is *always* still 0|1 in any row-subset, so filtering it down
# to an all-1s (or all-0s) subset should not change its identification as a binary.
# More importantly, consider the case where a variable that is NOT structurally
# binary (e.g. it has values 0, 1, *and* 2) but where a `where` clause coincidentally
# leaves only 0s|1s behind. This is why we want to use the pre-filtered column.

test_that("binary detection is based on the full column, not the `where`-filtered subset", {
  set.seed(98104)
  n <- 500
  dt <- data.table::data.table(
    id = seq_len(n),
    # genuinely 3-valued (0/1/2) in the full data -- not structurally binary
    score = c(rep(0L, 285), rep(1L, 15), rep(2L, 200)),
    grp = c(rep("low_only", 300), rep("mixed", 200))
  )
  ph <- dtsurvey::dtadmin(dt)

  # within "low_only", `score` only takes values 0 and 1 -- i.e., a false binary
  res <- calc(ph, what = "score", where = grp == "low_only", metrics = c("mean", "rse"))

  expect_equal(nrow(res), 1)

  # standard (non-flipped) rse formula should apply, since the full `score` column is not binary
  expect_equal(res$rse, 100 * res$mean_se / (res$mean))
})

# survey data: RSE should be same for both levels of a binary ----
test_that("SE(p) == SE(1-p) holds for weighted survey estimates too (2-level factor)", {
  set.seed(98104)
  n <- 1000
  dt <- data.table::data.table(
    id = seq_len(n),
    psu = seq_len(n),
    strata = sample(1:5, n, replace = TRUE),
    wt = runif(n, 0.5, 2),
    burden = factor(rbinom(n, 1, 0.08), levels = c(0, 1), labels = c("Not burdened", "Burdened"))
  )

  svy <- dtsurvey::dtsurvey(dt, weight = "wt", psu = "psu", strata = "strata")

  res <- calc(svy, what = "burden", metrics = c("mean", "rse"))
  expect_equal(nrow(res), 2)
  expect_equal(res$mean_se[1], res$mean_se[2])
  expect_equal(res$rse[1], res$rse[2])
})

# proportion` now also drives CI method selection, not just rse ----
test_that("autodetect uses the xlogit CI (like proportion = TRUE), not the plain mean CI, for a binary factor on survey data", {
  set.seed(98104)
  n <- 1000
  dt <- data.table::data.table(
    id = seq_len(n),
    psu = seq_len(n),
    strata = sample(1:5, n, replace = TRUE),
    wt = runif(n, 0.5, 2),
    burden = factor(rbinom(n, 1, 0.08), levels = c(0, 1), labels = c("Not burdened", "Burdened"))
  )
  svy <- dtsurvey::dtsurvey(dt, weight = "wt", psu = "psu", strata = "strata")

  res_auto  <- calc(svy, what = "burden", metrics = c("mean"), proportion = 'autodetect')
  res_true  <- calc(svy, what = "burden", metrics = c("mean"), proportion = TRUE)
  res_false <- calc(svy, what = "burden", metrics = c("mean"), proportion = FALSE)

  # autodetect should match the explicit xlogit (proportion = TRUE) CI, not the plain-mean one
  expect_equal(res_auto$mean_lower, res_true$mean_lower)
  expect_equal(res_auto$mean_upper, res_true$mean_upper)
  expect_false(isTRUE(all.equal(res_auto$mean_lower, res_false$mean_lower)))
  expect_false(isTRUE(all.equal(res_auto, res_false)))
})

test_that("autodetect gives a 3-level survey factor proportion-appropriate CIs too, even though it isn't binary", {
  set.seed(98104)
  n <- 1000
  dt <- data.table::data.table(
    id = seq_len(n),
    psu = seq_len(n),
    strata = sample(1:5, n, replace = TRUE),
    wt = runif(n, 0.5, 2),
    cat3 = factor(sample(c("Low", "Medium", "High"), n, replace = TRUE, prob = c(0.7, 0.2, 0.08)),
                  levels = c("Low", "Medium", "High"))
  )
  svy <- dtsurvey::dtsurvey(dt, weight = "wt", psu = "psu", strata = "strata")

  res_auto  <- calc(svy, what = "cat3", metrics = c("mean", "rse"), proportion = 'autodetect')
  res_true  <- calc(svy, what = "cat3", metrics = c("mean", "rse"), proportion = TRUE)
  res_false <- calc(svy, what = "cat3", metrics = c("mean", "rse"), proportion = FALSE)

  # autodetect's CIs should match the explicit proportion = TRUE (xlogit) CIs...
  expect_equal(res_auto$mean_lower, res_true$mean_lower)
  expect_equal(res_auto$mean_upper, res_true$mean_upper)
  # ...and differ from the plain mean-based CI (proportion = FALSE)
  expect_false(isTRUE(all.equal(res_auto$mean_lower, res_false$mean_lower)))
})

