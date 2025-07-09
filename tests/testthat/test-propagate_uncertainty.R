# test-propagate_uncertainty.R

library(testthat)
library(data.table)

# Test data setup
setup_test_data <- function() {
  data.table(
    location = c("A", "B", "C", "D"),
    comp_mean = c(10.5, 15.2, 8.7, 12.3),
    comp_se = c(1.2, 1.8, 0.9, 1.5),
    ref_mean = c(8.3, 12.1, 9.2, 10.8),
    ref_se = c(1.0, 1.5, 1.1, 1.3)
  )
}

setup_ci_data <- function() {
  data.table(
    outcome = c("Death", "Hospitalization", "ICU"),
    comp_rate = c(1.5, 3.2, 2.1),
    comp_lower = c(1.2, 2.8, 1.8),
    comp_upper = c(1.9, 3.7, 2.5),
    ref_rate = c(1.0, 2.1, 1.8),
    ref_lower = c(0.8, 1.8, 1.5),
    ref_upper = c(1.3, 2.5, 2.2)
  )
}

test_that("Basic functionality works with SE inputs", {
  dt <- setup_test_data()

  result <- propagate_uncertainty(
    ph.estimates = dt,
    comp_mean_col = "comp_mean",
    comp_se_col = "comp_se",
    ref_mean_col = "ref_mean",
    ref_se_col = "ref_se",
    draws = 1000,
    seed = 123
  )

  # Check that new columns are added
  expect_true("contrast" %in% names(result))
  expect_true("contrast_se" %in% names(result))
  expect_true("contrast_lower" %in% names(result))
  expect_true("contrast_upper" %in% names(result))
  expect_true("contrast_pvalue" %in% names(result))

  # Check dimensions
  expect_equal(nrow(result), 4)

  # Check that results are numeric and finite
  expect_true(all(is.finite(result$contrast)))
  expect_true(all(is.finite(result$contrast_se)))
  expect_true(all(result$contrast_se > 0))
})

test_that("CI-only inputs work correctly", {
  dt <- setup_ci_data()

  result <- propagate_uncertainty(
    ph.estimates = dt,
    comp_mean_col = "comp_rate",
    comp_lower_col = "comp_lower",
    comp_upper_col = "comp_upper",
    ref_mean_col = "ref_rate",
    ref_lower_col = "ref_lower",
    ref_upper_col = "ref_upper",
    draws = 1000,
    seed = 123
  )

  expect_true(all(is.finite(result$contrast)))
  expect_equal(nrow(result), 3)
})

test_that("Ratio contrasts work", {
  dt <- setup_test_data()

  result <- propagate_uncertainty(
    ph.estimates = dt,
    comp_mean_col = "comp_mean",
    comp_se_col = "comp_se",
    ref_mean_col = "ref_mean",
    ref_se_col = "ref_se",
    contrast_fn = function(x, y) x / y,
    draws = 1000,
    seed = 123
  )

  # Ratios should be positive
  expect_true(all(result$contrast > 0))
})

test_that("Lognormal distribution works", {
  dt <- setup_test_data()

  result <- propagate_uncertainty(
    ph.estimates = dt,
    comp_mean_col = "comp_mean",
    comp_se_col = "comp_se",
    ref_mean_col = "ref_mean",
    ref_se_col = "ref_se",
    dist = "lognormal",
    draws = 1000,
    seed = 123
  )

  expect_true(all(is.finite(result$contrast)))
})

test_that("Input validation works", {
  dt <- setup_test_data()

  # Missing required column
  expect_error(
    propagate_uncertainty(
      ph.estimates = dt,
      comp_mean_col = "nonexistent",
      comp_se_col = "comp_se",
      ref_mean_col = "ref_mean",
      ref_se_col = "ref_se"
    ),
    "Missing required columns"
  )

  # Invalid distribution
  expect_error(
    propagate_uncertainty(
      ph.estimates = dt,
      comp_mean_col = "comp_mean",
      comp_se_col = "comp_se",
      ref_mean_col = "ref_mean",
      ref_se_col = "ref_se",
      dist = "gamma"
    ),
    "dist must be 'normal' or 'lognormal'"
  )

  # Too few draws
  expect_error(
    propagate_uncertainty(
      ph.estimates = dt,
      comp_mean_col = "comp_mean",
      comp_se_col = "comp_se",
      ref_mean_col = "ref_mean",
      ref_se_col = "ref_se",
      draws = 50
    ),
    "draws must be at least 100"
  )

  # Invalid alpha
  expect_error(
    propagate_uncertainty(
      ph.estimates = dt,
      comp_mean_col = "comp_mean",
      comp_se_col = "comp_se",
      ref_mean_col = "ref_mean",
      ref_se_col = "ref_se",
      alpha = 1.5
    ),
    "alpha must be between 0 and 1"
  )

  # Not a data.table
  expect_no_warning(
    propagate_uncertainty(
      ph.estimates = setDF(copy(dt)),
      comp_mean_col = "comp_mean",
      comp_se_col = "comp_se",
      ref_mean_col = "ref_mean",
      ref_se_col = "ref_se"
    )
  )

  # Empty data.table
  expect_error(
    propagate_uncertainty(
      ph.estimates = data.table(),
      comp_mean_col = "comp_mean",
      comp_se_col = "comp_se",
      ref_mean_col = "ref_mean",
      ref_se_col = "ref_se"
    ),
    "ph.estimates cannot be empty"
  )

  # Missing uncertainty info
  expect_error(
    propagate_uncertainty(
      ph.estimates = dt,
      comp_mean_col = "comp_mean",
      ref_mean_col = "ref_mean"
    ),
    "Must provide either comp_se_col OR both comp_lower_col and comp_upper_col"
  )
})

test_that("Warning for both SE and CI provided", {
  dt <- setup_test_data()
  dt[, comp_lower := comp_mean - 1.96 * comp_se]
  dt[, comp_upper := comp_mean + 1.96 * comp_se]

  expect_warning(
    propagate_uncertainty(
      ph.estimates = dt,
      comp_mean_col = "comp_mean",
      comp_se_col = "comp_se",
      comp_lower_col = "comp_lower",
      comp_upper_col = "comp_upper",
      ref_mean_col = "ref_mean",
      ref_se_col = "ref_se",
      draws = 1000
    ),
    "Comparator has both SE and CI; prioritizing SE"
  )
})

test_that("All missing uncertainty data produces appropriate warning", {
  dt <- data.table(
    location = "test",
    comp_mean = 5.0,
    comp_se = NA,
    ref_mean = 3.0,
    ref_se = NA
  )

  expect_warning(
    result <- propagate_uncertainty(
      ph.estimates = dt,
      comp_mean_col = "comp_mean",
      comp_se_col = "comp_se",
      ref_mean_col = "ref_mean",
      ref_se_col = "ref_se",
      draws = 1000
    ),
    "insufficient uncertainty information"
  )
})

test_that("Zero standard errors are handled appropriately", {
  dt <- setup_test_data()
  dt[1, comp_se := 0]

  expect_warning(
    result <- propagate_uncertainty(
      ph.estimates = dt,
      comp_mean_col = "comp_mean",
      comp_se_col = "comp_se",
      ref_mean_col = "ref_mean",
      ref_se_col = "ref_se",
      draws = 1000,
      seed = 123
    ),
    "insufficient uncertainty information"
  )

  # Should still return a result
  expect_equal(nrow(result), 4)
})

test_that("Extremely wide confidence intervals work", {
  dt <- data.table(
    location = "test",
    comp_rate = 2.0,
    comp_lower = 0.001,  # Very wide CI
    comp_upper = 1000.0,
    ref_rate = 1.0,
    ref_lower = 0.1,
    ref_upper = 10.0
  )

  expect_no_error(
    result <- propagate_uncertainty(
      ph.estimates = dt,
      comp_mean_col = "comp_rate",
      comp_lower_col = "comp_lower",
      comp_upper_col = "comp_upper",
      ref_mean_col = "ref_rate",
      ref_lower_col = "ref_lower",
      ref_upper_col = "ref_upper",
      draws = 1000,
      seed = 123
    )
  )

  expect_true(result$contrast_se > 0)
})

test_that("Partially missing CI bounds are handled appropriately", {
  dt <- setup_test_data()

  # Test #1: Missing upper bound for comparator
  dt1 <- copy(dt)
  dt1[, comp_lower := comp_mean - 1.96 * comp_se]
  dt1[, comp_upper := NA]  # Missing upper bound
  dt1[, comp_se := NULL]   # Remove SE so it must use CI

  expect_warning(
    result1 <- propagate_uncertainty(
      ph.estimates = dt1,
      comp_mean_col = "comp_mean",
      comp_lower_col = "comp_lower",
      comp_upper_col = "comp_upper",
      ref_mean_col = "ref_mean",
      ref_se_col = "ref_se",
      draws = 1000,
      seed = 123
    ),
    "insufficient uncertainty information"
  )

  # Test #2: Missing lower bound for reference
  dt2 <- copy(dt)
  dt2[, ref_lower := NA]  # Missing lower bound
  dt2[, ref_upper := ref_mean + 1.96 * ref_se]
  dt2[, ref_se := NULL]   # Remove SE so it must use CI

  expect_warning(
    result2 <- propagate_uncertainty(
      ph.estimates = dt2,
      comp_mean_col = "comp_mean",
      comp_se_col = "comp_se",
      ref_mean_col = "ref_mean",
      ref_lower_col = "ref_lower",
      ref_upper_col = "ref_upper",
      draws = 1000,
      seed = 123
    ),
    "insufficient uncertainty information"
  )

  # Should still return results but with warnings
  expect_equal(nrow(result1), 4)
  expect_equal(nrow(result2), 4)
})

test_that("Convergence check works", {
  dt <- setup_test_data()

  result <- propagate_uncertainty(
    ph.estimates = dt,
    comp_mean_col = "comp_mean",
    comp_se_col = "comp_se",
    ref_mean_col = "ref_mean",
    ref_se_col = "ref_se",
    draws = 50000,
    convergence_check = TRUE,
    seed = 123
  )

  conv_results <- attr(result, "convergence_check")
  expect_true(!is.null(conv_results))
  expect_true("prop_converged" %in% names(conv_results))
})

test_that("Different p-value methods work", {
  dt <- setup_test_data()

  result_prop <- propagate_uncertainty(
    ph.estimates = dt,
    comp_mean_col = "comp_mean",
    comp_se_col = "comp_se",
    ref_mean_col = "ref_mean",
    ref_se_col = "ref_se",
    pvalue_method = "proportion",
    draws = 1000,
    seed = 123
  )

  result_ttest <- propagate_uncertainty(
    ph.estimates = dt,
    comp_mean_col = "comp_mean",
    comp_se_col = "comp_se",
    ref_mean_col = "ref_mean",
    ref_se_col = "ref_se",
    pvalue_method = "ttest",
    draws = 1000,
    seed = 123
  )

  expect_true(all(is.finite(result_prop$contrast_pvalue)))
  expect_true(all(is.finite(result_ttest$contrast_pvalue)))
  expect_true(all(result_prop$contrast_pvalue >= 0 & result_prop$contrast_pvalue <= 1))
  expect_true(all(result_ttest$contrast_pvalue >= 0 & result_ttest$contrast_pvalue <= 1))
})

test_that("Metadata is preserved", {
  dt <- setup_test_data()

  result <- propagate_uncertainty(
    ph.estimates = dt,
    comp_mean_col = "comp_mean",
    comp_se_col = "comp_se",
    ref_mean_col = "ref_mean",
    ref_se_col = "ref_se",
    draws = 1000,
    dist = "normal"
  )

  params <- attr(result, "propagate_uncertainty_params")
  expect_equal(params$dist, "normal")
  expect_equal(params$draws, 1000)
  expect_equal(params$alpha, 0.05)
})

test_that("Handle infinite values from division by zero", {
  dt <- data.table(
    location = "test",
    comp_mean = 5.0,
    comp_se = 0.5,
    ref_mean = 0.0,  # This will cause division by zero
    ref_se = 0
  )

  expect_warning(
    result <- propagate_uncertainty(
      ph.estimates = dt,
      comp_mean_col = "comp_mean",
      comp_se_col = "comp_se",
      ref_mean_col = "ref_mean",
      ref_se_col = "ref_se",
      contrast_fn = function(x, y) x / y,
      draws = 1000,
      seed = 123
    ),
    "infinite values in calculated contrasts"
  )

  # Should handle infinites by converting to NA
  expect_true(is.na(result$contrast) || is.finite(result$contrast))
})

test_that("Handle missing values in inputs", {
  dt <- setup_test_data()
  dt[1, comp_mean := NA]

  expect_warning(
    result <- propagate_uncertainty(
      ph.estimates = dt,
      comp_mean_col = "comp_mean",
      comp_se_col = "comp_se",
      ref_mean_col = "ref_mean",
      ref_se_col = "ref_se",
      draws = 1000
    ),
    "rows have missing point estimates"
  )

  expect_equal(nrow(result), 4)
})
