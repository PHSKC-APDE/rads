#' Propagate Uncertainty Between Two Point Estimates using Monte Carlo simulation
#'
#' @description
#' Propagates uncertainty when calculating contrasts (differences, ratios, etc.)
#' between two point estimates by using Monte Carlo simulation. This approach is
#' needed when assumptions of the normal approximation do not hold. Relevant
#' examples include exponentiated estimates from logistic or Poisson models, or
#' when confidence intervals are calculated using specialized methods like the
#' [Fay-Feuer](https://wonder.cdc.gov/controller/pdf/FayFeuerConfidenceIntervals.pdf)
#' method (which is used for age-standardized rates). Basically, unless
#' you have point estimates and uncertainty from a linear model of from simple
#' means and proportions, you should probably use this function rather than
#' traditional mathematical methods for error propagation that assume normality.
#'
#' @param ph.estimates A data.table containing the point estimates and
#' uncertainty measures
#' @param comp_mean_col Character. Column name for the comparator group point
#' estimates
#' @param comp_se_col Character. Column name for the comparator group standard
#' errors (optional)
#' @param comp_lower_col Character. Column name for the comparator group lower
#' CI (optional)
#' @param comp_upper_col Character. Column name for the comparator group upper
#' CI (optional)
#' @param ref_mean_col Character. Column name for the reference group point
#' estimates
#' @param ref_se_col Character. Column name for the reference group standard
#' errors (optional)
#' @param ref_lower_col Character. Column name for the reference group lower CI
#' (optional)
#' @param ref_upper_col Character. Column name for the reference group upper CI
#' (optional)
#' @param contrast_fn Function. User defined function to calculate contrast
#'   between groups.
#'   Common options include:
#'   - `function(x, y) x - y` ***... for differences (default)***
#'   - `function(x, y) x / y` ***... for ratios***
#'   - `function(x, y) 100 * (x - y) / y` ***... for percent differences***
#'   - `function(x, y) log(x / y)` ***... for log ratios***
#' @param dist Character. Distribution assumption for the INPUT estimates (not
#' the contrast):
#'   - `"normal"`: Use normal distribution. Appropriate when input estimates can
#'   theoretically take any real value, including: means, differences, proportions,
#'   log-odds, log-hazard ratios, and other log-transformed coefficients (default)
#'   - `"lognormal"`: Use lognormal distribution. Appropriate when input estimates
#'   are strictly positive and right-skewed, such as: exponentiated coefficients
#'   (odds ratios, hazard ratios, rate ratios), raw rates, or counts
#'   Note: This refers to the sampling distribution of your INPUT estimates,
#'   not the type of contrast function you're calculating.
#' @param se_scale Character. Scale that standard errors are reported on:
#'   - `"original"`: Standard errors are on the same scale as the point estimates
#'   (default)
#'   - `"log"`: Standard errors are on the log scale (sometimes used with
#'   exponentiated estimates)
#' @param draws Integer. Number of Monte Carlo draws per observation. Default
#'   `draws = 10000`
#' @param seed Integer. Random seed for reproducibility. Default `seed = 98104`
#' @param alpha Numeric. Alpha level for **OUTPUT (contrast)** confidence
#'   interval width (default 0.05 for 95% CI).
#'
#'   *Note:* alpha = 1 - CI level.
#' @param input_ci_level Numeric. The confidence level of the **INPUT** confidence
#'   intervals provided in the `*_lower_col` and `*_upper_col` parameters (default
#'   0.95 for 95% CIs).
#'
#'   *Note:* CI level = 1 - alpha.
#' @param h0_value Numeric. Value for null hypothesis testing. Default
#'   `h0_value = NULL` uses empirical detection (0 for differences, 1 for ratios).
#'   Set explicitly for custom tests (e.g., `h0_value = 10` to test if difference
#'   does not equal 10).
#' @param use_futures Logical. Whether to use future.apply for parallelization
#' @param pvalue_method Character. Method for p-value calculation:
#'   - "proportion": Empirical p-value based on proportion of draws crossing the
#'   null hypothesis (robust to non-normal distributions, recommended for most
#'   cases) (default)
#'   - "ttest": Traditional t-test that assumes normality (faster but flawed for
#'   skewed distributions)
#' @param convergence_check Logical. Whether to assess if Monte Carlo simulation
#' has converged (recommended for final analyses or if you have time to spare)
#' @param convergence_tolerance Numeric. Maximum acceptable relative change
#' between batches to consider "converged" (default 0.05 = 5%)
#' @param convergence_sample_size Integer. Number of randomly selected rows to
#' test for convergence (default 10 for efficiency)
#'
#' @return
#' A data.table with original columns plus:
#'   - `contrast`: Mean of contrast values
#'   - `contrast_se`: Standard error of contrast
#'   - `contrast_lower`: Lower confidence interval
#'   - `contrast_upper`: Upper confidence interval
#'   - `contrast_pvalue`: P-value for null hypothesis test
#'
#' @details
#' The function works by generating random draws from the uncertainty distributions
#' of both groups, applying the contrast function, and summarizing the resulting
#' empirical distribution.
#'
#' **Standard Errors vs Confidence Intervals**
#' If both are provided, standard errors take precedence with a warning. HOWEVER,
#' there are times when you will want to purposefully NOT provide the standard
#' error so the function will be fully dependent upon the confidence intervals.
#' This is appropriate when confidence intervals are calculated directly and the
#' standard error is an approximation at best (e.g., the Fay-Feuer method used
#' with age-standardized rates).
#'
#' **Distribution Choice:**
#' Choose based on the ORIGINAL INPUT estimates you're comparing, not the contrast:
#'
#' Use "normal" when your estimates:
#' - Can theoretically be negative (means, differences, log-coefficients)
#' - Are approximately symmetric around their true value
#' - Come from linear models or are log-transformed parameters
#'
#' Use "lognormal" when your estimates:
#' - Must be positive (rates, counts, exponentiated coefficients)
#' - Have right-skewed sampling distributions
#' - Come from models where uncertainty was calculated on the original scale
#'
#' Examples
#' - Comparing two log-odds ratios → use "normal"
#' - Comparing two odds ratios → use "lognormal"
#' - Comparing two mortality rates (deaths per 100,000) → use "lognormal"
#' - Comparing two risk differences → use "normal"
#' - Comparing two prevalences (e.g., 0.15 vs 0.20) → use "normal"
#'          (proportions are bounded but typically analyzed on original scale)
#'
#' **Standard Error Scale:**
#' Most standard errors are reported on the "original" scale. Use "log" scale ONLY
#' when you have standard errors that were calculated on the log scale (uncommon,
#' but sometimes seen with certain modeling approaches).
#'
#' **Convergence Checking:**
#' Monte Carlo methods require sufficient draws for stable results. The convergence
#' check examines whether estimates stabilize as more draws are added. If convergence
#' fails, increase the number of draws. This is particularly important for p-values
#' and when precise estimates are needed for smaller populations.
#'
#' **Null Hypothesis Testing:**
#' By default, the function tries to automatically detects the correct null hypothesis
#' value, e.g., 0 for differences and 1 for ratios. You can override this by setting
#' `h0_value`.
#'
#' @keywords uncertainty propagation monte-carlo statistics
#'
#' @examples
#' \dontrun{
#' library(data.table)
#'
#' # Example with normal distribution (differences)
#' dt <- data.table(
#'   location = c("A", "B", "C"),
#'   comp_mean = c(10.5, 15.2, 8.7),
#'   comp_se = c(1.2, 1.8, 0.9),
#'   ref_mean = c(8.3, 12.1, 9.2),
#'   ref_se = c(1.0, 1.5, 1.1)
#' )
#'
#' result <- propagate_uncertainty(
#'   ph.estimates = dt,
#'   comp_mean_col = "comp_mean",
#'   comp_se_col = "comp_se",
#'   ref_mean_col = "ref_mean",
#'   ref_se_col = "ref_se",
#'   draws = 10000
#' )
#'
#' # Example with ratios and confidence intervals
#' dt_ratios <- data.table(
#'   outcome = c("Death", "Hospitalization"),
#'   comp_rate = c(1.5, 3.2),
#'   comp_lower = c(1.2, 2.8),
#'   comp_upper = c(1.9, 3.7),
#'   ref_rate = c(1.0, 2.1),
#'   ref_lower = c(0.8, 1.8),
#'   ref_upper = c(1.3, 2.5)
#' )
#'
#' result_ratios <- propagate_uncertainty(
#'   ph.estimates = dt_ratios,
#'   comp_mean_col = "comp_rate",
#'   comp_lower_col = "comp_lower",
#'   comp_upper_col = "comp_upper",
#'   ref_mean_col = "ref_rate",
#'   ref_lower_col = "ref_lower",
#'   ref_upper_col = "ref_upper",
#'   contrast_fn = function(x, y) x / y,
#'   dist = "lognormal",
#'   draws = 10000
#' )
#'
#' # Example with custom null hypothesis value
#' # Testing that the ratio of x to y != 2
#' result_custom <- propagate_uncertainty(
#'   ph.estimates = dt,
#'   comp_mean_col = "comp_mean",
#'   comp_se_col = "comp_se",
#'   ref_mean_col = "ref_mean",
#'   ref_se_col = "ref_se",
#'   contrast_fn = function(x, y) x / y,
#'   h0_value = 2,
#'   draws = 10000
#' )
#'
#' }
#'
#' @importFrom data.table copy data.table setDT setattr :=
#' @importFrom stats median rnorm rlnorm qnorm quantile t.test sd
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @seealso [`multi_t_test()`] for comparing multiple groups against a
#'   reference group when estimates have symmetric confidence intervals and
#'   normality can be reasonably assumed.
#' @export
propagate_uncertainty <- function(
    ph.estimates,
    comp_mean_col,
    comp_se_col = NULL,
    comp_lower_col = NULL,
    comp_upper_col = NULL,
    ref_mean_col,
    ref_se_col  = NULL,
    ref_lower_col  = NULL,
    ref_upper_col  = NULL,
    contrast_fn = function(x, y) x - y,
    dist = "normal",
    se_scale = "original",
    draws = 10000,
    seed = 98104,
    alpha = 0.05,
    input_ci_level = 0.95,
    h0_value = NULL,
    use_futures = FALSE,
    pvalue_method = "proportion",
    convergence_check = FALSE,
    convergence_tolerance = 0.05,
    convergence_sample_size = 10) {

  # Parameter validation ----
    # ph.estimates
      if (!is.data.frame(ph.estimates)) {
        stop("\n\U0001F6D1 ph.estimates must be a data.frame or data.table")
      }

      if (!is.data.table(ph.estimates)) {
        ph.estimates <- setDT(ph.estimates)
      }

      if (nrow(ph.estimates) == 0) {
        stop("\n\U0001F6D1 ph.estimates cannot be empty")
      }

      ph.estimates <- copy(ph.estimates)

    # comp_mean_col & ref_mean_col < check that they exist
      required_cols <- c(comp_mean_col, ref_mean_col)
      missing_cols <- setdiff(required_cols, names(ph.estimates))
      if (length(missing_cols) > 0) {
        stop("\n\U0001F6D1 Missing required columns: ", paste(missing_cols, collapse = ", "))
      }

    # Check if specified uncertainty columns exist in ph.estimates
      optional_cols <- c(comp_se_col, comp_lower_col, comp_upper_col, ref_se_col, ref_lower_col, ref_upper_col)
      optional_cols <- optional_cols[!is.null(optional_cols)]  # drop NULL values, so keep only if specified
      missing_optional_cols <- setdiff(optional_cols, names(ph.estimates))
      if (length(missing_optional_cols) > 0) {
        stop("\n\U0001F6D1 Specified columns not found in data: ", paste(missing_optional_cols, collapse = ", "))
      }

    # Check that we specified some uncertainty for the comparator side of the contrast
      has_comp_uncertainty <- !is.null(comp_se_col) || (!is.null(comp_lower_col) && !is.null(comp_upper_col))
      if (!has_comp_uncertainty) {
        stop("\n\U0001F6D1 Must provide either comp_se_col OR both comp_lower_col and comp_upper_col")
      }

    # Check that we specified some uncertainty for the reference side of the contrast
      has_ref_uncertainty <- !is.null(ref_se_col) || (!is.null(ref_lower_col) && !is.null(ref_upper_col))
      if (!has_ref_uncertainty) {
        stop("\n\U0001F6D1 Must provide either ref_se_col OR both ref_lower_col and ref_upper_col")
      }

    # Check contrast_fun is a legit function
      if (!is.function(contrast_fn)) {
        stop("\n\U0001F6D1 contrast_fn must be a function")
      }
      tryCatch({ # identify when the contrast_fn simply does not work
        test_result <- contrast_fn(1, 1)
      }, error = function(e) {
        stop("\n\U0001F6D1 `contrast_fn` argument failed a basic test where x & y are both 1")
      })
      if (!is.numeric(test_result) || length(test_result) != 1) {
        stop("\n\U0001F6D1 contrast_fn must return a single numeric value")
      }

    # dist
      if (!dist %in% c("normal", "lognormal")) {
        stop("\n\U0001F6D1 dist must be 'normal' or 'lognormal'")
      }

    # se_scale
      if (!se_scale %in% c("original", "log")) {
        stop("\n\U0001F6D1 se_scale must be 'original' or 'log'")
      }

    # draws
      if (draws < 100) {
        stop("\n\U0001F6D1 draws must be at least 100")
      }

    # seed
      if (is.null(seed) || !is.numeric(seed) || length(seed) != 1 ) {
        stop("\n\U0001F6D1 seed must be a single numeric value")
      } else {set.seed(seed)}

    # alpha
      if ( !is.numeric(alpha) || length(alpha) != 1  || alpha <= 0 || alpha >= 1) {
        stop("\n\U0001F6D1 alpha must be between 0 and 1")
      }

    # input_ci_level
      if (!is.numeric(input_ci_level) || length(input_ci_level) != 1 ||
          input_ci_level <= 0 || input_ci_level >= 1) {
        stop("\n\U0001F6D1 input_ci_level must be a single numeric value between 0 and 1")
      } else {zscore <- qnorm(1 - (1 - input_ci_level)/2)}

    # h0_value
      if (!is.null(h0_value) && (!is.numeric(h0_value) || length(h0_value) != 1 )) {
        stop("\n\U0001F6D1 h0_value must be NULL or a single numeric value")
      }

    # use_futures
      if (use_futures) {
        if (!requireNamespace("future.apply", quietly = TRUE)) {
          stop("\n\U0001F6D1 future.apply package required for parallel processing")
        }

        if (!requireNamespace("future", quietly = TRUE)) {
          stop("\n\U0001F6D1 future package required for parallel processing")
        }

        # Check if futures are configured
        current_plan <- future::plan()

        if (inherits(current_plan, "sequential")) {
          message("\u26A0\ufe0f Future plan is 'sequential', so parallel processing is not configured.\n",
                  "For parallel processing, configure your futures with something like:\n",
                  "  future::plan(future::multisession, workers = future::availableCores() - 1)\n",
                  "  options(future.globals.maxSize = 2 * 1024^3)  # 2GB max per worker")
        }
      }

      if (!is.logical(use_futures) || length(use_futures) != 1) {
        stop("\n\U0001F6D1 use_futures must be a single logical value (TRUE or FALSE)")
      }

    # pvalue_method
      if (!pvalue_method %in% c("proportion", "ttest")) {
        stop("\n\U0001F6D1 pvalue_method must be 'proportion' or 'ttest'")
      }

    # convergence_check
      if (!is.logical(convergence_check) || length(convergence_check) != 1) {
        stop("\n\U0001F6D1 convergence_check must be a single logical value (TRUE or FALSE)")
      }

    # convergence_tolerance
      if (!is.numeric(convergence_tolerance) || length(convergence_tolerance) != 1 ||
          convergence_tolerance <= 0 || convergence_tolerance >= 1) {
        stop("\n\U0001F6D1 convergence_tolerance must be a single numeric value between 0 and 1")
      }

    # convergence_sample_size
      if (!is.numeric(convergence_sample_size) || length(convergence_sample_size) != 1 ||
          convergence_sample_size < 1 || convergence_sample_size != round(convergence_sample_size)) {
        stop("\n\U0001F6D1 convergence_sample_size must be a single positive integer")
      }

  # QA Checks ----
    # Giant dataset warning
      if (nrow(ph.estimates) > 1000) {
        warning("\n\u26A0\ufe0f Large dataset detected - consider using use_futures = TRUE for speed")
      }

      # Estimate memory usage: #_rows × draws × 8 bytes/double × 3 matrices (comp, ref, contrast)
      memory_mb <- (nrow(ph.estimates) * draws * 8 * 3) / (1024^2)
      memory_gb <- memory_mb / 1024
      if (memory_mb > 1000) {
        warning("\n\u26A0\ufe0f High memory usage expected: ", round(memory_gb, 2), " GB")
      }

    # Check if both the SE and CI are provided for comparator group
      if (!is.null(comp_se_col) && !is.null(comp_lower_col) && !is.null(comp_upper_col)) {
        has_both_comp <- ph.estimates[,
                                      !is.na(comp_se) & comp_se > 0 &
                                        (!is.na(comp_lower) & !is.na(comp_upper)),
                                      env = list(
                                        comp_se = comp_se_col,
                                        comp_lower = comp_lower_col,
                                        comp_upper = comp_upper_col
                                      )]

        if (any(has_both_comp)) {
          warning("\n\u26A0\ufe0f Comparator has both SE and CI; prioritizing SE. If CIs are from methods like Fay-Feuer \n",
                  "(for age standardized rates), consider using only CI inputs.")
        }
      }

    # Check if both the SE and CI and provided for reference group
      if (!is.null(ref_se_col) && (!is.null(ref_lower_col) || !is.null(ref_upper_col))) {
        has_both_ref <- ph.estimates[,
                                      !is.na(ref_se) & ref_se > 0 &
                                        (!is.na(ref_lower) & !is.na(ref_upper)),
                                      env = list(
                                        ref_se = ref_se_col,
                                        ref_lower = ref_lower_col,
                                        ref_upper = ref_upper_col
                                      )]

          if (any(has_both_ref)) {
            warning("\n\u26A0\ufe0f Reference has both SE and CI; prioritizing SE. If CIs are from methods like Fay-Feuer \n",
                    "(for age standardized rates), consider using only CI inputs.")
          }
        }

    # Check that values of comparator CI parameters are logical (if they are given)
      if (!is.null(comp_lower_col) && !is.null(comp_upper_col)) {
        invalid_ci_comp <- ph.estimates[,
                                        !is.na(comp_lower) & !is.na(comp_upper) &
                                          comp_lower >= comp_upper,
                                        env = list(
                                          comp_lower = comp_lower_col,
                                          comp_upper = comp_upper_col
                                        )]

        if (any(invalid_ci_comp)) {
          n_invalid <- sum(invalid_ci_comp)
          warning("\n\u26A0\ufe0f ", n_invalid, " comparator observations have lower CI >= upper CI")
        }

        if (dist == "lognormal") {
          negative_ci_comp <- ph.estimates[,
                                           !is.na(comp_lower) & !is.na(comp_upper) &
                                             (comp_lower <= 0 | comp_upper <= 0),
                                           env = list(
                                             comp_lower = comp_lower_col,
                                             comp_upper = comp_upper_col
                                           )]

          if (any(negative_ci_comp)) {
            n_negative_ci <- sum(negative_ci_comp)
            warning("\n\u26A0\ufe0f ", n_negative_ci, " comparator observations have non-positive CI bounds with lognormal assumption")
          }
        }
      }

    # Check that values of reference CI parameters are logical (if they are given)
      if (!is.null(ref_lower_col) && !is.null(ref_upper_col)) {
        invalid_ci_ref <- ph.estimates[,
                                       !is.na(ref_lower) & !is.na(ref_upper) &
                                         ref_lower >= ref_upper,
                                       env = list(
                                         ref_lower = ref_lower_col,
                                         ref_upper = ref_upper_col
                                       )]

        if (any(invalid_ci_ref)) {
          n_invalid <- sum(invalid_ci_ref)
          warning("\n\u26A0\ufe0f ", n_invalid, " reference observations have lower CI >= upper CI")
        }

        if (dist == "lognormal") {
          negative_ci_ref <- ph.estimates[,
                                          !is.na(ref_lower) & !is.na(ref_upper) &
                                            (ref_lower <= 0 | ref_upper <= 0),
                                          env = list(
                                            ref_lower = ref_lower_col,
                                            ref_upper = ref_upper_col
                                          )]

          if (any(negative_ci_ref)) {
            n_negative_ci <- sum(negative_ci_ref)
            warning("\n\u26A0\ufe0f ", n_negative_ci, " reference observations have non-positive CI bounds with lognormal assumption")
          }
        }
      }

    # Check for missing values in key columns
      n_missing_comp <- sum(is.na(ph.estimates[[comp_mean_col]]))
      n_missing_ref <- sum(is.na(ph.estimates[[ref_mean_col]]))
      if (n_missing_comp > 0 || n_missing_ref > 0) {
        warning("\n\u26A0\ufe0f ", n_missing_comp + n_missing_ref, " rows have missing point estimates")
      }

    # Distribution assumption checks
      if (dist == "lognormal") {
        n_negative_comp <- sum(ph.estimates[[comp_mean_col]] <= 0, na.rm = TRUE)
        n_negative_ref <- sum(ph.estimates[[ref_mean_col]] <= 0, na.rm = TRUE)
        if (n_negative_comp > 0 || n_negative_ref > 0) {
          warning("\n\u26A0\ufe0f ", n_negative_comp + n_negative_ref,
                  " observations have non-positive values with lognormal assumption")
        }
      }

  # get_draws() function ----
    get_draws <- function(mu, se, lower, upper, dist_type, se_scale_type) {

      # Priority: use SE if available and valid, if not it will use CI below
      if (!is.null(se) && !is.na(se) && se > 0) {
        if (dist_type == "normal") {
          return(rnorm(draws, mean = mu, sd = se))
        } else if (dist_type == "lognormal") {
          if (se_scale_type == "log") {
            # SE already on log scale
            return(rlnorm(draws, meanlog = log(mu), sdlog = se))
          } else {
            # SE on original scale - convert using delta method
            selog <- se / mu
            return(rlnorm(draws, meanlog = log(mu), sdlog = selog))
          }
        }
      }

      # Use CI if no valid SE
      else if (!is.null(lower) && !is.null(upper) &&
               !is.na(lower) && !is.na(upper) &&
               lower > 0 && upper > lower) {

        if (dist_type == "normal") {
          # Back-calculate SE from CI width
          est_se <- (upper - lower) / (2 * zscore)
          return(rnorm(draws, mean = mu, sd = est_se))
        } else if (dist_type == "lognormal") {
          # CI symmetric on log scale for lognormal
          est_selog <- (log(upper) - log(lower)) / (2 * zscore)
          return(rlnorm(draws, meanlog = log(mu), sdlog = est_selog))
        }
      }

      # If we get here, then there is a problem with our uncertainty data
      warning("\n\U000026A0 Row with point estimate = ", mu, " has insufficient uncertainty information\n",
              "Since the SE and CI data are not usable, the contrast calculation will use a constant value.\n",
              "In other words, no uncertainty propagated.")
      return(rep(mu, draws))
    }

  # process_single_row() function  ----
    process_single_row <- function(i) {
      # Extract estimates from the data.table
      comp_mu <- ph.estimates[[comp_mean_col]][i]
      ref_mu  <- ph.estimates[[ref_mean_col]][i]
      comp_se     <- if (!is.null(comp_se_col)) ph.estimates[[comp_se_col]][i] else NULL
      ref_se      <- if (!is.null(ref_se_col)) ph.estimates[[ref_se_col]][i] else NULL
      comp_lower  <- if (!is.null(comp_lower_col)) ph.estimates[[comp_lower_col]][i] else NULL
      comp_upper  <- if (!is.null(comp_upper_col)) ph.estimates[[comp_upper_col]][i] else NULL
      ref_lower   <- if (!is.null(ref_lower_col)) ph.estimates[[ref_lower_col]][i] else NULL
      ref_upper   <- if (!is.null(ref_upper_col)) ph.estimates[[ref_upper_col]][i] else NULL

      # Generate draws
      comp_draws_row <- get_draws(comp_mu, comp_se, comp_lower, comp_upper, dist, se_scale)
      ref_draws_row  <- get_draws(ref_mu, ref_se, ref_lower, ref_upper, dist, se_scale)

      return(list(comp = comp_draws_row, ref = ref_draws_row))
    }

  # Generate all draws ----
    message("\u25B6\ufe0f Generating Random Draws")
    message("\U023F3 Processing ", nrow(ph.estimates), " observations...be patient!")

    n <- nrow(ph.estimates)
    start_time <- Sys.time()

    all_draws <- lapply(seq_len(n), process_single_row)

    end_time <- Sys.time()
    message("Draw generation completed in ", round(as.numeric(end_time - start_time), 2),
            " seconds (", round(draws * n / 1000000, 1), "M total draws)")

  # Put draws into matrices for faster math / calculations ----
    comp_draws <- do.call(rbind, lapply(all_draws, `[[`, "comp")) # Combine comp draws into a matrix
    ref_draws <- do.call(rbind, lapply(all_draws, `[[`, "ref")) # Combine ref draws into a matrix

  # Apply contrast function ----
    message("\u25B6\ufe0f Applying Contrast Function")
    contrast_draws <- contrast_fn(comp_draws, ref_draws) # use the function provided by the user

    # Handle infinite values caused by division by zero (e.g., 10/0 = Inf)
    n_infinite <- sum(is.infinite(contrast_draws))
    if (n_infinite > 0) {
      warning("\n\u26A0\ufe0f ", n_infinite, " infinite values in calculated contrasts (likely division by zero) - setting to NA")
      contrast_draws[is.infinite(contrast_draws)] <- NA
    }

    # Check for other issues with contrast calculation
    n_na <- sum(is.na(contrast_draws))
    if (n_na > 0) {
      warning("\n\u26A0\ufe0f ", n_na, " missing values in calculated contrasts")
    }

  # Define null hypothesis value for consistent use ----
    null_hypothesis <- if (!is.null(h0_value)) {
      h0_value
    } else {
      contrast_fn(1, 1)  # NULL is defined by contrast when x == y
    }

  # Check convergence (optional / conditional) ----
    if (convergence_check) {

      message("\u25B6\ufe0f Convergence Diagnostics")

      conv_results <- check_batch_convergence(
        contrast_draws_matrix = contrast_draws, # this is the matrix of our calculations defined by the contrast_fn parameter
        alpha = alpha,
        sample_rows = convergence_sample_size,
        tolerance = convergence_tolerance,
        null_value = null_hypothesis
      )

      # Add convergence info to output
      setattr(ph.estimates, "convergence_check", conv_results)

      # Recommendations
      if (conv_results$prop_converged < 0.8) {
        recommended_draws <- ceiling(draws * 1.5)
        message("\U00002139 RECOMMENDATION: Consider increasing draws to ", recommended_draws, " or higher for better stability")
      }
    }

  # calc_pvalue function ----
    calc_pvalue <- function(draws, method, null_value) {
      if (all(is.na(draws)) || all(!is.finite(draws))) return(NA_real_)

      if (method == "proportion") {
        prop_above <- mean(draws > null_value, na.rm = TRUE)
        prop_below <- mean(draws < null_value, na.rm = TRUE)
        return(2 * min(prop_above, prop_below))

      } else if (method == "ttest") {
        return(t.test(draws, mu = null_value)$p.value)
      }

      return(NA_real_)
    }

  # Calculate summary statistics ----
    message("\u25B6\ufe0f Calculating Summary Statistics")

    message("\U0001F4CA Testing null hypothesis that contrast = ", null_hypothesis,
            " (set h0_value parameter to customize)")

    if (use_futures) {
      ph.estimates[, contrast := future.apply::future_apply(contrast_draws, MARGIN = 1, mean, na.rm = TRUE)]
      ph.estimates[, contrast_se := future.apply::future_apply(contrast_draws, MARGIN = 1, sd, na.rm = TRUE)]
      ph.estimates[, contrast_lower := future.apply::future_apply(contrast_draws, MARGIN = 1, quantile,
                                                                  probs = alpha/2, na.rm = TRUE)]
      ph.estimates[, contrast_upper := future.apply::future_apply(contrast_draws, MARGIN = 1, quantile,
                                                                  probs = 1 - alpha/2, na.rm = TRUE)]
      ph.estimates[, contrast_pvalue := future.apply::future_apply(contrast_draws, MARGIN = 1, calc_pvalue,
                                                                   method = pvalue_method, null_value = null_hypothesis)]
    } else {
      ph.estimates[, contrast := apply(contrast_draws, MARGIN = 1, mean, na.rm = TRUE)]
      ph.estimates[, contrast_se := apply(contrast_draws, MARGIN = 1, sd, na.rm = TRUE)]
      ph.estimates[, contrast_lower := apply(contrast_draws, MARGIN = 1, quantile, probs = alpha/2, na.rm = TRUE)]
      ph.estimates[, contrast_upper := apply(contrast_draws, MARGIN = 1, quantile, probs = 1 - alpha/2, na.rm = TRUE)]
      ph.estimates[, contrast_pvalue := apply(contrast_draws, MARGIN = 1, calc_pvalue, method = pvalue_method, null_value = null_hypothesis)]
    }

  # Results summary ----
    message("\u25B6\ufe0f Results Summary")

    n_significant <- sum(ph.estimates$contrast_pvalue < alpha, na.rm = TRUE)
    message(n_significant, " of ", nrow(ph.estimates), " contrasts significant at alpha = ", alpha,
            " (", round(100 * n_significant / nrow(ph.estimates), 1), "%)")

    # Distribution diagnostics
    if (any(is.infinite(ph.estimates$contrast) | is.infinite(ph.estimates$contrast_se), na.rm = TRUE)) {
      warning("\n\u26A0\ufe0f Some contrasts produced infinite values")
    }

    message("\U0001f642 Uncertainty propagation completed successfully!")

  # Add metadata ----
    setattr(ph.estimates, "propagate_uncertainty_params", list(
      dist = dist,
      se_scale = se_scale,
      draws = draws,
      alpha = alpha,
      h0_value = h0_value,
      pvalue_method = pvalue_method,
      contrast_fn_name = deparse(substitute(contrast_fn)),
      convergence_check = convergence_check
    ))

  # Return ph.estimates ----
    return(ph.estimates)
}


#' Check Monte Carlo convergence for a single contrast
#'
#' @description
#' Assesses whether Monte Carlo draws have stabilized by examining how
#' estimates change as more draws are added. Uses batch testing approach
#' to detect when relative changes fall below tolerance threshold.
#'
#' @param draws_vector Numeric vector of Monte Carlo draws to assess
#' @param alpha Numeric. Alpha level for confidence intervals
#' (default 0.05)
#' @param min_batch_size Integer. Minimum number of draws per batch for testing
#' (default 1000)
#' @param tolerance Numeric. Maximum acceptable relative change to consider
#' converged (default 0.05)
#' @param null_value Numeric. Null hypothesis value for p-value calculation.
#'
#' @return List containing:
#'   - converged: Logical indicating overall convergence
#'   - mean_converged: Logical for mean estimate convergence
#'   - ci_converged: Logical for confidence interval convergence
#'   - pval_converged: Logical for p-value convergence
#'   - max_rel_change_*: Maximum relative changes observed
#'   - convergence_table: Data.table with convergence diagnostics
#' @keywords internal
#'

check_convergence <- function(draws_vector,
                              alpha = 0.05,
                              min_batch_size = 1000,
                              tolerance = 0.05,
                              null_value = 0) {
  n_total <- length(draws_vector)
  if (n_total < min_batch_size * 3) {
    return(list(converged = TRUE, message = "Too few draws to assess convergence"))
  }

  # Test at different sample sizes
  test_sizes <- seq(min_batch_size, n_total, by = min_batch_size)
  if (test_sizes[length(test_sizes)] != n_total) {
    test_sizes <- c(test_sizes, n_total)
  }

  # Calculate stats at each sample size using data.table
  results <- data.table(
    n_draws = test_sizes,
    mean_est = numeric(length(test_sizes)),
    lower_ci = numeric(length(test_sizes)),
    upper_ci = numeric(length(test_sizes)),
    p_value = numeric(length(test_sizes))
  )

  for (i in seq_along(test_sizes)) {
    n <- test_sizes[i]
    subset_draws <- draws_vector[1:n]

    results[i, mean_est := mean(subset_draws, na.rm = TRUE)]
    results[i, lower_ci := quantile(subset_draws, probs = alpha/2, na.rm = TRUE)]
    results[i, upper_ci := quantile(subset_draws, probs = 1 - alpha/2, na.rm = TRUE)]

    # Calculate p-value
    prop_above <- mean(subset_draws > null_value, na.rm = TRUE)
    results[i, p_value := 2 * min(prop_above, 1 - prop_above)]
  }

  # Check convergence by looking at stability in the last few estimates
  n_check <- min(3, nrow(results) - 1)
  if (n_check < 2) {
    return(list(converged = TRUE, message = "Not enough test points"))
  }

  final_rows <- results[(nrow(results) - n_check):nrow(results)]

  # Calculate relative changes in key statistics using data.table
  final_rows[, rel_change_mean := c(NA, abs(diff(mean_est)) / abs(mean_est[-1]))]
  final_rows[, rel_change_lower := c(NA, abs(diff(lower_ci)) / abs(lower_ci[-1]))]
  final_rows[, rel_change_upper := c(NA, abs(diff(upper_ci)) / abs(upper_ci[-1]))]
  final_rows[, rel_change_pval := c(NA, abs(diff(p_value)) / pmax(p_value[-1], 0.001))]

  # Remove NA values for convergence checking
  rel_changes <- final_rows[!is.na(rel_change_mean)]

  # Check if all changes are within tolerance
  mean_converged <- all(rel_changes$rel_change_mean < tolerance, na.rm = TRUE)
  ci_converged <- all(rel_changes$rel_change_lower < tolerance &
                        rel_changes$rel_change_upper < tolerance, na.rm = TRUE)
  pval_converged <- all(rel_changes$rel_change_pval < tolerance * 2, na.rm = TRUE)

  overall_converged <- mean_converged && ci_converged && pval_converged

  return(list(
    converged = overall_converged,
    mean_converged = mean_converged,
    ci_converged = ci_converged,
    pval_converged = pval_converged,
    max_rel_change_mean = max(rel_changes$rel_change_mean, na.rm = TRUE),
    max_rel_change_ci = max(c(rel_changes$rel_change_lower, rel_changes$rel_change_upper), na.rm = TRUE),
    max_rel_change_pval = max(rel_changes$rel_change_pval, na.rm = TRUE),
    convergence_table = results
  ))
}

#' Helper function to check convergence across multiple contrasts
#'
#' @description
#' Checks convergence across a sample of contrast draws to assess overall
#' Monte Carlo stability
#'
#' @param contrast_draws_matrix Matrix. Matrix of contrast draws (rows = observations,
#' cols = draws)
#' @param alpha Numeric. Alpha level for confidence intervals (default 0.05)
#' @param sample_rows Integer. Number of rows to sample for convergence checking
#' (default 10)
#' @param tolerance Numeric. Maximum acceptable relative change to consider
#' converged (default 0.05)
#' @param null_value Numeric. Null hypothesis value for p-value calculation.
#'
#' @return List with batch convergence results containing:
#'   - prop_converged: Numeric indicating the proportion of contrasts that converged
#'   - n_converged: Integer indicating the number of contrasts that converged
#'   - n_checked: Integer indicating the number of convergence results checked
#'   - individual_results: List of all the convergence results checked
#' @keywords internal
#'
check_batch_convergence <- function(contrast_draws_matrix,
                                    alpha = 0.05,
                                    sample_rows = 10,
                                    tolerance = 0.05,
                                    null_value = 0) {

  n_rows <- nrow(contrast_draws_matrix)
  check_indices <- if (n_rows <= sample_rows) {
    seq_len(n_rows)
  } else {
    sort(sample(n_rows, sample_rows))
  }

  convergence_results <- list()
  n_converged <- 0

  for (i in seq_along(check_indices)) {
    row_idx <- check_indices[i]
    draws <- contrast_draws_matrix[row_idx, ]

    # Skip rows with too many missing values
    if (sum(is.finite(draws)) < 1000) next

    conv_check <- check_convergence(draws[is.finite(draws)],
                                    alpha = alpha,
                                    tolerance = tolerance,
                                    null_value = null_value)
    convergence_results[[i]] <- conv_check

    if (conv_check$converged) n_converged <- n_converged + 1
  }

  prop_converged <- n_converged / length(convergence_results)

  message("Batch convergence check (", length(check_indices), " rows sampled):")
  message("  ", n_converged, " of ", length(convergence_results),
          " contrasts converged (", round(prop_converged * 100, 1), "%)")

  if (prop_converged < 0.8) {
    warning("\n\u26A0\ufe0f Low convergence rate suggests more draws needed")

    # Find the worst case and report details
    if (length(convergence_results) > 0) {
      # Extract converged status, handling missing elements
      converged_status <- sapply(convergence_results, function(x) {
        if (is.null(x) || is.null(x$converged)) return(TRUE)  # Default to "converged" for missing
        return(x$converged)
      })

      # Find worst case among those that didn't converge
      non_converged <- which(!converged_status)
      if (length(non_converged) > 0) {
        # Among non-converged, find the one with highest max change
        max_changes <- sapply(non_converged, function(idx) {
          result <- convergence_results[[idx]]
          if (is.null(result)) return(0)
          max(result$max_rel_change_mean, result$max_rel_change_ci, result$max_rel_change_pval, na.rm = TRUE)
        })
        worst_case_idx <- non_converged[which.max(max_changes)]
        worst_result <- convergence_results[[worst_case_idx]]

        if (!is.null(worst_result)) {
          message("  Worst case max change: mean = ", round(worst_result$max_rel_change_mean, 4),
                  ", CI = ", round(worst_result$max_rel_change_ci, 4),
                  ", p-val = ", round(worst_result$max_rel_change_pval, 4))
        }
      }
    }
  }

  return(list(
    prop_converged = prop_converged,
    n_converged = n_converged,
    n_checked = length(convergence_results),
    individual_results = convergence_results
  ))
}
