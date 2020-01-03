#' Compute metrics from record (e.g. vital stats) or survey data
#' @name calc
#' @param ph.data data.table or tbl_svy. Dataset.
#' @param what character vector. Variable to calculate metrics for.
#' @param ... expressions to be passed to \code{\link{filter}} (or equivalent)
#' @param by character vector. Must refer to variables within ph.data. The variables within ph.data to compute `what` by
#' @param metrics character. See \code{\link{record_metrics}} for the available options. Note, except when 'distinct' is
#' selected, all metrics are calculated -- this argument just specifies which one gets returned
#' @param per integer. The denominator when "rate" or "adjusted-rate" are selected as the metric. Metrics will be multiplied by this value.
#' @param win integer. The number of consectutive units of time (e.g., years, months, etc.) over which the metrics will be calculated,
#' i.e., the 'window' for a rolling average, sum, etc.
#' @param time_var character. The name of the time variable in the dataset. Used in combination with the "win" argument to do time windowed calculations.
#' @param proportion logical. For survey data, should metrics be calculated assuming the output is proportion-like? See details for more.
#'                   Currently does not have functionality for non-survey data.
#' @param verbose logical. Mostly unused, but toggles on/off printed warnings.
#' @return a data.table containing the results
#' @details
#' This function calculates `metrics` for each variable in `what` from rows meeting the conditions specified
#' by `...` (i.e., where), for each grouping implied by `by`.
#'
#' Available metrics include:
#' 1) mean: Average response
#' 2) se: standard error
#' 3) lower: lower part of a normal 95% ci (e.g. 2.5th percentile draw)
#' 4) upper: upper part of a normal 95% ci (e.g. 97.5th percentile draw)
#' 5) numerator: sum of non-na values for `what``
#' 6) denominator: number of rows where `what` is not NA
#' 7) total: roughly the sum of weight * `what` with NAs removed. Weights are implicitly 1 for record data and therefore total will equal numerator
#' 8) total_se: For surveys. the standard error of the survey total estimate
#' 9) missing: Number of rows in a given grouping with an NA value for `what`. missing + denominator = Number of people in a given group
#' 10) res: relative standard error. mean/se
#' 11) missing.prop: missing/(missing + denominator)
#' 12) ndistinct: number of non-na distinct values `what` takes on in a given grouping (after filtering and all that jazz)
#'
#' For survey data, use the \code{proportion} argument where relevant to ensure metrics are calculated using special proportion (e.g \code{svyciprop})
#' methods. That is, when you want to find the fraction of ____, toggle \code{proportion} to \code{TRUE}.
#'
#' @export
#'
#' @examples
#'
#' #record data
#' test.data <- get_data_birth(year = 2015:2017)
#'
#' test.results <- calc(test.data,
#'                      what = c("kotelchuck", "fetal_pres"),
#'                      "chi_year == 2016 & chi_sex %in% c('Male', 'Female')",
#'                       by = c("chi_year", "chi_sex"),
#'                       metrics = c("mean", "numerator", "denominator",
#'                                   "total", "lower", "upper", "se"))
#'
calc <- function(ph.data, ...) {
  UseMethod("calc")
}
