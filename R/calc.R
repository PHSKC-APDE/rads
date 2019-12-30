#' Compute metrics from record (e.g. vital stats) or survey data
#'
#' @param ph.data data.table or tbl_svy. Dataset.
#' @param what character vector. Variable to calculate metrics for.
#' @param ... expressions to be passed to \code{\link{filter}} (or equivalent)
#' @param by character vector. Must refer to variables within ph.data. The variables within ph.data to compute `what` by
#' @param metrics character. See \code{\link{record_metrics}} for the available options. Note, except when 'distinct' is
#' selected, all metrics are calculated -- this argument just specifies which one gets returned
#' @param per integer. The denominator when "rate" or "adjusted-rate" are selected as the metric. Metrics will be multiplied by this value.
#' @param win integer. The number of consectutive units of time (e.g., years, months, etc.) over which the metrics will be calculated,
#' i.e., the 'window' for a rolling average, sum, etc.
#' @param time_var character. The name of the time variable in the dataset. Typically this is the year variable, i.e., "chi_year"
#' @param proportion logical. For survey data, should metrics be calculated assuming the output is proportion-like? See details for more.
#'                   Currently does not have functionality for non-survey data.
#'
#' @return a data.table containing the results
#' @details
#' This function calculates `metrics` for each variable in `what` from rows meeting the conditions specified
#' by `...` (i.e., where), for each grouping implied by `by`.
#'
#' For survey data, use the \code{proportion} argument where relevant to ensure metrics are calculated using special proportion (e.g \code{svyciprop})
#' methods. That is, when you want to find the fraction of ____, toggle \code{proportion} to \code{TRUE}.
#'
#' @importFrom rlang quos
#' @import data.table
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
#'                       metrics = c("mean", "numerator", "denominator", "total", "lower", "upper", "se"))
#'
calc <- function(ph.data, ...) {
  UseMethod("calc")
}
