#' Compute metrics from record (e.g. vital stats) or survey data
#' @name calc
#' @param ph.data data.table or tbl_svy. Dataset.
#' @param what character vector. Variable to calculate metrics for.
#' @param where subsetting expression
#' @param by character vector. Must refer to variables within ph.data. The variables within ph.data to compute `what` by
#' @param metrics character. See \code{\link{metrics}} for the available options. Note, except when 'distinct' is
#' selected, all metrics are calculated -- this argument just specifies which one gets returned
#' @param per integer. The denominator when "rate" or "adjusted-rate" are selected as the metric. Metrics will be multiplied by this value.
#' @param win integer. The number of consectutive units of time (e.g., years, months, etc.) over which the metrics will be calculated,
#' i.e., the 'window' for a rolling average, sum, etc.
#' @param time_var character. The name of the time variable in the dataset. Used in combination with the "win" argument to do time windowed calculations.
#' @param fancy_time logical. If TRUE, a record of all the years going into the data is provided. If FALSE, just a simple range (where certain years within the range might not be represented)
#' @param proportion logical. For survey data, should metrics be calculated assuming the output is proportion-like? See details for more.
#'                   Currently does not have functionality for non-survey data.
#' @param ci numeric. Confidence level, >0 & <1, typically 0.95
#' @param verbose logical. Mostly unused, but toggles on/off printed warnings.
#' @return a data.table containing the results
#' @details
#' This function calculates `metrics` for each variable in `what` from rows meeting the conditions specified
#' by `where` for each grouping implied by `by`.
#'
#' Available metrics include:
#' 1) mean, se, upper, lower: Average response and associated metrics of uncertainty. Underlying ci (e.g. upper and lower) is 95%
#' 2) rate, rate_se, rate_upper, rate_lower: mean \* per and associated metrics of uncertainty \* per. Underlying ci (e.g. upper and lower) is 95%
#' 3) total, total_se, total_upper, total_lower: Count of people with the given value. Mostly relevent for surveys (where total is approximately mean * sum(pweights)). SE/Upper/Lower only valid for survey data
#' 4) numerator: sum of non-na values for `what``
#' 5) denominator: number of rows where `what` is not NA
#' 6) missing, missing.prop: Number of rows in a given grouping with an NA value for `what`. missing + denominator = Number of people in a given group.
#'    When `what` is a factor/character, the missing information is provided for the other
#' 7) rse: relative standard error. mean/se
#' 8) ndistinct: number of non-na distinct values `what` takes on in a given grouping (after filtering and all that jazz)
#' 9) obs: number of unique observations
#' 10) median: the median non NA response. Not populated when `what` is a factor or character. Even for surveys, the median is the unweighted result.
#' 11) unique.time: Number of unique time points (from `time_var`) included in each tabulation
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
