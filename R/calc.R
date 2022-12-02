#' Compute metrics from record (e.g. vital stats) or survey data
#' @name calc
#' @param ph.data data.table or tbl_svy. Dataset.
#' @param what character vector. Variable to calculate metrics for.
#' @param where subsetting expression
#' @param by character vector. Must refer to variables within ph.data. The variables within ph.data to compute `what` by
#' @param metrics character. See \code{\link{metrics}} or scroll below for the available options.
#' @param per integer. The denominator when "rate" or "adjusted-rate" are selected as the metric. Metrics will be multiplied by this value.
#' @param win integer. The number of consecutive units of time (e.g., years, months, etc.) over which the metrics will be calculated,
#' i.e., the 'window' for a rolling average, sum, etc.
#' @param time_var character. The name of the time variable in the dataset. Used in combination with the "win" argument to do time windowed calculations.
#' @param fancy_time logical. If TRUE, a record of all the years going into the data is provided.
#' If FALSE, just a simple range (where certain years within the range might not be represented in your data).
#' @param proportion logical. For survey data, should metrics be calculated assuming the output is proportion-like? See details for more.
#'                   Currently does not have functionality for non-survey data.
#' @param ci numeric. Confidence level, >0 & <1, typically 0.95
#' @param verbose logical. Mostly unused, but toggles on/off printed warnings.
#' @param ... not implemented
#' @references \url{https://github.com/PHSKC-APDE/rads/wiki/calc}
#' @return a data.table containing the results
#' @details
#' This function calculates `metrics` for each variable in `what` from rows meeting the conditions specified
#' by `where` for each grouping implied by `by`.
#'
#' Available metrics include:
#'
#' 1) total: Count of people with the given value. Mostly relevant for surveys
#' (where total is approximately mean * sum(pweights)).
#' Returns total, total_se, total_upper, total_lower.
#' total_se, total_upper, & total_lower are only valid for survey data.
#' Default ci (e.g. upper and lower) is 95 percent.
#'
#' 2) mean: Average response and associated metrics of uncertainty.
#' Returns mean, mean_se, mean_lower, mean_upper.
#' Default ci (e.g. upper and lower) is 95 percent.
#'
#' 3) rse: Relative standard error. 100*se/mean.
#'
#' 4) numerator: Sum of non-NA values for `what``.
#' The numerator is always unweighted.
#'
#' 5) denominator: Number of rows where `what` is not NA.
#' The denominator is always unweighted.
#'
#' 6) obs: Number of unique observations (i.e., rows), agnostic as to whether
#' there is missing data for `what`. The obs is always unweighted.
#'
#' 7) median: The median non NA response. Not populated when `what` is a factor
#' or character. Even for surveys, the median is the unweighted result.
#'
#' 8) unique.time: Number of unique time points (from `time_var`) included in
#' each tabulation (i.e., number of unique time points when the `what` is not missing).
#'
#' 9) missing: Number of rows in a given grouping with an NA value for `what`.
#'    missing + denominator = Number of people in a given group.
#'    When `what` is a factor/character, the missing information is provided for the other.
#'
#' 10) missing.prop: The proportion of the data that has an NA value for `what`.
#'
#' 11) rate: mean * per. Provides rescaled mean estimates (i.e., per 100 or per 100,0000).
#' Returns rate, rate_se, rate_lower, rate_upper.
#' Default ci (e.g. upper and lower) is 95 percent.
#'
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
#'                      chi_year == 2016 & chi_sex %in% c('Male', 'Female'),
#'                       by = c("chi_year", "chi_sex"),
#'                       metrics = c("mean", "numerator", "denominator",
#'                                   "total"))
#'
calc <- function(ph.data, ...) {
  UseMethod("calc")
}

#' @noRd
#' @export
calc.data.frame <- function(ph.data, ...){

  # Catch data.frame, and send it as a dtadmin
  ph.data = dtsurvey::dtadmin(ph.data)
  calc(ph.data, ...)

  # stop('calc no longer accepts raw data.frames/data.tables/tbl_dfs as an option. Please convert ph.data to an appropriate object type instead.
  #      Use `ph.data <- dtsurvey::dtadmin(ph.data)` for non-survey data.')
}

#' @noRd
#' @export
calc.survey.design2 <- function(ph.data, ...){

  ph.data = dtsurvey::as.dtsurvey(ph.data)
  calc(ph.data, ...)

  # stop('calc no longer accepts tbl_svys or survey.design objects as an option. Please convert ph.data to an appropriate object type instead.
  #      Review the documentation for dtsurvey::dtsurvey to properly convert/encode survey data for use with `calc`.')
}

#' @noRd
#' @export
calc.svyrep.design <- function(ph.data, ...){
  ph.data = dtsurvey::dtrepsurvey(ph.data)
  calc(ph.data, ...)
}

#' @noRd
#' @export
calc.grouped_df <- function(ph.data, ...){
  stop("calc doesn't know how to handle `grouped_df` objects. Likely, you have a dplyr::group_by somewhere higher up in the code.
       Instead of grouping before running calc, use the `by` argument in calc")
}
