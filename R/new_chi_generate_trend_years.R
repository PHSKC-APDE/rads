#' CHI Generate Trend Years
#'
#' @param indicator_key
#' @param span
#' @param end.year
#' @param trend.periods
#'
#' @description
#' !!! What does this do !!!
#'
#' @details
#' uuuuu
#'
#' @returns figure this out
#' @keywords CHI, Tableau, Production
#' @import dtsurvey

chi_generate_trend_years <- function(indicator_key = NULL,
                                     trend.span = NULL,
                                     end.year = NULL,
                                     trend.periods = NULL){
  last.start <- end.year-(span-1)
  all.start.years <- last.start:(last.start-(trend.periods-1))
  all.end.years <- end.year:(end.year-(trend.periods-1))
  spandt <- data.table(end = all.end.years, start = all.start.years)
  spandt <- setorder(setDT(tidyr::crossing(data.table(indicator_key), spandt)), indicator_key, -end)
  return(spandt)
}
