#' CHI Generate Trend Years
#'
#' @param indicator_key
#' @param span
#' @param begin.year
#' @param final.year
#'
#' @description
#' !!! What does this do !!!
#'
#' @details
#' uuuuu
#'
#' @returns figure this out
#' @keywords CHI, Tableau, Production
#'
#' @export
#'
#' @import dtsurvey

chi_generate_trend_years <- function(indicator_key = NULL,
                                     span = NULL,
                                     begin.year = NULL,
                                     final.year = NULL){
  last.possible.begin.year <- final.year - (span-1)
  all.start.years <- begin.year:last.possible.begin.year
  all.end.years <- all.start.years + (span-1)
  spandt <- data.table(end = all.end.years, start = all.start.years)
  spandt <- setorder(setDT(tidyr::crossing(data.table(indicator_key), spandt)), indicator_key, -end)
  return(spandt)
}
