#' CHI Generate Trend Years
#'
#' @param indicator_key
#' @param span
#' @param end.year
#' @param trend.periods
#'
#' @description
#' helper fucntion for chi_generate_tro_shell
#'
#' @details
#' called by chi_generate_tro_shell to calculate and create rows for expected trends analyses.
#'
#'
#' @returns TRO with rows for each indicator key and span of years within the provided time frame
#' @keywords CHI, Tableau, Production
#' @import dtsurvey

chi_process_trends <- function(indicator_key = NULL,
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
