#' CHI Generate Instructions
#'
#' @description
#' Applies CHI Generate Template across multiple rows of an instruction file
#'
#' @details
#' This should be better unpacked
#'
#' @param ph.analysis_set
#' @param end.year
#' @param year.span
#' @param trend.span
#'
#' @return
#' @export
#'
#' @examples
chi_generate_instructions <- function(ph.analysis_set = NULL,
                                      end.year = 2021,
                                      year.span = 5,
                                      trend.span = 3){
  # apply the template generating function
  template <- rbindlist(
    lapply(X = seq(1, length(unique(ph.analysis_set$set))),
           FUN = chi_generate_template, ph.analysis_set = ph.analysis_set))

  # split trends from other tabs because processed for multiple years
  template.trends <- template[tab=='trends']
  template <- template[tab != 'trends']

  # add years to template (non-trend)
  template[, end := end.year]
  template[, start := end.year - (year.span - 1)]
  template <- rbind(template,
                    template[tab == '_kingcounty'][, tab := 'metadata'][, start := end.year])

  # add years to template (trends)
  trend.years <- CHI_generate_trend_years(indicator_key = unique(template$indicator_key),
                                          span = trend.span,
                                          begin.year = end.year - 9,
                                          final.year = end.year)
  template.trends <- merge(template.trends, trend.years, by = 'indicator_key', all = T, allow.cartesian = T)

  # append trends template to main template
  template <- rbind(template, template.trends)

  return(template)
}
