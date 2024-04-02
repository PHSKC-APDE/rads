#' CHI generate TRO shell
#'
#' @description
#' This function takes an analysis set file and indicator of which set should be processed. It returns a skeleton of CHI Tableau Ready Output.
#'
#' @details
#' It takes in a data.table containing a compact list of variables, byvariables, and analysis types, and returns a shell table of the rows and columns expected in a CHI Tableau ready output. For details on TRO format, review here: https://kc1.sharepoint.com/:x:/r/teams/DPH-CommunityHealthIndicators/CHIVizes/CHI-Standards-TableauReady%20Output.xlsx?d=wbed2f507b8344d288658c5724f64c001&csf=1&web=1&e=qEIPcc&nav=MTVfezAwMDAwMDAwLTAwMDEtMDAwMC0wMjAwLTAwMDAwMDAwMDAwMH0
#'
#' the expected format of the analysis file is:
#' set: numeric integer 1...x, indicates set the observations are calcualted as part of (why are sets valueable? should this be discarded?)
#' cat1: character, the name expected in CHI TRO for cat1
#' cat1_varname: character, the name expected in CHI TRO for cat1_varname
#' kingcounty: character "":"X", indicator of if analysis is king county specific (could be removed, this is imputable by variable name)
#' wastate: character "":"x", indicator of if analysis is of wa state
#' demgroups: character "":"x", indicator of if analysis includes single demographic
#' crosstabs: character "":"x", indicator of if analysis includes crosstabulations
#' trands: character "":"x", indicator of if analysis includes trends
#' set_idictaor_keys character comma sep list, list of indicators variables expected from data source
#'
#' @param ph.analysis_set name of data.table to parse
#' @param start.year the earliest year to be used for estimates
#' @param end.year the latest year to be used for aggregate estimates (note, the earliest year for trends estimates is calculated from from the span and number of periods)
#' @param trend.span the number of years to be included in a single trend period
#' @param trend.periods the number of periods to be included in a trend
#' @returns data table with a single row for each calculation to be performed in generating Tableau Ready Output for CHI reporting
#' @keywords CHI, Tableau, Production
#' @import dtsurvey
#' @import future
#' @import future.apply
#' @export
#'
chi_generate_tro_shell <- function(ph.analysis_set,
                                      start.year,
                                      end.year,
                                      trend.span = NULL,
                                      trend.periods = NULL){
  #parameterization checks
  if("x" %in% ph.analysis_set$trends & (is.null(trend.span) | is.null(trend.periods))) {stop("you have indicated that a trends analysis is to be conducted, but have not indicated both the span and number of periods for this analysis.")}

  #ph.analysis_set checks


  #advisory messages
  if("x" %in% ph.analysis_set$trends) {message("Note: trends are applied backwards from end.year")}


  # apply the template generating function
  template <- rbindlist(
    lapply(X = seq(1, length(unique(ph.analysis_set$set))),
           FUN = chi_process_nontrends, ph.analysis_set = ph.analysis_set))

  # split trends from other tabs because processed for multiple years
  template.trends <- template[tab=='trends']
  template <- template[tab != 'trends']

  # add years to template (non-trend)
  template[, end := end.year]
  template[, start := end.year - (year.span - 1)]
  template <- rbind(template,
                    template[tab == '_kingcounty'][, tab := 'metadata'][, start := end.year])

  # add years to template (trends)
  trend.years <- chi_process_trends(indicator_key = unique(template$indicator_key),
                                          trend.span = trend.span,
                                          end.year = end.year,
                                          trend.periods = trend.periods)
  template.trends <- merge(template.trends, trend.years, by = 'indicator_key', all = T, allow.cartesian = T)

  # append trends template to main template
  template <- rbind(template, template.trends)

  return(template)
}


