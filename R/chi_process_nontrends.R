#' CHI generate_nontrend_years
#'
#' @description
#' This function takes an analysis set file and indicator of which set should be processed. It returns a skeleton of CHI Tableau Ready Output. Hidden and ment to be called by CHI generate instructions. Not exported, but called by chi_generate_tro_shell().
#'
#' @details
#' It takes in a data.table containing a compact list of variables, byvariables, and analysis types, and returns a shell table of the rows and columns expected in a CHI Tableau ready output. For details on TRO format, review here: https://kc1.sharepoint.com/:x:/r/teams/DPH-CommunityHealthIndicators/CHIVizes/CHI-Standards-TableauReady%20Output.xlsx?d=wbed2f507b8344d288658c5724f64c001&csf=1&web=1&e=qEIPcc&nav=MTVfezAwMDAwMDAwLTAwMDEtMDAwMC0wMjAwLTAwMDAwMDAwMDAwMH0
#'
#' the expected format of the analysis file is:
#' set: numeric integer 1...x, indicates set the observations are calcualted as part of (why are sets valueable? should this be discarded?)
#' cat1: character, the name expected in CHI TRO for cat1
#' cat1_varname: character, the name expected in CHI TRO for cat1_varname
#' _kingcounty: character "":"X", indicator of if analysis is king county specific (could be removed, this is imputable by variable name)
#' _wastate: character "":"x", indicator of if analysis is of wa state
#' demgroups: character "":"x", indicator of if analysis includes single demographic
#' crosstabs: character "":"x", indicator of if analysis includes crosstabulations
#' trands: character "":"x", indicator of if analysis includes trends
#' set_idictaor_keys character comma sep list, list of indicators variables expected from data source
#'
#' @param ph.analysis_set name of data.table to parse
#' @param myset chosen set number from table
#' @returns data table with a single row for each calculation to be performed in generating Tableau Ready Output for CHI reporting
#' @keywords CHI, Tableau, Production
#' @import dtsurvey
#' @import future
#' @import future.apply
chi_process_nontrends <- function(ph.analysis_set = NULL,
                                  myset = NULL){

  subsets <- ph.analysis_set[set == myset] # process a single analysis set
  sub_indicators <- unlist(strsplit(unique(subsets$set_indicator_keys), ",")) # create vector of distinct indicator keys
  subtabs = setdiff(names(subsets), c('set', 'set_indicator_keys', 'cat1', 'cat1_varname', 'crosstabs')) # identify the tabs of interest

  # create table for all tabs except crosstabs
  tempy <- rbindlist(lapply(as.list(seq(1, length(subtabs))),
                            FUN = function(subtab){
                              tempx <- subsets[get(subtabs[subtab]) == 'x',
                                               .(tab = subtabs[subtab], cat1, cat1_varname, cat2 = NA_character_, cat2_varname = NA_character_)]
                              tempx <- setDT(tidyr::crossing(tempx, data.table(indicator_key = sub_indicators)))

                            }))

  # crosstabs are a bit more complicated
  sub_crosstabs = setDT(tidyr::crossing(
    unique(subsets[crosstabs == 'x', .(cat1, cat1_varname)]),
    unique(subsets[crosstabs == 'x', .(cat2 = cat1, cat2_varname = cat1_varname)]) ))
  sub_crosstabs <- sub_crosstabs[cat1 == 'King County' | cat1_varname != cat2_varname]
  sub_crosstabs <- sub_crosstabs[!(cat1_varname == 'race3' & cat2_varname %in% c('race3', 'race4'))] # do not want race x race
  sub_crosstabs <- sub_crosstabs[!(cat2_varname == 'race3' & cat1_varname %in% c('race3', 'race4'))] # do not want race x race
  sub_crosstabs <- sub_crosstabs[!(grepl('aic', cat1_varname) & grepl('race3|race4', cat2_varname))] # do not want race_aic x race
  sub_crosstabs <- sub_crosstabs[!(grepl('aic', cat2_varname) & grepl('race3|race4', cat1_varname))] # do not want race_aic x race
  sub_crosstabs <- sub_crosstabs[!(grepl('_aic_', cat1_varname) & grepl('_aic_', cat2_varname))] # do not want race_aic x race_aic

  sub_crosstabs[, tab := 'crosstabs']
  sub_crosstabs <- setDT(tidyr::crossing(sub_crosstabs, data.table(indicator_key = sub_indicators)))


  # append crosstabs
  tempy <- rbind(tempy, sub_crosstabs)

  # tidy
  tempy[cat1 %in% c('Ethnicity', "Birthing person's ethnicity") & cat1_varname == 'race3', cat1_varname := 'race3_hispanic']
  tempy[cat2 %in% c('Ethnicity', "Birthing person's ethnicity") & cat2_varname == 'race3', cat2_varname := 'race3_hispanic']
  setcolorder(tempy, 'indicator_key')
  rads::sql_clean(tempy)
  tempy <- tempy[!(tab == 'crosstabs' & cat1 == 'King County' & cat2 != 'King County')] # only legit xtab for KC is KC by itself
  tempy[tab == 'crosstabs' & cat2 == 'King County', `:=` (cat2 = 'Overall', cat2_varname = 'overall')]

  # return object
  return(tempy)
}

