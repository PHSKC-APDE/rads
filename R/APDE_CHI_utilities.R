#' CHI Generate Template
#'
#' @description
#' This function function generates one or more Tableau ready CHI analytic outputs.
#'
#' @details
#' It takes in a compacy list of variables, byvariables, and analysis types, and returns a table with one row corrosponding to each row in the Tableau ready output. This should largely corrospond with
#' "//phshare01/epe_share/WORK/CHI Visualizations/Tableau Ready Output Format_v2.xlsx"
#'
#' @param ph.analysis_set
#' @param myset
#' @returns data table with a single row for each calculation to be performed in generating Tableau Ready Output for CHI reporting
#' @keywords CHI, Tableau, Production
#'
#' @export
#'
#' @import dtsurvey
#' @import future
#' @import future.apply
#'
CHI_generate_template <- function(ph.analysis_set = NULL,
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



CHI_generate_trend_years <- function(indicator_key = NULL,
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


CHI_generate_instructions <- function(ph.analysis_set = NULL,
                                      end.year = 2021,
                                      year.span = 5,
                                      trend.span = 3){
  # apply the template generating function
  template <- rbindlist(
    lapply(X = seq(1, length(unique(ph.analysis_set$set))),
           FUN = CHI_generate_template, ph.analysis_set = ph.analysis_set))

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

CHI_calc <- function(ph.data = NULL,
                     ph.instructions = NULL,
                     rate = F,
                     rate_per = NULL,
                     small_num_suppress = T,
                     suppress_low = 0,
                     suppress_high = 9,
                     source_name = 'blahblah',
                     source_date = NULL){

  # Error if ph.instructions has no data ----
  if(nrow(ph.instructions) == 0){
    stop("\n\U0001f47f the table ph.instructions does not have any rows.")
    #tempCHIest <- data.table(setNames(data.frame(matrix(ncol = length(chi_cols()), nrow = 0), stringsAsFactors = FALSE), chi_cols()))
  }
  # Create 'Overall' if needed for crosstabs ----
  if(!'overall' %in% names(ph.data)){
    ph.data$overall <- with(ph.data, ifelse(chi_geo_kc == 'King County', 'Overall', NA_character_))
  }

  # Check to make sure all variables needed exist in the data ----
  neededbyvars <- setdiff(unique(c(ph.instructions$cat1_varname, ph.instructions$cat2_varname)), c(NA))
  if("race3" %in% neededbyvars & !"race3_hispanic" %in% neededbyvars){neededbyvars <- c(neededbyvars, 'race3_hispanic')} # By definition, Hispanic cannot be contained within race3

  neededvars <- setdiff(unique(c(ph.instructions$indicator_key, neededbyvars)), c(NA))

  missingvars <- setdiff(neededvars, names(ph.data))
  if(length(missingvars) > 0 ){
    stop(paste0("\n\U2620 ph.data is missing the following columns that are specified in ph.instructions: ", paste0(missingvars, collapse = ', '), ". ",
                "\nIf `race3_hispanic` is listed, that is because, by definition, `race3` cannot have a Hispanic ethnicity in the same variable. So, two ",
                "\nvariables (`race3` & `race3_hispanic`) will be processed and in the output, it will be called `race3`"))
  } else{message("\U0001f642 All specified variables exist in ph.data")}

  # Check to make sure all byvariables have the CHI specified encoding ----
  stdbyvars <- rads.data::misc_chi_byvars[varname %in% setdiff(unique(c(ph.instructions$cat1_varname, ph.instructions$cat2_varname)), c(NA))][, list(varname, group, keepme, reference = 1)]
  stdbyvars[group %in% c("Hispanic", 'Non-Hispanic') & varname == 'race3', varname := 'race3_hispanic'] # necessary because race3 & Hispanic must be two distinct variables in raw data
  phbyvars <- rbindlist(lapply(
    X=as.list(neededbyvars),
    FUN = function(X){data.table(varname = X, group = setdiff(unique(ph.data[[X]]), NA), ph.data = 1)}))
  compbyvars <- merge(stdbyvars, phbyvars, by = c('varname', 'group'), all = T)
  if(nrow(compbyvars[is.na(reference)| is.na(ph.data)]) > 0){
    print(compbyvars[is.na(reference)| is.na(ph.data)])
    stop("\n\U2620 the table above shows the varname/group combinations that do not align between the reference table and your ph.data.")
  } else {message("\U0001f642 All specified cat1_group and cat2_group values align with the reference standard.")}

  # Use Daniel's calc function to generate estimates for each row of ph.instructions ----
  message("\U023F3 Be patient! The function is generating estimates for each row of ph.instructions.")

  tempCHIest <- rbindlist(future_lapply(
    X = as.list(seq(1, nrow(ph.instructions), 1)),
    FUN = function(X){
      message(paste0("Calculating estimates for ph.instructions row ", X, " of ", nrow(ph.instructions), "..."))

      # create constants for calc()----
      tempbv1 <- setdiff(ph.instructions[X][['cat1_varname']], c())
      tempbv2 <- setdiff(ph.instructions[X][['cat2_varname']], c())
      if(length(tempbv2) == 0){tempbv2 = NA}
      tempbv <- setdiff(c(tempbv1, tempbv2), c(NA))

      # send constants to global environment so it can be used by the calc() function below
      assign("tempbv", tempbv, envir = .GlobalEnv)
      assign("tempend", ph.instructions[X][['end']], envir = .GlobalEnv)
      assign("tempstart", ph.instructions[X][['start']], envir = .GlobalEnv)

      # use calc()----
      if(rate == FALSE){ # standard proportion analysis
        if(any(grepl('wastate', tempbv))){
          tempest <- calc(ph.data = ph.data,
                          what = ph.instructions[X][['indicator_key']],
                          where = chi_year >= tempstart & chi_year <= tempend,
                          by = tempbv,
                          metrics = c('mean', 'numerator', 'denominator', 'rse'))
        } else {
          tempest <- calc(ph.data = ph.data,
                          what = ph.instructions[X][['indicator_key']],
                          where = chi_year >= tempstart & chi_year <= tempend & chi_geo_kc == 'King County',
                          by = tempbv,
                          metrics = c('mean', 'numerator', 'denominator', 'rse'))
        }
      }
      if(rate == TRUE){
        if(any(grepl('wastate', tempbv))){
          tempest <- calc(ph.data = ph.data,
                          what = ph.instructions[X][['indicator_key']],
                          where = chi_year >= tempstart & chi_year <= tempend,
                          by = tempbv,
                          metrics = c('rate', 'numerator', 'denominator', 'rse'),
                          per = rate_per)
        } else {
          tempest <- calc(ph.data = ph.data,
                          what = ph.instructions[X][['indicator_key']],
                          where = chi_year >= tempstart & chi_year <= tempend & chi_geo_kc == 'King County',
                          by = tempbv,
                          metrics = c('rate', 'numerator', 'denominator', 'rse'),
                          per = rate_per)
        }
        setnames(tempest, gsub("^rate", "mean", names(tempest)))
      }

      # add on CHI standard columns that are from ph.instructions (in order of standard results output)----
      tempest[, indicator_key := ph.instructions[X][['indicator_key']]]
      tempest[, tab := ph.instructions[X][['tab']]]
      tempest[ph.instructions[X][['end']] != ph.instructions[X][['start']],
              year := paste0(ph.instructions[X][['start']], "-", ph.instructions[X][['end']])]
      tempest[ph.instructions[X][['end']] == ph.instructions[X][['start']],
              year := ph.instructions[X][['end']]]
      tempest[, cat1 := ph.instructions[X][['cat1']]]
      setnames(tempest, ph.instructions[X][['cat1_varname']], 'cat1_group')
      tempest[, cat1_varname := ph.instructions[X][['cat1_varname']]]
      tempest[, cat2 := ph.instructions[X][['cat2']]]
      if(!is.na(tempbv2) & tempbv1 != tempbv2){
        setnames(tempest, ph.instructions[X][['cat2_varname']], 'cat2_group')} else{
          tempest[, cat2_group := NA] }
      tempest[, cat2_varname := ph.instructions[X][['cat2_varname']]]
      setnames(tempest,
               c("mean", "mean_lower", "mean_upper", "mean_se"),
               c("result", "lower_bound", "upper_bound", "se"))
    }
  ), use.names = TRUE)

  # Tidy results ----
  # drop when cat1_group is missing (e.g., cat1 == 'Regions' and region is NA) ----
  tempCHIest <- tempCHIest[!is.na(cat1_group)]

  # drop when cat2_group is missing but cat2 is not missing ----
  tempCHIest <- tempCHIest[!(is.na(cat2_group) & !is.na(cat2))]

  # drop if cat1_group | cat2_group had `keepme == "No"` in the reference table ----
  dropme <- unique(stdbyvars[keepme == 'No'][, reference := NULL])
  tempCHIest <- merge(tempCHIest,
                      dropme,
                      by.x = c('cat1_varname', 'cat1_group'),
                      by.y = c('varname', 'group'),
                      all.x = T,
                      all.y = F)
  tempCHIest <- tempCHIest[is.na(keepme)][, keepme := NULL]

  tempCHIest <- merge(tempCHIest,
                      dropme,
                      by.x = c('cat2_varname', 'cat2_group'),
                      by.y = c('varname', 'group'),
                      all.x = T,
                      all.y = F)
  tempCHIest <- tempCHIest[is.na(keepme)][, keepme := NULL]

  # change all NaN to a normal NA or SQL will vomit ----
  for(col in names(tempCHIest)) set(tempCHIest, i=which(is.nan(tempCHIest[[col]])), j=col, value=NA)

  # apply rounding rules for proportions----
  if(rate == FALSE){
    tempCHIest[, c("result", "lower_bound", "upper_bound", "rse") := lapply(.SD, round2, 3), .SDcols = c("result", "lower_bound", "upper_bound", "rse")]
    tempCHIest[, c("se") := lapply(.SD, round2, 4), .SDcols = c("se")]
  }

  # apply rounding rules for rates----
  if(rate == TRUE){
    tempCHIest[, c("result", "lower_bound", "upper_bound") := lapply(.SD, round2, 1), .SDcols = c("result", "lower_bound", "upper_bound")]
    tempCHIest[, c("rse") := lapply(.SD, round2, 3), .SDcols = c("rse")]
    tempCHIest[, c("se") := lapply(.SD, round2, 2), .SDcols = c("se")]
  }

  # prevent negative lower CI ----
  tempCHIest[lower_bound < 0, lower_bound := 0]

  # race3/race4 messiness ----
  tempCHIest[cat1_varname == 'race3_hispanic', cat1_varname := 'race3']
  tempCHIest[cat2_varname == 'race3_hispanic', cat2_varname := 'race3']

  if(any(grepl("Birthing per", unique(tempCHIest$cat1)))){
    tempCHIest[cat1_varname %in% c("race3", "race4") & tab == 'trends', cat1 := "Birthing person's race/ethnicity"]
    tempCHIest[cat2_varname %in% c("race3", "race4") & tab == 'trends', cat2 := "Birthing person's race/ethnicity"]

    tempCHIest[cat1_varname %in% c('race3') & tab %in% c('crosstabs', 'demgroups') & cat1_group == 'Hispanic',
               cat1 := "Birthing person's ethnicity"]
    tempCHIest[cat2_varname %in% c('race3') & tab %in% c('crosstabs', 'demgroups') & cat2_group == 'Hispanic',
               cat2 := "Birthing person's ethnicity"]
  } else {
    tempCHIest[cat1_varname %in% c("race3", "race4") & tab == 'trends', cat1 := "Race/ethnicity"]
    tempCHIest[cat2_varname %in% c("race3", "race4") & tab == 'trends', cat2 := "Race/ethnicity"]

    tempCHIest[cat1_varname %in% c('race3') & tab %in% c('crosstabs', 'demgroups') & cat1_group == 'Hispanic',
               cat1 := 'Ethnicity']
    tempCHIest[cat2_varname %in% c('race3') & tab %in% c('crosstabs', 'demgroups') & cat2_group == 'Hispanic',
               cat2 := 'Ethnicity']
  }

  # drop temporary vars that were sent to global environment for calc() ----
  rm(tempbv, tempend, tempstart, envir = .GlobalEnv)

  # Create additional necessary CHI columns ----
  tempCHIest[, source_date := as.Date(source_date)]
  tempCHIest[, run_date := as.Date(Sys.Date(), "%Y%m%d")]
  tempCHIest[, chi := 1] # set to 1 by default, because this is for CHI. Manually set to zero when needed afterward
  tempCHIest[tab == 'metadata', chi := 0]
  tempCHIest[, data_source := source_name]
  tempCHIest[, time_trends := NA] # NA because no longer calculated

  tempCHIest <- compare_estimate(mydt = tempCHIest,
                                 id_vars = c("indicator_key", "year"),
                                 key_where = cat1_group == "King County" & tab != "crosstabs",
                                 new_col = "comparison_with_kc",
                                 tidy = TRUE)
  setnames(tempCHIest, "comparison_with_kc_sig", "significance")

  if(small_num_suppress == TRUE){
    tempCHIest <- rads::suppress(sup_data = tempCHIest,
                                 suppress_range = c(suppress_low, suppress_high),
                                 secondary = T,
                                 secondary_exclude = cat1_varname != 'race3')
  } else {tempCHIest[, suppression := NA_character_]}

  tempCHIest[rse>=30, caution := "!"]


  # Keep and order standard CHI columns ----
  tempCHIest <- tempCHIest[, c(chi_cols()[]), with = F]

  tempCHIest <- tempCHIest[, cat1 := factor(cat1, levels = c("King County", sort(setdiff(unique(tempCHIest$cat1), "King County"))) )]
  tempCHIest <- tempCHIest[, tab := factor(tab, levels = c(c("_kingcounty","demgroups", "trends"),  sort(setdiff(unique(tempCHIest$tab), c("_kingcounty","demgroups", "trends")))) )]
  setorder(tempCHIest, indicator_key, tab, -year, cat1, cat1_group, cat2, cat2_group)

  # return the CHI table ----
  return(tempCHIest)
}


CHI_count_by_age <- function(ph.data = NULL,
                             ph.instructions = NULL,
                             source_date = NULL){
  # Create 'Overall' if needed for crosstabs ----
  if(!'overall' %in% names(ph.data)){
    ph.data$overall <- with(ph.data, ifelse(chi_geo_kc == 'King County', 'Overall', NA_character_))
  }

  # Check to make sure all variables needed exist in the data ----
  neededbyvars <- setdiff(unique(c(ph.instructions$cat1_varname, ph.instructions$cat2_varname)), c(NA))
  if("race3" %in% neededbyvars & !"race3_hispanic" %in% neededbyvars){neededbyvars <- c(neededbyvars, 'race3_hispanic')} # By definition, Hispanic cannot be contained within race3

  neededvars <- setdiff(unique(c(ph.instructions$indicator_key, neededbyvars)), c(NA))

  missingvars <- setdiff(neededvars, names(ph.data))
  if(length(missingvars) > 0 ){
    stop(paste0("\n\U2620 ph.data is missing the following columns that are specified in ph.instructions: ", paste0(missingvars, collapse = ', '), ". ",
                "\nIf `race3_hispanic` is listed, that is because, by definition, `race3` cannot have a Hispanic ethnicity in the same variable. So, two ",
                "\nvariables (`race3` & `race3_hispanic`) will be processed and in the output, it will be called `race3`"))
  } else{message("\U0001f642 All specified variables exist in ph.data")}

  # Check to make sure all byvariables have the CHI specified encoding ----
  stdbyvars <- rads.data::misc_chi_byvars[varname %in% setdiff(unique(c(ph.instructions$cat1_varname, ph.instructions$cat2_varname)), c(NA))][, list(varname, group, keepme, reference = 1)]
  stdbyvars[group %in% c("Hispanic", 'Non-Hispanic') & varname == 'race3', varname := 'race3_hispanic'] # necessary because race3 & Hispanic must be two distinct variables in raw data
  phbyvars <- rbindlist(lapply(
    X=as.list(neededbyvars),
    FUN = function(X){data.table(varname = X, group = setdiff(unique(ph.data[[X]]), NA), ph.data = 1)}))
  compbyvars <- merge(stdbyvars, phbyvars, by = c('varname', 'group'), all = T)
  if(nrow(compbyvars[is.na(reference)| is.na(ph.data)]) > 0){
    print(compbyvars[is.na(reference)| is.na(ph.data)])
    stop("\n\U2620 the table above shows the varname/group combinations that do not align between the reference table and your ph.data.")
  } else {message("\U0001f642 All specified cat1_group and cat2_group values align with the reference standard.")}

  # Cycle through a function that generates counts by age ----
  message("\U023F3 Be patient! The function is generating counts for each row of ph.instructions.")
  tempCHIcount <- rbindlist(future_lapply(
    X = as.list(seq(1, nrow(ph.instructions), 1)),
    FUN = function(X){
      message(paste0("Calculating estimates for ph.instructions row ", X, " of ", nrow(ph.instructions), "..."))

      # create constants for calc----
      tempbv1 <- setdiff(ph.instructions[X][['cat1_varname']], c())
      tempbv2 <- setdiff(ph.instructions[X][['cat2_varname']], c())
      if(length(tempbv2) == 0){tempbv2 = NA}
      tempbv <- setdiff(c(tempbv1, tempbv2), c(NA))
      tempbv <- c(tempbv, "chi_age")

      # send constants to global environment so it can be used by the calc() function below
      assign("tempbv", tempbv, envir = .GlobalEnv)
      assign("tempend", ph.instructions[X][['end']], envir = .GlobalEnv)
      assign("tempstart", ph.instructions[X][['start']], envir = .GlobalEnv)

      # use calc----
      if(any(grepl('wastate', tempbv))){
        tempcount <- calc(ph.data = ph.data,
                          what = ph.instructions[X][['indicator_key']],
                          where = chi_year >= tempstart & chi_year <= tempend,
                          by = tempbv,
                          metrics = c('numerator'))
      } else {
        tempcount <- calc(ph.data = ph.data,
                          what = ph.instructions[X][['indicator_key']],
                          where = chi_year >= tempstart & chi_year <= tempend & chi_geo_kc == 'King County',
                          by = tempbv,
                          metrics = c('numerator'))
      }

      # tidy----
      tempcount[, cat1 := ph.instructions[X][['cat1']]]
      setnames(tempcount, ph.instructions[X][['cat1_varname']], 'cat1_group')
      tempcount[, cat1_varname := ph.instructions[X][['cat1_varname']]]
      tempcount[, cat2 := ph.instructions[X][['cat2']]]
      if(!is.na(tempbv2) & tempbv1 != tempbv2){
        setnames(tempcount, ph.instructions[X][['cat2_varname']], 'cat2_group')} else{
          tempcount[, cat2_group := NA] }
      tempcount[, cat2_varname := ph.instructions[X][['cat2_varname']]]

      tempcount <- tempcount[!is.na(cat1_group)]
      tempcount <- tempcount[!(is.na(cat2_group) & !is.na(cat2))]
      tempcount <- tempcount[!is.na(chi_age)]
      tempcount <- tempcount[, list(cat1, cat1_varname, cat1_group, cat2, cat2_varname, cat2_group, chi_age, count = numerator)]

      # create reference table with every combination of cat1 x cat2 and age----
      cat1table <- data.table(cat1 = ph.instructions[X][['cat1']],
                              cat1_varname = tempbv1,
                              cat1_group = sort(setdiff(as.character(unique(ph.data[[tempbv1]])), NA)) )
      cat2table <- suppressWarnings(data.table(cat2 = ph.instructions[X][['cat2']],
                                               cat2_varname = tempbv2,
                                               cat2_group = sort(setdiff(as.character(unique(ph.data[[tempbv2]])), NA))) )
      cattable <- tidyr::crossing(cat1table, cat2table)
      cattable <- setDT(tidyr::crossing(cattable, data.table(chi_age = 0:100)))

      # merge the counts onto the reference table to get every combo of age x cat1 x cat2----
      tempcount <- merge(cattable, tempcount, by = c('cat1', 'cat1_varname', 'cat1_group', 'cat2', 'cat2_varname', 'cat2_group', 'chi_age'), all = T)
      tempcount[is.na(count), count := 0] # when count is NA, it is actually zero and zero is needed for calculating age adjusted rates

      # add on remaining essential identifiers----
      tempcount[, indicator_key := ph.instructions[X][['indicator_key']]]
      tempcount[, tab := ph.instructions[X][['tab']]]
      tempcount[ph.instructions[X][['end']] != ph.instructions[X][['start']],
                year := paste0(ph.instructions[X][['start']], "-", ph.instructions[X][['end']])]
      tempcount[ph.instructions[X][['end']] == ph.instructions[X][['start']],
                year := ph.instructions[X][['end']]]

      # order output----
      tempcount <- tempcount[, .(indicator_key, year, tab, cat1, cat1_varname, cat1_group, cat2, cat2_varname, cat2_group, chi_age, count)]
      setorder(tempcount, cat1_group, cat2_group, chi_age)

    }
  ), use.names = TRUE)

  # Return ----
  return(tempCHIcount)
}

# Get populations that correspond with standard counts ----
# CHI_generate_instructions_pop() - function to generate instructions for get_population based on structure of count data----
CHI_generate_instructions_pop <- function(mycount.data, povgeo = NA){
  pop.template <- copy(mycount.data)
  pop.template <- unique(copy(pop.template)[, .(year, cat1, cat1_varname, cat2, cat2_varname, tab)])
  pop.template[, c("start", "stop") := tstrsplit(year, split = '-') ]
  pop.template[is.na(stop), stop := start] # need to have an end date even if it is just one year

  pop.template[, race_type := 'race_eth'] # by default has race and OMB 97 with Hispanic as race

  # Drop prefix when using maternal data because do not want to create multiple alternative codings below ----
  pop.template[grepl("birthing person", cat1, ignore.case = T), cat1 := tools::toTitleCase(gsub("Birthing person's ", "", cat1))]
  pop.template[grepl("birthing person", cat2, ignore.case = T), cat2 := tools::toTitleCase(gsub("Birthing person's ", "", cat2))]

  # Create geo_type & group_by arguments ----
  omb_aic <- c("chi_race_aic_aian", "chi_race_aic_asian", "chi_race_aic_black", "chi_race_aic_his", "chi_race_aic_nhpi", "chi_race_aic_wht")

  for(catnum in c("1", "2")){
    temp.cat <- paste0("cat", catnum)
    pop.template[get(temp.cat) == "Cities/neighborhoods", geo_type := "hra"]
    pop.template[get(paste0(temp.cat, "_varname")) == "race3", c("race_type", paste0("group_by", catnum)) := 'race']
    pop.template[get(paste0(temp.cat, "_varname")) == "race4", c("race_type", paste0("group_by", catnum)) := 'race_eth']
    pop.template[get(paste0(temp.cat, "_varname")) %in% omb_aic, c("race_type", paste0("group_by", catnum)) := 'race_aic']

    # the only AIC race/eth with pop data are the standard OMB categories
    pop.template <- pop.template[!(grepl('_aic_', get(paste0(temp.cat, "_varname"))) & !get(paste0(temp.cat, "_varname")) %in% omb_aic)]

    pop.template[get(temp.cat) == "Ethnicity", c("race_type", paste0("group_by", catnum)) := 'race_eth']
    pop.template[get(temp.cat) == "Gender",  paste0("group_by", catnum) := 'genders']
    pop.template[get(temp.cat) %in% c("Race", "Race/ethnicity") & get(paste0(temp.cat, "_varname")) == 'race4',
                 paste0("group_by", catnum) := 'race_eth']
    pop.template[(get(temp.cat) == "Race" & get(paste0(temp.cat, "_varname")) == 'race3') ,
                 paste0("group_by", catnum) := 'race']
    pop.template[get(temp.cat) == "Regions" & (is.na(geo_type) | geo_type != 'hra'), `:=` (geo_type = "region")]
    pop.template[get(temp.cat) == "Big cities", `:=` (geo_type = "hra")]
    pop.template[get(temp.cat) == "Washington State", `:=` (geo_type = "wa")]
  }

  pop.template[grepl("poverty$", cat1, ignore.case = T) | grepl("poverty$", cat2, ignore.case = T), geo_type := "blk"]
  if(povgeo == 'zip'){
    pop.template[grepl("poverty$", cat1, ignore.case = T) | grepl("poverty$", cat2, ignore.case = T), geo_type := "zip"]
  }
  pop.template[(cat1 == "Regions" & cat2 == "Cities/neighborhoods") |
                 (cat2 == "Regions" & cat1 == "Cities/neighborhoods"),
               geo_type := "blk"]

  # the only AIC race/eth with population data are the OMB standard categories

  pop.template[is.na(cat2), cat2 := "NA"] # temporarily set NA to "NA" to facilitate processing with function

  pop.template[is.na(geo_type), geo_type := 'kc'] # when not specified, it is for KC

  pop.template <- unique(pop.template) # because want to minimize the calls to get_popualation to improve speed
}

# CHI_get_proper_pop() - function to get population for a single row specified by the output of CHI_generate_instructions_pop() ----
CHI_get_proper_pop <- function(pop.template = NULL, pop.genders = NULL, pop.ages = NULL){
  # check for valid values of pop.genders ----
  if(is.null(pop.genders)){gendery = c("f", "m")
  }else{if(!tolower(pop.genders) %in% c('f', 'female', 'm', 'male')){
    stop("\n\U0001f47f if pop.genders is specified it must have one of the following values: 'F', 'f', 'Female', 'female', 'M', 'm', 'Male', or 'male'")
  } else {gendery = pop.genders}}

  # check for valid values of pop.ages ----
  if(is.null(pop.ages)){agesy = c(0:100)
  }else{if(!is.integer(pop.ages)){
    stop("\n\U0001f47f if pop.ages is specified it must be vector of integers, e.g., c(0:65)")
  } else {agesy = pop.ages}}

  # create function to generate the population table corresponding to each row of the pop.template----
  CHI_get_proper_pop_engine <- function(X, pop.template = NULL){
    # Status updates ----
    print(paste0("Process ID ", Sys.getpid(), ": Getting population ", X, " out of ", nrow(pop.template)))

    # Drop prefix when using maternal data because do not want to create multiple alternative codings below ----
    pop.template[grepl("birthing person", cat1, ignore.case = T), cat1 := tools::toTitleCase(gsub("Birthing person's ", "", cat1))]
    pop.template[grepl("birthing person", cat2, ignore.case = T), cat2 := tools::toTitleCase(gsub("Birthing person's ", "", cat2))]

    # create the group_by argument ----
    groupy <- unique(c(c("ages", "geo_id"), setdiff(c(pop.template[X, group_by1], pop.template[X, group_by2]), c(NA))))

    # use rads::get_population ----
    if(is.na(pop.template[X, geo_type])){
      tempy <- get_population(group_by = groupy,
                              race_type = pop.template[X, race_type],
                              years = pop.template[X, start]:pop.template[X, stop],
                              genders = gendery,
                              ages = agesy,
                              round = F)
    }
    if(!is.na(pop.template[X, geo_type])){
      tempy <- get_population(group_by = groupy,
                              geo_type = pop.template[X, geo_type],
                              race_type = pop.template[X, race_type],
                              years = pop.template[X, start]:pop.template[X, stop],
                              genders = gendery,
                              ages = agesy,
                              round = F)
    }

    # tidy the population data ----
    for(catnum in c("cat1", "cat2")){
      # misc ----
      tempy[, paste0(catnum) := pop.template[X, get(catnum)]]
      tempy[, paste0(catnum, "_varname") := pop.template[X, get(paste0(catnum, "_varname"))]]

      tempy[get(catnum) == "King County", paste0(catnum, "_group") := "King County"]

      tempy[get(catnum) == "Washington State", paste0(catnum, "_group") := "Washington State"]

      suppressWarnings(tempy[get(catnum) == "NA" | is.na(get(catnum)),
                             c(catnum, paste0(catnum, "_group"), paste0(catnum, "_varname")) := "NA"]) # just a random fill value for NA, which will be changed to true NA later

      tempy[get(catnum) %in% c("Cities/neighborhoods", "Regions") &  pop.template[X, geo_type] != 'blk',
            paste0(catnum, "_group") := geo_id]

      tempy[get(catnum) %in% c("Gender"), paste0(catnum, "_group") := gender]

      tempy[get(catnum) %in% c("Overall"), paste0(catnum, "_group") := "Overall"]


      # race/eth ----
      tempy[get(catnum) == "Ethnicity" | get(paste0(catnum, "_varname")) %in% c('race4'), paste0(catnum, "_group") := race_eth]
      tempy[get(catnum) == 'Race' & get(paste0(catnum, "_varname")) %in% c('race3'), paste0(catnum, "_group") := race]
      tempy[get(paste0(catnum, "_group")) == "Multiple race", paste0(catnum, "_group") := "Multiple"]
      tempy <- tempy[get(catnum) != "Ethnicity" | (get(catnum) == "Ethnicity" & get(paste0(catnum, "_group")) == 'Hispanic'), ]

      # race_aic ----
      if(pop.template[X, race_type] == 'race_aic'){
        tempy <- tempy[!(grepl('_aic_', get(paste0(catnum, "_varname"))) &
                           !((get(paste0(catnum, "_varname")) == 'chi_race_aic_aian' & race_aic == 'AIAN') |
                               (get(paste0(catnum, "_varname")) == 'chi_race_aic_asian' & race_aic == 'Asian') |
                               (get(paste0(catnum, "_varname")) == 'chi_race_aic_black' & race_aic == 'Black')|
                               (get(paste0(catnum, "_varname")) == 'chi_race_aic_his' & race_aic == 'Hispanic') |
                               (get(paste0(catnum, "_varname")) == 'chi_race_aic_nhpi' & race_aic == 'NHPI') |
                               (get(paste0(catnum, "_varname")) == 'chi_race_aic_wht' & race_aic == 'White'))
        )]
        tempy[grep('_aic', get(paste0(catnum, "_varname"))), paste0(catnum, "_group") := race_aic]
      }

      # HRAS ----
      if(tempy[1, geo_type] == 'blk' & tempy[1, get(catnum)] == 'Cities/neighborhoods'){
        temp.xwalk <- rads.data::spatial_block20_to_hra20_to_region20[, .(geo_id = GEOID20, hra20_name)]
        tempy <- merge(tempy, temp.xwalk, by = "geo_id", all.x = T, all.y = F)
        tempy[, paste0(catnum, "_group") := hra20_name]
      }

      # Regions ----
      if(tempy[1, geo_type] == 'blk' & tempy[1, get(catnum)] == 'Regions'){
        temp.xwalk <- rads.data::spatial_block20_to_hra20_to_region20[, .(geo_id = GEOID20, region_name)]
        tempy <- merge(tempy, temp.xwalk, by = 'geo_id', all.x = T, all.y = F)

        tempy[, paste0(catnum, "_group") := region_name]
      }

      if(tempy[1, geo_type] == 'hra' & tempy[1, get(catnum)] == 'Regions'){
        temp.xwalk <- rads.data::spatial_hra20_to_region20[, .(geo_id = hra20_name, region_name)]
        tempy <- merge(tempy, temp.xwalk, by = 'geo_id', all.x = T, all.y = F)

        tempy[, paste0(catnum, "_group") := region_name]
      }

      if(tempy[1, geo_type] == 'zip' & tempy[1, get(catnum)] == 'Regions'){
        zip_2_region <- rads.data::spatial_zip_to_hra20_pop
        zip_2_region <- merge(zip_2_region,
                              rads.data::spatial_hra20_to_region20[, .(hra20_name, region = region_name)],
                              by = 'hra20_name',
                              all = T)
        zip_2_region <- zip_2_region[, .(s2t_fraction = sum(s2t_fraction)), # collapse fractions down to region level
                                     .(geo_id = as.character(source_id), region)]

        tempy <- merge(tempy, zip_2_region, by = "geo_id", all.x = T, all.y = F, allow.cartesian = T)
        tempy[, pop := pop * s2t_fraction] # calculate weighted pop
        tempy[, paste0(catnum, "_group") := region]
      }

      # Big Cities ----
      if(tempy[1, get(catnum)] == 'Big cities'){
        if(tempy[1, geo_type] == 'blk'){
          blk20_hra20 <- rads.data::spatial_block20_to_hra20_to_region20[, .(geo_id = GEOID20, hra20_name)]
          tempy <- merge(tempy, blk20_hra20, by = "geo_id", all.x = T, all.y = F)
          hra20_bigcity <- rads.data::spatial_hra20_to_bigcities[, .(hra20_name, bigcity)]
          tempy <- merge(tempy, hra20_bigcity, by = 'hra20_name', all.x = T, all.y = F)
        }
        if(tempy[1, geo_type] == 'hra'){
          hra20_bigcity <- rads.data::spatial_hra20_to_bigcities[, .(hra20_name, bigcity)]
          tempy <- merge(tempy, hra20_bigcity, by.x = 'geo_id', by.y = 'hra20_name', all.x = T, all.y = F)
        }
        tempy[, paste0(catnum, "_group") := bigcity]
      }

      # age6 ----
      tempy[get(paste0(catnum, "_varname")) == "age6" & age %in% 0:17, paste0(catnum, "_group") := "<18"]
      tempy[get(paste0(catnum, "_varname")) == "age6" & age %in% 18:24, paste0(catnum, "_group") := "18-24"]
      tempy[get(paste0(catnum, "_varname")) == "age6" & age %in% 25:44, paste0(catnum, "_group") := "25-44"]
      tempy[get(paste0(catnum, "_varname")) == "age6" & age %in% 45:64, paste0(catnum, "_group") := "45-64"]
      tempy[get(paste0(catnum, "_varname")) == "age6" & age %in% 65:74, paste0(catnum, "_group") := "65-74"]
      tempy[get(paste0(catnum, "_varname")) == "age6" & age >= 75, paste0(catnum, "_group") := "75+"]

      # mage5 ----
      tempy[get(paste0(catnum, "_varname")) == "mage5" & age %in% 10:17, paste0(catnum, "_group") := "10-17"]
      tempy[get(paste0(catnum, "_varname")) == "mage5" & age %in% 18:24, paste0(catnum, "_group") := "18-24"]
      tempy[get(paste0(catnum, "_varname")) == "mage5" & age %in% 25:34, paste0(catnum, "_group") := "25-34"]
      tempy[get(paste0(catnum, "_varname")) == "mage5" & age %in% 35:44, paste0(catnum, "_group") := "35-44"]
      tempy[get(paste0(catnum, "_varname")) == "mage5" & age >=45, paste0(catnum, "_group") := "45+"]

      # yage4 ----
      tempy[get(paste0(catnum, "_varname")) == "yage4" & age %in% 0:4, paste0(catnum, "_group") := "0-4"]
      tempy[get(paste0(catnum, "_varname")) == "yage4" & age %in% 5:9, paste0(catnum, "_group") := "5-9"]
      tempy[get(paste0(catnum, "_varname")) == "yage4" & age %in% 10:14, paste0(catnum, "_group") := "10-14"]
      tempy[get(paste0(catnum, "_varname")) == "yage4" & age %in% 15:17, paste0(catnum, "_group") := "15-17"]

      # pov200grp ----
      if(tempy[1, geo_type] == 'blk' & grepl("poverty$", tempy[1, get(catnum)], ignore.case = T)){
        tempy[, geo_tract2020 := substr(geo_id, 1, 11)] # have blocks (15 char), so keep first 11 for tracts
        tempy <- merge(tempy,
                       rads.data::misc_poverty_groups[geo_type=='Tract'][, .(geo_tract2020 = geo_id, pov200grp)],
                       by = "geo_tract2020",
                       all.x = T,
                       all.y = F)
        tempy[, paste0(catnum, "_group") := pov200grp]
      }
      if( tempy[1, geo_type] == 'zip' & grepl("poverty$", tempy[1, get(catnum)], ignore.case = T)){
        tempy <- merge(tempy,
                       rads.data::misc_poverty_groups[geo_type=='ZCTA'][, .(geo_id, pov200grp)],
                       by = 'geo_id',
                       all.x = T,
                       all.y = F)
        tempy[, paste0(catnum, "_group") := pov200grp]
      }

    }

    # Drop if is.na(cat1_group)
    tempy <- tempy[!is.na(cat1_group)]

    # drop if is.na(cat2_group)
    tempy <- tempy[!(cat2 != 'NA' & (cat2_group == 'NA') | is.na(cat2_group))] # did not yet switch back to true NA at this point

    # collapse to one row per demographic combination and keep minimum needed columns ----
    tempy <- tempy[, .(pop = sum(pop)), .(chi_age = age, year, cat1, cat1_varname, cat1_group, cat2, cat2_varname, cat2_group)]

    # ensure each demographic has rows for all relevant ages & only relevant ages ----
    if(tempy[1]$cat1 == "Age"){
      if(tempy[1]$cat1_varname == 'age6'){
        tempage <- data.table(cat1 = "Age", cat1_varname = "age6", chi_age = 0:100)
        tempage[, cat1_group := cut(chi_age,
                                    breaks = c(-1, 17, 24, 44, 64, 74, 120),
                                    labels = c("<18", "18-24", "25-44", "45-64", "65-74", "75+"))]}

      if(tempy[1]$cat1_varname == 'mage5'){
        tempage <- data.table(cat1 = "Age", cat1_varname = "mage5", chi_age = 10:100)
        tempage[, cat1_group := cut(chi_age,
                                    breaks = c(9, 17, 24, 34, 44, 120),
                                    labels = c('10-17', '18-24', '25-34', '35-44', '45+'))]}

      if(tempy[1]$cat1_varname == 'yage4'){
        tempage <- data.table(cat1 = "Age", cat1_varname = "yage4", chi_age = 0:17)
        tempage[, cat1_group := cut(chi_age,
                                    breaks = c(-1, 4, 9, 14, 17),
                                    labels = c('0-4', '5-9', '10-14', '15-17'))]}

      temp.demog <- setDT(tidyr::crossing(unique(tempy[, .(year = as.character(year), cat2, cat2_varname, cat2_group)]),
                                          tempage))
    }

    if(tempy[1]$cat2 == "Age"){
      if(tempy[1]$cat2_varname == 'age6'){
        tempage <- data.table(cat2 = "Age", cat2_varname = "age6", chi_age = 0:100)
        tempage[, cat2_group := cut(chi_age,
                                    breaks = c(-1, 17, 24, 44, 64, 74, 120),
                                    labels = c("<18", "18-24", "25-44", "45-64", "65-74", "75+"))]}

      if(tempy[1]$cat2_varname == 'mage5'){
        tempage <- data.table(cat2 = "Age", cat2_varname = "mage5", chi_age = 10:100)
        tempage[, cat2_group := cut(chi_age,
                                    breaks = c(9, 17, 24, 34, 44, 120),
                                    labels = c('10-17', '18-24', '25-34', '35-44', '45+'))]}

      if(tempy[1]$cat2_varname == 'yage4'){
        tempage <- data.table(cat2 = "Age", cat2_varname = "yage4", chi_age = 0:17)
        tempage[, cat2_group := cut(chi_age,
                                    breaks = c(-1, 4, 9, 14, 17),
                                    labels = c('0-4', '5-9', '10-14', '15-17'))]}

      temp.demog <- setDT(tidyr::crossing(unique(tempy[, .(year = as.character(year), cat1, cat1_varname, cat1_group)]),
                                          tempage))
    }

    if(!"Age" %in% unique(c(tempy$cat1, tempy$cat2))){
      # all combinations of cat1 x cat2
      temp.demog <- setDT(tidyr::crossing(
        unique(tempy[, .(cat1, cat1_varname, cat1_group)]),
        unique(tempy[, .(cat2, cat2_varname, cat2_group)])
      ))
      # all combination of cat table with year and age
      temp.demog <- setDT(tidyr::crossing(
        temp.demog,
        data.table(year = as.character(pop.template[X, ]$year), chi_age = 0:100)
      ))
    }

    tempy <- suppressWarnings(merge(tempy, temp.demog, all = T))
    tempy[is.na(pop), pop := 0]

    # create combinations of cat1_group and cat2_group that have no population and set == 0 ----
    # no need with current get_population function, but keep as a placeholder / reminder

    # add tab column ----
    tempy[, tab := pop.template[X, tab]]

    # tidy when is.na(cat2) ----
    tempy[cat2 == "NA", c("cat2", "cat2_varname", "cat2_group") := NA]

    # return object ----
    return(tempy)
  }

  # use lapply to cycle over each rows and create one big final dataset ----
  tempy.allpop <- rbindlist(
    future_lapply(X = as.list(seq(1, nrow(pop.template))),
                  FUN = function(X){
                    set.seed(98104) # another attempt to set a seed
                    CHI_get_proper_pop_engine(X, pop.template = pop.template)},
                  future.seed = 98104)
  )

  tempy.allpop <- unique(tempy.allpop)

  # return object ----
  return(tempy.allpop)
}


## Misc functions ----
# CHI_drop_illogical_ages() - drop rows that don't make sense (e.g., 50 year old in <18) ----
CHI_drop_illogical_ages <- function(DTx, agevar = 'chi_age'){
  DTx = copy(DTx)
  for(CatNum in c("cat1", "cat2")){
    DTx[, paste0(CatNum, '_group_temp') := fcase(get(paste0(CatNum, "_group")) == '<1', '0-0', # <1 is special!
                                                 get(CatNum) %in% c("Age", "Birthing person's age"), gsub("<", "0-", gsub("\\+", "-120", get(paste0(CatNum, '_group')))))]
    DTx[, AgeMin := gsub("-.*", "", get(paste0(CatNum, '_group_temp')))]
    DTx[, AgeMax := gsub(".*-", "", get(paste0(CatNum, '_group_temp')))]
    DTx <- DTx[!get(CatNum) %in% c("Age", "Birthing person's age")  | between(get(agevar), AgeMin, AgeMax)]
    DTx[, c("AgeMin", paste0(CatNum, '_group_temp'), "AgeMax") := NULL]
  }
  return(DTx)
}

# CHI_generate_metadata() - function to generate metadata table combining existing metadata and latest estimates ----
CHI_generate_metadata <- function(meta.old = NULL,
                                  est.current = NULL){
  # get new metadata ----
  meta.new <- unique(est.current[tab == "metadata",
                                 .(indicator_key,
                                   latest_yearx = as.integer(year),
                                   latest_year_resultx = result,
                                   run_datex = run_date,
                                   latest_year_countx = as.integer(numerator),
                                   latest_year_kc_popx = as.integer(denominator))])

  # merge new metadata onto old metadata ----
  meta.new <- merge(meta.old, meta.new, by = c("indicator_key"), all = T)

  # update with newest data ----
  # only replace old data when there is new data because may stop calculating indicators, in which case, would want to keep old data
  meta.new[!is.na(latest_yearx), latest_year := as.numeric(latest_yearx)]
  meta.new[!is.na(latest_year_resultx), latest_year_result := latest_year_resultx]
  meta.new[, run_date := as.Date(gsub("-", "", run_datex), "%Y%m%d")]
  meta.new[!is.na(latest_year_countx), latest_year_count := latest_year_countx]
  meta.new[!is.na(latest_year_kc_popx), latest_year_kc_pop := latest_year_kc_popx]
  meta.new[, c("latest_yearx", "latest_year_resultx", "run_datex", "latest_year_countx", "latest_year_kc_popx") := NULL]

  # update valid_years ----
  meta.new[as.integer(latest_year) > suppressWarnings(as.integer(substrRight(valid_years, 1, 4))),
           valid_years := suppressWarnings(paste(as.integer(substr(valid_years, 1, 4)):as.integer(latest_year), collapse = " "))]

  # Ensure there are no missing important metadata cells ----
  missing.per.col <- sapply(meta.new, function(x) sum(is.na(x)))
  if(sum(missing.per.col) > 0){
    stop("You are missing at least one critical meta.new[] value.")
  }

  # order metadata table ----
  setorder(meta.new, indicator_key)

  # return table ----
  return(meta.new)
}

# CHI_sql_update() - function to update (or replace) results and metadata in SQL 51 (dev/WIP) or 50 (prod) ----
CHI_sql_update <- function(CHIestimates = NULL,
                           CHImetadata = NULL,
                           table_name = NULL,
                           server = 'wip', # options include c('wip', 'dev', 'prod', '51', '50')
                           replace_table = F # default is to update select rows rather than replace the entire table
){
  # load CHI yaml config file ----
  chi_config <- yaml::yaml.load(httr::GET(url = "https://raw.githubusercontent.com/PHSKC-APDE/rads/main/ref/chi_qa.yaml", httr::authenticate(Sys.getenv("GITHUB_TOKEN"), "")))

  # check CHIestimates argument----
  if(!exists('CHIestimates')){stop("\n\U0001f47f The results table to push to SQL (CHIestimates) is missing ")}
  if( inherits(CHIestimates, "data.frame") == FALSE){stop("\n\U0001f47f CHIestimates must be a data.frame or a data.table.")}
  if( inherits(CHIestimates, "data.table") == FALSE){setDT(CHIestimates)}
  rads::validate_yaml_data(DF = CHIestimates, YML = chi_config, VARS = "vars")

  # check CHImetadata argument----
  if(!exists('CHImetadata')){stop("\n\U0001f47f The metadata table to push to SQL (CHImetadata) is missing ")}
  if( inherits(CHImetadata, "data.frame") == FALSE){stop("\n\U0001f47f CHImetadata must be a data.frame or a data.table.")}
  if( inherits(CHImetadata, "data.table") == FALSE){setDT(CHImetadata)}
  rads::validate_yaml_data(DF = CHImetadata, YML = chi_config, VARS = "metadata")

  # ensure indicator_key is consistent across estimates and metadata
  if(!identical(sort(as.character(unique(CHIestimates$indicator_key))), sort(as.character(CHImetadata$indicator_key)))){
    stop("\n\U0001f47f The indicator_key values in CHIestimates and CHImetadata are not identical ... but they should be!")
  }

  # check server argument----
  server = tolower(as.character(server))
  if(!server %in% c('wip', 'dev', 'prod', '51', '50')){stop("\n\U0001f47f The server argument is limited to: 'wip', 'dev', 'prod', '51', '50'")}
  if(length(server) != 1){stop("\n\U0001f47f The `server` argument must be of length 1")}

  # check replace argument----
  if(!is.logical(replace_table)){stop("\n\U0001f47f The `replace` argument must be a logical, i.e., TRUE | FALSE")}
  if(length(replace_table) != 1){stop("\n\U0001f47f The `server` argument must be of length 1")}

  # open database connection----
  if(server %in% c('wip', 'dev', '51')){
    CHI_db_cxn <- odbc::dbConnect(odbc::odbc(),
                                  Driver = "SQL Server",
                                  Server = "KCITSQLUTPDBH51",
                                  Database = "PHExtractStore")
    schema_suffix = '_WIP'
    complete_servername = 'KCITSQLUTPDBH51'
  }
  if(server %in% c('prod', '50')){
    CHI_db_cxn <- odbc::dbConnect(odbc::odbc(),
                                  Driver = "SQL Server",
                                  Server = "KCITSQLPRPDBM50",
                                  Database = "PHExtractStore")
    schema_suffix = ''
    complete_servername = 'KCITSQLPRPDBM50'
  }

  # check if *_results and *_metadata tables already exist in the appropriate schema----
  if(!DBI::dbExistsTable(conn = CHI_db_cxn, glue::glue_sql("[PHExtractStore].[APDE{DBI::SQL(schema_suffix)}].[{DBI::SQL(table_name)}_results]", .con = CHI_db_cxn))){
    tempquestion <- paste0("The table `[PHExtractStore].[APDE", schema_suffix, "].[", table_name, "_results]` does NOT currently exist. Are you sure you want to continue? (y/n) ")
    answer <- readline(tempquestion)
    if (answer == "y") {
      message("Continuing...")
    } else {
      stop(paste0("\n\U0001f47f The table `[PHExtractStore].[APDE", schema_suffix, "].[", table_name, "_results]` does NOT currently exist and you gave instructions not to continue."))
    }
  }

  if(!DBI::dbExistsTable(conn = CHI_db_cxn, glue::glue_sql("[PHExtractStore].[APDE{DBI::SQL(schema_suffix)}].[{DBI::SQL(table_name)}_metadata]", .con = CHI_db_cxn))){
    tempquestion <- paste0("The table `[PHExtractStore].[APDE", schema_suffix, "].[", table_name, "_metadata]` does NOT currently exist. Are you sure you want to continue? (y/n) ")
    answer <- readline(tempquestion)
    if (answer == "y") {
      message("Continuing...")
    } else {
      stop(paste0("\n\U0001f47f The table `[PHExtractStore].[APDE", schema_suffix, "].[", table_name, "_metadata]` does NOT currently exist and you gave instructions not to continue."))
    }
  }

  # if replace_table = F, delete existing data that will be replaced in SQL----
  if(isFALSE(replace_table)){
    # delete results
    if(DBI::dbExistsTable(conn = CHI_db_cxn, glue::glue_sql("[PHExtractStore].[APDE{DBI::SQL(schema_suffix)}].[{DBI::SQL(table_name)}_results]", .con = CHI_db_cxn))){
      DBI::dbGetQuery(
        conn = CHI_db_cxn,
        glue::glue_sql("DELETE FROM [PHExtractStore].[APDE{DBI::SQL(schema_suffix)}].[{DBI::SQL(table_name)}_results]
                         WHERE indicator_key IN ({paste(unique(CHIestimates$indicator_key))*})", .con = CHI_db_cxn))
    }


    # delete metadata
    if(DBI::dbExistsTable(conn = CHI_db_cxn, glue::glue_sql("[PHExtractStore].[APDE{DBI::SQL(schema_suffix)}].[{DBI::SQL(table_name)}_metadata]", .con = CHI_db_cxn))){
      DBI::dbGetQuery(
        conn = CHI_db_cxn,
        glue::glue_sql("DELETE FROM [PHExtractStore].[APDE{DBI::SQL(schema_suffix)}].[{DBI::SQL(table_name)}_metadata]
                           WHERE indicator_key IN ({paste(unique(CHIestimates$indicator_key))*})", .con = CHI_db_cxn))
    }

  }

  # if replace_table = T, check if there are indicators that would be lost----
  if(isTRUE(replace_table) &
     DBI::dbExistsTable(conn = CHI_db_cxn, glue::glue_sql("[PHExtractStore].[APDE{DBI::SQL(schema_suffix)}].[{DBI::SQL(table_name)}_results]", .con = CHI_db_cxn))){

    existing_indicators <- DBI::dbGetQuery(conn = CHI_db_cxn,
                                           statement = glue::glue_sql("SELECT DISTINCT indicator_key FROM [PHExtractStore].[APDE{DBI::SQL(schema_suffix)}].[{DBI::SQL(table_name)}_results]
                                                                            WHERE indicator_key IS NOT NULL"))$indicator_key
    new_indicators <- unique(CHIestimates[]$indicator_key)
    missing_indicators <- setdiff(existing_indicators, new_indicators)

    if(length(missing_indicators) > 0){
      tempquestion <- paste0("\U00026A0 You submitted the argument `replace_table = TRUE`, but your new data is missing ", length(missing_indicators),
                             "\nindicator_keys that are in the existing SQL table. These indicator_keys will be permanently lost! \nAre you sure you want to continue? (y/n) ")
      answer <- readline(tempquestion)
      if (answer == "y") {
        message("Continuing...")
      } else {
        stop(paste0("\n\U0001f47f Replacing [PHExtractStore].[APDE", schema_suffix, "].[", table_name, "_results] would have resulted in indicator_keys being permanently lost and you decided not to continue."))
      }

    }
  }

  # push data to SQL----
  if(isTRUE(replace_table)){
    # results
    DBI::dbWriteTable(conn = CHI_db_cxn,
                      name = DBI::Id(schema = paste0('APDE', schema_suffix), table = paste0(table_name, '_results')),
                      value = as.data.frame(copy(CHIestimates)),
                      overwrite = T,
                      append = F,
                      field.types = unlist(chi_config$vars))

    # metadata
    DBI::dbWriteTable(conn = CHI_db_cxn,
                      name = DBI::Id(schema = paste0('APDE', schema_suffix), table = paste0(table_name, '_metadata')),
                      value = as.data.frame(copy(CHImetadata)),
                      overwrite = T,
                      append = F,
                      field.types = unlist(chi_config$metadata))
  }

  if(isFALSE(replace_table)){
    # results
    DBI::dbWriteTable(conn = CHI_db_cxn,
                      name = DBI::Id(schema = paste0('APDE', schema_suffix), table = paste0(table_name, '_results')),
                      value = as.data.frame(copy(CHIestimates)),
                      overwrite = F,
                      append = T)

    # metadata
    DBI::dbWriteTable(conn = CHI_db_cxn,
                      name = DBI::Id(schema = paste0('APDE', schema_suffix), table = paste0(table_name, '_metadata')),
                      value = as.data.frame(copy(CHImetadata)),
                      overwrite = F,
                      append = T)
  }

  # quick QA (row counts) ----
  # results
  sqlcount_results <- DBI::dbGetQuery(conn = CHI_db_cxn,
                                      glue::glue_sql("SELECT newcount = count(*) FROM [PHExtractStore].[APDE{DBI::SQL(schema_suffix)}].[{DBI::SQL(table_name)}_results]
                         WHERE indicator_key IN ({paste(unique(CHIestimates$indicator_key))*})", .con = CHI_db_cxn))$newcount
  if(sqlcount_results != nrow(CHIestimates)){
    stop("\n\U0001f47f For the indicator_keys in CHIestimates, the number of rows in SQL does not match the number of rows in CHIestimates. In other words, your upload failed ... try again")
  }

  # metadata
  sqlcount_metadata <- DBI::dbGetQuery(conn = CHI_db_cxn,
                                       glue::glue_sql("SELECT newcount = count(*) FROM [PHExtractStore].[APDE{DBI::SQL(schema_suffix)}].[{DBI::SQL(table_name)}_metadata]
                         WHERE indicator_key IN ({paste(unique(CHImetadata$indicator_key))*})", .con = CHI_db_cxn))$newcount
  if(sqlcount_metadata != nrow(CHImetadata)){
    stop("\n\U0001f47f For the indicator_keys in CHImetadata, the number of rows in SQL does not match the number of rows in CHImetadata. In other words, your upload failed ... try again")
  }

  # return----
  message(paste0("\n\U0001f389Congratulations!\U0001f973\nYour data were successfully pushed to the SQL server!\n",
                 "The result are here: [", complete_servername, "].[PHExtractStore].[APDE", schema_suffix, "].[", table_name, "_results]\n",
                 "The metadata are here: [", complete_servername, "].[PHExtractStore].[APDE", schema_suffix, "].[", table_name, "_metadata]\n"))


}
