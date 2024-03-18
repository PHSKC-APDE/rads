#' Title
#'
#' @param ph.data
#' @param ph.instructions
#' @param rate
#' @param rate_per
#' @param small_num_suppress
#' @param suppress_low
#' @param suppress_high
#' @param source_name
#' @param source_date
#'
#' @return
#' @export
#'
#' @examples
chi_calc <- function(ph.data = NULL,
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
