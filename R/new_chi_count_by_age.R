#' Title
#'
#' @param ph.data
#' @param ph.instructions
#' @param source_date
#'
#' @return
#' @export
#'
#' @examples
chi_count_by_age <- function(ph.data = NULL,
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
