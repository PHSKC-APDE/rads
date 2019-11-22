#Open questions
# 1) How to express the difference between DT[, .N, by = 'year'] and DT[year %in% c(2016, 2018), .N] (that is, year combinations vs individual years)
#
#
#2) How to handle complex by conditions? The equivalent of DT[, blah, by = c('a','b')].
#3) Should the function support multiple bys? Something like r1 <- DT[, blah, by = 'a']; r2 <- DT[, blah, by = 'b']; r3 <- rbind(r1,r2, fill = T)
#4) For surveys, could it be as simple as something like tab_survy(data, variable) where all the group_bys have been prespecidied and tab_survey just does some loose formating

# #potential approach for survey data
# tab_survey <- function(data, variable){
#   indicator_quo <- enquo(variable)
#   ret <- data %>% summarise(
#     result = survey_mean(!!indicator_quo, na.rm = TRUE, vartype = "se", proportion = T),
#     ci = survey_mean(!!indicator_quo, na.rm = TRUE, vartype = "ci", proportion = T),
#     numerator = unweighted(sum(!!indicator_quo, na.rm = TRUE)),
#     denominator = unweighted(n())) %>%
#     select(-ci) %>%
#     rename(se = result_se,
#            lower_bound = ci_low,
#            upper_bound = ci_upp)
#
#   return(ret)
# }


#' Title
#'
#' @param dat tbl_dat object
#' @param ... additional arguments passed to methods
#'
#' @return data.table with the results of the tabulation
#' @export
#'
#' @examples
#'
#' \dontrun{
#'  d <- get_data_hys()
#'  res <- tabulate_variable(d, variable = 'ecig_vape')
#' }
#'
#'
tabulate_variable<- function(dat, ...){
  UseMethod('tabulate_variable', dat)
}

#' Tabulate data from HYS
#'
#' @param dat data.table. Birth dataset
#' @param variable Character vector. Variable(s) to be tabulated according to the subsetting/grouping instructions implied by the other variables
#' @param metrics Character vector. Metrics to be returned. Options include, mean, lower, upper, median, numerator, and denominator. Default is all.
#' @param age  numeriuc vector. Each number defines the beginning of an interval.  
#' @param sex  character vector. One of 'both' or 'seperate'. 'seperate' returns results for each sex (at birth-- meaning male or female) while 'both' will provide a both sex estimate
#' @param region Character vector or a list of character vectors. Determines how regions will be grouped. See details for how to structure the input. Default is all regions (e.g. King County).
#' @param race character vector or a list of character vectors. Three standard options exist: 'all', 'all-NH' and 'aic'.
#'             With 'all', each race is computed seperately, with the caveat that the multiple race category includes hispanic
#'             'All-NH' is like 'all', but the multiple race category is also non-hispanic/
#'             'aic' indicates that the rest of the instructions will be looped over, once per aic race class.
#'             Instructions passed in the list format (or as character vectors length > 1) described below will be checked to ensure no mismatch between aic and non-aic designations.
#' @param race_var Character vector of length 1. Only used when the `race` argument is passed a list as described in details
#' @param year numeric vector or a list of numeric vectors. See details for how to structure the input.
#' @param sexual_orientation character vector or a list of character vectors. See details for how to structure the input.
#' @param na.rm logical. Removes rows with NA by variables from the results.
#' @param proportion logical. Is the metric being computed a proportion or a percentage?
#' @param ... unused
#' @details
#'
#' Function arguments, `grade`, `region`, `race`, `year`, and `sexual_orientation`, largely exist to standardize `by` type of operations.
#' Unless otherwise noted in the parameter-specific descriptions, a vector of a certain type or a (named) list of those vectors are expected-- and vectors will be converted into a list of length 1.
#' Each item of the list will be grouped together. For example, passing \code{list(8, c(10,12))} as the response to the `grade` argument indicates that the tabulation will return
#' results (e.g. seperate rows) for 8th graders and then 10th & 12th graders combined. There should also be no duplicate values or grouping.
#' For example, passing \code{list(8, c(8,10,12))} as a response to the `grade` argument will throw an error because the value 8 appears more than once.
#' If the input is named (e.g. \code{list(`Middle Schoolers` = c(8,10), `High Schoolers` = c(10, 12))}), the resulting tabulation will inherit that naming.
#' Otherwise, a name will be inferred from the column name and values. Passing \code{NULL} will default to grouping by all non-NA values in the underlying variable.
#'
#'
#' @return a data.table containing the results of the tabulation.
#' @export
#'
#' @importFrom rlang !!
#' @importFrom srvyr mutate
#' @importFrom stats na.omit setNames
#'
tabulate_variable.birth <- function(dat, 
                                       variable,
                                       metrics = c('mean', 'lower', 'upper', 'numerator', 'denominator'),
                                       age = 'all',
                                       sex = 'both',
                                       race = 'all-NH',
                                       race_var = 'chi_race_eth8', # what is the point of this?
                                       geo_var = 'chi_geo_hra_short', # limit to one geography at a time?
                                       geo_level = 'all', 
                                       year = 2017,
                                       na.rm = T,
                                       proportion = T,
                                       ...){

  # test data
  dat <- get_data_birth(year=c(2015:2017))
  variable <- c("bw_low")
  metrics = c('mean', 'lower', 'upper', 'numerator', 'denominator')
  age <- c(13,18,25,35,44,45)
  sex <- "both"
  race <- "all-NH"
  race_var <- 'chi_race_eth8'
  geo_var <- 'chi_geo_regions_4'
  geo_level <- 'all'
  geo_level <- list(Seattle = "Seattle", "Other" = c("North", "South", "East"))
  year <- 2017
  na.rm = T
  proportion = T
  
  
  #confirm that variable is in the dataset ----
  if(!all(variable %in% names(dat))) 
    stop(paste0("The following are not valid variable names: ", paste(setdiff(variable, names(dat)), collapse = ", ") ) )
  
  #confirm metrics are valid ----
  if(!all(metrics %in% survey_metrics()))
    stop(paste0(paste(metrics[!metrics %in% survey_metrics()], collapse = ', '), ' is/are not valid metrics'))
  
  #confirm age range is valid & create age bins ----
  if(age[1]=='all'){age <- c(min(dat$chi_age, na.rm = T) : max(dat$chi_age, na.rm = T))}
  if(min(age) < min(dat$chi_age, na.rm = T) | max(age) > max(dat$chi_age, na.rm = T) )
    stop(paste0("The age range falls outside the minimum (", min(dat$chi_age, na.rm = T), ") and maximum (", max(dat$chi_age, na.rm = T), ") ages in the data"))
  
  age_bins <- age
  if(max(age) != max(dat$chi_age, na.rm = T)){age_bins <- c(age_bins, max(dat$chi_age, na.rm = T))}
  dat[, age_bin := cut(chi_age, breaks = age_bins, right = F)]
  dat[, age_bin := gsub("\\[", "", age_bin)][, age_bin := gsub("\\)", "", age_bin)] # format = ##,##
  dat[as.integer(gsub('.*,', "", age_bin)) != max(dat$chi_age, na.rm = T), age_bin := paste0(gsub(",.*$", "", age_bin), ",", (as.integer(gsub('.*,', "", age_bin)) - 1) )] # make the actual cut points more clean
  dat[gsub(",.*$", "", age_bin) == gsub('.*,', "", age_bin), age_bin := gsub(",.*$", "", age_bin)]  # When span is just 1 year, keep first year

  #confirm sex is valid option ----
  if(!is.list(sex)){
    sex = match.arg(sex, c('both', 'separate'))
    if(sex == 'seperate'){
      sex = list(Male = 'Male', Female = "Female")
    }else{
      sex = list(Both = c("Male", "Female"))
    }
  }

  sex = validate_list_input(sex, values = dat[['chi_sex']], variable_name = 'chi_sex')

  #validate race ----
  if(length(race) == 1){
    race = match.arg(race, c('all', 'all-NH'))
    race_var = ifelse(race == 'all', 'chi_race_7', 'chi_race_eth8')
    race = switch(race,
                  all = unique(dat$chi_race_7),
                  `all-NH` = unique(dat$chi_race_eth8))
    race = stats::setNames(lapply(race, function(x) as.character(x)), race)
  }
  
  #validate geographies ----
  #confirm there is only one geography variable
    if(length(geo_var) != 1)
    {stop(paste0("Select *only one* of the following: ", paste(grep('chi_geo', names(dat), value = T), collapse=', ') ) )}
  
  #confirm geography variable exists
    if(!geo_var %in% grep("chi_geo", names(dat), value = T))
      {stop(paste0("'", geo_var, "' is not a valid geography variable. Choose from the following: ", paste(grep('chi_geo', names(dat), value = T), collapse=', ') ))}
  
  #validate geo_level
    if(geo_level[1]=='all'){geo_level <- setdiff(unique(dat[[geo_var]]), NA)}
  
    geo_level = validate_list_input(geo_level, values = dat[[geo_var]], variable_name = 'geo_level', prefix = geo_var)
  
  #validate year ----
  year = validate_list_input(year, values = dat[['chi_year']], variable_name = 'chi_year', prefix = 'Year Group')
  
  #apply the recodes ----
  dat <- setDT(dat %>% dplyr::mutate(.Age = age_bin, 
                               .Sex = apply_instructions(chi_sex, !!sex),
                               .Geography = apply_instructions(get(geo_var), !!geo_level),
                               .Year = apply_instructions(year, !!year)))
    
  #convert to symbol
  sss = rlang::sym(variable)

  #Tabulate the variable
    res = count_tabulate(dat,
                          what = variable,
                          !is.na(!!sss),
                          by = c('.Age', '.Sex', '.Geography', '.Year'),
                          metrics = metrics,
                          proportion = proportion)

  if(na.rm){
    res = na.omit(res, c('.Age', '.Sex', '.Geography', '.Year'))
  }

  return(res)


} # close function




