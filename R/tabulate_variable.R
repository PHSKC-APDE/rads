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
#' @param svy tbl_svy object
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
tabulate_variable <- function(svy, ...){
  UseMethod('tabulate_variable', svy)
}

#' Tabulate data from HYS
#'
#' @param .data tab_svy. HYS dataset
#' @param variable Character vector. Variable(s) to be tabulated according to the subsetting/grouping instructions implied by the other variables
#' @param metrics Character vector. Metrics to be returned. Options include, mean, lower, upper, median, numerator, and denominator. Default is all.
#' @param sex  character vector. One of 'both' or 'seperate'. 'seperate' returns results for each sex (at birth-- meaning male or female) while 'both' will provide a both sex estimate
#' @param grade numeric vector or a list of numeric vectors. Determines how grades will be grouped. See details for how to structure the input. Default is all grades.
#' @param region Character vector or a list of character vectors. Determines how regions will be grouped. See details for how to structure the input. Default is all regions (e.g. King County).
#' @param race character vector or a list of character vectors. Three standard options exist: 'all', 'all-NH' and 'aic'.
#'             With 'all', each race is computed seperately, with the caveat that the multiple race category includes hispanic
#'             'All-NH' is like 'all', but the multiple race category is also non-hispanic/
#'             'aic' indicates that the rest of the instructions will be looped over, once per aic race class.
#'             Instructions passed in the list format (or as character vectors length > 1) described below will be checked to ensure no mismatch between aic and non-aic designations.
#'             The variable underpinning 'all' will be used when non-aic values are present.
#' @param year numeric vector or a list of numeric vectors. See details for how to structure the input.
#' @param sexual_orientation character vector or a list of character vectors. See details for how to structure the input.
#' @param na.rm logical. Removes rows with NA by variables from the results.
#'
#' @details
#' \code{\link{apde_hys_options}} provides an overview of the available valid options/arguments for each function argument.
#'
#' Function arguments, `grade`, `region`, `race`, `year`, and `sexual_orientation`, largely exist to standardize `by` type of operations.
#' Unless otherwise noted in the parameter-specific descriptions, a vector of a certain type or a (named) list of those vectors are expected-- and vectors will be converted into a list of length 1.
#' Each item of the list will be grouped together. For example, passing \code{list(8, c(10,12))} as the response to the `grade` argument indicates that the tabulation will return
#' results (e.g. seperate rows) for 8th graders and then 10th & 12th graders combined. There should also be no duplicate values or grouping.
#' For example, passing \code{list(8, c(8,10,12))} as a response to the `grade` argument will throw an error because the value 8 appears more than once.
#' If the input is named (e.g. \code{list(`Middle Schoolers` = c(8,10), `High Schoolers` = c(10, 12))}), the resulting tabulation will inherit that naming.
#' Otherwise, a name will be inferred from the column name and values. Passing \code{NULL} will default to grouping by all NA values in the underlying variable.
#'
#'
#' @return a data.table containing the results of the tabulation.
#' @export
#'
#' @importFrom rlang !!
#' @importFrom srvyr mutate
#' @examples
#'
#'
tabulate_variable.apde_hys <- function(svy, variable,
                                       metrics = c('mean', 'lower', 'upper', 'numerator', 'denominator'),
                                       sex = 'both',
                                       grade = list(6,8,10,12),
                                       region = list(`King County` = c('North', 'South', "East", 'Seattle')),
                                       race = "all",
                                       year = 2018,
                                       sexual_orientation = list(`Heterosexual (Straight)` = 'Heterosexual (Straight)',
                                                                 `LBG+` = c('Gay or Lesbian', 'Bisexual', 'Something else fits better'),
                                                                 `Not Sure` = 'Questioning/Not Sure'),
                                       na.rm = T){

  #confirm that variable is in the dataset
  var_check <- check_names('variable',  'svy', names(svy$variables), variable)
  if(var_check != "") stop(var_check)

  #confirm metrics are valid
  if(!all(metrics %in% survey_metrics())){
    stop(paste0(paste(metrics[!metrics %in% survey_metrics()], collapse = ', '), 'are not valid metrics'))
  }

  #confirm sex is valid option
  sex = match.arg(sex, c('both', 'seperate'))
  if(sex == 'seperate'){
    sex = list(Male = 'Male', Female = "Female")
  }else{
    sex = list(Both = c("Male", "Female"))
  }
  sex = validate_list_input(sex, values = svy$variables[['a_sex']], variable_name = 'sex')

  #validate grade
  grade = validate_list_input(grade, values = svy$variables[['a_grade']], variable_name = 'grade', prefix = 'Grade')

  #validate region
  region = validate_list_input(region, values = svy$variables[['kc4reg']], variable_name = 'region', prefix = 'Region')

  #validate year
  year = validate_list_input(year, values = svy$variables[['year']], variable_name = 'year', prefix = 'Year Group')

  #validate sexual_orientation
  sexual_orientation = validate_list_input(sexual_orientation, values = svy$variables[['sexual_orientation']], variable_name = 'sexual_orientation', prefix = 'Sexual Orientation')

  #validate race
  if(is.character(race)){
    if(length(race) == 1){
      race = match.arg(race, c('all', 'all-NH', 'aic'))
      race_var = ifelse(race == 'all', 'a_race8', 'raceeth')
      race = switch(race,
                    all = unique(svy$variables$a_race8),
                    `all-NH` = unique(svy$variables$raceeth),
                    aic = grep('_aic', names(svy$variables), value = T))

    }

    #check for the number of aics in the character
    aics = grepl('_aic', race, fixed = T)
    sum_aics = sum(aics)
    if((sum_aics > 0 & sum_aics < length(aics))){
      stop('aic race instructions have been mixed with non-aic instructions. Please fix this.')
    }

    aic_check <- sum_aics == length(aics)
  }else{
    aic_check <- FALSE
  }

  if(!aic_check){
    #This should catch "aic" race variables mixed with other classifications
    race = validate_list_input(race, values = svy$variables[[race_var]], variable_name = 'race', prefix = 'Race')
    new_col = apply_instructions(values = svy$variables[[race_var]], race_var, '.Race', race)
    svy <- svy %>% dplyr::mutate(.Race = new_col)
  }

  #apply the recodes
  svy <- svy %>% dplyr::mutate(.Sex = apply_instructions(a_sex, 'sex', '.Sex', !!sex),
                               .Grade = apply_instructions(a_grade, 'a_grade', '.Grade', !!grade),
                               .Region = apply_instructions(kc4reg, 'kc4reg', '.Region', !!region),
                               .Year = apply_instructions(year, 'year', '.Year', !!year),
                               .Sexual_Orientation = apply_instructions(sexual_orientation, 'sexual_orientation', '.Sexual_Orientation', !!sexual_orientation))

  #convert to symbol
  sss = rlang::sym(variable)

  #Tabulate the variable
  if(aic_check){
    res <- rbindlist(race, function(x){
      svy <- svy %>% mutate(.Race = !!x) %>%
        survey_tabulate(
                      what = variable,
                      kingco == 1, !is.na(!!sss), kcfinalwt > 0,
                      by = c('.Sex', '.Grade', '.Region', '.Year', '.Sexual_Orientation', '.Race'),
                      metric = metrics,
                      proportion = T)


    })

    res = rbindlist(res)

  }else{
    res = survey_tabulate(svy,
                          what = variable,
                          kingco == 1, !is.na(!!sss), kcfinalwt > 0,
                          by = c('.Sex', '.Grade', '.Region', '.Year', '.Sexual_Orientation', '.Race'),
                          metric = metrics,
                          proportion = T)
  }

  if(na.omit){
    res = na.omit(res, c('.Sex', '.Grade', '.Region', '.Year', '.Sexual_Orientation', '.Race'))
  }

  return(res)




}




