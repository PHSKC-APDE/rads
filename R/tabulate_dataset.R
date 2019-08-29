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
#' @param data
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
tabulate_variable <- function(data, ...){
  UseMethod('tabulate_dataset', data)
}


#' Tabulate data from HYS
#'
#' @param data tab_svy. HYS dataset
#' @param variable Character vector. Variable(s) to be tabulated according to the subsetting/grouping instructions implied by the other variables
#' @param metrics Character vector. Metrics to be returned. Options include, mean, lower, upper, median, numerator, and denominator. Default is all.
#' @param sex  character vector. One of 'sep' or 'combo'. 'sep' returns results for each sex (at birth-- meaning male or female) while 'both' will provide a both sex estimate
#' @param grade numeric vector or a list of numeric vectors. Determines how grades will be grouped. See details for how to structure the input. Default is all grades.
#' @param region Character vector or a list of character vectors. Determines how regions will be grouped. See details for how to structure the input. Default is all regions (e.g. King County).
#' @param race character vector or a list of character vectors. See details for how to structure the input. Special conditions apply for "alone or in combination" (aic) inputs.
#'             aic inputs cannot be combined with inputs that imply mutually exclusive race categories. Computing metrics for aic races should occur seperately from mutually exclusive race calculations.
#' @param year numeric vector or a list of numeric vectors. See details for how to structure the input.
#' @param sexual_orientation character vector or a list of character vectors. See details for how to structure the input.
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
#' Otherwise, a name will be inferred from the column name and values.
#'
#'
#' @return a data.table containing the results of the tabulation.
#' @export
#'
#' @examples
#'
#'
tabulate_variable.apde_hys <- function(data, variable,
                                       metrics = c('mean', 'lower', 'upper', 'median', 'numerator', 'denominator'),
                                       sex = 'both',
                                       grade = list(6,8,10,12),
                                       region = list(`King County` = c('North', 'South', "East", 'Seattle')),
                                       race = "all",
                                       year = 2018,
                                       sexual_orientation = 'all'){

}
