#Open questions
# 1) How to express the difference between DT[, .N, by = 'year'] and DT[year %in% c(2016, 2018), .N] (that is, year combinations vs individual years)
#
#
#2) How to handle complex by conditions? The equivalent of DT[, blah, by = c('a','b')].
#3) Should the function support multiple bys? Something like r1 <- DT[, blah, by = 'a']; r2 <- DT[, blah, by = 'b']; r3 <- rbind(r1,r2, fill = T)
#4) For surveys, could it be as simple as something like tab_survy(data, variable) where all the group_bys have been prespecidied and tab_survey just does some loose formating

#potential approach for survey data
tab_survey <- function(data, variable){
  indicator_quo <- enquo(variable)
  data %>% summarise(
    result = survey_mean(!!indicator_quo, na.rm = TRUE, vartype = "se", proportion = T),
    ci = survey_mean(!!indicator_quo, na.rm = TRUE, vartype = "ci", proportion = T),
    numerator = unweighted(sum(!!indicator_quo, na.rm = TRUE)),
    denominator = unweighted(n())) %>%
    select(-ci) %>%
    rename(se = result_se,
           lower_bound = ci_low,
           upper_bound = ci_upp)
}


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


tabulate_variable.apde_hys <- function(data, variable, filter = T, by =  ){}
