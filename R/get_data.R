
#' Get (micro)data from APDE storage.
#'
#' @description
#'
#' @param dataset. Character vector of length 1. Identifies the dataset to be fetched. Use \code{list_apde_data} for available options
#' @param cols Character vector of length >-1. Identifies which columns should be returned. NA returns all columns in the analytic dataset.
#'     See \code{\link{list_analytic_columns}} for more information on which columns are considered default by dataset.
#' @param year Numeric vector. Identifies which years of data should be pulled
#' @param ... Additional named arguments based on the specific dataset. To see what these options should be, do \code{help(get_data_`dataset`)}
#'
#'
#' @return dataset either in data.table (adminstrative data) or svy_tbl (survey data) for further analysis/tabulation
#' @export
#'
#' @examples
#' \dontrun{
#'  get_data(dataset = 'hys', cols = NA, year = c(2016, 2018)
#' }
get_data <- function(dataset, cols = NA, year = 2018, ...){

  #ensure that the requested dataset exists

  #ensure that the years are legit

  #ensure that the selected columns are valid

  #validate ... for the specific dataset function

  #dispatch dataset specific function

  #add dataset to the class list

  #return dataset

}


#' Get HYS microdata from storage.
#'
#' @description
#'
#' @param dataset. Character vector of length 1. Identifies the dataset to be fetched. Use \code{list_apde_data} for available options
#' @param cols Character vector of length >-1. Identifies which columns should be returned. NA returns all columns in the analytic dataset.
#'     See \code{\link{list_analytic_columns}} for more information on which columns are considered default by dataset.
#' @param year Numeric vector. Identifies which years of data should be pulled
#' @param weight_variable Character vector of length 1. Identifies which weight column
#'
#' @return dataset either in data.table (adminstrative data) or svy_tbl (survey data) for further analysis/tabulation
#' @export
#'
#' @examples
#'
#' \dontrun{
#'  get_data_hys(dataset = 'hys', cols = NA, year = c(2016, 2018), weight_variable = 'kcfinalwt')
#' }
get_data.hys <- function(dataset, cols = NA, year = 2018, weight_variable = 'kcfinalwt'){

}

