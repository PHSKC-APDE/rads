
#' Get (micro)data from APDE storage.
#'
#'
#' @param dataset Character vector of length 1. Identifies the dataset to be fetched. Use \code{list_apde_data} for available options
#' @param cols Character vector of length >-1. Identifies which columns should be returned. NA returns all columns in the analytic dataset.
#'     See \code{\link{list_dataset_columns}} for more information on which columns are considered default by dataset.
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
#'
#' @param dataset Character vector of length 1. Identifies the dataset to be fetched. Use \code{list_apde_data} for available options
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
get_data_hys <- function(dataset, cols = NA, year = 2018, weight_variable = 'kcfinalwt'){

}

#' Get Birth microdata from storage.
#'
#' @param cols Character vector of length >=1. Identifies which columns should be returned. NA returns all columns in the analytic dataset.
#'     See \code{\link{list_dataset_columns}} for more information on which columns are considered default by dataset.
#' @param year Numeric vector. Identifies which years of data should be pulled
#' @param kingco logical. Return dataset for analyses where mother's residence is in King County only.
#'
#' @return dataset either data.table (adminstrative data) for further analysis/tabulation
#'
#' @importFrom data.table ':=' .SD setcolorder
#' @importFrom odbc dbConnect odbc dbDisconnect
#' @importFrom glue glue_sql
#' @importFrom DBI dbGetQuery
#' @export
#'
#' @examples
#'
#' \dontrun{
#'  get_data_birth(cols = NA, year = c(2015, 2016, 2017), kingco = F)
#' }
get_data_birth <- function(cols = NA, year = c(2017),  kingco = T){

  #data.table bindings
  ..string.columns <- NULL

  # pull columns and years from sQL
  ifelse(is.na(cols), cols <- "*", cols <- paste(cols, collapse=", "))

  query.string <- glue::glue_sql ("SELECT ",  cols, " FROM [PH_APDEStore].[final].[bir_wa]
                                   WHERE chi_year IN (",  paste(year, collapse=", "), ")")

  if(kingco == T){query.string <- glue:: glue_sql (query.string, " AND chi_geo_kc = 1")}


  con <- odbc::dbConnect(odbc::odbc(),
                         Driver = "SQL Server",
                         Server = "KCITSQLPRPDBM50",
                         Database = "PH_APDEStore")

  dat <- data.table::setDT(DBI::dbGetQuery(con, query.string))
  odbc::dbDisconnect(con)

  # Format string variables due to SQL import quirks
  original.order <- names(dat)
  string.columns <- sapply(dat,is.character) # identify string columns as a logical vector
  string.columns <- names(dat[, ..string.columns]) # identify string columns as a character vector
  dat <- dat[, (string.columns) := lapply(.SD, trimws,which="r"), .SDcols = string.columns] # trim white space to right
  dat <- dat[, (string.columns) := lapply(.SD, factor), .SDcols = string.columns] # convert strings to factors

  # reorder table
  data.table::setcolorder(dat, original.order)

  return(dat)
}
