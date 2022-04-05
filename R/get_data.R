
#' Get (micro)data from APDE storage.
#'
#' @description Simple front-end for pulling in standard APDE data
#'
#' @param dataset Character vector of length 1. Identifies the dataset to be fetched. Use \code{list_apde_data} for available options
#' @param cols Character vector of length >-1. Identifies which columns should be returned. NULL or NA returns all columns in the analytic dataset.
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
#'  get_data(dataset = 'hys', cols = NULL, year = c(2016, 2018)
#' }
get_data <- function(dataset, cols = NULL, year = 2018, ...){

  f <- match.fun(paste0('get_data_', dataset))
  f(cols = cols, year = year, ...)

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
#' @param cols Character vector of length >-1. Identifies which columns should be returned. NULL or NA returns all columns in the analytic dataset.
#'     See \code{\link{list_dataset_columns}} for more information on which columns are considered default by dataset.
#' @param year Numeric vector. Identifies which years of data should be pulled
#' @param weight_variable Character vector of length 1. Identifies which weight column
#' @param kingco logical. Return dataset for analyses in King County only. The only option
#'
#' @return dataset either in data.table (adminstrative data) or svy_tbl (survey data) for further analysis/tabulation
#'
#' @import dtsurvey
#' @importFrom data.table ":=" .I
#' @export
#'
#' @examples
#'
#' \dontrun{
#'  get_data_hys(cols = NULL, year = c(2016, 2018), weight_variable = 'kcfinalwt')
#' }
get_data_hys <- function(cols = NULL, year = c(2021), weight_variable = 'wt_sex_grade_kc', kingco = TRUE){

  stopifnot(all(year %in% c(seq(2004,2018,2), 2021)))

  #J:\HYSdata\hys\2021\v1
  fps = file.path('//PHDATA01/EPE_Data/HYSdata/hys/2021/v1/', paste0('hys_ar_', year, '.rds'))
  dat <- data.table::rbindlist(lapply(fps, readRDS), use.names = T, fill = T)

  #prep the dataset
  dat[is.na(psu), psu := -1 * .I]
  dat[is.na(get(weight_variable)), (weight_variable) := 0]

  #subset by year
  yvar = year
  dat = dat[chi_year %in% yvar, ]

  #identify invalid columns
  if(!is.null(cols) && !all(is.na(cols))){
    invalid.cols <- setdiff(cols, names(dat))
    if(length(invalid.cols) == length(cols)){stop("HYS data cannot be extracted because no valid column names have been submitted. To get all columns, use the argument 'cols = NA'")}
    if(length(invalid.cols) > 0){message(paste0("The following column names do not exist in the HYS data and have not be extracted: ", paste0(invalid.cols, collapse = ", ")))}

  }else{
    cols = names(dat)
  }

  #create the survey object
  if(kingco == T){
    dat <- dat[chi_geo_kc == 1,]
  }else{
    warning('Survey will be set to self-weighting so that rows outside of KC do not get dropped for having weights of 0')
    dat[, weight1 := 1]
    weight_variable = 'weight1'
  }

  dat = dat[get(weight_variable)>0]
  svy <- dtsurvey::dtsurvey(dat, psu = 'psu', strata = 'chi_year', weight = weight_variable, nest = T)


  if(!all(is.na(cols))) svy <- svy[, .SD, .SDcols = c(cols, '_id')]

  return(svy)
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
#' @import data.table
#' @export
#'
#' @examples
#'
#' \dontrun{
#'  get_data_birth(cols = NA, year = c(2015, 2016, 2017), kingco = F)
#' }
get_data_birth <- function(cols = NA, year = c(2017),  kingco = T){
  if(is.null(cols)) cols <- NA

  # get list of all colnames from SQL
    con <- odbc::dbConnect(odbc::odbc(),
                           Driver = "SQL Server",
                           Server = "KCITSQLPRPDBM50",
                           Database = "PH_APDEStore")
    birth.names <- names(DBI::dbGetQuery(con, "SELECT top (0) * FROM [PH_APDEStore].[final].[bir_wa]"))
    birth.years <- unique(DBI::dbGetQuery(con, "SELECT DISTINCT chi_year FROM [PH_APDEStore].[final].[bir_wa]")$chi_year)

  # identify columns and years to pull from SQL
    if(!all(is.na(cols))){
      invalid.cols <- setdiff(cols, birth.names)
      valid.cols <- intersect(birth.names, cols)
      if(length(valid.cols) > 0){cols <- paste(valid.cols, collapse=", ")}
      if(length(valid.cols) == 0){stop("Birth data cannot be extracted because no valid column names have been submitted. To get all columns, use the argument 'cols = NA'")}
      if(length(invalid.cols) > 0){message(paste0("The following column names do not exist in the birth data and have not be extracted: ", paste0(invalid.cols, collapse = ", ")))}
    }
    if(all(is.na(cols))){cols <- "*"}

    invalid.year <- setdiff(year, birth.years)
    year <- intersect(year, birth.years)
    if(length(year) == 0){stop(paste0("Birth data cannot be extracted because no valid years have been provided. Valid years include: ", paste0(birth.years, collapse = ", ")))}
    if(length(invalid.year)>0){message(paste0("The following years do not exist in the birth data and have not be extracted: ", paste0(invalid.year, collapse = ", ")))}

  # pull columns and years from SQL
  query.string <- glue:: glue_sql ("SELECT ",  cols, " FROM [PH_APDEStore].[final].[bir_wa]
                                   WHERE chi_year IN (",  paste(year, collapse=", "), ")")

  if(kingco == T){query.string <- glue:: glue_sql (query.string, " AND chi_geo_kc = 1")}

  dat <- data.table::setDT(DBI::dbGetQuery(con, query.string))
  odbc::dbDisconnect(con)

  # Format string variables due to SQL import quirks
  original.order <- names(dat)
  sql_clean(dat, stringsAsFactors = TRUE) # clean random white spaces and change strings to factors


  # reorder table
  setcolorder(dat, original.order)

  setDT(dat) # set it as a data.table again b/c otherwise, ascribing the new class above makes a copy

  dat = dtsurvey::dtadmin(dat, FALSE)

  return(dat)
}
