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
#' @references \url{https://github.com/PHSKC-APDE/rads/wiki/get_data}
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
#' @param version version of the HYS dataset to pull. Defaults to best. Don't change unless you know what you are doing.
#' @param ar logical. Whether to pull from the analytic ready dataset. FALSE will load stage data
#' @return dataset either in data.table (administrative data) or svy_tbl (survey data) for further analysis/tabulation
#'
#' @import dtsurvey
#' @importFrom data.table ":=" .I
#' @export
#'
#' @examples
#'
#' \dontrun{
#'  get_data_hys(cols = NULL, year = c(2016, 2018), weight_variable = 'wt_sex_grade_kc')
#' }
get_data_hys <- function(cols = NULL, year = c(2021), weight_variable = 'wt_sex_grade_kc', kingco = TRUE, version = 'best', ar = TRUE){

  chi_geo_kc <- weight1 <- psu <- chi_year <- NULL

  stopifnot(all(year %in% c(seq(2004,2018,2), 2021)))

  #J:\HYSdata\hys\2021\v1
  if(ar){
    fps = file.path('//PHDATA01/EPE_Data/HYSdata/hys/2021/',version, '/', paste0('hys_ar_', year, '.rds'))
  }else{
    fps = file.path('//PHDATA01/EPE_Data/HYSdata/hys/2021/',version, '/', paste0('hys_stage_', year, '.rds'))
  }

  dat <- data.table::rbindlist(lapply(fps, readRDS), use.names = T, fill = T)

  #identify invalid columns
  if(!is.null(cols) && !all(is.na(cols))){
    cols = tolower(cols)
    invalid.cols <- setdiff(cols, names(dat))
    if(length(invalid.cols) == length(cols)){stop("HYS data cannot be extracted because no valid column names have been submitted. To get all columns, use the argument 'cols = NA'")}
    if(length(invalid.cols) > 0){message(paste0("The following column names do not exist in the HYS data and have not be extracted: ", paste0(invalid.cols, collapse = ", ")))}

  }else{
    cols = names(dat)
  }

  #create the survey object
  if(kingco == TRUE){
    dat <- dat[chi_geo_kc == 1,]
  }else{
    warning('Survey will be set to self-weighting so that rows outside of KC do not get dropped for having weights of 0')
    dat[, weight1 := 1]
    weight_variable = 'weight1'
  }
  if(!ar){
    warning('Requested staged data. This dataset does not have weights. Survey set to be self weighting')
    dat[, weight1 := 1]
    weight_variable = 'weight1'
  }

  #prep the dataset
  dat[is.na(psu), psu := -1 * .I]
  dat[is.na(get(weight_variable)), (weight_variable) := 0]

  #subset by year
  yvar = year
  dat = dat[chi_year %in% yvar, ]

  dat = dat[get(weight_variable)>0]
  svy <- dtsurvey::dtsurvey(dat, psu = 'psu', strata = 'chi_year', weight = weight_variable, nest = T)


  if(!all(is.na(cols))) svy <- svy[, .SD, .SDcols = c(cols, '_id')]

  return(svy)
}

#' Get Birth microdata from storage.
#'
#' @param cols Character vector of length >=1. Identifies which columns should be returned. NA returns all columns in the analytic dataset.
#'     See \code{\link{list_dataset_columns}} for more information on which columns are considered default by dataset.
#' @param year Numeric vector. Identifies which year(s) of data should be pulled. Defaults to the most recent year.
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
get_data_birth <- function(cols = NA, year = NA,  kingco = T){
  if(is.null(cols)) cols <- NA
  if(is.null(year)) year <- NA

  # get list of all colnames from SQL
  con <- odbc::dbConnect(odbc::odbc(),
                         Driver = getOption('rads.odbc_version'),
                         Server = "KCITSQLPRPDBM50",
                         Database = "PH_APDEStore",
                         Encrypt = 'yes',
                         TrustServerCertificate = 'yes',
                         Authentication = 'ActiveDirectoryIntegrated',
                         encoding = 'latin1')
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

    if(length(year) == 1 && is.na(year)){
      year = max(birth.years)
      message(paste0("You did not specify a year so the most recent available year, ", max(birth.years), ", was selected for you. Available years include ", format_time(birth.years)))}
    invalid.year <- setdiff(year, birth.years)
    year <- intersect(year, birth.years)
    if(length(year) == 0){stop(paste0("Birth data cannot be extracted because no valid years have been provided. Valid years include: ", format_time(birth.years)))}
    if(length(invalid.year)>0){message(paste0("The following years do not exist in the birth data and have not be extracted: ", format_time(invalid.year)))}

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


#' Get Death microdata from storage.
#'
#' @param cols Character vector of length >=1. Identifies which columns should be returned. NA returns all columns in the analytic dataset.
#'     See \code{\link{list_dataset_columns}} for more information on which columns are considered default by dataset.
#'
#' Default = NA
#' @param year Numeric vector. Identifies which years of data should be pulled. Defaults to the most recent year.
#'
#' Default = most recent year only
#' @param kingco logical. Return dataset for analyses where county of decedent's residence is King County.
#'
#' Default = T
#'
#' @param topcode logical. Do you want to top code chi_age at 100 to match population data?
#'
#' Default = T
#'
#' @return dataset either data.table (adminstrative data) for further analysis/tabulation
#'
#' @import data.table
#' @import DBI
#' @import odbc
#' @import dtsurvey
#' @export
#'
#' @examples
#'
#' \dontrun{
#'  get_data_death(cols = NA, year = c(2019), kingco = T, topcode = F)
#' }
get_data_death <- function(cols = NA, year = NA,  kingco = T, topcode = T){

  chi_age <- date_of_birth <- date_of_death <- age_years <- chi_race_eth7 <- chi_race_6 <-
    chi_race_eth8 <- chi_race_7 <- geo_id_code <- NULL


  # get list of all colnames from SQL
  con <- odbc::dbConnect(odbc::odbc(),
                         Driver = "SQL Server",
                         Server = "KCITSQLUTPDBH51",
                         Database = "PH_APDEStore")
  death.names <- names(DBI::dbGetQuery(con, "SELECT TOP (0) * FROM [PH_APDEStore].[death].[final_analytic]"))
  death.years <- sort(unique(DBI::dbGetQuery(con, "SELECT DISTINCT chi_year FROM [PH_APDEStore].[death].[final_analytic]")$chi_year))

  # identify columns and years to pull from SQL
  if(!all(is.na(cols))){
    invalid.cols <- setdiff(cols, death.names)
    valid.cols <- intersect(death.names, cols)
    if(length(valid.cols) > 0){cols <- glue::glue_sql_collapse(valid.cols, sep=", ")}
    if(length(invalid.cols) > 0){message(paste0("The following column names do not exist in the death data and have not be extracted: ", paste0(invalid.cols, collapse = ", ")))}
    if(length(valid.cols) == 0){stop("Death data cannot be extracted because no valid column names have been submitted. To get all columns, use the argument 'cols = NA'")}
  }
  if(all(is.na(cols))){cols <- glue::glue_sql_collapse("*", sep = ', ')}

  if(length(year) == 1 && is.na(year)){
    year = max(death.years)
    message(paste0("You did not specify a year so the most recent available year, ", max(death.years), ", was selected for you. Available years include ", format_time(death.years)))}

  invalid.year <- setdiff(year, death.years)
  year <- intersect(year, death.years)
  if(length(year) == 0){stop(paste0("Death data cannot be extracted because no valid years have been provided. Valid years include: ", format_time(death.years)))}
  if(length(invalid.year)>0){message(paste0("The following years do not exist in the death data and have not been extracted: ", format_time(invalid.year)))}

  # pull columns and years from SQL
  validyears <- glue::glue_sql_collapse(year, sep=", ")
  query.string <- glue:: glue_sql ("SELECT {cols} FROM [PH_APDEStore].[death].[final_analytic]
                                   WHERE chi_year IN ({validyears})", .con = con)

  if(kingco == T){query.string <- glue:: glue_sql (query.string, " AND chi_geo_kc = 1")}

  dat <- data.table::setDT(DBI::dbGetQuery(con, query.string))

  datevars <- DBI::dbGetQuery(con, "SELECT ColumnName FROM [PH_APDEStore].[death].[crosswalk_fields] WHERE ColumnType = 'Date'")[]$ColumnName
  datevars <- intersect(datevars, names(dat)) # names of all date variables that are in actual dataset

  odbc::dbDisconnect(con)

  # Format string variables due to SQL import quirks
  original.order <- names(dat)

  if(length(datevars > 0)){
    for(i in 1:length(datevars)){
      dat[, datevars[i] := as.Date(as.character(get(datevars[i])))]
    }
  }

  if( 'chi_age' %in% cols | 'chi_age' %in% names(dat) ){
    dat[chi_age < 0, chi_age := NA] # cannot have a negative age (due to 9999 as year of birth)
    if(topcode == T){
      dat[chi_age > 100, chi_age := 100] # top code to 100 to match population data
    }
  }

  sql_clean(dat, stringsAsFactors = FALSE) # clean random white spaces and change strings to factors

  # label race_ethnicity data
  if( 'chi_race_eth7' %in% cols | 'chi_race_eth7' %in% names(dat) ){
    dat[, chi_race_eth7 := factor(chi_race_eth7,
                                levels = c(2, 1, 5, 7, 8, 6, 3),
                                labels = c("Black", "White", "Multiple", "Asian", "NHPI", "Hispanic", "AIAN"))]
  }
  if( 'chi_race_eth8' %in% cols | 'chi_race_eth8' %in% names(dat) ){
    dat[, chi_race_eth8 := factor(chi_race_eth8,
                                  levels = c(2, 1, 5, 7, 8, 6, 3, 9),
                                  labels = c("Black", "White", "Multiple", "Asian", "NHPI", "Hispanic", "AIAN", "Oth/unk"))]
  }
  if( 'chi_race_6' %in% cols | 'chi_race_6' %in% names(dat) ){
    dat[, chi_race_6 := factor(chi_race_6,
                                  levels = c(3, 6, 4, 2, 1, 5),
                                  labels = c("Black", "White", "Multiple", "Asian", "AIAN", "NHPI"))]
  }
  if( 'chi_race_7' %in% cols | 'chi_race_7' %in% names(dat) ){
    dat[, chi_race_7 := factor(chi_race_7,
                               levels = c(3, 6, 4, 2, 1, 5, 9),
                               labels = c("Black", "White", "Multiple", "Asian", "AIAN", "NHPI", "Oth/unk"))]
  }

  # reorder table
  setcolorder(dat, original.order)

  setDT(dat) # set it as a data.table again b/c otherwise, ascribing the new class above makes a copy

  dat = dtsurvey::dtadmin(dat, FALSE)

  # return object
  return(dat)
}
