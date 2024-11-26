# get_data() ----
#' Get (micro)data from APDE storage.
#'
#' @description Simple front-end for pulling in standard APDE data
#'
#' @param dataset Character vector of length 1. Identifies the dataset to be
#' fetched. Use \code{list_apde_data} for available options
#' @param cols Character vector of length >=1. Identifies which columns should
#' be returned. NULL returns all columns in the analytic dataset. See
#' \code{\link{list_dataset_columns}} for more information on which columns are
#' considered default by dataset.
#' @param year Numeric vector. Identifies which year(s) of data should be pulled
#' @param ... Additional named arguments based on the specific dataset. To see
#' what these options should be, do \code{help(get_data_`dataset`)}
#'
#'
#' @return Typically a data.table (adminstrative data) or
#' \code{\link[dtsurvey]{dtsurvey}} object (survey data) for further analysis.
#' When requesting HRA or Region data from the BRFSS dataset, it will return an
#' \code{\link[mitools]{imputationList}} comprised of
#' survey-weighted \code{\link[dtsurvey]{dtsurvey}} objects.
#'
#' @export
#' @references \url{https://github.com/PHSKC-APDE/rads/wiki/get_data}
#' @examples
#' \donttest{
#'  test <- get_data(
#'           dataset = 'death',
#'           cols = c('chi_year', 'chi_geo_kc', 'chi_geo_seattle'),
#'           year = c(2021))
#'
#'  head(test)
#' }
get_data <- function(dataset, cols = NULL, year = 2021, ...){

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

# get_data_birth() ----
#' Get Birth microdata from storage.
#'
#' @param cols Character vector of length >=1. Identifies which columns should be returned. NA returns all columns in the analytic dataset.
#'     See \code{\link{list_dataset_columns}} for more information on which columns are considered default by dataset.
#'
#' Default = NA
#'
#' @param year Numeric vector. Identifies which year(s) of data should be pulled. Defaults to the most recent year.
#'
#' Default = most recent year only
#'
#' @param kingco Logical. Return dataset for analyses where mother's residence is in King County only.
#'
#' Default = T
#'
#' @param version Character vector of length 1. Either 'final' or 'stage'.
#'
#' Default = 'final'
#'
#' @param mykey Character vector of length 1. Identifies
#' the keyring:: 'service' name that can be used to access the Health & Human Services
#' Analytic Workspace (HHSAW).
#'
#' Default == 'hhsaw'
#'
#' @return a single data.table
#'
#' @import data.table
#' @export
#'
#' @examples
#'
#' \donttest{
#'  test <- get_data_birth(
#'             cols = c('chi_year', 'chi_geo_kc', 'chi_sex'),
#'             year = c(2019),
#'             kingco = FALSE,
#'             version = 'final',
#'             mykey = 'hhsaw')
#'
#'  head(test)
#' }
get_data_birth <- function(cols = NA,
                           year = NA,
                           kingco = T,
                           version = 'final',
                           mykey = 'hhsaw'){
  if(is.null(cols)) cols <- NA
  if(is.null(year)) year <- NA

  # validate arguments other than mykey ----
    if(!(length(cols) == 1 && is.na(cols))){
      if(!is.character(cols)){stop('\n\U0001f6d1 `cols` must specify a vector of variables or be NA (to get all possible columns).')}
    }
    if(!(length(year) == 1 && is.na(year))){
      if( (!is.numeric(year)) | sum(year%%1) != 0 ) {stop('\n\U0001f6d1 `year` must specify a vector of integers (e.g., c(2017, 2019)) or be NA (to get the most recent year).')}
    }
    if((length(kingco) == 1 && is.na(kingco)) | !is.logical(kingco)){stop('\n\U0001f6d1 `kingco` must be a logical (TRUE | FALSE, or equivalently, T | F).')}
    if(length(version) != 1){stop("\n\U0001f6d1 `version` must have a single value, either 'final' or 'stage'.")}
    if((length(version) == 1 && is.na(version)) | !version %in% c('final', 'stage')){stop("\n\U0001f6d1 `version` must have the value 'final' or 'stage'.")}

  # validate mykey ----
    con <- validate_hhsaw_key(hhsaw_key = mykey)

  # create SQL table name ----
    mysqltable <- DBI::Id(schema = 'birth', table = paste0(version, '_analytic'))

  # get list of all colnames from SQL ----
    birth.names <- tolower(names(DBI::dbGetQuery(con, glue::glue_sql("SELECT top (0) * FROM  {`mysqltable`}", .con = con))))
    birth.years <- unique(DBI::dbGetQuery(con, glue::glue_sql("SELECT DISTINCT chi_year FROM {`mysqltable`}", .con = con))$chi_year)

  # identify columns and years to pull from SQL ----
    cols <- tolower(cols)
    if(!all(is.na(cols))){
      invalid.cols <- setdiff(cols, birth.names)
      valid.cols <- intersect(birth.names, cols)
      if(length(valid.cols) > 0){cols <- glue::glue_sql_collapse(valid.cols, sep=", ")}
      if(length(valid.cols) == 0){stop("Birth data cannot be extracted because no valid column names have been submitted. To get all columns, use the argument 'cols = NA'")}
      if(length(invalid.cols) > 0){message(paste0("The following column names do not exist in the birth data and have not been extracted: ", paste0(invalid.cols, collapse = ", ")))}
    }
    if(all(is.na(cols))){cols <- "*"}

      if(length(year) == 1 && is.na(year)){
        year = max(birth.years)
        message(paste0("You did not specify a year so the most recent available year, ", max(birth.years), ", was selected for you. Available years include ", format_time(birth.years)))}
      invalid.year <- setdiff(year, birth.years)
      year <- intersect(year, birth.years)
      if(length(year) == 0){stop(paste0("Birth data cannot be extracted because no valid years have been provided. Valid years include: ", format_time(birth.years)))}
      if(length(invalid.year)>0){message(paste0("The following years do not exist in the birth data and have not been extracted: ", format_time(invalid.year)))}

  # pull columns and years from SQL ----
    validyears <- glue::glue_sql_collapse(year, sep=", ")

    if(kingco == T){
        kco_sub <- SQL(" AND chi_geo_kc = 'King County'")
    }else{
        kco_sub = SQL('')
      }

    query.string <- glue_sql('select {DBI::SQL(cols)} from {`mysqltable`} where chi_year in ({`validyears`*}) {kco_sub}', .con = con)



    dat <- data.table::setDT(DBI::dbGetQuery(con, query.string))

  # Format string variables due to SQL import quirks ----
    original.order <- names(dat)
    string_clean(dat, stringsAsFactors = TRUE) # clean random white spaces and change strings to factors

  # reorder table----
    setcolorder(dat, original.order)

    setDT(dat) # set it as a data.table again b/c otherwise, ascribing the new class above makes a copy

  # return object ----
    return(dat)
}

# get_data_brfss() ----
#' Get BRFSS microdata with adjusted weights from storage.
#'
#' @description
#' Retrieves Behavioral Risk Factor Surveillance System (BRFSS) data
#' and adjusts survey weights to ensure accurate representation for multi-year
#' analysis.
#'
#' @param cols Character vector specifying which columns to include in the
#' returned data. If NULL, all columns identified by
#' \code{list_dataset_columns('brfss')} will be included. Defaults to
#' \code{cols = NULL}
#' @param year Integer vector specifying which years to include in the data.
#' If NULL, the most recent year available in the data set will be used. Defaults
#' to \code{year = NULL}
#' @param wt_method Character string specifying the name of the method used
#' to rescale the weights when selecting multiple years. Options include:
#'
#' - '\code{obs}': Rescales weights based on the number of observations per year.
#' This is WA DOH's recommendation
#' - '\code{pop}': Rescales weights by the survey weighted population for each year
#' - '\code{simple}': Rescales weights uniformly by the number of surveys. Use
#' when the survey years have approximately the same sample sizes
#'
#'  Defaults to '\code{obs}'
#'
#' @details
#' Note that while \code{get_data_brfss} automatically creates multi-year weights
#' for all years included in the data download, these weights may not be
#' appropriate for all analyses. Some BRFSS questions are only asked in specific
#' years, requiring custom weights to be calculated for those specific time
#' periods. Please refer to \code{\link{pool_brfss_weights}}
#' to learn how to easily re-weight and survey set the data.
#'
#' As stated in the \bold{Value} section below, this function will return a
#' \code{\link[mitools]{imputationList}} when selecting King County Health
#' Reporting Area (HRA) or region variables. This
#' is necessary because BRFSS is provided at the ZIP code level and ZIP codes
#' do not nest perfectly within HRAs (and regions are defined by HRAs). When using
#' a BRFSS \code{\link[mitools]{imputationList}} as the \code{ph.data} argument
#' in \code{\link{calc}}, the function will properly process the
#' \code{\link[mitools]{imputationList}} to account for the uncertainty in
#' allocation of ZIP codes to HRAs. In other words, it is fine if you are
#' unfamiliar with imputation because \code{\link{calc}} will deal with the
#' details for you.
#'
#' @return
#' If '\code{hra20_id}', \code{hra20_name}', and '\code{chi_geo_region}'
#' \emph{\bold{are not requested}}: Returns a survey-weighted
#' \code{\link[dtsurvey]{dtsurvey}}/data.table object with the specified columns,
#' years, and '\code{default_wt}` (the rescaled / adjusted weight).
#'
#' If any of '\code{hra20_id}', '\code{hra20_name}', or '\code{chi_geo_region}'
#' \emph{\bold{are requested}}: Returns an \code{\link[mitools]{imputationList}} comprised of
#' survey-weighted \code{\link[dtsurvey]{dtsurvey}}/data.table objects with the
#' specified columns, years, and '\code{default_wt}` (the rescales / adjusted
#' weight).
#'
#' @references
#' For information regarding the BRFSS ETL process, file locations, etc.,
#' see: \url{https://github.com/PHSKC-APDE/BRFSS}
#'
#' @examples
#' \dontrun{
#' # Get data for specific columns and years
#' brfss_data <- get_data_brfss(
#'   cols = c('chi_sex'),
#'   year = 2019:2022
#' )
#'
#' # Get data for all columns for the most recent year
#' brfss_data <- get_data_brfss()
#' }
#'
#' @import data.table
#' @import mitools
#' @export
#'
get_data_brfss <- function(cols = NULL,
                           year = NULL,
                           wt_method = 'obs'){
  # Visible bindings for data.table/check global variables ----
    chi_year <- finalwt1 <- x_ststr <- hra20_id <- hra20_name <- NULL
    chi_geo_region <- region_name <- NULL

  # Load data ----
    myfile_path <- "//dphcifs/APDE-CDIP/BRFSS/prog_all/final_analytic.rds"
    validate_network_path(myfile_path, is_directory = FALSE)
    dt <- setDT(readRDS(myfile_path))

  # Validate arguments ----
    # Validate the `cols` argument
    if (!is.null(cols)) {
      cols <- unique(c(cols, 'chi_year'))
      impute_cols <- intersect(c('hra20_id', 'hra20_name', 'chi_geo_region'), cols)
      cols <- setdiff(cols, impute_cols)
      invalid_cols <- cols[!cols %in% names(dt)]

      if (length(invalid_cols) > 0) {
        stop(sprintf("\n\U1F6D1 The following columns are not available in the dataset: %s",
                     paste(invalid_cols, collapse = ", ")))
      }
    } else {
      cols <- names(dt)
      impute_cols <- c('hra20_id', 'hra20_name', 'chi_geo_region')
      }

    # Validate the `year` argument
    if (!is.null(year)) {
      # Check if year is numeric or can be converted to integer losslessly
      if (!is.numeric(year) || !all(year == as.integer(year))) {
        stop('\n\U0001f6d1 `year` must specify a vector of integers (e.g., c(2019, 2022)) or be NULL (to get the most recent year).')
      }
      # Check if all years are available in the dataset
      if (!all(year %in% unique(dt$chi_year))) {
        missing_years <- year[!year %in% unique(dt$chi_year)]
        if (length(missing_years) > 0) {
          stop(sprintf("\n\U1F6D1 The following years are not available in the dataset: %s",
                       paste(missing_years, collapse = ", ")))
        }
      }
    } else {year <- max(dt$chi_year)}

    # Validate the `wt_method` argument
    if (!is.character(wt_method) || length(wt_method) != 1 ||
        is.na(wt_method) || !wt_method %in% c("simple", "obs", "pop")) {
      stop("\n\U1F6D1 'wt_method' must be one of: 'obs', 'pop', or 'simple'")
    }

  # Subset the data ----
    dt <- dt[chi_year %in% year]

    if(length(impute_cols) > 0){
        dt <- dt[, unique(c(cols, 'finalwt1', 'x_ststr', grep('hra20_id', names(dt), value = T, ignore.case = T))), with = FALSE]
    } else {dt <- dt[, unique(c(cols, 'finalwt1', 'x_ststr')), with = FALSE]}

  # Adjust weights and survey set ----
    dt <- pool_brfss_weights(
      ph.data = dt,
      years = year,
      year_var = 'chi_year',
      old_wt_var = 'finalwt1',
      new_wt_var = 'default_wt',
      wt_method = wt_method,
      strata = 'x_ststr')

  # Create an imputation list if requesting HRA or region (because xwalked from ZIP codes) ----
    if(length(impute_cols) > 0){
      dt <- lapply(1:10, function(i) {
        temp_dt <- copy(dt)
        temp_dt[, hra20_id := get(paste0('hra20_id_', i))] # Create new hra20_id column from spatagg::assign_cases column
        # temp_dt[, (paste0('hra20_id_', 1:10)) := NULL] # drop the columns hra20_id_1 to hra20_id_10
        temp_dt <- merge(temp_dt,
                         rads.data::spatial_hra20_to_region20[, c('hra20_id', 'hra20_name', 'region_name')],
                         by = 'hra20_id',
                         all.x = T,
                         all.y = F)
        setnames(temp_dt, 'region_name', 'chi_geo_region')
        temp_dt <- temp_dt[, c(setdiff(c('hra20_id', 'hra20_name', 'chi_geo_region'), impute_cols)) := NULL]
        return(temp_dt)
      })

      dt <- mitools::imputationList(dt) # convert generic R list of tables to mitools::imputationList
    }

  # Return object ----
    return(dt)
}

# get_data_chars() ----
#' Get CHARS (Comprehensive Hospital Abstract Reporting System) microdata from storage.
#'
#' @param cols Character vector of length >=1. Identifies which columns should be returned. NA returns all columns in the analytic dataset.
#'     See \code{\link{list_dataset_columns}} for more information on which columns are considered default by dataset.
#'
#' Default = NA
#' @param year Numeric vector. Identifies which years of data should be pulled. Defaults to the most recent year.
#'
#' Default = most recent year only
#' @param kingco logical OR 'zip'.
#'
#' When `kingco = T`,  returns dataset where the patient county is King County -- based on country code.
#'
#' When `kingco = 'zip'`, returns a dataset where the patient county is King County -- based on zip code.
#'
#' Default = T
#'
#' @param wastate logical. Return dataset for Washington State CHARS data only. When FALSE includes Oregon.
#'
#' Default = T
#'
#' @param version Character vector of length 1. Either 'final' or 'stage'.
#'
#' Default = 'final'
#'
#' @param inpatient logical. Return dataset for inpatients only. When FALSE includes observation patients.
#'
#' Default = T
#'
#' @param deaths logical. Return dataset with or without patients who died in the hospital. When TRUE the
#' dataset includes those who died.
#'
#' Default = T
#'
#' @param topcode logical. Do you want to top code chi_age at 100 to match population data?
#'
#' Default = T
#'
#' @param mykey Character vector of length 1. Identifies
#' the keyring:: 'service' name that can be used to access the Health & Human Services
#' Analytic Workspace (HHSAW).
#'
#' Default == 'hhsaw'
#'
#' @return a single data.table
#'
#' @import data.table
#' @import DBI
#' @import odbc
#' @import dtsurvey
#' @export
#'
#' @examples
#'
#' \donttest{
#'  test <- get_data_chars(
#'           cols = c('seq_no', 'chi_age'),
#'           year = c(2020),
#'           kingco = TRUE,
#'           wastate = TRUE,
#'           version = 'final',
#'           inpatient = TRUE,
#'           deaths = FALSE,
#'           topcode = FALSE,
#'           mykey = 'hhsaw')
#'
#'  head(test)
#' }
get_data_chars <- function(cols = NA,
                           year = NA,
                           kingco = T,
                           version = 'final',
                           wastate = T,
                           inpatient = T,
                           deaths = T,
                           topcode = T,
                           mykey = 'hhsaw'){

  chi_age <- date_of_birth <- date_of_chars <- age_years <- chi_race_eth7 <- NULL
  chi_race_6 <- chi_race_eth8 <- chi_race_7 <- geo_id_code <- chi_geo_wastate <- NULL
  chi_sex <- chi_geo_kc <- chi_race_aic_hisp <- yage4 <- age <- age6 <- NULL
  geo_type <- geo_id <- race4 <- race3 <- race3_hispanic <- NULL

  # validate arguments other than mykey ----
    if(!(length(cols) == 1 && is.na(cols))){
      if(!is.character(cols)){stop('\n\U0001f6d1 `cols` must specify a vector of variables or be NA (to get all possible columns).')}
    }
    if(!(length(year) == 1 && is.na(year))){
      if( (!is.numeric(year)) | sum(year%%1) != 0 ) {stop('\n\U0001f6d1 `year` must specify a vector of integers (e.g., c(2017, 2019)) or be NA (to get the most recent year).')}
    }
  if((length(kingco) == 1 && is.na(kingco)) | !(is.logical(kingco) | kingco == 'zip') ){stop('\n\U0001f6d1 `kingco` must be a logical (TRUE | FALSE, or equivalently, T | F).')}
  if((length(wastate) == 1 && is.na(wastate)) | !is.logical(wastate)){stop('\n\U0001f6d1 `wastate` must be a logical (TRUE | FALSE, or equivalently, T | F).')}
  if((length(inpatient) == 1 && is.na(inpatient)) | !is.logical(inpatient)){stop('\n\U0001f6d1 `inpatient` must be a logical (TRUE | FALSE, or equivalently, T | F).')}
  if((length(deaths) == 1 && is.na(deaths)) | !is.logical(deaths)){stop('\n\U0001f6d1 `deaths` must be a logical (TRUE | FALSE, or equivalently, T | F).')}
  if((length(topcode) == 1 && is.na(topcode)) | !is.logical(topcode)){stop('\n\U0001f6d1 `topcode` must be a logical (TRUE | FALSE, or equivalently, T | F).')}

  # Validate mykey ----
    con <- validate_hhsaw_key(hhsaw_key = mykey)

  # create SQL table name ----
    mysqltable <- DBI::Id(schema = 'chars', table = paste0(version, '_analytic'))

  # Get list of all colnames from SQL ----
      chars.names <- tolower(names(DBI::dbGetQuery(con, glue::glue_sql("SELECT TOP (0) * FROM {`mysqltable`}", .con = con))))
      chars.years <- sort(unique(DBI::dbGetQuery(con, glue::glue_sql("SELECT DISTINCT chi_year FROM {`mysqltable`}", .con = con))$chi_year))

  # Identify columns and years to pull from SQL ----
      cols <- tolower(cols)
      if(!all(is.na(cols))){
        # for custom CHI/CHNA vars
          original.cols <- copy(cols)
          var.2.calc <- c()
          if('wastate' %in% cols){cols <- c(cols, 'chi_geo_wastate'); var.2.calc=c(var.2.calc, 'wastate')}
          if('yage4' %in% cols){cols <- c(cols, 'age'); var.2.calc=c(var.2.calc, 'yage4')}
          if('age6' %in% cols){cols <- c(cols, 'age'); var.2.calc=c(var.2.calc, 'age6')}
          if('race3' %in% cols){cols <- c(cols, 'chi_race_6', 'chi_race_aic_hisp'); var.2.calc=c(var.2.calc, 'race3')}
          if('race4' %in% cols){cols <- c(cols, 'chi_race_eth7'); var.2.calc=c(var.2.calc, 'race4')}
          cols <-  unique(setdiff(cols, var.2.calc))
        # generic vars
          invalid.cols <- setdiff(cols, chars.names)
          valid.cols <- intersect(chars.names, cols)
          if(length(valid.cols) > 0){cols <- glue::glue_sql_collapse(valid.cols, sep=", ")}
          if(length(invalid.cols) > 0){message(paste0("The following column names do not exist in the chars data and have not been extracted: ", paste0(invalid.cols, collapse = ", ")))}
          if(length(valid.cols) == 0){stop("chars data cannot be extracted because no valid column names have been submitted. To get all columns, use the argument 'cols = NA'")}
      }
      if(all(is.na(cols))){
        original.cols <- copy(cols)
        cols <- glue::glue_sql_collapse("*", sep = ', ')
        }


      if(length(year) == 1 && is.na(year)){
        year = max(chars.years)
        message(paste0("You did not specify a year so the most recent available year, ", max(chars.years), ", was selected for you. Available years include ", format_time(chars.years)))}

      invalid.year <- setdiff(year, chars.years)
      year <- intersect(year, chars.years)
      if(length(year) == 0){stop(paste0("chars data cannot be extracted because no valid years have been provided. Valid years include: ", format_time(chars.years)))}
      if(length(invalid.year)>0){message(paste0("The following years do not exist in the chars data and have not been extracted: ", format_time(invalid.year)))}

  # Pull columns and years from SQL ----
      validyears <- glue::glue_sql_collapse(year, sep=", ")
      query.string <- glue_sql('select {DBI::SQL(cols)} from {`mysqltable`} where chi_year in ({`validyears`*})', .con = con)

      if(inpatient == T){query.string <- glue:: glue_sql (query.string, " AND STAYTYPE = 1", .con = con)}
      if(deaths == F){query.string <- glue:: glue_sql (query.string, " AND STATUS != 20", .con = con)} # 20 means Expired / did not recover
      if(wastate == T){query.string <- glue:: glue_sql (query.string, " AND chi_geo_wastate = 1", .con = con)}
      if(kingco == T){query.string <- glue:: glue_sql (query.string, " AND chi_geo_kc = 1", .con = con)}
      if(kingco == 'zip'){query.string <- glue:: glue_sql (query.string, " AND chi_geo_kczip = 1", .con = con)}

      dat <- data.table::setDT(DBI::dbGetQuery(con, query.string))
      setnames(dat, tolower(names(dat)))

      odbc::dbDisconnect(con)

  # Top code age (if wanted) ----
      if( 'chi_age' %in% cols | 'chi_age' %in% names(dat) ){
        dat[chi_age < 0, chi_age := NA] # cannot have a negative age (due to 9999 as year of birth)
        if(topcode == T){
          dat[chi_age > 100, chi_age := 100] # top code to 100 to match population data
        }
      }

  # Format string variables due to SQL import quirks ----
      original.order <- names(dat)

      string_clean(dat, stringsAsFactors = F) # clean random white spaces and ensure all factors are strings
      string.vars <- setdiff(names(dat)[sapply(dat, is.character)], c('diag1', 'proc1', 'ecode1'))
      if(length(string.vars) > 0){dat[, (string.vars) := lapply(.SD, tolower), .SDcols = string.vars]}

  # Label race_ethnicity data ----
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

  # Label gender ----
      if( 'chi_sex' %in% cols | 'chi_sex' %in% names(dat) ){
        dat[, chi_sex := factor(chi_sex, levels = 0:1, labels = c("Female", "Male"))]
      }

  # Create custom CHI vars if requested ----
    if('chi_geo_kc' %in% names(dat)){
      dat[, chi_geo_kc := as.character(chi_geo_kc)]
      dat[, chi_geo_kc := fcase(chi_geo_kc=='TRUE', 'King County',
                                default = NA_character_)]
    }
    if('wastate' %in% original.cols || (length(original.cols) == 1 && is.na(original.cols))){
      dat[chi_geo_wastate == TRUE, wastate := 'Washington State']
    }
    if('yage4' %in% original.cols || (length(original.cols) == 1 && is.na(original.cols))){
      dat[, yage4 := fcase(age %in% 0:4, '0-4',
                           age %in% 5:9, '5-9',
                           age %in% 10:14, '10-14',
                           age %in% 15:17, '15-17',
                           default = NA_character_)]
    }
    if('age6' %in% original.cols || (length(original.cols) == 1 && is.na(original.cols))){
      dat[, age6 := fcase(age %in% 0:17, '<18',
                          age %in% 18:24, '18-24',
                          age %in% 25:44, '25-44',
                          age %in% 45:64, '45-64',
                          age %in% 65:74, '65-74',
                          age >= 75, '75+',
                          default = NA_character_)]
    }
    if('race4' %in% original.cols || (length(original.cols) == 1 && is.na(original.cols))){
      dat[, race4 := chi_race_eth7]
    }
    if('race3' %in% original.cols | (length(original.cols) == 1 && is.na(original.cols))){
      dat[, race3 := chi_race_6]
      dat[chi_race_aic_hisp == T, race3_hispanic := 'Hispanic']
    }

  # reorder table ----
      if(!(length(original.cols) == 1 && is.na(original.cols))){
        if('race3_hispanic' %in% names(dat)){
          original.cols <- c(original.cols, 'race3_hispanic')
          }
        dat <- dat[, original.cols, with = F]
        }

      data.table::setDT(dat) # set it as a data.table again b/c otherwise, ascribing the new class above makes a copy

  # return object ----
      return(dat)
}


# get_data_death() ----
#' Get Death microdata from storage.
#'
#' @param cols Character vector of length >=1. Identifies which columns should be returned. NA returns all columns in the analytic dataset.
#'     See \code{\link{list_dataset_columns}} for more information on which columns are considered default by dataset.
#'
#' Default = NA
#' @param year Numeric vector. Identifies which years of data should be pulled. Defaults to the most recent year.
#'
#' Default = most recent year only
#'
#' @param kingco logical. Return dataset for analyses where county of decedent's residence is King County.
#'
#' Default = T
#'
#' @param version Character vector of length 1. Either 'final' or 'stage'.
#'
#' Default = 'final'
#'
#' @param topcode logical. Do you want to top code chi_age at 100 to match population data?
#'
#' Default = T
#'
#' @param mykey Character vector of length 1. Identifies
#' the keyring:: 'service' name that can be used to access the Health & Human Services
#' Analytic Workspace (HHSAW).
#'
#' Default == 'hhsaw'
#'
#' @return a single data.table
#'
#' @import data.table
#' @import DBI
#' @import odbc
#' @import dtsurvey
#' @export
#'
#' @examples
#'
#' \donttest{
#'  test <- get_data_death(
#'           cols = c('chi_year', 'chi_geo_kc', 'chi_sex'),
#'           year = c(2019),
#'           kingco = TRUE,
#'           version = 'final',
#'           topcode = FALSE,
#'           mykey = 'hhsaw')
#'
#'  head(test)
#' }
get_data_death <- function(cols = NA,
                           year = NA,
                           kingco = T,
                           version = 'final',
                           topcode = T,
                           mykey = 'hhsaw'){

  chi_age <- date_of_birth <- date_of_death <- age_years <- chi_race_eth7 <- NULL
  chi_race_6 <- chi_race_eth8 <- chi_race_7 <- geo_id_code <- chi_sex <-  NULL
  age6 <- chi_geo_kc <- chi_geo_wastate <- chi_race_hisp <- geo_id <- geo_type <- NULL
  race3 <- race3_hispanic <- race4 <- wastate <- bigcities <- chi_geo_bigcities <- NULL
  hra20_name <- chi_geo_hra2020_long <- chi_geo_region <- chi_geo_reg2020_name <- NULL

  # Validate arguments other than mykey ----
    if(!(length(cols) == 1 && is.na(cols))){
      if(!is.character(cols)){stop('\n\U0001f6d1 `cols` must specify a vector of variables or be NA (to get all possible columns).')}
    }
    if(!(length(year) == 1 && is.na(year))){
      if( (!is.numeric(year)) | sum(year%%1) != 0 ) {stop('\n\U0001f6d1 `year` must specify a vector of integers (e.g., c(2017, 2019)) or be NA (to get the most recent year).')}
    }
    if((length(kingco) == 1 && is.na(kingco)) | !is.logical(kingco)){stop('\n\U0001f6d1 `kingco` must be a logical (TRUE | FALSE, or equivalently, T | F).')}
    if((length(topcode) == 1 && is.na(topcode)) | !is.logical(topcode)){stop('\n\U0001f6d1 `topcode` must be a logical (TRUE | FALSE, or equivalently, T | F).')}

  # Validate mykey ----
      con <- validate_hhsaw_key(hhsaw_key = mykey)

  # create SQL table name
    mysqltable <- DBI::Id(schema = 'death', table = paste0(version, '_analytic'))

  # Get list of all colnames from SQL ----
      death.names <- tolower(names(DBI::dbGetQuery(con, glue::glue_sql("SELECT TOP (0) * FROM {`mysqltable`}", .con = con))))
      death.years <- sort(unique(DBI::dbGetQuery(con, glue::glue_sql("SELECT DISTINCT chi_year FROM {`mysqltable`}",.con = con))$chi_year))

  # Identify columns and years to pull from SQL ----
      cols <- tolower(cols)
      if(!all(is.na(cols))){
        # for custom CHI/CHNA vars
        original.cols <- copy(cols)
        var.2.calc <- c()
        if('age6' %in% cols){cols <- c(cols, 'chi_age'); var.2.calc=c(var.2.calc, 'age6')}
        if('bigcities' %in% cols){cols <- c(cols, 'chi_geo_bigcities'); var.2.calc=c(var.2.calc, 'bigcities')}
        if('chi_geo_region' %in% cols){cols <- c(cols, 'chi_geo_reg2020_name'); var.2.calc=c(var.2.calc, 'chi_geo_region')}
        if('hra20_name' %in% cols){cols <- c(cols, 'chi_geo_hra2020_long'); var.2.calc=c(var.2.calc, 'hra20_name')}
        if('race3' %in% cols){cols <- c(cols, 'chi_race_6', 'chi_race_hisp'); var.2.calc=c(var.2.calc, 'race3')}
        if('race4' %in% cols){cols <- c(cols, 'chi_race_eth7'); var.2.calc=c(var.2.calc, 'race4')}
        if('wastate' %in% cols){cols <- c(cols, 'chi_geo_wastate'); var.2.calc=c(var.2.calc, 'wastate')}
        cols <-  unique(setdiff(cols, var.2.calc))
        # generic vars
        invalid.cols <- setdiff(cols, death.names)
        valid.cols <- intersect(death.names, cols)
        if(length(valid.cols) > 0){cols <- glue::glue_sql_collapse(valid.cols, sep=", ")}
        if(length(invalid.cols) > 0){message(paste0("The following column names do not exist in the chars data and have not been extracted: ", paste0(invalid.cols, collapse = ", ")))}
        if(length(valid.cols) == 0){stop("chars data cannot be extracted because no valid column names have been submitted. To get all columns, use the argument 'cols = NA'")}
      }

      if(all(is.na(cols))){
        original.cols <- copy(cols)
        cols <- glue::glue_sql_collapse("*", sep = ', ')
        }

      if(length(year) == 1 && is.na(year)){
        year = max(death.years)
        message(paste0("You did not specify a year so the most recent available year, ", max(death.years), ", was selected for you. Available years include ", format_time(death.years)))}

      invalid.year <- setdiff(year, death.years)
      year <- intersect(year, death.years)
      if(length(year) == 0){stop(paste0("Death data cannot be extracted because no valid years have been provided. Valid years include: ", format_time(death.years)))}
      if(length(invalid.year)>0){message(paste0("The following years do not exist in the death data and have not been extracted: ", format_time(invalid.year)))}

  # Pull columns and years from SQL ----
      validyears <- glue::glue_sql_collapse(year, sep=", ")
      query.string <- glue_sql('select {DBI::SQL(cols)} from {`mysqltable`} where chi_year in ({`validyears`*})', .con = con)

      if(kingco == T){query.string <- paste0(query.string, " AND chi_geo_kc = 1")}

      dat <- data.table::setDT(DBI::dbGetQuery(con, query.string))

      datevars <- DBI::dbGetQuery(con, "SELECT ColumnName FROM [death].[crosswalk_fields] WHERE ColumnType = 'Date'")[]$ColumnName
      datevars <- intersect(datevars, names(dat)) # names of all date variables that are in actual dataset

  # Top code age (if wanted) ----
      if( 'chi_age' %in% cols | 'chi_age' %in% names(dat) ){
        dat[chi_age < 0, chi_age := NA] # cannot have a negative age (due to 9999 as year of birth)
        if(topcode == T){
          dat[chi_age > 100, chi_age := 100] # top code to 100 to match population data
        }
      }


  # Format string variables due to SQL import quirks ----
      original.order <- names(dat)

      if(length(datevars > 0)){
        for(i in 1:length(datevars)){
          dat[, datevars[i] := as.Date(as.character(get(datevars[i])))]
        }
      }

      string_clean(dat, stringsAsFactors = FALSE) # clean random white spaces and change strings to factors

  # Label race_ethnicity data ----
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

  # Label gender ----
      if("chi_sex" %in% names(dat)){
        dat[, chi_sex := factor(chi_sex, levels = 0:1, labels = c("Female", "Male"))]
      }

  # Create custom CHI vars if requested ----
      if('bigcities' %in% original.cols || (length(original.cols) == 1 && is.na(original.cols))){
        dat[, bigcities := chi_geo_bigcities]
      }
      if('chi_geo_kc' %in% original.cols || (length(original.cols) == 1 && is.na(original.cols))){
        dat[, chi_geo_kc := as.character(chi_geo_kc)]
        dat[, chi_geo_kc := fcase(chi_geo_kc=='TRUE', 'King County',
                                  default = NA_character_)]
      }
      if('hra20_name' %in% original.cols || (length(original.cols) == 1 && is.na(original.cols))){
        dat[, hra20_name := chi_geo_hra2020_long]
      }
      if('chi_geo_region' %in% original.cols || (length(original.cols) == 1 && is.na(original.cols))){
        dat[, chi_geo_region := chi_geo_reg2020_name]
      }
      if('wastate' %in% original.cols || (length(original.cols) == 1 && is.na(original.cols))){
        dat[chi_geo_wastate == TRUE, wastate := 'Washington State']
      }
      if('age6' %in% original.cols || (length(original.cols) == 1 && is.na(original.cols))){
        dat[, age6 := fcase(chi_age %in% 0:17, '<18',
                            chi_age %in% 18:24, '18-24',
                            chi_age %in% 25:44, '25-44',
                            chi_age %in% 45:64, '45-64',
                            chi_age %in% 65:74, '65-74',
                            chi_age >= 75, '75+',
                            default = NA_character_)]
      }
      if('race4' %in% original.cols || (length(original.cols) == 1 && is.na(original.cols))){
        dat[, race4 := chi_race_eth7]
      }
      if('race3' %in% original.cols | (length(original.cols) == 1 && is.na(original.cols))){
        dat[, race3 := chi_race_6]
        dat[chi_race_hisp == 1, race3_hispanic := 'Hispanic']
      }


  # reorder table ----
      if(!(length(original.cols) == 1 && is.na(original.cols))){
        if('race3_hispanic' %in% names(dat)){
          original.cols <- c(original.cols, 'race3_hispanic')
        }
        dat <- dat[, original.cols, with = F]
      }

      setDT(dat) # set it as a data.table again b/c otherwise, ascribing the new class above makes a copy

  # return object ----
      return(dat)
}

# get_data_hys() ----
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
#' \donttest{
#'  get_data_hys(cols = NULL, year = c(2016, 2018), weight_variable = 'wt_grade_kc')
#' }
get_data_hys <- function(cols = NULL, year = c(2021), weight_variable = 'wt_grade_kc', kingco = TRUE, version = 'best', ar = TRUE){

  colname <- chi_geo_kc <- weight1 <- psu <- chi_year <- NULL

  stopifnot(all(year %in% c(seq(2004,2018,2), 2021, 2023)))

  # pull the list of vars
  vars = file.path('//dphcifs/APDE-CDIP/HYS/releases/',version, 'hys_cols.csv')
  vars = data.table::fread(vars)

  # subset by year
  yyy = year
  vars = vars[year %in% yyy]

  # confirm that the columns requested exist in the dataset
  # and load them if that is the case
  if(!is.null(cols)){
    noexist = setdiff(cols, vars[, colname])
    if(length(noexist)>0){
      stop(paste('The following columns were requested but do not exist for the supplied years:',
                 paste(noexist, collapse = ',')))
    }
  } else{
    if(ar) cols = vars[ar == TRUE, colname]
    if(!ar) cols = vars[ar == FALSE, colname]
  }
  cols = unique(cols)

  #figure out whether to load stage, analytic ready or both
  vars = vars[colname %in% cols]
  arfp = c()
  sfp = c()
  if(any(vars[, ar])){
    arfp = file.path('//dphcifs/APDE-CDIP/HYS/releases/',version, '/', paste0('hys_ar_', year, '.rds'))
    ardat = data.table::rbindlist(lapply(arfp, readRDS), use.names = T, fill = T)

  }
  if(any(!vars[, ar])){
    sfp = file.path('//dphcifs/APDE-CDIP/HYS/releases/',version, '/', paste0('hys_stage_', year, '.rds'))
    sdat = data.table::rbindlist(lapply(sfp, readRDS), use.names = T, fill = T)

  }
  # If both were loaded, merge them
  if(exists('ardat') && exists('sdat')){
    scols = unique(vars[ar == FALSE, colname]) # prevent column duplication
    dat = merge(ardat, sdat[, .SD, .SDcols = c('obs_id', scols)], all.x = T, by = 'obs_id')
  }else if(exists('ardat'))(
    dat = ardat
  )else{
    dat = sdat
  }

  #create the survey object
  if(kingco == TRUE){
    dat <- dat[chi_geo_kc == 1,]
  }else{
    warning('Survey will be set to self-weighting so that rows outside of KC do not get dropped for having weights of 0')
    dat[, weight1 := 1]
    weight_variable = 'weight1'
  }
  if(all(vars[, ar == FALSE])){
    warning('Requested staged data only. This dataset does not have weights. Survey set to be self weighting')
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

  svy <- svy[, .SD, .SDcols = c(cols, '_id')]

  return(svy)
}


# get_data_pums() ----
#' Get PUMS microdata from storage
#'
#' @description
#' Retrieves American Community Survey (ACS) Public Use Microdata Sample (PUMS) data
#' from storage. Can return person-level, household-level, or combined records
#' with appropriate survey weights applied.
#'
#' @param cols Character vector specifying which columns to include in the
#' returned data. If NULL, all columns will be included. Note that survey weight
#' columns (wgtp/pwgtp) and chi_year are always included regardless of selection.
#' Defaults to \code{cols = NULL}
#' @param year Integer vector specifying which years to include in the data.
#' Can be either a single year for 1-year estimates or five consecutive years
#' for 5-year estimates. If NULL, the most recent single year available will be used.
#' Note that 2020 is not available due to COVID-19 pandemic survey disruptions.
#' Defaults to \code{year = NULL}
#' @param kingco Logical indicating whether to restrict the data to King County
#' records only. Defaults to \code{kingco = TRUE}
#' @param records Character string specifying whether to return person-level,
#' household-level, or combined records. Must be one of "person", "household",
#' or "combined". When 'combined' is selected, person and household records are
#' merged using the household identifier (serialno) and survey set for
#' person-level analyses. Defaults to \code{records = 'person'}
#'
#' @details
#' The function automatically applies the appropriate survey weights (person or
#' household) based on the \code{records} parameter. For person-level and
#' combined records, it uses the person weight (pwgtp) and its replicate weights.
#' For household-level records, it uses the household weight (wgtp) and its
#' replicate weights.
#'
#' The function uses the JK1 (jackknife) method for variance estimation with
#' 80 replicate weights, following Census Bureau recommendations for PUMS data.
#'
#' When you select \code{records = "combined"}, household-level variables with
#' the same names as person-level variables are given a '_hh' suffix to
#' distinguish them. You are strongly encouraged to review the Census Bureau's
#' [ACS PUMS documentation](https://www.census.gov/programs-surveys/acs/microdata.html)
#' if you plan to set \code{records = "combined"}.
#'
#' @return
#' Returns a survey-weighted \code{\link[dtsurvey]{dtsurvey}}/data.table object
#' with the specified columns and years that is ready for use with
#' \code{\link{calc}}.
#'
#' @references
#' For information regarding the ACS PUMS ETL process, file locations, data
#' dictionaries, etc., see: \url{https://github.com/PHSKC-APDE/svy_acs}
#'
#' @examples
#' \donttest{
#' # Get person-level data for specific columns from the most recent year
#' pums_person <- get_data_pums(
#'   cols = c("agep", "race4"),
#'   kingco = TRUE
#' )
#'
#' # Get household-level data for a 5-year period
#' pums_households <- get_data_pums(
#'   year = 2018:2022,
#'   records = "household"
#' )
#'
#' # Get combined person-household level data for WA State in 2022
#' pums_combo <- get_data_pums(
#'   year = 2022,
#'   records = "combined",
#'   kingco = FALSE
#' )
#' }
#'
#'
#'
#' @import data.table
#' @import srvyr
#' @import dtsurvey
#' @export
#'
get_data_pums <- function(cols = NULL,
                          year = NULL,
                          kingco = TRUE,
                          records = "person") {
# Visible bindings for data.table/check global variables ----

# Get PUMS file availability ----
baseDir <- "//dphcifs/APDE-CDIP/ACS/PUMS_data/"
if (!dir.exists(baseDir)) {
  stop("\n\U1F6D1 Unable to access directory: ", baseDir, "\n\n",
       "Please verify:\n",
       "1. You're connected to KC network\n",
       "2. You have file permissions")
} else {
  preppedFiles <- grep("\\.RData$", grep("prepped_R_files", list.files(baseDir, full.names = TRUE, recursive = TRUE), value = TRUE), value = TRUE, ignore.case = T)
  if (uniqueN(preppedFiles) == 0){
    stop("\n\U1F6D1 There are no available analytic ready RData files in: ", baseDir, "\n\n",
         "The standard file paths should have the form:\n",
         "* ", baseDir, "YYYY_1_year/prepped_R_files/YYYY_1_year_data.RData\n",
         "* ", baseDir, "YYYY_yyyy_5_year/prepped_R_files/YYYY_yyyy_5_year_data.RData")
  }
}

maxYear = max(as.integer(substr(gsub(baseDir, "", grep("1_year", preppedFiles, value = TRUE)), 1, 4)))
minYear = min(as.integer(substr(gsub(baseDir, "", grep("1_year", preppedFiles, value = TRUE)), 1, 4)))
max5Year = max(as.integer(substr(gsub(baseDir, "", grep("5_year", preppedFiles, value = TRUE)), 1, 4)))
min5Year = min(as.integer(substr(gsub(baseDir, "", grep("5_year", preppedFiles, value = TRUE)), 1, 4)))

# Validate arguments ----
## Validate the `year` argument ----
  if (is.null(year)) {
    year <- maxYear
    useFile <- grep(paste0(year, "_1_year"), preppedFiles, value = TRUE)
  } else {
    # Check if year is numeric or can be converted to integer losslessly & is correct length
    if (!is.numeric(year) || !all(year == as.integer(year)) || !length(year) %in% c(1, 5)) {
      stop("\n\U1F6D1 `year` must be an integer vector with: \n one value (e.g., 2022), ",
           "\n five continuous values (e.g., 2018:2022), or \n NULL for the most recent year.")
    }

    # Check if select years are available
    if (length(year) == 1) {
      if (year == 2020) {
        stop("\n\U1F6D1 `year` cannot equal 2020 due to the COVID-19 pandemic survey disruptions.")
      }
      useFile <- grep(paste0(year, "_1_year"), preppedFiles, value = TRUE)
    } else if (length(year) == 5) {
      if (all(diff(sort(year)) != 1)) {
        stop("\n\U1F6D1 the values of `year` are not continuous.")
      }
      useFile <- grep(paste0(max(year), "_", min(year), "_5_year"), preppedFiles, value = TRUE)
    }

    if (length(useFile) == 0) {
      stop("\n\U1F6D1 The `year` value is invalid.\n",
           "The minimum single year is ", minYear, " and the maximum single year is ", maxYear, ".\n",
           "The minimum five-year period is ", min5Year-4, ":", min5Year, " and the maximum is ", max5Year-4, ":", max5Year, ".")
    } else if (length(useFile) > 1) {
      stop("\n\U1F6D1 The `year` value returned more than one potential folder. Please report this to the rads team.")
    }
  }

## Validate the records argument ----
if (length(records) != 1 || !records %in% c("person", "household", "combined")) {
  stop("\n\U1F6D1 `records` must have the value 'person', 'household', or 'combined'.")
}

### Load data for remaining validation ----
  # function to load without disrupting the global environment
  load_file <- function(file) {
    tryCatch({
      e <- new.env()
      load(file, envir = e)
      return(as.list(e))
    }, error = function(e) {
      stop(paste0("\n\U1F6D1 Failed to load data file: ", file))
    })
  }

  tempdata <- load_file(useFile)
  if (records == "person") {
    dt <- tempdata$person.wa
  } else if (records == "household") {
    dt <- tempdata$household.wa
  } else if (records == "combined") {

    # Ensure serialno exists in both datasets
    if (!all(c("serialno") %in% names(tempdata$person.wa)) ||
        !all(c("serialno") %in% names(tempdata$household.wa))) {
      stop("\n\U1F6D1 Required column 'serialno' missing from person or household data")
    }

    # Merge person and household data
    dt <- merge(
      tempdata$person.wa,
      tempdata$household.wa,
      by = "serialno",
      all.x = TRUE,
      suffixes = c("", "_hh")
    )

    # Verify combined has same number of rows as person level data
    if (nrow(dt) != nrow(tempdata$person.wa)) {
      stop("\n\U1F6D1 Merge resulted in unexpected number of records")
    }
  }

## Validate the cols argument ----
  if (is.null(cols)) {
    cols <- names(dt)
  } else {
    cols <- unique(c(cols,
                     "chi_year", # always include CHI year
                     grep("wgtp", names(dt), value = TRUE))) # pwgtp = weights for person, wgtp = weights for household
    missing_cols <- cols[!cols %in% names(dt)]
    if (length(missing_cols) > 0) {
      stop(sprintf("\n\U1F6D1 The following columns are not available in the dataset: %s",
                   paste(missing_cols, collapse = ", ")))
    }
  }

  if (isTRUE(kingco)) { cols <- unique(c(cols, "chi_geo_kc")) }

## Validate the kingco argument ----
  if (isTRUE(kingco) && !("chi_geo_kc" %in% names(dt))) {
    stop("\n\U1F6D1 Column 'chi_geo_kc' not found in the dataset, required when `kingco` is TRUE.")
  }
  if (length(kingco) != 1 || !is.logical(kingco) || is.na(kingco)) {
    stop("\n\U0001f6d1 `kingco` must be a logical (TRUE | FALSE, or equivalently, T | F).")
  }

# Subset the data ----
  if (isTRUE(kingco)) {dt <- dt[chi_geo_kc == "King County"]} # subset KC before columns in case do not want column
  dt <- dt[, c(cols), with = FALSE]

# Survey set ----
  # confirm all replicate weights exist
    weight_cols <- if (records %in% c("person", "combined")) {
      c("pwgtp", grep("pwgtp[0-9]+", names(dt), value = TRUE))
    } else {
      c("wgtp", grep("wgtp[0-9]+", names(dt), value = TRUE))
    }
    if (length(weight_cols) < 81) { # 1 base weight + 80 replicates
      stop("\n\U1F6D1 Missing required weight columns")
    } else {weight_cols <- sort(weight_cols)}

  if (records %in% c("person", "combined")) {  # Use person weights for combined data
    dt <- srvyr::as_survey_rep(
      dt,
      weights = pwgtp,
      combined.weights = TRUE,
      repweights = grep("pwgtp[0-9]+", names(dt), value = TRUE),
      scale = 4 / 80,
      rscales = rep( 1, 80 ),
      mse = TRUE,
      type = "JK1"
    )
  } else {
    dt <- srvyr::as_survey_rep(
      dt,
      weights = wgtp,
      combined.weights = TRUE,
      repweights = grep("wgtp[0-9]+", names(dt), value = TRUE),
      scale = 4 / 80,
      rscales = rep( 1, 80 ),
      mse = TRUE,
      type = "JK1"
    )
  }
  dt <- dtsurvey::dtrepsurvey(dt)

# Return dtsurvey object ----
  message("Your data was survey set with the following parameters is ready for rads::calc():\n",
          " - record type = ", records, "\n",
          " - valid years = ", format_time(year), "\n",
          " - replicate weights = ", weight_cols[1], ", ",  weight_cols[2], " ... ", weight_cols[length(weight_cols)], " \n")

  return(dt)
}


# The end ----
