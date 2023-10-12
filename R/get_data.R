# get_data() ----
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
#' \dontrun{
#'  get_data_hys(cols = NULL, year = c(2016, 2018), weight_variable = 'wt_sex_grade_kc')
#' }
get_data_hys <- function(cols = NULL, year = c(2021), weight_variable = 'wt_sex_grade_kc', kingco = TRUE, version = 'best', ar = TRUE){

  colname <- chi_geo_kc <- weight1 <- psu <- chi_year <- NULL

  stopifnot(all(year %in% c(seq(2004,2018,2), 2021)))

  # pull the list of vars
  vars = file.path('//dphcifs/APDE-CDIP/HYS/releases/2021/',version, 'hys_cols.csv')
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
    arfp = file.path('//dphcifs/APDE-CDIP/HYS/releases/2021/',version, '/', paste0('hys_ar_', year, '.rds'))
    ardat = data.table::rbindlist(lapply(arfp, readRDS), use.names = T, fill = T)

  }
  if(any(!vars[, ar])){
    sfp = file.path('//dphcifs/APDE-CDIP/HYS/releases/2021/',version, '/', paste0('hys_stage_', year, '.rds'))
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

# get_data_birth() ----
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
get_data_birth <- function(cols = NA,
                           year = NA,
                           kingco = T){
  if(is.null(cols)) cols <- NA
  if(is.null(year)) year <- NA

  # validate arguments
  if(!(length(cols) == 1 && is.na(cols))){
    if(!is.character(cols)){stop('\n\U0001f6d1 `cols` must specify a vector of variables or be NA (to get all possible columns).')}
  }
  if(!(length(year) == 1 && is.na(year))){
    if( (!is.numeric(year)) | sum(year%%1) != 0 ) {stop('\n\U0001f6d1 `year` must specify a vector of integers (e.g., c(2017, 2019)) or be NA (to get the most recent year).')}
  }
  if((length(kingco) == 1 && is.na(kingco)) | !is.logical(kingco)){stop('\n\U0001f6d1 `kingco` must be a logical (TRUE | FALSE, or equivalently, T | F).')}


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

  # pull columns and years from SQL
  query.string <- glue:: glue_sql ("SELECT ",  cols, " FROM [PH_APDEStore].[final].[bir_wa]
                                   WHERE chi_year IN (",  paste(year, collapse=", "), ")")

  if(kingco == T){query.string <- glue:: glue_sql (query.string, " AND chi_geo_kc = 'King County'")}


  dat <- data.table::setDT(DBI::dbGetQuery(con, query.string))
  odbc::dbDisconnect(con)

  # Format string variables due to SQL import quirks
  original.order <- names(dat)
  sql_clean(dat, stringsAsFactors = TRUE) # clean random white spaces and change strings to factors


  # reorder table
  setcolorder(dat, original.order)

  setDT(dat) # set it as a data.table again b/c otherwise, ascribing the new class above makes a copy

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
#' @param kingco logical. Return dataset for analyses where county of decedent's residence is King County.
#'
#' Default = T
#'
#' @param topcode logical. Do you want to top code chi_age at 100 to match population data?
#'
#' Default = T
#'
#' @param mykey Character vector of length 1 OR a database connection. Identifies
#' the keyring:: key that can be used to access the Health & Human Services
#' Analytic Workspace (HHSAW).
#'
#' Default == 'hhsaw'
#'
#' @return data.table (adminstrative data) for further analysis/tabulation
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
#'  get_data_death(cols = NA, year = c(2019), kingco = T, topcode = F, mykey = 'hhsaw')
#' }
get_data_death <- function(cols = NA,
                           year = NA,
                           kingco = T,
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

  # Get list of all colnames from SQL ----
      death.names <- names(DBI::dbGetQuery(con, "SELECT TOP (0) * FROM [death].[final_analytic]"))
      death.years <- sort(unique(DBI::dbGetQuery(con, "SELECT DISTINCT chi_year FROM [death].[final_analytic]")$chi_year))

  # Identify columns and years to pull from SQL ----
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
      query.string <- glue:: glue_sql ("SELECT {cols} FROM [death].[final_analytic]
                                       WHERE chi_year IN ({validyears})", .con = con)

      if(kingco == T){query.string <- glue:: glue_sql (query.string, " AND chi_geo_kc = 1")}

      dat <- data.table::setDT(DBI::dbGetQuery(con, query.string))

      datevars <- DBI::dbGetQuery(con, "SELECT ColumnName FROM [death].[crosswalk_fields] WHERE ColumnType = 'Date'")[]$ColumnName
      datevars <- intersect(datevars, names(dat)) # names of all date variables that are in actual dataset

      # odbc::dbDisconnect(con)

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

      sql_clean(dat, stringsAsFactors = FALSE) # clean random white spaces and change strings to factors

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
#' @param mykey Character vector of length 1 OR a database connection. Identifies
#' the keyring:: key that can be used to access the Health & Human Services
#' Analytic Workspace (HHSAW).
#'
#' Default == 'hhsaw'
#'
#' @return data.table (adminstrative data) for further analysis/tabulation
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
#'  get_data_chars(cols = c('seq_no', 'chi_age'),
#'                 year = c(2020),
#'                 kingco = T,
#'                 wastate = T,
#'                 inpatient = T,
#'                 deaths = F,
#'                 topcode = F,
#'                 mykey = 'hhsaw')
#' }
get_data_chars <- function(cols = NA,
                           year = NA,
                           kingco = T,
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

  # Get list of all colnames from SQL ----
      chars.names <- names(DBI::dbGetQuery(con, "SELECT TOP (0) * FROM [chars].[final_analytic]"))
      chars.years <- sort(unique(DBI::dbGetQuery(con, "SELECT DISTINCT chi_year FROM [chars].[final_analytic]")$chi_year))

  # Identify columns and years to pull from SQL ----
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
      query.string <- glue:: glue_sql ("SELECT {cols} FROM [PH_APDEStore].[chars].[final_analytic]
                                       WHERE chi_year IN ({validyears})", .con = con)

      if(inpatient == T){query.string <- glue:: glue_sql (query.string, " AND STAYTYPE = 1")}
      if(deaths == F){query.string <- glue:: glue_sql (query.string, " AND STATUS != 20")} # 20 means Expired / did not recover
      if(wastate == T){query.string <- glue:: glue_sql (query.string, " AND chi_geo_wastate = 1")}
      if(kingco == T){query.string <- glue:: glue_sql (query.string, " AND chi_geo_kc = 1")}
      if(kingco == 'zip'){query.string <- glue:: glue_sql (query.string, " AND chi_geo_kczip = 1")}

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

      sql_clean(dat, stringsAsFactors = F) # clean random white spaces and ensure all factors are strings
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

# The end ----
