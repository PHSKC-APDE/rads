
#' Get (micro)data from APDE storage.
#'
#' @description
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
#' @param cols Character vector of length >=1. Identifies which columns should be returned. NA returns all columns in the analytic dataset.
#'     See \code{\link{list_dataset_columns}} for more information on which columns are considered default by dataset.
#' @param year Numeric vector. Identifies which years of data should be pulled
#' @param weight_variable Character vector of length 1. Identifies which weight column
#' @param kingco logical. Return dataset for analyses in King County only.
#'
#' @return dataset either in data.table (adminstrative data) or svy_tbl (survey data) for further analysis/tabulation
#'
#' @importFrom srvyr %>% as_survey_design filter select
#' @import data.table
#' @importFrom labelled to_factor
#' @export
#'
#' @examples
#'
#' \dontrun{
#'  get_data_hys(cols = NA, year = c(2016, 2018), weight_variable = 'kcfinalwt')
#' }
get_data_hys <- function(cols = NA, year = c(2016, 2018), weight_variable = 'kcfinalwt', kingco = T){
  dat <- haven::read_dta("J:/HYSdata/hys/Data/hys0418_final_8.dta")
  data.table::setDT(dat)

  #prep the dataset
  dat[, sur_psu := schgnoid]
  dat[is.na(sur_psu), sur_psu := -1 * .I]
  dat <- dat[is.na(get(weight_variable)), (weight_variable) := 0]

  #subset by year
  yvar = year
  dat = dat[year %in% yvar, ]

  #convert some variables from haven_labelled to factor
  converts = c('kc4reg', 'a_race8', grep('_aic', names(dat), value = T), 'a_sex', 'sexual_orientation')

  dat[, (converts) := lapply(.SD, function(x){
    if(labelled::is.labelled(x)){
      labelled::to_factor(x)
    }else{
      x
    }
  }), .SDcols = converts]


  #create the survey object
  svy <- srvyr::as_survey_design(dat, ids = sur_psu, strata = year, weights = kcfinalwt, nest = T)
  wt  <- rlang::sym(weight_variable)
  svy <- svy %>% filter(!!wt != 0)

  if(kingco == T) svy <- svy %>% filter(kingco == 1)

  if(!is.na(cols)) svy <- svy %>% select({{cols}})
  class(svy) <- c(class(svy), 'apde_hys')



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
#' @importFrom labelled to_factor
#' @export
#'
#' @examples
#'
#' \dontrun{
#'  get_data_birth(cols = NA, year = c(2015, 2016, 2017), kingco = F)
#' }
get_data_birth <- function(cols = NA, year = c(2017),  kingco = T){
  # pull columns and years from sQL
  ifelse(is.na(cols), cols <- "*", cols <- paste(cols, collapse=", "))
  
  query.string <- glue:: glue_sql ("SELECT ",  cols, " FROM [PH_APDEStore].[final].[bir_wa] 
                                   WHERE chi_year IN (",  paste(year, collapse=", "), ")")  
  
  if(kingco == T){query.string <- glue:: glue_sql (query.string, " AND chi_geo_kc = 1")}
  
  db.apde50 <- odbc::dbConnect(odbc::odbc(), "PH_APDEStore50")
  dat <- data.table::setDT(DBI::dbGetQuery(db.apde50, query.string))
  odbc::dbDisconnect(db.apde50)
  
  # Format string variables due to SQL import quirks
  original.order <- names(dat)
  string.columns <- sapply(dat,is.character) # identify string columns as a logical vector
  string.columns <- names(dat[, ..string.columns]) # identify string columns as a character vector
  dat <- dat[, (string.columns) := lapply(.SD, trimws,which="r"), .SDcols = string.columns] # trim white space to right
  dat <- dat[, (string.columns) := lapply(.SD, factor), .SDcols = string.columns] # convert strings to factors
  
  # reorder table
  setcolorder(dat, original.order)
  
  return(dat)
}
