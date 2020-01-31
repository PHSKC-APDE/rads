#' Returns the list of datasets currently available for analysis in RADS
#'
#' @return Character vector of available datasets.
#' @export
#' @name list_apde_data
#' @examples
#' \dontrun{
#'  list_apde_data()
#' }
list_apde_data <- function(){

  ret <- c('hys', 'birth', 'bsk')

  return(ret)


}

#' List columns available for analysis for a particular dataset in RADS
#'
#' @param dataset Character vector of length 1. Identifies the dataset to be fetched. Use \code{list_apde_data} for available options
#' @param analytic_only logical. Controls whether columns outside the analytic dataset should be returned.
#'
#'
#' @return Data.frame with two columns. First column is the variable name, while the second identifies whether or not it is in the analytic ready dataset
#' @export
#' @name list_dataset_columns
#' @examples
#' \dontrun{
#'  list_dataset_columns('hys', T)
#' }
list_dataset_columns <- function(dataset, analytic_only = F){
  dat = match.arg(dataset, list_apde_data())

  warning('list_dataset_columns not currently available/implemented')
  return(data.frame(variable_name = '', analytic_ready = 'Sure. Why not?'))
}


#' List available metrics depending on type of dataset
#' @return character vector. A vector of the available metrics for calculation.
#' @name metrics
NULL

#' @rdname metrics
#' @export
survey_metrics = function(){
  #c('mean', 'se', 'lower', 'upper', 'numerator', 'denominator', 'total', 'total_se', 'missing', 'rse', 'missing.prop', 'ndistinct')
  c('total', 'total_se', 'total_lower', 'total_upper',
    'mean', 'mean_se', 'mean_lower', 'mean_upper', 'rse',
    'numerator', 'denominator', 'obs', 'median',
    'unique.time', 'ndistinct',
    'missing', 'missing.prop',
    'rate', 'rate_per', 'rate_se', 'rate_lower', 'rate_upper')
}

#' List of available metrics for calculation
#' @rdname metrics
#' @export
record_metrics = function(){
  c('mean', 'median', 'obs', 'rate', 'rse', 'numerator', 'denominator', 'missing', 'missing.prop', 'total', 'ndistinct')
}

#' Improved rounding function
#' @param x values to be rounded
#' @param n number of digits
#' @export
#' @return numeric
round2 = function(x, n = 0) {
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5
  z = trunc(z)
  z = z/10^n
  z*posneg
}


#' Substring selection from the right to complement base R substr
#' @param x character
#' @param x.start digit to start (counting from the right)
#' @param x.stop digit to end  (counting from the right)
#' @export
#' @return character vector
#'
#' @examples
#' \dontrun{
#' substrRight("Good morning!", 2, 8)
#' }
substrRight <- function(x, x.start, x.stop){
  substr(x, nchar(x)-x.stop+1, nchar(x)-x.start+1)
}

#' Compare tabular results to a reference set of results in the same data
#' @param orig Character vector of length 1. Identifies the data.table/data.frame to be fetched. Note the table must have the following columns:
#' 'result', 'lower_bound', & 'upper_bound' and all three must be numeric
#' @param new.col.name Character vector of length 1. It is the name of the column containining the comparison results.
#'
#' @importFrom data.table setnames ":=" setDT
#'
#' @export
#' @return data.table comprised of the original data.table and two additional columns ... 'significance' and 'comparison_with_kc' (or alternatively specified name)
chi_compare_kc <- function(orig,
                        new.col.name = "comparison_with_kc"){

  #Bindings for data.table/check global variables
  cat1_varname <- result <- comp.result <- lower_bound <- comp.upper_bound <- upper_bound <- comp.lower_bound <- significance <- NULL

  data.table::setDT(orig)

  #Copy & subset comparator data
  orig <- copy(chi)
  data.table::setDT(orig)

  #Copy & subset comparator data
  comparator <- orig[cat1=="King County" & tab!="crosstabs", c("indicator_key", "year", "result", "lower_bound", "upper_bound")]
  data.table::setnames(comparator, c("result", "lower_bound", "upper_bound"), c("comp.result", "comp.lower_bound", "comp.upper_bound"))

  #Merge comparator data onto all other data
  orig <- merge(orig, comparator, by=c("indicator_key", "year"), all.x = TRUE, all.y = TRUE)

  #Compare estimates with comparator
  orig[result == comp.result, paste0(new.col.name) := "no different"]
  orig[result > comp.result, paste0(new.col.name) := "higher"]
  orig[result < comp.result, paste0(new.col.name) := "lower"]

  #According to APDE protocol, we check for overlapping CI rather than SE and Z scores
  orig[(lower_bound > comp.upper_bound) | (upper_bound < comp.lower_bound), significance := "*"]

  #Keep comparison only if statistically significant
  orig[is.na(significance), paste0(new.col.name) := "no different"]

  #Drop KC level estimates that were just used for the comparisons
  orig[, c("comp.result", "comp.upper_bound", "comp.lower_bound") := NULL]

  return(orig)
}

#' Format a vector of time into a series of human readable chunks
#' @param x numeric
#' @export
#' @return character vector
#'
#' @examples
#' format_time(c(1:5, 10, 12, 24, 25))
#'
format_time <- function(x){

  #get the unique values
  x <- sort(unique(x))

  #find breaks in runs
  breaks = data.table::shift(x, type = 'lead') == (x + 1)
  bps = which(!breaks)

  #seperate
  if(length(bps)>0){
    seper = split(x, cut(x, c(-Inf, x[bps], Inf)))
  }else{
    seper = list(x)
  }

  #format into string
  seper = lapply(seper, function(y){

    if(length(y)>1){
      return(paste(min(y), max(y), sep = '-'))
    }else{
      return(paste(y))
    }

  })

  ret = paste(seper, collapse = ', ')

  return(ret)

}


#' Clean string columns read from SQL
#' @param dat name of data.table
#' @export
#' @return data.table
  sql_clean <- function(dat = NULL){
    data.table::setDT(dat)
    original.order <- names(dat)
    string.columns <- which(vapply(dat,is.character, FUN.VALUE=logical(1) )) # identify string columns
    if(length(string.columns)>0) {
      dat[, (string.columns) := lapply(.SD, trimws,which="r"), .SDcols = string.columns] # trim white space to right
      dat[, (string.columns) := lapply(.SD, function(x){gsub("^$|^ $", NA, x)}), .SDcols = string.columns] # replace blanks with NA
      dat <- dat[, (string.columns) := lapply(.SD, factor), .SDcols = string.columns] # convert strings to factors
    }
    # reorder table
    data.table::setcolorder(dat, original.order)
  }
