#' Returns the list of datasets currently available for RADS
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

#' List columns available for analysis
#'
#' @param dataset Character vector of length 1. Identifies the dataset to be fetched. Use \code{list_apde_data} for available options
#' @param analytic_only logical. Controls whether columns outside the analytic dataset should be returned.
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
  c('mean', 'se', 'lower', 'upper', 'numerator', 'denominator', 'total', 'total_se')
}

#' List of available metrics for calculation
#' @rdname metrics
#' @export
record_metrics = function(){
  c('mean', 'median', 'sum', 'rate', 'se', 'lower', 'upper', 'rse', 'numerator', 'denominator', 'missing', 'missing.prop', 'total', 'distinct')
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
#' @param merge.by Character vector of indeterminate length. It specifies the unique combination of variables needed to merge the comparator
#' data onto the main data. Typically, all that is needed is an indicator_key and a time indicator
#' @param compare.name Character vector of length 1. It is the name of the column containining the comparison results.
#'
#' @importFrom data.table setnames ":=" setDT
#'
#' @export
#' @return data.table comprised of the original data.table and two additional columns ... 'significance' and 'comparison_with_kc' (or alternatively specified name)
chi_compare <- function(orig,
                        merge.by = c("indicator_key", "year"),
                        compare.name = "comparison_with_kc"){

  #Bindings for data.table/check global variables
  cat1_varname <- result <- comp.result <- lower_bound <- comp.upper_bound <- upper_bound <- comp.lower_bound <- significance <- NULL

  data.table::setDT(orig)

  #Copy & subset comparator data
  comparator <- orig[cat1_varname=="chi_geo_kc", c("indicator_key", "year", "result", "lower_bound", "upper_bound")]
  data.table::setnames(comparator, c("result", "lower_bound", "upper_bound"), c("comp.result", "comp.lower_bound", "comp.upper_bound"))

  #Merge comparator data onto all other data
  orig <- merge(orig, comparator, by=merge.by, all.x = TRUE, all.y = TRUE)

  #Compare estimates with comparator
  orig[result == comp.result, compare.name := "no different"]
  orig[result > comp.result, compare.name := "higher"]
  orig[result < comp.result, compare.name := "lower"]

  #According to APDE protocol, we check for overlapping CI rather than SE and Z scores
  orig[(lower_bound > comp.upper_bound) | (upper_bound < comp.lower_bound), significance := "*"]

  #Keep comparison only if statistically significant
  orig[is.na(significance), compare.name := "no different"]

  #Drop KC level estimates that were just used for the comparisons
  orig[, c("comp.result", "comp.upper_bound", "comp.lower_bound") := NULL]

  return(orig)
}



#' format list of years into a well formatted string
#' @param temp character.
#' @export
#' @return character vector
#'
#' @examples
#' \dontrun{
#' x <- data.table(var = c(rep("one", 6), rep("two", 6)),
#'                 chi_year = rep(c(2010, 2011, 2012, 2015, 2016, 2017), 2))
#' x[, years := format_years(list(sort(unique(chi_year))))]
#' }
#'
format_years <- function(temp){

  for(i in seq(1, length(temp))){
    if(i == 1){
      new <- c(as.character(temp[[1]][i]))
    }
    if(i > 1){
      if(temp[[1]][i] - as.integer(substrRight(new, 1, 4)) == 1){
        if(substrRight(new, 5, 5) == "-"){
          new <- paste0(gsub(substrRight(new, 1, 4), "", new), temp[[1]][i])
        }else{
          new <- paste0(new, "-", temp[[1]][i])
        }
      }else{
        new <- paste0(new, ", ", temp[[1]][i])
      }
    }
  }

  return(new)
}


