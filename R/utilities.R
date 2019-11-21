#' Returns the list of datasets currently available for analysis in RADS
#'
#' @return Character vector of available datasets.
#' @export
#'
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
#'
#' @examples
#' \dontrun{
#'  list_dataset_columns('hys', T)
#' }
list_dataset_columns <- function(dataset, analytic_only = F){
  dat = match.arg(dataset, list_apde_data())

  warning('list_dataset_columns not currently available/implemented')
  return(data.frame(variable_name = '', analytic_ready = 'Sure. Why not?'))

}

#' List of available metrics for calculation
#' @export
#' @return character vector. A vector of the available metrics for calculation.
survey_metrics = function(){
  c('mean', 'se', 'lower', 'upper', 'numerator', 'denominator', 'total', 'total_se')
}

record_metrics = function(){
  c('mean', 'median', 'sum', 'rate', 'se', 'lower', 'upper', 'rse', 'numerator', 'denominator', 'missing', 'missing.prop', 'total')
}


#' Improved rounding function
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


#' format list of years into a well formatted string
#' @export
#' @return character vector 
#' 
#' @examples
#' \dontrun{
#' x <- data.table(var = c(rep("one", 6), rep("two", 6)), chi_year = rep(c(2010, 2011, 2012, 2015, 2016, 2017), 2))
#' x[, years := format.years(list(sort(unique(chi_year))))]
#' } 
#' 
format.years <- function(temp){
  for(i in 1:(length(temp[[1]])) ){
    if(i == 1){
      new <- c(as.character(temp[[1]][i]))
    }
    if(i > 1){
      if(temp[[1]][i] - as.integer(substrRight(new, 1, 4)) == 1){
        if(substrRight(new, 5, 5) == "-"){
          new <- paste0(gsub(substrRight(new, 1, 4), "", new), temp[[1]][i])
        }else{ new <- paste0(new, "-", temp[[1]][i])}
      } else{new <- paste0(new, ", ", temp[[1]][i])}
    }
  }
  return(new)
}