#' Returns the list of datasets currently available for RADS
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

#' List columns available for analysis
#'
#' @param dataset Character vector of length 1. Identifies the dataset to be fetched. Use \code{list_apde_data} for available options
#' @param analytic_only logical. Controls whether columns outside the analytic dataset should be returned.
#' @return Data.frame with two columns. First column is the variable name, while the second identifies whether or not it is in the analytic ready dataset
#' @export
#'
#' @examples
#' \dontrun{
#'  list_dataset_columns('hys', T)
#' }
list_dataset_columns <- function(dataset, analytic_only = F){

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

#' Compare tabular results to a reference set of results in the same data
#' @param orig Character vector of length 1. Identifies the data.table/data.frame to be fetched. Note the table must have the following columns:
#' 'result', 'lower_bound', & 'upper_bound' and all three must be numeric  
#' @param merge.by Character vector of indeterminate length. It specifies the unique combination of variables needed to merge the comparator
#' data onto the main data. Typically, all that is needed is an indicator_key and a time indicator
#' @param compare.name Character vector of length 1. It is the name of the column containining the comparison results.
#' 
#' 
#' @export
#' @return data.table comprised of the original data.table and two additional columns ... 'significance' and 'comparison_with_kc' (or alternatively specified name)
chi_compare <- function(orig, 
                        merge.by = c("indicator_key", "year"), 
                        compare.name = "comparison_with_kc"){
  setDT(orig)
  
  #Copy & subset comparator data 
  comparator <- orig[cat1_varname=="chi_geo_kc", c("indicator_key", "year", "result", "lower_bound", "upper_bound")]
  setnames(comparator, c("result", "lower_bound", "upper_bound"), c("comp.result", "comp.lower_bound", "comp.upper_bound"))
  
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
