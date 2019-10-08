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

#' List of available metrics for calculation
survey_metrics = function(){
  c('mean', 'median', 'se', 'lower', 'upper', 'numerator', 'denominator', 'total', 'total_se')
}
