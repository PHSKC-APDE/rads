#' Suppress data according to APDE standards or custom requests
#'
#' #' @description
#' Defaul suppression is according to APDE/DOH standards (https://www.doh.wa.gov/Portals/1/Documents/1500/SmallNumbers.pdf)
#' Each dataset may have it's own more stringent standards. When the reference sheet of all suppression guidelines is
#' made available, this code should be updated to use that resource.
#'
#' This function expects data that has already been formatted for CHI
#'
#'
#' @param sup_data a data.table or data.frame. Must contain the data to be suppressed with standard metric names,
#' i.e., mean, median, sum, rate, lower, upper, se, rse, numerator, denominator, proportion
#' @param suppress_range integer vector of length 2. They specify the minimum and maximum range for suppression.
#'
#' @return a data.table with suppression applied to CHI standard columns.
#'
#' @export
#'
#' @keywords suppression
#'
#' @importFrom data.table ':='
#'
#' @examples
#'
#'   set.seed(98104)
#'   dt <- data.table(
#'     chi_year = rep(2018, 100),
#'     mean = rnorm(100, .25, .05),
#'     numerator = round(rnorm(100, 20, 9), 0)
#'   )
#'   table(dt$numerator) # before
#'   suppress(dt, suppress_range = c(0, 20)) # apply suppression
#'   table(dt$numerator) # after
#'

suppress <- function(sup_data = NULL,
                     suppress_range = c(0, 10) ){

  #visible bindings for data.table
  numerator <- suppression <- NULL

  #validate 'sup_data'
      if(is.null(sup_data)){
        stop("You must specify a dataset (i.e., 'sup_data' must be defined)")
      }

      if(is.data.frame(sup_data)){
        setDT(sup_data)
      } else {
        stop(paste0("<{sup_data}> must be the name of a data.frame or data.table."))
      }


  #validate 'suppress_range'
      if(is.null(suppress_range)){suppress_range <- c(0, 10)}


      if(!is.null(suppress_range) &
          (length(suppress_range) != 2 | suppress_range[1] %% 1 != 0 | suppress_range[2] %% 1 != 0 | suppress_range[1] < 0 | suppress_range[2] < 0) ){
        stop("<suppress_range> must be comprised of two non-negative integers (i.e., 'c(0, 10)'")}


  #apply suppression
        sup_metrics <- intersect(names(sup_data), c("mean", "median", "sum", "rate", "lower", "upper", "se", "rse", "numerator", "denominator", "proportion"))
        sup_data[numerator %in% suppress_range[1]:suppress_range[2], suppression := "^"]
        sup_data[suppression=="^", (sup_metrics) := NA]


  return(sup_data)

} # close suppress function