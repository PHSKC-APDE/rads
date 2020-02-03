#' Suppress data according to APDE standards or custom requests & adding caution flag for high RSE
#'
#' #' @description
#' Default suppression is according to APDE/DOH standards (https://www.doh.wa.gov/Portals/1/Documents/1500/SmallNumbers.pdf)
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
                     suppress_range = c(0, 9) ){

  #visible bindings for data.table
  numerator <- suppression <- NULL

  #validate 'sup_data'
      if(is.null(sup_data)){
        stop("You must specify a dataset (i.e., 'sup_data' must be defined)")
      }

      if(!is.data.table(sup_data)){
          if(is.data.frame(sup_data)){
            setDT(sup_data)
          } else {
            stop(paste0("<{sup_data}> must be the name of a data.frame or data.table."))
          }
      }

  #validate 'suppress_range'
      if(is.null(suppress_range)){suppress_range <- c(0, 9)}

      if(!is.null(suppress_range) &
          (length(suppress_range) != 2 | suppress_range[1] %% 1 != 0 | suppress_range[2] %% 1 != 0 | suppress_range[1] < 0 | suppress_range[2] < 0) ){
        stop("<suppress_range> must be comprised of two non-negative integers (i.e., 'c(0, 9)'")}

  #copy sup_data to avoid chaning the underlying data.table
    temp.dt <- data.table::setDT(copy(sup_data))


  #apply suppression
      sup_metrics <- intersect(names(temp.dt), c("result", "lower_bound", "upper_bound", "se", "rse",
                                                  "mean", "mean_se", "mean_lower", "mean_upper",
                                                  "rate", "rate_se", "rate_lower", "rate_upper",
                                                  "total", "total_se", "total_lower", "total_upper",
                                                  "median", "numerator", "denominator", "proportion"))
      temp.dt[numerator %in% suppress_range[1]:suppress_range[2], suppression := "^"]
      temp.dt[suppression=="^", (sup_metrics) := NA]


  #apply caution flag if possible
      if("rse" %in% names(temp.dt)){
        temp.dt[rse >0.3, caution := "!"]
      }

  return(temp.dt)

} # close suppress function
