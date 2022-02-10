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
#' @param secondary logical (T, TRUE, F, or FALSE) indicating whether secondary suppression should be run
#' @param secondary_ids character vector of column names which are used to define groups for secondary suppression
#' @param secondary_where an expression identifying the rows to be filtered / excluded from secondary suppression because
#' the categories are not mutually exclusive (e.g., race3)
#'
#'
#' @return a data.table with suppression applied to CHI standard columns.
#'
#' @export
#'
#' @keywords suppression
#'
#' @importFrom data.table ':=' data.table
#'
#' @examples
#'
#'   set.seed(98104)
#'   dt <- data.table::data.table(
#'     chi_year = rep(2018, 100),
#'     mean = rnorm(100, .25, .05),
#'     numerator = round(rnorm(100, 20, 9), 0)
#'   )
#'   table(dt$numerator) # before
#'   new.dt <- suppress(dt, suppress_range = c(0, 20), secondary = FALSE) # apply suppression
#'   table(newdt$numerator) # after
#'

suppress <- function(sup_data = NULL,
                     suppress_range = c(0, 9),
                     secondary = FALSE,
                     secondary_ids = c("tab", "indicator_key", "cat1", "cat2_group", "year"),
                     secondary_where = NULL){

  ## Global variables used by data.table declared as NULL here to play nice with devtools::check()
    numerator <- suppression <- group <- counter <- suppress2 <- rse <- caution <- NULL

  #validate 'sup_data' ----
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

  #validate 'suppress_range' ----
      if(is.null(suppress_range)){suppress_range <- c(0, 9)}

      if(!is.null(suppress_range) &
          (length(suppress_range) != 2 | suppress_range[1] %% 1 != 0 | suppress_range[2] %% 1 != 0 | suppress_range[1] < 0 | suppress_range[2] < 0) ){
        stop("<suppress_range> must be comprised of two non-negative integers (i.e., 'c(0, 9)'")}

  #validate 'secondary' ----
      if(!is.logical(secondary)){
        stop("'secondary' must be specified as a logical (i.e., TRUE, T, FALSE, or F)")
      }

  #validate 'secondary_ids' ----
      if(secondary==T & length(setdiff(secondary_ids, names(sup_data))) > 0 ){
        stop("At least one name in 'secondary_ids' is not found among the column names in 'sup_data'")
      }

  #validate 'secondary_where' ----
      where <- NULL
      if(!is.null(secondary_where)){
        where <- tryCatch(parse(text = paste0(list(secondary_where))),  error = function (e) parse(text = paste0(list(bquote(secondary_where))))) # convert 'where' into an expression
        if(nrow(sup_data[eval(where), ]) <1 ){
          stop(paste0("Your '...' (i.e., ", where, ") filters out all rows of data. Please revise and submit again"))
        }
      }

  #copy sup_data to avoid chaning the underlying data.table ----
    temp.dt <- data.table::setDT(copy(sup_data))


  #apply primary suppression ----
      temp.dt[, suppression := as.character(suppression)]
      temp.dt[numerator %in% suppress_range[1]:suppress_range[2], suppression := "^"]
      sup_metrics <- intersect(names(temp.dt), c("result", "lower_bound", "upper_bound", "se", "rse",
                                                 "mean", "mean_se", "mean_lower", "mean_upper",
                                                 "rate", "rate_se", "rate_lower", "rate_upper",
                                                 "total", "total_se", "total_lower", "total_upper",
                                                 "median", "numerator", "denominator", "proportion",
                                                 "comparison_with_kc", "significance"))
      temp.dt[suppression=="^", (sup_metrics) := NA]

  #apply secondary suppression ----
      if(secondary==T){

        # create group identifiers
          if(!is.null(where)){
            temp.dt[eval(where), group := .GRP, by = c(secondary_ids)]
          } else {temp.dt[, group := .GRP, by = c(secondary_ids)]}

        # identify groups that have at least one row suppressed
          suppressed.groups <- temp.dt[suppression == "^"]$group

        # identify groups that have ONLY ONE row suppressed (if have 2+, no need for additional suppression)
          suppressed.solo <- temp.dt[group %in% suppressed.groups & suppression == "^", counter := .N, by = group][counter==1]$group

        # identify rows needing secondary suppression
          temp.dt[group %in% suppressed.solo & numerator == min(numerator, na.rm = TRUE), suppress2:=seq(1, .N), by = group] # identify the row with the second smallest numerator among groups where one row was suppressed
          temp.dt[suppress2 == 1, suppression := "^"][, suppress2 := NULL] # when there are multiple rows in one group that could have secondary suppression, only suppress the first

        # apply secondary suppression marker
          temp.dt[suppression=="^", (sup_metrics) := NA]
          temp.dt[, c("group", "counter") := NULL]
      }

  #apply caution flag if possible ----
      if("rse" %in% names(temp.dt)){
        temp.dt[rse >=30, caution := "!"]
      }

  return(temp.dt)

} # close suppress function
