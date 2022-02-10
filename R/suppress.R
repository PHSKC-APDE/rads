#' Suppress data according to APDE standards or custom requests & adding caution flag for high RSE
#'
#' @description
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
#' @param secondary_ids character vector of column names which are used to define groups for secondary suppression.
#' Note, this should not include the most granular level. For example, if you wanted secondary suppression for race/ethnicity
#' where category == "race/ethnicity" and group %in% c(AIAN, Asian, Black, etc.), you should have
#' secondary_ids = c("geography", "category") rather than secondary_ids = c("geography", "category", "group")
#' @param secondary_where an expression identifying the rows to be filtered / excluded from secondary suppression because
#' the categories are not mutually exclusive (e.g., race3)
#' @param flag_only logical (T, TRUE, F, or FALSE) indicating whether data to be suppressed should be flagged without setting estimates to NA
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
#'   newdt <- suppress(dt, suppress_range = c(0, 20), secondary = FALSE) # apply suppression
#'   table(newdt$numerator) # after
#'

suppress <- function(sup_data = NULL,
                     suppress_range = c(0, 9),
                     secondary = FALSE,
                     secondary_ids = c("tab", "indicator_key", "cat1", "cat2_group", "year"),
                     secondary_where = NULL,
                     flag_only = FALSE){

  ## Global variables used by data.table declared as NULL here to play nice with devtools::check()
    numerator <- suppression <- my.group <- my.order <- my.rowct <- suppressed.group <- my.flag <- rse <- caution <- NULL

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

  #validate 'secondary' ----
      if(!is.logical(flag_only)){
        stop("'flag_only' must be specified as a logical (i.e., TRUE, T, FALSE, or F)")
      }

  #copy sup_data to avoid changing the underlying data.table ----
    temp.dt <- data.table::setDT(copy(sup_data))

  #apply primary suppression ----
      if("suppression" %in% names(temp.dt)){temp.dt[, suppression := as.character(suppression)]}
      temp.dt[numerator %in% suppress_range[1]:suppress_range[2], suppression := "^"]

  #apply secondary suppression ----
      if(secondary==T){

        # apply secondary_where argument
          if(!is.null(secondary_where)){
            where <- tryCatch(parse(text = paste0(list(secondary_where))),  error = function (e) parse(text = paste0(list(bquote(secondary_where))))) # convert 'where' into an expression
            temp.dt.aside <- temp.dt[!eval(where)]
            temp.dt <- temp.dt[eval(where)]
          }

        # identify max number of rows per group defined by secondary_ids
          max.grp.rows <- max(copy(temp.dt)[, my.rowct := .N, secondary_ids]$my.rowct)

        # create group id for each set of secondary_ids
          temp.dt[, my.group := .GRP, secondary_ids]

        # identify groups that had initial suppression
          temp.dt[, suppressed.group := F][my.group %in% temp.dt[suppression=="^"]$my.group, suppressed.group := T]

        # within groups that had suppression, count the number of rows that were not suppressed
          temp.dt[is.na(suppression), my.rowct := .N, secondary_ids]

        # identify when the number of un-suppressed rows (in groups that had suppression) is max rows minus 1 (these need secondary suppression)
          temp.dt[is.na(suppression) & my.rowct == max.grp.rows - 1, my.flag := "group needs secondary suppression"]

        # sort table so the smallest numerator per group that needs secondary suppression is first
          setorder(temp.dt, my.group, numerator, na.last = T)

        # suppress row with smallest numerator among groups needing secondary suppression
          temp.dt[my.flag == "group needs secondary suppression", my.order := 1:.N, my.group]
          temp.dt[my.order==1, suppression := "^"]

        # drop all temporary variables
          temp.dt[, c("my.group", "suppressed.group", "my.rowct", "my.flag", "my.order") := NULL]

        # combine back with data filtered out by secondary_where
          if(exists("temp.dt.aside")){
            temp.dt <- rbind(temp.dt, temp.dt.aside)
            rm(temp.dt.aside)
          }
      }

  #suppress data if has suppression flag
      sup_metrics <- intersect(names(temp.dt), c("result", "lower_bound", "upper_bound", "se", "rse",
                                                 "mean", "mean_se", "mean_lower", "mean_upper",
                                                 "rate", "rate_se", "rate_lower", "rate_upper",
                                                 "total", "total_se", "total_lower", "total_upper",
                                                 "median", "numerator", "denominator", "proportion",
                                                 "comparison_with_kc", "significance"))
      if(isFALSE(flag_only)){temp.dt[suppression=="^", (sup_metrics) := NA]}

  #apply caution flag if possible ----
      if("caution" %in% names(temp.dt)){temp.dt[, caution := as.character(caution)]}
      if("rse" %in% names(temp.dt)){
        temp.dt[rse >=30, caution := "!"]
      }

  return(temp.dt)

} # close suppress function
