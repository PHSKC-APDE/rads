#' Tabulate data from a survey
#'
#' @param svy tab_svy object
#' @param what character vector. Variable to tabulate "over". Must match a column name in svy.
#' @param ... expressions to be passed to \code{\link{filter}}
#' @param by character vector. Must refer to variables within svy. The variables within svy to compute `what` by
#' @param metric character. One or more of mean, median, se, lower, upper, numerator, denominator, or total. All metrics are calculated-- this argument just specifies which one gets returned
#' @param proportion logical. Toggles whether or not se/lower/upper are calculated for proportions.
#'
#'
#' @return a data.table containing the results
#' @details
#' This function calculates `metrics` for each variable in `what` from rows meeting the conditions specified by `where` for each grouping implied by `by`.
#'
#' @importFrom srvyr select summarize group_by `%>%`
#' @import data.table
#' @example
#'
#' svy <- apisrs %>% as_survey_design(ids = 1, fpc = fpc)
#' svy_res <- survey_tabulate(svy, 'api00', cname == 'Los Angeles', by = 'stype', metric = 'mean proportion = F)
#'
#'
survey_tabulate = function(svy, what, ..., by = NULL, metric = c('mean', 'median', 'se', 'lower', 'upper', 'numerator', 'denominator', 'total'), proportion = T){

  #confirm that svy is a tab_svy
  stopifnot(inherits(svy, 'tbl_svy'))

  svy_names <- names(svy$variables)

  #confirm that what is in the dataset
  stopifnot(is.character(what))
  stopifnot(length(what) == 1)
  what_check <- check_names('what', 'svy', svy_names, what)
  if(what_check != '') stop(what_check)

  #validate where
  if(!missing(...)){
    where <- quos(...)
  }else{
    where = NULL
  }

  #validate by
  if(!missing(by)){

    stopifnot(is.character(by))

    by_check <- check_names('by', 'svy', svy_names, by)
    if(by_check != '') stop(by_check)

  }

  #confirm that metrics are properly specified
  metric <- match.arg(metric, c('mean', 'median', 'se', 'lower', 'upper', 'numerator', 'denominator', 'total'), several.ok = T)

  #subset svy to only the columns needed (and rows)
  if(!is.null(where)){
    svy <- svy %>% filter(!!!where)
  }

  svy <- svy %>% select(what, by)


  what = sym(what)

  if(!is.null(by)){
    svy <- svy %>% group_by(.data[[by]])
  }

  res <- svy %>% summarize(
    mean = srvyr::survey_mean(!!what, na.rm = T, vartype = 'se', proportion = proportion),
    ci = srvyr::survey_mean(!!what, na.rm = T, vartype = 'ci', proportion = proportion),
    median = srvyr::survey_median(!!what, na.rm = T, vartype = NULL),
    total = srvyr::survey_total(!!what, na.rm = T),
    numerator = srvyr::unweighted(sum(!!what == 1, na.rm = T)),
    denominator = srvyr::unweighted(n())
  )

  data.table::setDT(res)
  res[, ci := NULL]
  data.table::setnames(res, c('mean_se', 'ci_low', 'ci_upp'), c('se', 'lower', 'upper'))
  res[, variable := as.character(what)]


  return(res)

}
