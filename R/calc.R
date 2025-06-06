#' Compute metrics from record (e.g. vital stats) or survey data
#' @name calc
#' @param ph.data data.table or tbl_svy. Dataset.
#' @param what character vector. Variable to calculate metrics for.
#' @param where subsetting expression
#' @param by character vector. Must refer to variables within ph.data. The variables within ph.data to compute `what` by
#' @param metrics character. See \code{\link{metrics}} or scroll below for the available options.
#' @param per integer. The denominator when "rate" or "adjusted-rate" are selected as the metric. Metrics will be multiplied by this value.
#' @param win integer. The number of consecutive units of time (e.g., years, months, etc.) over which the metrics will be calculated,
#' i.e., the 'window' for a rolling average, sum, etc.
#' @param time_var character. The name of the time variable in the dataset. Used in combination with the "win" argument to do time windowed calculations.
#' @param fancy_time logical. If TRUE, a record of all the years going into the data is provided.
#' If FALSE, just a simple range (where certain years within the range might not be represented in your data).
#' @param proportion logical. For survey data, should metrics be calculated assuming the output is proportion-like? See details for more.
#'                   Currently does not have functionality for non-survey data.
#' @param ci numeric. Confidence level, >0 & <1, typically 0.95
#' @param verbose logical. Mostly unused, but toggles on/off printed warnings.
#' @param ... not implemented
#' @references \url{https://github.com/PHSKC-APDE/rads/wiki/calc}
#' @return a data.table containing the results
#' @details
#' This function calculates `metrics` for each variable in `what` from rows meeting the conditions specified
#' by `where` for each grouping implied by `by`.
#'
#' Available metrics include:
#'
#' 1) total: Count of people with the given value. Mostly relevant for surveys
#' (where total is approximately mean * sum(pweights)).
#' Returns total, total_se, total_upper, total_lower.
#' total_se, total_upper, & total_lower are only valid for survey data.
#' Default ci (e.g. upper and lower) is 95 percent.
#'
#' 2) mean: Average response and associated metrics of uncertainty.
#' Returns mean, mean_se, mean_lower, mean_upper.
#' Default ci (e.g. upper and lower) is 95 percent.
#'
#' 3) rse: Relative standard error. 100*se/mean.
#'
#' 4) numerator: Sum of non-NA values for `what``.
#' The numerator is always unweighted.
#'
#' 5) denominator: Number of rows where `what` is not NA.
#' The denominator is always unweighted.
#'
#' 6) obs: Number of unique observations (i.e., rows), agnostic as to whether
#' there is missing data for `what`. The obs is always unweighted.
#'
#' 7) median: The median non NA response. Not populated when `what` is a factor
#' or character. Even for surveys, the median is the unweighted result.
#'
#' 8) unique.time: Number of unique time points (from `time_var`) included in
#' each tabulation (i.e., number of unique time points when the `what` is not missing).
#'
#' 9) missing: Number of rows in a given grouping with an NA value for `what`.
#'    missing + denominator = Number of people in a given group.
#'    When `what` is a factor/character, the missing information is provided for the other.
#'
#' 10) missing.prop: The proportion of the data that has an NA value for `what`.
#'
#' 11) rate: mean * per. Provides rescaled mean estimates (i.e., per 100 or per 100,0000).
#' Returns rate, rate_se, rate_lower, rate_upper.
#' Default ci (e.g. upper and lower) is 95 percent.
#'
#'
#' For survey data, use the \code{proportion} argument where relevant to ensure metrics are calculated using special proportion (e.g \code{svyciprop})
#' methods. That is, when you want to find the fraction of ____, toggle \code{proportion} to \code{TRUE}.
#'
#' @export
#'
#' @examples
#'
#' #record data
#' test.data <- get_data_birth(
#'                year = 2015:2017,
#'                cols = c("chi_year", "kotelchuck",
#'                         "chi_sex", "fetal_pres"))
#'
#' test.results <- calc(test.data,
#'                      what = c("kotelchuck", "fetal_pres"),
#'                      chi_year == 2016 & chi_sex %in% c('Male', 'Female'),
#'                       by = c("chi_year", "chi_sex"),
#'                       metrics = c("mean", "numerator", "denominator",
#'                                   "total"))
#'
#' print(test.results)
#'
calc <- function(ph.data, ...) {
  UseMethod("calc")
}

#' @noRd
#' @keywords internal
#' @export
calc.data.frame <- function(ph.data, ...){

  # Catch data.frame, and send it as a dtadmin
  ph.data = dtsurvey::dtadmin(ph.data)
  calc(ph.data, ...)

  # stop('calc no longer accepts raw data.frames/data.tables/tbl_dfs as an option. Please convert ph.data to an appropriate object type instead.
  #      Use `ph.data <- dtsurvey::dtadmin(ph.data)` for non-survey data.')
}

#' @noRd
#' @keywords internal
#' @export
calc.survey.design2 <- function(ph.data, ...){

  ph.data = dtsurvey::as.dtsurvey(ph.data)
  calc(ph.data, ...)

  # stop('calc no longer accepts tbl_svys or survey.design objects as an option. Please convert ph.data to an appropriate object type instead.
  #      Review the documentation for dtsurvey::dtsurvey to properly convert/encode survey data for use with `calc`.')
}

#' @noRd
#' @keywords internal
#' @export
calc.svyrep.design <- function(ph.data, ...){
  ph.data = dtsurvey::dtrepsurvey(ph.data)
  calc(ph.data, ...)
}

#' @noRd
#' @keywords internal
#' @export
calc.grouped_df <- function(ph.data, ...){
  stop("calc doesn't know how to handle `grouped_df` objects. Likely, you have a dplyr::group_by somewhere higher up in the code.
       Instead of grouping before running calc, use the `by` argument in calc")
}

#' @noRd
#' @export
#' @importFrom mitools MIcombine
#' @importFrom stats coef qt as.formula
#' @importFrom utils head
calc.imputationList = function(ph.data,
                               what = NULL,
                               where = NULL, #this is a change from the main calc framework
                               by = NULL,
                               metrics = c('mean', 'numerator', 'denominator'),
                               per = NULL,
                               win = NULL,
                               time_var = NULL,
                               proportion = FALSE,
                               fancy_time = TRUE,
                               ci = .95,
                               verbose = FALSE,
                               ...){
  call = match.call()

  # visible bindings ----
  level <- lower <- upper <- se <- `_miiter` <- `mean_se` <-  NULL

  # dots = list()
  # dots = list(...)
  # dot_nms = names(dots)
  if(length(what) >1) stop('When `ph.data` is an imputationList, only one value of `what` can be specified')

  # make sure metrics is specified
  if(!'metrics' %in% names(call)){
    stop('metrics argument must be explictly specified for the MI method to work')
  }else{

    if(any(c('mean', 'total') %in% metrics) && !'vcov' %in% metrics){
      metrics = c(metrics, 'vcov')
    }

  }

  # Borrowed from mitools::summary
  # Set the CI boundary
  if(!missing(ci)){
    alpha = 1 - ci
  } else{
    alpha = .05
  }

  if(!missing(where)){
    where = substitute(where)
    wherecheck = T
  } else{
    wherecheck = F
  }

  # For each imputation realization, run calc
  res = lapply(ph.data[[1]], function(`_x`){

    # Evaluate where early
    if(wherecheck) r <- eval(where, `_x`, parent.frame()) else r <- TRUE

    do.call(calc, list(ph.data = `_x`[r,], what = what,
         by = by, metrics = metrics,
         per = per, win = win,
         time_var = time_var,
         proportion = proportion, fancy_time = fancy_time,
         ci = ci,
         verbose = verbose))
  })

  # format so that we can combine with MIcombine

  # Organizes the vcov
  make_vcov = function(v){

    # For factors, return the first one. The rest are duplicates
    if(ncol(v[[1]][[1]])>1) return(v[[1]][[1]])

    # Otherwise they need to be constructed
    d = unlist(v)
    m = matrix(0, length(d), length(d))
    diag(m)<-d

    m
  }

  isfactor = !all(is.na(res[[1]][,level]))
  res = lapply(seq_along(res), function(i) res[[i]][, `_miiter` := i])
  res = rbindlist(res)
  print_and_capture <- function(x)
  {
    paste(capture.output(print(x)), collapse = "\n")
  }

  misdat = res[mean_se == 0 | is.na(mean_se), .SD, .SDcols = c('_miiter', 'variable', 'level', by)]

  if(nrow(misdat) > 0){
    oot = print_and_capture((head(misdat, 10)))
    msg = paste0(nrow(misdat), ' permutations have a no variance (or NA). The first 10 are presented below. This usually will occur when there is no variation within a given combination of by variables (or factor levels) within one of the iterations. \n \n',
                 oot, collapse = ' ')
    warning(msg)
  }


  mi_res = lapply(intersect(c('mean', 'total'), names(res)), function(vvv){

    # Convert to the format required
    lhs = paste(paste(by, collapse = ' + '), 'level', 'variable', sep = ' + ')
    mform = as.formula(paste(lhs, '~', '`_miiter`'))
    r = dcast(res[, .SD, .SDcols = c(by, 'level', 'variable', vvv, paste0(vvv, '_vcov'), '_miiter')],
              mform,
              value.var = c(vvv, paste0(vvv, '_vcov')))

    # Fix NULL VCOVs
    vcov_col = grep('vcov', names(r), value = T)
    val_col = grep(paste0(vvv,'_[0-9]'), names(r), value = T)
    for(vc in vcov_col){
      vcol = r[[vc]]
      mis_chk = sapply(vcol, length)
      vcol[which(mis_chk == 0)] <- list(list(var = matrix(NA_real_)))
      r[[vc]] <- vcol
    }

    # Remelt things, and split
    r = melt(r,
             id.vars = c(by, 'level', 'variable'),
             measure.vars = list(val_col, vcov_col),
             variable.name = '_miiter',
             value.name = c(vvv, paste0(vvv, '_vcov')))

    res2 = split(r, by = c('_miiter'))

    # extract and organize the estimates and their variances
    r = lapply(res2, function(x){

      if(isfactor){

        y = x[, list(ests = list(get(vvv)),
                     varz = list(make_vcov(get(paste0(vvv, '_vcov')))),
                     levels = list(level)), keyby = by]
      }else{
        y = x[, list(ests = list(get(vvv)),
                     varz = list(make_vcov(get(paste0(vvv, '_vcov')))),
                     levels = list(level))]
      }

      y
    })

    # organize them by "by variables"
    r = rbindlist(r)
    if(isfactor && !is.null(by)){
      r = split(r, by = by)
    }else{
      r = list(r)
    }

    # compute estimates
    # This is borrowed/adapted from mitools
    mi = lapply(r, function(a){
      # if(!isfactor) a = list(ests = list(a$ests[[1]]), varz = list(a$varz[[1]]))
      m = mitools::MIcombine(a$ests, a$varz)
      mdt = data.table(coef = coef(m), se = survey::SE(m))
      crit <- qt(alpha/2, m$df, lower.tail = FALSE)
      mdt[, lower := coef - crit * se]
      mdt[, upper := coef + crit * se]
      mdt[, level := a$levels[1]]
      if(isfactor & !is.null(by)) mdt = cbind(mdt, a[1,.SD,.SDcols = by])
      mdt
    })

    # combine results
    mi = rbindlist(mi)
    updateme = c(vvv, paste0(vvv,'_se'), paste0(vvv, '_lower'), paste0(vvv, '_upper'))
    setnames(mi,
             c('coef', 'se', 'lower', 'upper'),
             updateme
    )

    # # Clean up
    # if(!isfactor && !is.null(by)) mi[, (by) := ans[, .SD, .SDcols = c(by)]]
    stub = res2[[1]][, .SD, .SDcols = c(by, 'variable')]
    mi = mi[, .SD, .SDcols = setdiff(names(mi), names(stub))]


    return(cbind(stub, mi))




  })

  if(length(mi_res) == 1){
    mi_res = mi_res[[1]]
  }else{
    mi_res = merge(mi_res[[1]], mi_res[[2]], by = intersect(names(mi_res[[1]]), names(mi_res[[2]])))
  }


  # For the variables that don't go through MI routines (e.g numerator, take the average)
  vars = setdiff(metrics, c('mean', 'total', 'mean_vcov', 'total_vcov', 'vcov'))
  non_mi = res[, lapply(vars, function(v){
    if(v %in% c('vcov', 'unique.time')){
      f = data.table::first
    }else{
      f = function(x) mean(x, na.rm = T)
    }
    f(get(v))
  }) , keyby = c(by, 'variable', 'level')]

  setnames(non_mi, c(c(by, 'variable', 'level'), vars))

  ans = merge(mi_res, non_mi, all.x = T, by = c(by, 'variable', 'level'))

  if(!is.null(by)) data.table::setorderv(ans, cols = c(by, 'level'))


  ans

}
