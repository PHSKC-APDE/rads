#' Compute metrics from records (e.g. vital stats) or survey data
#' @name calc
#' @param ... Additional arguments passed to specific `calc` methods.
#' @param ph.data data.table or tbl_svy. Dataset.
#' @param what character vector. Variable to calculate metrics for. Must refer to a numeric or factor column.
#' @param where subsetting expression
#' @param by character vector. Must refer to variables within ph.data. The variables within ph.data to compute `what` by
#' @param metrics character. What calculation(s) do you need? See [metrics] for the available options.
#' @param per integer. The denominator when "rate" is selected as the metric. Metrics will be multiplied by this value.
#' @param win integer. The number of consecutive units of time (e.g., years, months, etc.) over which the metrics will be calculated,
#' i.e., the 'window' for a rolling average, sum, etc.
#' @param time_var character. The name of the time variable in the dataset. Used in combination with the "win" argument to perform time windowed calculations.
#' @param fancy_time logical. If TRUE, a record of all the years going into the data is provided.
#' If FALSE, just a simple range (where certain years within the range might not be represented in your data).
#' @param proportion character or logical. Should metrics be calculated assuming the output is
#' proportion-like? See `Proportion-like and binary variables` below for more info. Must be one of
#' `'autodetect'` (the default), `TRUE`, or `FALSE`:
#'
#'   - `'autodetect'`: for each `what` variable, rads determines whether it is structurally
#'   proportion-like and/or binary, and applies proportion-appropriate confidence interval methods
#'   and/or the binary RSE adjustment accordingly.
#'
#'   - `TRUE`: asserts that `what` is expected to be proportion-like (a factor, logical, or 0/1
#'   numeric), which would drive the CI method for survey data. If it is not structurally
#'   proportion-like, a warning is issued and standard (non-proportion) calculations are used
#'   instead, equivalent to `proportion = 'autodetect'`.
#'
#'   - `FALSE`: force `what` to *not* be treated as proportion-like or binary, even if it is
#'   structurally one or both of those things.
#'
#' @param ci numeric. Confidence level, `[0, 1]`, typically 0.95
#' @param verbose logical. Mostly unused, but toggles on/off printed warnings.
#' @references <https://github.com/PHSKC-APDE/rads/wiki/calc>
#' @return a data.table containing the results
#' @details
#' This function calculates `metrics` for each variable in `what` from rows meeting the conditions specified
#' by `where` for each grouping implied by `by`. See the [metrics] helpfile for details.
#'
#' @section Proportion-like and binary variables:
#' The `proportion` argument (`'autodetect'`, `TRUE`, or `FALSE`) controls how **`what`**
#' variables that represent a proportion are handled. This covers two related but distinct
#' ideas:
#'
#' - **Proportion-like**: any factor (regardless of how many levels it has), a logical, or a numeric
#' column containing only 0s and 1s. For these variables, every presented `mean` is itself a bounded
#' `[0, 1]` proportion -- a factor level's share of the whole, or a 0/1 indicator's prevalence.
#'
#' - **Binary**: the narrower case of a proportion-like variable that has *exactly* two possible outcomes.
#'
#' These two ideas drive different pieces of the calculation:
#'
#' - **Confidence intervals (survey data only):** any *proportion-like* **`what`** gets a CI method
#' appropriate for bounded `[0, 1]` quantities (e.g. `svyciprop`-style methods) instead of a
#' standard mean-based CI that could extend outside that range. This only matters for survey
#' data; administrative data always uses proportion-appropriate CIs for factors regardless of this
#' argument.
#'
#' - **RSE (both survey and administrative data):** only *binary* **`what`** variables get the symmetric
#' RSE adjustment, which changes the RSE denominator from the estimate itself to
#' `min(estimate, 1 - estimate)`. See `rse` in [metrics] for more detail.
#'
#' Under `'autodetect'` (the default), rads determines both properties empirically,
#' separately for each **`what`** variable.
#'
#' `TRUE` asserts that **`what`** is *expected* to be proportion-like, and drives
#' the CI method the same  way `'autodetect'` would if the expectation holds. If
#' `what` turns out not to be structurally proportion-like, a warning is issued
#' and calculations fall back to standard (non-proportion) treatment. It flags a
#' mismatch between expectation and structure. As with `'autodetect'`, the RSE
#' adjustment is only applied if the variable is *also* structurally binary.
#'
#' `FALSE` forces standard (non-proportion, non-binary)
#' treatment for both CIs and RSE, even if the variable is structurally proportion-like and/or binary.
#'
#' @export
#'
#' @examples
#' test.data <- rads.data::synthetic_birth
#'
#' # convert string to character for calculation
#' test.data[, fetal_pres := as.factor(fetal_pres)]
#'
#' test.results <- calc(test.data,
#'                      what = c("kotelchuck", "fetal_pres"),
#'                      where = year == 2022 &
#'                              sex %in% c('Male', 'Female'),
#'                      by = c("year", "sex"),
#'                      metrics = c("mean", "numerator", "denominator", "total"))
#'
#' print(test.results)
#'
calc <- function(ph.data, ...) {
  UseMethod("calc")
}

#' @keywords internal
#' @export
#' @method calc data.frame
calc.data.frame <- function(ph.data, ...){

  # Catch data.frame, and send it as a dtadmin
  ph.data = dtsurvey::dtadmin(ph.data)
  calc(ph.data, ...)

  # stop('calc no longer accepts raw data.frames/data.tables/tbl_dfs as an option. Please convert ph.data to an appropriate object type instead.
  #      Use `ph.data <- dtsurvey::dtadmin(ph.data)` for non-survey data.')
}

#' @keywords internal
#' @method calc survey.design2
#' @export
calc.survey.design2 <- function(ph.data, ...){

  ph.data = dtsurvey::as.dtsurvey(ph.data)
  calc(ph.data, ...)

  # stop('calc no longer accepts tbl_svys or survey.design objects as an option. Please convert ph.data to an appropriate object type instead.
  #      Review the documentation for dtsurvey::dtsurvey to properly convert/encode survey data for use with `calc`.')
}

#' @keywords internal
#' @method calc svyrep.design
#' @export
calc.svyrep.design <- function(ph.data, ...){
  ph.data = dtsurvey::dtrepsurvey(ph.data)
  calc(ph.data, ...)
}

#' @keywords internal
#' @method calc grouped_df
#' @export
calc.grouped_df <- function(ph.data, ...){
  stop("calc doesn't know how to handle `grouped_df` objects. Likely, you have a dplyr::group_by somewhere higher up in the code.
       Instead of grouping before running calc, use the `by` argument in calc")
}

#' @keywords internal
#' @export
#' @method calc imputationList
calc.imputationList = function(ph.data,
                               what = NULL,
                               where = NULL, #this is a change from the main calc framework
                               by = NULL,
                               metrics = c('mean', 'numerator', 'denominator'),
                               per = NULL,
                               win = NULL,
                               time_var = NULL,
                               proportion = 'autodetect',
                               fancy_time = TRUE,
                               ci = .95,
                               verbose = FALSE,
                               ...){
  call = match.call()

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
  res = data.table::rbindlist(res)
  print_and_capture <- function(x)
  {
    paste(utils::capture.output(print(x)), collapse = "\n")
  }

  misdat = res[mean_se == 0 | is.na(mean_se), .SD, .SDcols = c('_miiter', 'variable', 'level', by)]

  if(nrow(misdat) > 0){
    oot = print_and_capture((utils::head(misdat, 10)))
    msg = paste0(nrow(misdat), ' permutations have a no variance (or NA). The first 10 are presented below. This usually will occur when there is no variation within a given combination of by variables (or factor levels) within one of the iterations. \n \n',
                 oot, collapse = ' ')
    warning(msg)
  }


  mi_res = lapply(intersect(c('mean', 'total'), names(res)), function(vvv){

    # Convert to the format required
    lhs = paste(paste(by, collapse = ' + '), 'level', 'variable', sep = ' + ')
    mform = stats::as.formula(paste(lhs, '~', '`_miiter`'))
    r = data.table::dcast(res[, .SD, .SDcols = c(by, 'level', 'variable', vvv, paste0(vvv, '_vcov'), '_miiter')],
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
    r = data.table::melt(r,
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
    r = data.table::rbindlist(r)
    if(isfactor && !is.null(by)){
      for(bbb in by){
        r[is.na(get(bbb)), (bbb) := '_NA_']
      }

      r = split(r, by = by)
    }else{
      r = list(r)
    }

    mr = sapply(r, nrow)
    r[names(mr[mr ==0])] <- NULL

    # compute estimates
    # This is borrowed/adapted from mitools
    mi = lapply(r, function(a){
      # if(!isfactor) a = list(ests = list(a$ests[[1]]), varz = list(a$varz[[1]]))
      m = mitools::MIcombine(a$ests, a$varz)
      mdt = data.table::data.table(coef = stats::coef(m), se = survey::SE(m))
      crit <- stats::qt(alpha/2, m$df, lower.tail = FALSE)
      mdt[, lower := coef - crit * se]
      mdt[, upper := coef + crit * se]
      mdt[, level := a$levels[1]]
      if(isfactor & !is.null(by)) mdt = cbind(mdt, a[1,.SD,.SDcols = by])
      mdt
    })

    # combine results
    mi = data.table::rbindlist(mi)
    if(isfactor && !is.null(by)){
      for(bbb in by){
        mi[get(bbb) == '_NA_', (bbb) := NA]
      }
    }
    updateme = c(vvv, paste0(vvv,'_se'), paste0(vvv, '_lower'), paste0(vvv, '_upper'))
    data.table::setnames(mi,
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
  vars = setdiff(names(res), c('mean', 'total', 'mean_vcov', 'total_vcov', 'vcov', by, 'variable', 'level', '_miiter', names(mi_res)))
  non_mi = res[, lapply(.SD, function(v){
    if(is.numeric(v)) return(mean(v,na.rm = T))
    data.table::first(v)
  }), .SDcols = vars, keyby = c(by, 'variable', 'level')]

  # setnames(non_mi, c(c(by, 'variable', 'level'), vars))



  ans = merge(mi_res, non_mi, all.x = T, by = c(by, 'variable', 'level'))

  if(!is.null(by)) data.table::setorderv(ans, cols = c(by, 'level'))


  ans

}
