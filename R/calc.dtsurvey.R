#' @rdname calc
#' @export
#' @method calc dtsurvey
calc.dtsurvey <- function(ph.data,
                         what = NULL,
                         where, #this is a change from the main calc framework
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

  if(!all(c('stype', 'sdes') %in% names(attributes(ph.data)))){

    stop('`ph.data` input does not have the right attributes for this to work. Usually this is caused by using dplyr verbs on a dtsurvey object.
         If you run into this issue on a administrative type dataset, just call dtadmin after all your data munging. If this occurs on a survey,
         and you do some filtering and you just have to use dplyr: use the srvyr package (srvyr::as_survey_design()) to convert your data.frame up front,
         to a survey like object and then do your dplyr-ing. After the data is ready, use dtsurvey::as.dtsurvey (or as.dtrepsurvey) to convert it into the right format.')
  }

  call = match.call() # get 'call' object containing function name plus every argument

  # Preserve a reference to the pre-`where`-filter data. This is used later (after `what` has been
  # validated) to autodetect whether each `what` variable is structurally binary. Purposefully
  # detect using *unfiltered* data because a `where` clause could entirely eliminate some `what`
  # values within a given call. This would make an otherwise-binary variable look non-binary (or vice
  # versa). This is just a reference, not a copy, so not costly.
  ph.data_prefilter = ph.data

  #filter the dataset
  if(!missing(where)){

    if(is.character(call[['where']])){
      where = str2lang(call[['where']])
      warning('`where` is a string. It was converted so that it would work, but in the future, this might turn into an error. Please pass unquoted commands that will resolve to a logical' )
    }

    e <- substitute(where)
    # r <- eval(e, ph.data, parent.frame())
    # stopifnot('`where` does not resolve to a logical' = is.logical(r))
    ph.data = ph.data[r,env = list(r = e)]
    #do.call(subset, args = list(x = ph.data, subset = e)) an alternative approach

    if(nrow(ph.data) == 0){
      warning(paste0('Provided `where` statement subsets out all rows : ', utils::capture.output(print(e))))
    }


  }

  #validate other inputs
  #validate what
  # Check if `what` does not exist or is NULL
      if (missing(what) || is.null(what)) {
        stop("\n\U0001F92C The `what` argument must be provided!")
      }

      # Check if `what` is not a character vector
      if (!is.character(what)) {
        stop("\n\U0001F92C The `what` argument must be a character vector.")
      }

      # Check if `what` values are not names in `ph.data`
      missing_columns <- setdiff(what, names(ph.data))
      if (length(missing_columns) > 0) {
        stop(paste0("\n\U0001F92C The following `what` values are not names of columns in `ph.data`: ",
                     paste(missing_columns, collapse = ", "), "."))
      }

  #validate by
      # Check if `by` is not a character vector
      if (!is.null(by) & !is.character(by)) {
        stop("\n\U0001F92C The `by` argument must be a character vector.")
      }

      # Check if `by` values are not names in `ph.data`
      missing_columns <- setdiff(by, names(ph.data))
      if (length(missing_columns) > 0) {
        stop(paste0("\n\U0001F92C The following `by` values are not names of columns in `ph.data`: ",
                     paste(missing_columns, collapse = ", "), "."))
      }

  #validate 'metrics'
  # pull list of standard available metrics
  opts <- metrics()

  # limits metrics to those that have been pre-specified, i.e., non-standard metrics are dropped
  if(!is.null(metrics)){
    naughty = setdiff(metrics,opts)
    if(length(naughty)>0){
      stop(paste('Requested invalid metric(s):', paste(naughty, collapse = ', ')))
    }
  }else{
    stop('Must specify a `metric`')
  }

  #validate 'proportion'
  # Must be one of 'autodetect', TRUE, or FALSE. Anything else (including NA, other strings, etc.) is rejected.
  if(!(is.character(proportion) && length(proportion) == 1 && identical(proportion, 'autodetect')) &&
     !(is.logical(proportion) && length(proportion) == 1 && !is.na(proportion))){
    stop("\n\U0001F92C The `proportion` argument must be one of: 'autodetect' (the default), TRUE, or FALSE.")
  }

  #validate rate
  if("rate" %in% metrics & is.null(per)){
    per <- 1 # default denominator of 1
  }
  if("rate" %in% metrics & !is.null(per) & all.equal(per, as.integer(per))!=T ){
    stop("If specified, the 'per' argument must be an integer")
  }

  #validate time var and window
  if(!is.null(time_var) && !time_var %in% names(ph.data)) stop(paste0('time_var ', '[', time_var,'] not found in ph.data'))
  if(is.null(time_var) && any(metrics %in% c('unique.time'))) stop('Did not specify a time variable (`time_var`), but asked to include the unique dates')
  if(is.null(time_var) && !is.null(win)) stop('Specified a win(dow) without providing a time_var')
  if(!is.null(time_var) && !is.numeric(ph.data[[time_var]])) stop(paste0('time_var ', '[', time_var,'] must be numeric'))
  if(!is.null(win)){
    if(!is.numeric(win)) stop('`win` must either be NULL or numeric or a non-NA numeric of length one greater than 1')
    if(is.na(win) || length(win) != 1 || win <=0 ) stop('`win` must either be NULL, or a non-NA numeric of length one greater than 1')
  }
  if(!is.null(time_var) && !is.null(by) && time_var %in% by) stop('`time_var` should not also show up in `by`. If you need/want both, create a duplicate column and pass that instead')

  #validate 'fancy_time'
  if(!is.logical(fancy_time)){
    stop("'fancy_time' must be specified as a logical (i.e., TRUE, T, FALSE, or F)")
  }

  #### CREATE CALC FUNCTIONS ####
  #select type of time formatting
  if(fancy_time==T){time_format <- format_time}else{time_format <- format_time_simple}

  #calculate windows
  if(!is.null(time_var) & !is.null(win)){
    times = unique(stats::na.omit(ph.data[[time_var]]))
    if(length(times)>0 && !is.null(win)){
      wins = seq(min(times), max(times - win + 1))
      wins = lapply(wins, function(x) seq(x, x + win - 1))
      usewins = T
    }else{
      wins = 1
      sub_i = TRUE
      usewins = F
      # warning('Because the `where` condition removed all rows, windowing is ignored')
    }
  }else{
    wins = 1
    sub_i = TRUE
    usewins = F
  }


  #if multiple whats are provided, compute per what
  res = lapply(what, function(wht){

    #Determine, for this specific `what` variable, whether it is:
    # -- proportion-like (important for CI calculation)
    # -- binary (important for RSE calculation)
    is_proportion_detected = is_proportion_var(ph.data_prefilter[[wht]])
    is_binary_detected = is_binary_var(ph.data_prefilter[[wht]])

    if(identical(proportion, 'autodetect')){
      proportion_resolved = is_proportion_detected
    }else if(isTRUE(proportion)){
      proportion_resolved = is_proportion_detected
      if(!is_proportion_detected){
        warning(paste0(
          '\n\u26A0\ufe0f `proportion` was set to TRUE for `', wht, '`, but this variable does not ',
          'seem to be proportion-like (i.e., it is not a factor, a logical, or a numeric containing ',
          'only 0s and 1s). `proportion` cannot be honored for this variable, so standard ',
          '(non-proportion) calculations will be used instead (equivalent to `proportion = \'autodetect\'`).'
        ))
      }
    }else{
      proportion_resolved = FALSE
    }

    #Determine the type of CI method to use
    meth = 'mean' #the default
    st = attr(ph.data, 'stype')
    whatfactor = is.factor(ph.data[[wht]])
    if(st == 'admin' && (whatfactor || proportion_resolved)) meth = 'unweighted_binary'
    if(st != 'admin' && proportion_resolved) meth = 'xlogit'

    #Compute the metric
    r = lapply(wins, function(w){

      if(usewins == TRUE){
        sub_i = substitute(tv %in% w, list(tv = as.name(time_var), w = w))
      }

      compute(DT = eval(substitute(ph.data[sub_i], env = list(sub_i = sub_i))),
              x = wht,
              by = by,
              metrics,
              ci_method = meth,
              level = ci,
              time_var = time_var,
              time_format = time_format,
              per = per,
              window = !(is.logical(sub_i) && sub_i),
              binary = proportion_resolved && is_binary_detected)
    })

    data.table::rbindlist(r)

  })


  res = data.table::rbindlist(res, fill = TRUE)

  return(res)

}

#' Determine whether a variable is structurally binary.
#' Used by `calc.dtsurvey()` to decide whether the RSE calculation should be:
#' RSE = 100 * mean_se / mean
#' OR
#' RSE = 100 * mean_se / (min(mean, 1-mean))
#' @param x a vector -- typically a column pulled from `ph.data` prior to any `where` filtering.
#' @noRd
#' @keywords internal
is_binary_var <- function(x){
  if(is.factor(x)){
    return(nlevels(x) == 2)
  }
  if(is.logical(x)){
    return(TRUE)
  }
  if(is.numeric(x)){
    vals = unique(x[!is.na(x)])
    if(length(vals) == 0) return(FALSE)
    return(all(vals %in% c(0, 1)))
  }
  return(FALSE)
}

#' Determine whether a variable is proportion-like.
#' Any factor qualifies here regardless of how many levels it has.
#' Used by `calc.dtsurvey()` to resolve `proportion = 'autodetect'` and
#' to decide whether proportion-appropriate CI methods should be applied for survey analyses.
#' @param x a vector -- typically a column pulled from `ph.data` prior to any `where` filtering.
#' @noRd
#' @keywords internal
is_proportion_var <- function(x){
  if(is.factor(x)) return(TRUE)
  is_binary_var(x) # if it is binary, treat it as a factor for CI calculations, even if not a true factor
}

#' A function to compute a metric as part of calc.dtsurvey
#' see the help/documentation for calc and/or smeanto better understand the inputs
#' @param binary logical. Whether `x` should be treated as a binary variable. When TRUE, `rse` is
#' calculated as `100 * mean_se / min(mean, 1 - mean)` instead of `100 * mean_se / mean`.
#' In essences, this will ascribe the maximal RSE from the estimate and it's complement. See
#' [calc] documentation for details.
#' @noRd
#' @keywords internal
compute <- function(DT,
                    x,
                    by = NULL,
                    metrics,
                    ci_method = 'mean',
                    level = .95,
                    time_var,
                    time_format,
                    per = 1,
                    window = FALSE,
                    binary = FALSE){


  # if(nrow(DT) == 0) warning('No valid rows to compute on given `where` and `win` conditions')

  sv = attr(DT, 'sdes')
  st = attr(DT, 'stype')

  #For each metric, define a function to compute it-- or ignore it if not called for.
  xisfactor = is.factor(DT[[x]])

  xvar = x
  x = as.name(x)

  #construct the query
  if(any(c('mean', 'rate') %in% metrics)){
    mean_fun = substitute(list(dtsurvey::smean(x,
                                               na.rm = T,
                                               var_type = c('se', 'ci'),
                                               ci_method = cim,
                                               level = l,
                                               ids = `_id`,
                                               sv = ..sv,
                                               st = ..st)),
                          list(x = x,
                               l = level,
                               cim = I(ci_method)))
  }else{
    mean_fun = NULL
  }

  if('total' %in% metrics){
    total_fun = substitute(list(dtsurvey::stotal(x,
                                                 na.rm = T,
                                                 var_type = c('se', 'ci'),
                                                 ci_method = 'total',
                                                 level = l,
                                                 ids = `_id`,
                                                 sv = ..sv,
                                                 st = ..st)),
                           list(x = x,
                                l = level))
  }else{
    total_fun = NULL
  }

  #numerator
  if('numerator' %in% metrics){
    num_fun = substitute(sum(x,na.rm = T), list(x = x))
  }else{
    num_fun = NULL
  }
  #denominator
  if('denominator' %in% metrics){
    denom_fun = substitute(sum(!is.na(x)), list(x = x))
  }else{
    denom_fun = NULL
  }

  #obs (see below)

  #median
  if('median' %in% metrics){
    if(xisfactor){
      med_fun = NULL
      warning('Ignoring a request to calculate the median on a factor')
    }else{
      med_fun = substitute(stats::median(x, na.rm = T) * 1.0, list(x=x))
    }
  }else{
    med_fun = NULL
  }

  #time var
  if(!is.null(time_var)){
    time_fun = substitute(time_format(time_var[!is.na((x))]), list(time_var = as.name(time_var), x=x))

    #if we're in a window, don't "by" by time var. Instead, let time_format handle things
    #if(!window) by = c(by, time_var) #add time_var to by is specified
  }else{
    time_fun = NULL
  }

  #unique.time
  if('unique.time' %in% metrics){
    ut_fun = substitute(length(unique( (tv)[!is.na(x)] )), list(tv = as.name(time_var), x =x ))
  }else{
    ut_fun =NULL
  }

  #missing
  if('missing' %in% metrics){
    mis_fun = substitute(sum(is.na( x )), list(x = x))
  }else{
    mis_fun = NULL
  }

  if('obs' %in% metrics){
    obs_fun = quote(.N)
  }else{
    obs_fun = NULL
  }


  #missing.prop
  if('missing.prop' %in% metrics){
    misp_fun = substitute(sum(is.na(x) / .N), list(x = x))
  }else{
    misp_fun = NULL
  }

  #ndistinct
  if('ndistinct' %in% metrics){
    ndis_fun = substitute(length(unique(x)), list(x = x))
  }else{
    ndis_fun = NULL
  }

  #vcov
  if('vcov' %in% metrics){
    stopifnot( 'One of `mean` or `total` must be in the metrics for vcov to make sense' = any(c('mean', 'total') %in% metrics))
    if('mean' %in% metrics){
      mean_vcov_fun = substitute(dtsurvey::sur_var(x, na.rm = T, type = 'mean', as_list = TRUE,  sv = dtsurvey::sv(DT), ids = `_id`, st = dtsurvey::st(DT)), list(x=x))
    }else{
      mean_vcov_fun = NULL
    }
    if('total' %in% metrics){
      total_vcov_fun = substitute(dtsurvey::sur_var(x, na.rm = T, type = 'total', as_list = TRUE,  sv = dtsurvey::sv(DT), ids = `_id`, st = dtsurvey::st(DT)), list(x=x))
    }else{
      total_vcov_fun = NULL
    }
  }else{
    mean_vcov_fun = NULL
    total_vcov_fun = NULL
  }
  # use something like a = DT[, list(list(a), list(b)), env = list(a = mean_fun, b = total_fun), by = byvar]
  # to capture the se and ci returns and then break out post hoc
  # if it is a factor, compute some things separately
  # Following bit creates the call that will be executed within the data.table DT
  # This construction is used for flexibility (build the whole call and take out the null bits)
  the_call = substitute(list(
    time = time_fun,
    variable = X,
    mean = mean_fun,
    median = med_fun,
    total = total_fun,
    numerator = num_fun,
    denominator = denom_fun,
    obs = obs_fun,
    missing = mis_fun,
    missing.prop = misp_fun,
    unique.time = ut_fun,
    ndistinct = ndis_fun,
    mean_vcov = mean_vcov_fun,
    total_vcov = total_vcov_fun
  ),list(X = as.character(x),
         time_fun = time_fun,
         mean_fun = mean_fun,
         med_fun = med_fun,
         total_fun = total_fun,
         num_fun = num_fun,
         denom_fun = denom_fun,
         mis_fun = mis_fun,
         misp_fun = misp_fun,
         ut_fun = ut_fun,
         obs_fun = obs_fun,
         ndis_fun = ndis_fun,
         mean_vcov_fun = mean_vcov_fun,
         total_vcov_fun = total_vcov_fun))

  #remove nulls
  the_call = as.list(the_call)
  nulls = vapply(the_call, is.null, TRUE)
  the_call = the_call[!nulls]

  #Remove numerator if x is a factor since that gets calculated differently
  if(xisfactor) the_call = the_call[which(!names(the_call) %in% c('ndistinct', 'numerator'))]
  the_call = as.call(the_call)

  #compute the aggregations
  if(!xisfactor){

    res = eval(substitute(DT[, ccc, by = by], list(ccc = the_call)))
    res[, level := NA]

    # convert to standard data.table and clean up
    data.table::setDT(res)
    data.table::setattr(res, 'sdes', NULL)
    data.table::setattr(res, 'stype', NULL)
  }else{
    r1 = eval(substitute(DT[, ccc, by = by], list(ccc = the_call)))

    if('ndistinct' %in% metrics){
      r1[, ndistinct := length(unique(DT[[x]]))]
    }
    #for factors, the numerator needs to be calculated separately per level.
    r2 = DT[, list(
      numerator = .N
    ),
    by = c(by, as.character(x))]
    data.table::setnames(r2, as.character(x), 'level')

    r1[, id := .I]

    # convert to standard data.table and clean up
    data.table::setDT(r1)
    data.table::setattr(r1, 'sdes', NULL)
    data.table::setattr(r1, 'stype', NULL)
    data.table::setDT(r2)
    data.table::setattr(r2, 'sdes', NULL)
    data.table::setattr(r2, 'stype', NULL)



  }

  #if mean/total were asked for, split out things
  if(any(c('rate', 'mean') %in% metrics)){

    if(xisfactor){
      r1m = r1[, unlist(mean, recursive = FALSE), id]
      data.table::setnames(r1m, c('id', 'mean', 'mean_se', 'mean_lower', 'mean_upper', 'level'))

    }else{
      r1m = NULL
      if(nrow(res) == 0){
        res[, mean := NULL]
        res[, c('mean', 'mean_se', 'mean_lower', 'mean_upper') := NA_real_]
      }else{
        res[, c('mean', 'mean_se', 'mean_lower', 'mean_upper') := data.table::rbindlist(mean)]

      }
    }
  }

  if('total' %in% metrics){

    if(xisfactor){
      r1t = r1[, unlist(total, recursive = FALSE), id]
      data.table::setnames(r1t, c('id', 'total', 'total_se', 'total_lower', 'total_upper', 'level'))

    }else{
      r1t = NULL
      if(nrow(res)==0){
        res[, total := NULL]
        res[, c('total', 'total_se', 'total_lower', 'total_upper') := NA_real_]
      }else{
        res[, c('total', 'total_se', 'total_lower', 'total_upper') := data.table::rbindlist(total)]

      }
    }

  }

  #assemble factors
  if(any(c('mean', 'rate', 'total') %in% metrics) && xisfactor){
    if(exists('r1m')){
      r1 = merge(r1[, mean:=NULL], r1m, by = 'id')
    }
    if(exists('r1t')){
      byby = c('id', 'level')
      byby = byby[c(T, 'level' %in% names(r1))]
      r1 = merge(r1[, total:= NULL], r1t, by= byby)
    }
  }
  if(xisfactor){
    if(!any(c('rate','total','mean') %in% metrics)){
      r1[, one := 1]
      r2[, one := 1]

      res = merge(r1, r2, by = c('one',by), all.x = T)
      res[, one := NULL]

      if(!'numerator' %in% metrics){
        res[, numerator := NULL]
      }


    }else if('numerator' %in% metrics){
      res = merge(r1,r2, by = c(by, 'level'), all.x = T)
    } else{
      res = r1
    }
    res[, id := NULL]
  }

  #if asked for, compute rse and rate
  if('rse' %in% metrics){
    if(isTRUE(binary)){
      res[, rse := 100 * (mean_se / pmin(mean, 1 - mean))] # need pmin because need minimum for each row
    }else{
      res[, rse := 100*(mean_se / mean)]
    }
  }

  if('rate' %in% metrics){
    res[, c('rate', paste0('rate', c('_se', '_lower', '_upper'))) := .SD * per,
        .SDcols = c('mean', paste0('mean_',c('se', 'lower', 'upper')))]
    res[, rate_per := per]

    if(!'mean' %in% metrics) res[, c('mean', paste0('mean_',c('se', 'lower', 'upper'))) := NULL]

  }


  if(!is.null(time_var)) data.table::setnames(res, 'time', time_var)

  if(!is.null(by)) data.table::setorderv(res, cols = c(by, 'level'))

  #make 0 row if ph.data is 0
  if(nrow(DT) == 0) res = res[FALSE]

  # Check for dupes (usually factors with NAs an some such)
  res[!duplicated(res, by = unique(c(by, 'level', 'variable'))),]


  return(res)


}

