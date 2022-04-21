#' @rdname calc
#' @export
#' @importFrom stats median na.omit
calc.dtsurvey = function(ph.data,
                         what,
                         where, #this is a change from the main calc framework
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

  #global variables used by data.table declared as NULL here to play nice with devtools::check()
  tv <- NULL

  if(!all(c('stype', 'sdes') %in% names(attributes(ph.data)))){

    stop('`ph.data` input does not have the right attributes for this to work. Usually this is caused by using dplyr verbs on a dtsurvey object.
         If you run into this issue on a administrative type dataset, just call dtadmin after all your data munging. If this occurs on a survey,
         and you do some filtering and you just have to use dplyr: use the srvyr package (srvyr::as_survey_design()) to convert your data.frame up front,
         to a survey like object and then do your dplyr-ing. After the data is ready, use dtsurvey::as.dtsurvey (or as.dtrepsurvey) to convert it into the right format.')
  }

  call = match.call()

  #filter the dataset
  if(!missing(where)){

    if(is.character(call[['where']])){
      where = str2lang(call[['where']])
      warning('`where` is a string. It was converted so that it would work, but in the future, this might turn into an error. Please pass unquoted commands that will resolve to a logical' )
    }

    e <- substitute(where)
    r <- eval(e, ph.data, parent.frame())
    stopifnot('`where` does not resolve to a logical' = is.logical(r))
    ph.data = ph.data[r,]
    #do.call(subset, args = list(x = ph.data, subset = e)) an alternative approach

    if(nrow(ph.data) == 0){
      warning(paste0('Provided `where` statement subsets out all rows : ', capture.output(print(e))))
    }


  }

  #validate other inputs
  #validate what
  stopifnot('what must be a character value and a column in `ph.data`' =
              is.character(what) && all(what %in% names(ph.data)))
  #validate by
  if(!is.null(by)){
    stopifnot('`by` must be a chatacter vector and represent (a) column(s) in `ph.data`'=
                is.character(by) && all(by %in% names(ph.data)))
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
    times = unique(na.omit(ph.data[[time_var]]))
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
    #Determine the type of CI method to use
    meth = 'mean' #the default
    st = attr(ph.data, 'stype')
    whatfactor = is.factor(ph.data[[wht]])
    if(st == 'admin' && (whatfactor || proportion == T)) meth = 'unweighted_binary'
    if(st != 'admin' && proportion == T) meth = 'xlogit'

    #Compute the metric
    r = lapply(wins, function(w){

      if(usewins == TRUE){
        sub_i = substitute(tv %in% w, list(tv = as.name(time_var), w = w))
      }

      compute(eval(substitute(ph.data[sub_i], env = list(sub_i = sub_i))), wht, by = by, metrics,
              ci_method = meth, level = ci,
              time_var = time_var, time_format = time_format,
              per = per, window = !(is.logical(sub_i) && sub_i))
    })

    data.table::rbindlist(r)

  })


  res = rbindlist(res, fill = TRUE)

  return(res)

}

#' A function to compute a metric as part of calc.dtsurvey
#' see the help/documentation for calc and/or smeanto better understand the inputs
#' @noRd
compute = function(DT, x, by = NULL, metrics, ci_method = 'mean', level = .95, time_var, time_format, per = 1, window = FALSE){


  # if(nrow(DT) == 0) warning('No valid rows to compute on given `where` and `win` conditions')


  #global variables used by data.table declared as NULL here to play nice with devtools::check()
  cim <- l <- `_id` <- `..sv` <- `..st` <- tv <- X <- ccc <- ndistinct <- id <- total <- one <- numerator <- rse <- mean_se <- rate_per <- NULL

  sv = attr(DT, 'sdes')
  st = attr(DT, 'stype')

  #For each metric, define a function to compute it-- or ignore it if not called for.
  xisfactor = is.factor(DT[[x]])

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
      med_fun = substitute(median(x, na.rm = T) * 1.0, list(x=x))
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

  #use something like a = DT[, .(list(a), list(b)), env = list(a = mean_fun, b = total_fun), by = byvar]
  #to capture the se and ci returns and then break out post hoc
  #if it is a factor, compute some things separately
  # browser()
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
    ndistinct = ndis_fun
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
         ndis_fun = ndis_fun))

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
    setnames(r2, as.character(x), 'level')

    r1[, id := .I]



  }

  #if mean/total were asked for, split out things
  if(any(c('rate', 'mean') %in% metrics)){

    if(xisfactor){
      r1m = r1[, unlist(mean, recursive = FALSE), id]
      setnames(r1m, c('id', 'mean', 'mean_se', 'mean_lower', 'mean_upper', 'level'))

    }else{
      r1m = NULL
      if(nrow(res) == 0){
        res[, mean := NULL]
        res[, c('mean', 'mean_se', 'mean_lower', 'mean_upper') := NA_real_]
      }else{
        res[, c('mean', 'mean_se', 'mean_lower', 'mean_upper') := rbindlist(mean)]

      }
    }
  }

  if('total' %in% metrics){

    if(xisfactor){
      r1t = r1[, unlist(total, recursive = FALSE), id]
      setnames(r1t, c('id', 'total', 'total_se', 'total_lower', 'total_upper', 'level'))

    }else{
      r1t = NULL
      if(nrow(res)==0){
        res[, total := NULL]
        res[, c('total', 'total_se', 'total_lower', 'total_upper') := NA_real_]
      }else{
        res[, c('total', 'total_se', 'total_lower', 'total_upper') := rbindlist(total)]

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
    res[, rse := 100*(mean_se / mean)]
  }

  if('rate' %in% metrics){
    res[, c('rate', paste0('rate', c('_se', '_lower', '_upper'))) := .SD * per,
        .SDcols = c('mean', paste0('mean_',c('se', 'lower', 'upper')))]
    res[, rate_per := per]

    if(!'mean' %in% metrics) res[, c('mean', paste0('mean_',c('se', 'lower', 'upper'))) := NULL]

  }


  if(!is.null(time_var)) data.table::setnames(res, 'time', time_var)

  #make 0 row if ph.data is 0
  if(nrow(DT) == 0) res = res[FALSE]

  return(res)


}

