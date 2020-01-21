#' @rdname calc
#' @importFrom srvyr filter group_by %>% select summarize
#' @importFrom dplyr n
#' @importFrom data.table ":=" setnames
#' @importFrom rlang quos !! !!! syms
#' @export
calc.tbl_svy <- function(ph.data,
                         what,
                         ...,
                         by = NULL,
                         metrics = c('mean', "numerator", "denominator", "missing", "total"),
                         per = NULL,
                         win = NULL,
                         time_var = "chi_year",
                         proportion = FALSE,
                         verbose = FALSE){

  #catches
  if(verbose && !missing(per)){
    warning('Argument `per` is not implemented for tbl_svy arguments. It will be ignored')
  }

  if(!is.null(win)){
    if(is.null(time_var)) stop('win(dow) specified without a time_var')
  }

  #data.table visible bindings
  variable <- ci <- NULL

  opts = survey_metrics()
  #confirm that svy is a tab_svy
  stopifnot(inherits(ph.data, 'tbl_svy'))

  svy_names <- names(ph.data$variables)

  #confirm that what is in the dataset
  stopifnot(is.character(what))

  what_check <- check_names('what', 'ph.data', svy_names, what)
  if(what_check != '') stop(what_check)

  #validate where
  if(!missing(...)){
    where <- rlang::quos(...)
  }else{
    where = NULL
  }

  #validate by
  if(!missing(by)){

    stopifnot(is.character(by))

    by_check <- check_names('by', 'ph.data', svy_names, by)
    if(by_check != '') stop(by_check)

    #and if its not missing, filter such that no by variable has NAs
    #svy <- svy %>% filter_at(by, ~ !is.na(.))

    # mis_vars = apply(ph.data$variables[, by], 1, function(x) sum(is.na(x)))
    #
    # #remove missing by vars
    # ph.data <- ph.data %>% srvyr::filter(!!mis_vars==0)

  }

  #confirm that metrics are properly specified
  metrics <- match.arg(metrics, opts, several.ok = T)

  #subset ph.data to only the columns needed (and rows)
  if(!is.null(where)){
    ph.data <- ph.data %>% srvyr::filter(!!!where)
  }

  delete_time = F
  if(is.null(time_var)){
    time_var = '_THETIME'
    ph.data <- ph.data %>% mutate(`_THETIME` = NA)
    delete_time = T
  }

  ph.data <- ph.data %>% srvyr::select(what, by, time_var)

  whats = rlang::syms(what)
  time_var = rlang::sym(time_var)

  if(!is.null(by)){
    by = rlang::syms(by)
  }

  #Group the dataset if relevant
  if(!is.null(by)) ph.data <- ph.data %>% srvyr::group_by(!!!by)

  #create the time windows
  times = na.omit(ph.data$variables[[as.character(time_var)]])
  if(length(times)>0 && !is.null(win)){
    wins = seq(min(times), max(times - win + 1))
    wins = lapply(wins, function(x) seq(x, x + win - 1))
  }else{
    wins = list(integer(0))
  }

  #For each variable and window, calculate specified metrics
  res <- lapply(whats, function(what){

    #Make sure the data does not have NA values in the chosen variable
    whatvar = as.character(what)

    #make sure there are some values left
    if(nrow(ph.data) == 0){
      stop(paste('When computing metrics for', whatvar, 'there are no non-NA values given the contraints provided via ...'))
    }

    #for each window, compute results
    fin <- lapply(wins, function(windo){

      if(length(windo) > 0){
        ret <- suppressMessages(srvyr::filter(ph.data, !!time_var %in% windo))
      }else{
        ret <- ph.data
      }

      #if the variable is numeric, compute normally
      #if a factor find the relative fractions
      if(is.numeric(ret$variables[[as.character(what)]])){
        ret <- ret %>%
          srvyr::summarize(
            mean = srvyr::survey_mean(!!what, na.rm = T, vartype = c('se', 'ci'), proportion = proportion),
            median = unweighted(median(!!what)), #srvyr::survey_median(!!what, na.rm = T, vartype = NULL), #This isn't working at the moment. Figure it out later
            total = srvyr::survey_total(!!what, vartype = c('se', 'ci'),na.rm = T),
            numerator = srvyr::unweighted(sum(!!what, na.rm = T)), #only relevant for binary variables
            denominator = srvyr::unweighted(dplyr::n()),
            missing = srvyr::unweighted(sum(is.na(!!what))),
            time = srvyr::unweighted(format_time(!!time_var)),
            ndistinct = srvyr::unweighted(length(na.omit(unique(!!what)))),
            unique.time = srvyr::unweighted(length(unique(!!time_var)))
        ) %>% setDT


        ret[, level := NA]
        ret[, denominator := denominator - missing]
        data.table::setnames(ret, c('mean_low', 'mean_upp') , c('mean_lower', 'mean_upper'))
        data.table::setnames(ret, c('total_low', 'total_upp') , c('total_lower', 'total_upper'))
      }else{

        #make sure there is not NAs in the what variable
        ph.data <- suppressMessages(ph.data %>% filter(!is.na(!!what)))
        #move to a different function since its more involved
        ret <- calc_factor(ret, what, by, time_var)
        ret[, median := NA]
      }

      ret[, variable := as.character(what)]
      ret[, missing.prop := missing/(missing + numerator + denominator)]
      ret[, rse := mean_se/mean]
      ret[, obs := (missing + numerator + denominator)]
      return(ret)
    })

    fin <- data.table::rbindlist(fin, use.names = T)
    data.table::setnames(fin,'time', as.character(time_var))
    fin[, rate_per := NA]
    fin[, rate := NA]

    return(fin)

  })

  #combine
  res = data.table::rbindlist(res, use.names = TRUE)

  #clean up
  #numerators don't make sense when its not a proportion
  #if(!proportion) metrics = metrics[!metrics %in% 'numerator']

  #compute rate
  if(!is.null(per)){
    metrics = unique(metrics, c('rate', paste0('rate', c('_se', '_lower', '_upper'))))
  }else{
    per = 1
  }

  res[, c('rate', paste0('rate', c('_se', '_lower', '_upper'))) := .SD * per, .SDcols = c('mean', paste0('mean_',c('se', 'lower', 'upper')))]
  res[, rate_per := per]


  if(length(opts[!opts %in% metrics]) >0){
    res[, opts[!opts %in% metrics] := NULL]
  }

  if(delete_time) res[, `_THETIME` := NULL]

  return(res)

}
