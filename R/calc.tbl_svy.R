#' @rdname calc
#' @importFrom srvyr filter group_by %>% select summarize
#' @importFrom dplyr n
#' @importFrom data.table ":=" setnames
#' @importFrom rlang quos !! !!! syms
#' @importFrom tidyselect all_of
#' @importFrom data.table setnames setDT
#' @importFrom forcats fct_explicit_na
#' @export
calc.tbl_svy <- function(ph.data,
                         what,
                         ...,
                         by = NULL,
                         metrics = metrics(),
                         per = NULL,
                         win = NULL,
                         time_var = NULL,
                         proportion = FALSE,
                         fancy_time = TRUE,
                         ci = .95,
                         verbose = FALSE){

  if(verbose && !missing(per)){
    warning('Argument `per` is not implemented for tbl_svy arguments. It will be ignored')
  }

  if(!is.null(win)){
    if(is.null(time_var)) stop('win(dow) specified without a time_var')
  }

  # validate 'fancy_time'
  if(!is.logical(fancy_time)){
    stop("'fancy_time' must be specified as a logical (i.e., TRUE, T, FALSE, or F)")
  }
  if(fancy_time==TRUE){time_format <- format_time}else{time_format <- format_time_simple}

  #data.table visible bindings
  variable <- NULL

  opts = metrics()
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
  }

  #confirm that metrics are properly specified
  invalid = setdiff(metrics,opts)
  if(length(invalid)>0){
    stop(paste0('Invalid metrics detected: ', paste(invalid, collapse = ','), '. ', 'Review the list of available metrics by calling `metrics()`'))
  }

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

  ph.data <- ph.data %>% srvyr::select(tidyselect::all_of(what), tidyselect::all_of(by), tidyselect::all_of(time_var))

  whats = rlang::syms(what)
  time_var = rlang::sym(time_var)

  if(!is.null(by)){
    #capture the by classes so they can be converted back
    by_class = lapply(by, function(x) class(ph.data$variables[[x]]))
    convert_by = T
    ph.data <- ph.data %>% mutate_at(tidyselect::all_of(by), forcats::fct_explicit_na)
    by = rlang::syms(by)
  }else{
    convert_by = T
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

        missin <- ret %>% summarize(missing = srvyr::unweighted(sum(is.na(!!what))))
        gvs = group_vars(ret)
        ret <- ret %>% filter(!is.na({{what}})) %>%
          summarize(
            mean = srvyr::survey_mean(!!what, vartype = c('se', 'ci'), proportion = proportion, level = ci),
            median = unweighted(median(!!what)), #srvyr::survey_median(!!what, na.rm = T, vartype = NULL), #This isn't working at the moment. Figure it out later
            total = srvyr::survey_total(!!what, vartype = c('se', 'ci'),level = ci),
            numerator = srvyr::unweighted(sum(!!what)), #only relevant for binary variables
            denominator = srvyr::unweighted(dplyr::n()),
            time = srvyr::unweighted(time_format({{time_var}})),
            ndistinct = srvyr::unweighted(length(na.omit(unique(!!what)))),
            unique.time = srvyr::unweighted(length(unique({{time_var}})))
        )

        ret = merge(ret, missin, all.x = T, by = gvs)
        data.table::setDT(ret)

        ggg = unique(ret[, .SD, .SDcols = gvs])

        if(verbose && (nrow(missin) != nrow(ggg))){
          ggg = merge(ggg, missin, by = gvs, all.x = T)
          warning(paste(capture.output(print(ggg[!is.na(missing),.SD, .SDcols = gvs])), collapse = "\n"))
        }



        ret[, level := NA]
        data.table::setnames(ret, c('mean_low', 'mean_upp') , c('mean_lower', 'mean_upper'))
        data.table::setnames(ret, c('total_low', 'total_upp') , c('total_lower', 'total_upper'))
      }else{

        #make sure there are no NAs in the what variable
        ph.data <- suppressMessages(ph.data %>% filter(!is.na(!!what)))
        #move to a different function since its more involved
        ret <- calc_factor(ret, what, by, time_var, fancy_time, ci)
        ret[, median := NA_real_]
      }

      ret[, variable := as.character(what)]
      ret[, missing.prop := missing/(denominator + missing)]
      ret[, rse := 100*(mean_se/mean)]
      ret[, obs := denominator + missing]
      return(ret)
    })

    fin <- data.table::rbindlist(fin, use.names = T)
    data.table::setnames(fin,'time', as.character(time_var))
    fin[, rate_per := NA_real_]
    fin[, rate := NA_real_]

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
    per = 1L
  }

  if('rate' %in% metrics){
    res[, c('rate', paste0('rate', c('_se', '_lower', '_upper'))) := .SD * per, .SDcols = c('mean', paste0('mean_',c('se', 'lower', 'upper')))]
    res[, rate_per := per]
    metrics = c(metrics, 'rate_per')
  }
  #if mean, total or rate are requested, add the se, lower, and upper
  isect = intersect(metrics, c('rate', 'total', 'mean'))

  if(length(isect)>0){
    new = as.vector(outer(isect, c('_se', '_lower', '_upper'), paste0))
    metrics = unique(c(metrics, new))
  }

  #keep requested metrics
  na_mets = intersect(metrics, c(grep('total', metrics, value = T), grep('mean', metrics, value = T), 'rse'))
  res[is.na(numerator), (na_mets) := NA]
  res <- res[, c('variable', 'level', as.character(time_var), as.character(by), metrics), with = F]


  if(delete_time) res[, `_THETIME` := NULL]
  if(convert_by){
    byc = as.character(by)
    res[, (byc) := lapply(.SD, function(x) ifelse(x == '(Missing)', NA_character_, as.character(x))), .SDcols = byc]
    res[, (byc) := lapply(seq_len(length(byc)), function(x) dumb_convert(get(byc[x]), by_class[[x]]))]

  }

  return(res)

}
