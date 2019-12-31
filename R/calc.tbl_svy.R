#' @rdname calc
#' @importFrom srvyr filter group_by %>% select summarize
#' @importFrom dplyr n
#' @importFrom data.table ":="
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
                         proportion = FALSE){

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

    mis_vars = apply(ph.data$variables[, by], 1, function(x) sum(is.na(x)))

    #remove missing by vars
    ph.data <- ph.data %>% srvyr::filter(!!mis_vars==0)

  }

  #confirm that metrics are properly specified
  metrics <- match.arg(metrics, opts, several.ok = T)

  #subset ph.data to only the columns needed (and rows)
  if(!is.null(where)){
    ph.data <- ph.data %>% srvyr::filter(!!!where)
  }

  ph.data <- ph.data %>% srvyr::select(what, by)


  whats = rlang::syms(what)

  #make sure there is not NAs in the what variable
  #svy <- svy %>% filter(!is.na(!!what))

  if(!is.null(by)){
    by = rlang::syms(by)
  }


  res <- lapply(whats, function(what){
    svydata = suppressMessages(filter(ph.data, !is.na(!!what)))
    whatvar = as.character(what)
    out <- svydata %>% srvyr::group_by(!!!by) %>% srvyr::summarize(
      mean = srvyr::survey_mean(!!what, na.rm = T, vartype = 'se', proportion = proportion),
      ci = srvyr::survey_mean(!!what, na.rm = T, vartype = 'ci', proportion = proportion),
      #median = srvyr::survey_median(!!what, na.rm = T, vartype = NULL), This isn't working at the moment. Figure it out later
      total = srvyr::survey_total(!!what, na.rm = T),
      numerator = srvyr::unweighted(sum(!!what == 1, na.rm = T)),
      denominator = srvyr::unweighted(dplyr::n())
    ) %>% setDT

    out[, variable := whatvar]

    return(out)

  })

  res = rbindlist(res)

  res[, ci := NULL]
  data.table::setnames(res, c('mean_se', 'ci_low', 'ci_upp'), c('se', 'lower', 'upper'))

  if(length(opts[!opts %in% metrics]) >0){
    res[, opts[!opts %in% metrics] := NULL]
  }

  return(res)

}
