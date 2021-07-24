#' @rdname calc
#' @importFrom data.table copy data.table rbindlist tstrsplit .N "%like%" "%between%" as.data.table
#' @importFrom stats median na.omit prop.test qnorm qt var na.omit
#' @importFrom rlang quos
#' @importFrom stats na.omit median var prop.test qnorm
#' @export
calc.data.frame = function(ph.data,
                           what,
                           ...,
                           by = NULL,
                           metrics = c('mean', 'numerator', 'denominator'),
                           per = NULL,
                           win = NULL,
                           time_var = NULL,
                           proportion = FALSE,
                           fancy_time = TRUE,
                           ci = .95,
                           verbose = FALSE){

  #global variables used by data.table declared as NULL here to play nice with devtools::check()
  se <- rse <- rate <- rate_per <- level <- time <- variable <- mean_lower <- mean_upper <- mean_se <- `___THETIME___` <- NULL

  # copy data.table to prevent changing the underlying data, also sets copy as class == data.table
  temp.dt <- data.table::as.data.table(ph.data)

  #### VALIDATION ####
  #validate '...' (i.e., where)
  if(!missing(...)){
    #where <- tryCatch(parse(text = paste0(list(...))),  error = function (e) parse(text = paste0(list(bquote(...))))) # convert ... to an expression
    where <- tryCatch(parse(text = paste(unlist(list(...)), collapse = " & ")),
                      error = function (e) parse(text = gsub("~", "", paste(rlang::quos(...), collapse = " & "))) ) # convert ... to an expression
    }else{
      where <- NULL
    }

  #subset temp.dt to only the rows needed
  if(!is.null(where)){
    temp.dt <- temp.dt[eval(where), ]
    if(nrow(temp.dt) <1 ){
      stop(paste0("Your '...' (i.e., ", where, ") filters out all rows of data. Please revise and submit again"))
    }
  }


  #validate 'what'
  if(!is.character(what))
    stop(paste0("The `what` argument must be submitted as a character (i.e., in quotes)"))

  what_check <- check_names('what', 'temp.dt', names(temp.dt), what)
  if(what_check != '') stop(what_check)

  #identify when 'what' is binary (0, 1), other numerics, or a factor. When a factor, convert it to a series of binary columns
  # binary columns
  binary.col <- vapply(temp.dt[, .SD, .SDcols = what],function(x) { all(stats::na.omit(x) %in% 0:1) }, FUN.VALUE=logical(1)) # logical vector
  binary.col <- what[binary.col]  # character vector

  # numeric columns
  numeric.col <- vapply(temp.dt[, .SD, .SDcols = what], is.numeric, FUN.VALUE=logical(1)) # logical vector
  numeric.col <- setdiff(what[numeric.col], binary.col)

  # factor columns (convert to character first to drop off levels that are empty when selected out by ...)
  factor.col <- vapply(temp.dt[, .SD, .SDcols = what], is.factor, FUN.VALUE=logical(1)) # logical vector
  factor.col <- what[factor.col]
  if(length(factor.col) > 0){
    temp.dt[, c(factor.col) := lapply(.SD, droplevels), .SDcols = factor.col]
  }

  # character columns (convert to factors)
  character.col <- vapply(temp.dt[, .SD, .SDcols = what], is.character, FUN.VALUE=logical(1)) # logical vector
  character.col <- what[character.col]
  if(length(character.col) > 0){
    temp.dt[, c(character.col) := lapply(.SD, as.factor), .SDcols = character.col]
  }

  # factor columns
  factor.col <- vapply(temp.dt[, .SD, .SDcols = what], is.factor, FUN.VALUE=logical(1)) # logical vector
  factor.col <- what[factor.col]

  names.before <- names(copy(temp.dt))

  # convert factors to series of binary columns (xxx_prefix is to identify the expanded data below)
  if(length(factor.col) > 0){ # sometimes there are no factors, so this should be conditional
    for(i in 1:length(factor.col)){
      temp.dt[, paste0(factor.col[i], "_SPLIT_HERE_", levels(temp.dt[[factor.col[i]]]) ) :=
                lapply(levels( get(factor.col[i]) ), function(x) as.integer(x == get(factor.col[i]) ))]
    }
  }

  # update 'what' to reflect all binaries, including those made from factors
  what.metrics <- c(setdiff(what, factor.col), setdiff(names(temp.dt), names.before) )

  #validate 'by'
  if(!missing(by) & !is.null(by)){
    if(!is.character(by))
      stop(paste0("The `by` argument must be submitted as a character (i.e., in quotes)"))

    by_check <- check_names('by', 'svy', names(temp.dt), by)
    if(by_check != '') stop(by_check)
  }

  #validate 'metrics'
  # pull list of standard available metrics
  opts <- metrics()

  # limits metrics to those that have been pre-specified, i.e., non-standard metrics are dropped
  if(!is.null(metrics)){
    metrics <- match.arg(metrics, opts, several.ok = T)
  }

  #validate 'per'
  if("rate" %in% metrics & is.null(per)){
    per <- 1 # default denominator of 1
  }
  if("rate" %in% metrics & !is.null(per) & all.equal(per, as.integer(per))!=T ){
    stop("If specified, the 'per' argument must be an integer")
  }

  #validate 'time_var'
  if(is.null(time_var)){
    #stop("The 'time_var' must be specified, (e.g., time_var = 'chi_year')")
    time_var = "___THETIME___"
    temp.dt[, (time_var) := -1]
    droptime = TRUE
  }else{
    droptime = FALSE
  }

  if(!is.null(time_var) & !time_var %in% names(temp.dt)){
    stop("You have specified a 'time_var' that does not exist in 'ph.data'.
         Please check your spelling and try again.")
  }

  #validate 'win'
  if(!is.null(win)){
    if(win %% 1 != 0){
      stop("If specified, the 'win' argument must be an integer")
    }
  }

  if(!is.null(time_var) & !is.numeric(temp.dt[[time_var]])){
    stop("The specified 'time_var' must be of type numeric.")
  }

  # validate 'fancy_time'
  if(!is.logical(fancy_time)){
    stop("'fancy_time' must be specified as a logical (i.e., TRUE, T, FALSE, or F)")
  }

  #### CREATE CALC FUNCTIONS ####
  #select type of time formatting
  if(fancy_time==T){time_format <- format_time}else{time_format <- format_time_simple}

  # function to calculate metrics
  calc_metrics <- function(X, DT){

    ndistinct_adjustement <- 0
    if(grepl("_SPLIT_HERE_", X)){ndistinct_adjustement <- 1} # these are cases where a factor was made into a series of binaries of 0|1, but shoudl only count the 1.

    DT[, list(
      time = time_format(get(time_var)[!is.na(get(X))]),
      variable = as.character(X),
      mean = mean(get(X), na.rm = T),
      median = as.numeric(stats::median(get(X), na.rm = T)),
      total = sum(get(X), na.rm = T),
      numerator = sum(get(X), na.rm = T),
      denominator = sum(!is.na( get(X) )),
      se = sqrt(stats::var(get(X), na.rm = T)/sum(!is.na( get(X) )) ),
      obs = .N,
      missing = sum(is.na( get(X) )),
      missing.prop = sum(is.na( get(X) ) / .N),
      unique.time = length(unique( get(time_var)[!is.na(get(X))] )),
      ndistinct = length(unique(na.omit(get(X)))) - ndistinct_adjustement
    ),
    by = by]
  }

  #### APPLY CALC FUNCTION ####
  # apply the calc_metrics
  if(is.null(win)){
    res.metrics <- lapply(X = as.list(what.metrics), FUN = calc_metrics, DT = temp.dt)
    res.metrics <- data.table::rbindlist(res.metrics, use.names = T)
  }

  if(!is.null(win)){
    res.metrics <- data.table::data.table() # empty table for appending results
    for(yr in seq(min(temp.dt[, c(get(time_var))]), (max(temp.dt[, c(get(time_var))])-win+1) ) ){
      temp.results <- lapply(X = as.list(what.metrics), FUN = calc_metrics, DT = temp.dt[get(time_var) %in% seq(yr, (yr+win-1) )])
      temp.results <- data.table::rbindlist(temp.results, use.names = T)
      res.metrics <- rbind(res.metrics, temp.results, fill = TRUE)
    }
  }

  #### ADDITIONAL CALCULATIONS & DATA PREP ####
  # Split names for factor columns
  splittys = grepl('_SPLIT_HERE_', res.metrics[,variable], fixed = TRUE)
  if(any(splittys)){
    res.metrics[, c("variable", "level") := data.table::tstrsplit(variable, "_SPLIT_HERE_", fixed=TRUE)]
  }else{
    res.metrics[, level := NA]
  }

  # Calculate lower, upper, se, rse
  # PROPORTIONS : Binary (& factor) variables will use prop.test function for CI. This uses the score method ... suggested by DOH & literature
  res.metrics.prop <- res.metrics[variable %in% c(binary.col, factor.col) & denominator!=0] # split off just binary/factor data. Undefined when denominator == 0, so drop
  if(length(binary.col) + length(factor.col) > 0){
    numerator <- res.metrics.prop$numerator
    denominator <- res.metrics.prop$denominator
    lower <- rep(NA, nrow(res.metrics.prop)) # create empty vector to hold res.metricsults
    upper <- rep(NA, nrow(res.metrics.prop)) # create empty vector to hold res.metricsults
    for(i in 1:nrow(res.metrics.prop)){
      lower[i] <- suppressWarnings(stats::prop.test(x = numerator[i], n = denominator[i], conf.level = ci, correct = F)$conf.int[1]) # the score method ... suggested by DOH & others
      upper[i] <- suppressWarnings(stats::prop.test(x = numerator[i], n = denominator[i], conf.level = ci, correct = F)$conf.int[2])
    }
    res.metrics.prop[, mean_lower := lower]
    res.metrics.prop[, mean_upper := upper]
    # res.metrics.prop[, se := sqrt((mean*(1-mean))/denominator) ] # calculated the SE empirically above. Confirmed that res.metricsults are ~same as from this formula
    # the calculation based on variance differed from this forumla when samples were tiny. In those cases, the empirical ones were larger and therefore more conservative
  }

  # MEANS: Numeric/non-binary need to have their CI calculated separately
  res.metrics.mean <- res.metrics[variable %in% c(numeric.col)]
  if(length(numeric.col) > 0){

    res.metrics.mean[denominator>30, mean_lower := mean - stats::qnorm(1 - ((1-ci)/2))*se] # when n>30, central limit theorm states distribution is normal & can use Z-scores.metrics
    res.metrics.mean[denominator>30, mean_upper := mean + stats::qnorm(1 - ((1-ci)/2))*se] # when n>30, central limit theorm states distribution is normal & can use Z-scores.metrics
    suppressWarnings(res.metrics.mean[denominator<=30, mean_lower := mean - qt(1 - ((1-ci)/2),df=denominator-1)*se]) # when n<=30, use t-distribution which accounts for smaller n having greater spread (assumes underlying data is normally distributed)
    suppressWarnings(res.metrics.mean[denominator<=30, mean_upper := mean + qt(1 - ((1-ci)/2),df=denominator-1)*se]) # when n<=30, use t-distribution which accounts for smaller n having greater spread (assumes underlying data is normally distributed)
    res.metrics.mean[mean_lower < 0, mean_lower := 0] # prevent negative values for confidence interval
  }

  # Append data for proportions and means
  res.metrics <- rbind(res.metrics.prop, res.metrics.mean, fill = T)
  setnames(res.metrics, "se", "mean_se")

  # Calculate RSE
  res.metrics[, rse := 100*(mean_se / mean)] # Informed by Abby 2/7/2020 that the proportion should be multiplied by 100

  # Set Median to NA if variable is not a numeric/continuous
  res.metrics[!variable %in% numeric.col, median := NA]

  # apply the 'per' if rate was specified in metric (rates are only applicable to proportions)
  if("rate" %in% metrics){
    res.metrics[variable %in% unique(res.metrics.prop$variable), c('rate', paste0('rate', c('_se', '_lower', '_upper'))) := .SD * per, .SDcols = c('mean', paste0('mean_',c('se', 'lower', 'upper')))]
    res.metrics[variable %in% unique(res.metrics.prop$variable), rate_per := per]
  } else{res.metrics[, c("rate", paste0('rate', c('_per', '_lower', '_upper', '_se'))) := NA]}

  #### CLEAN UP ####
  res <- res.metrics
  data.table::setcolorder(res, c("variable", "level", "time", setdiff(by, time_var), "median",
                                 "mean", 'mean_lower', 'mean_upper', 'mean_se', "rse",
                                 "rate", "rate_per", "rate_lower", "rate_upper", "rate_se",
                                 "total", "obs", "numerator", "denominator", "missing", "missing.prop", "unique.time"))

  # When metrics() includes missing counts / proportions, fix factors (otherwise have the number of missing for each level of a factor)
  if( nrow(res[variable %in% factor.col]) > 0 & sum(grepl("missing", metrics) ) > 0){
    res.factors <- res[variable %in% factor.col] # split off rows with variables that are factors
    res <- res[!variable %in% factor.col] # keep rows with non factors
    cols.to.drop <- setdiff(metrics(), c("obs", "unique.time", "missing", "missing.prop")) # identify columns to drop
    cols.to.keep <- setdiff(names(res.factors), c(cols.to.drop, "level", "mean_se", "mean_lower", "mean_upper")) # identify columns to keep
    mi.table <- unique(copy(res.factors)[, ..cols.to.keep]) # create a table of factor variables with minimal missing data
    res.factors[, grep("missing", metrics, value = T) := NA] # set missing to NA in rows with factor variables because have data in new table (mi.table)
    res <- rbind(res, res.factors, mi.table, fill = T) # combine all three datasets
  }

  # Sort / order results
  data.table::setorder(res, variable, level, time)
  data.table::setnames(res, 'time', time_var)

  #if mean, total or rate are requested, add the se, lower, and upper
  isect = intersect(metrics, c('rate', 'total', 'mean'))

  if(length(isect)>0){
    new = as.vector(outer(isect, c('_se', '_lower', '_upper'), paste0))
    metrics = unique(c(metrics, new))
  }

  if('rate' %in% metrics) metrics <- c(metrics, 'rate_per')

  #keep requested metrics
  na_mets = intersect(metrics, c(grep('total', metrics, value = T), grep('mean', metrics, value = T), 'rse'))
  res[is.na(numerator), (na_mets) := NA]
  res <- res[, c('variable', 'level', as.character(time_var), as.character(by), metrics), with = F]
  # return.vars.start <- c("variable", "level", "time", by)
  # #return.vars.end <- c("obs", "numerator", "denominator", "missing", "unique.time")
  # return.vars.middle <- c()
  # if("mean" %in% metrics){
  #   return.vars.middle <- c(return.vars.middle, "mean", paste0('mean', c('_se', '_lower', '_upper')))
  # }
  # if("rate" %in% metrics){
  #   return.vars.middle <- c(return.vars.middle, "rate", paste0('rate', c('_per', '_se', '_lower', '_upper')))
  # }
  # for(i in c("median", "rse", "missing.prop", "ndistinct", "total")){
  #   if(i %in% metrics){return.vars.middle <- c(return.vars.middle, i)}
  # }
  #
  # return.vars <- c(return.vars.start, return.vars.middle)

  # drop columns no longer needed
  #res <- res[, ..return.vars]

  if(droptime) res[, (time_var) := NULL]

  #### CLOSE ####
  return(res)

  }
