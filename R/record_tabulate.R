#' Compute metrics from vital stats or other count data
#'
#' @param my.dt data.table with count data
#' @param what character vector. Variable to tabulate "over". Must match a column name in my.dt
#' @param ... expressions to be passed to \code{\link{filter}}
#' @param by character vector. Must refer to variables within my.dt. The variables within my.dt to compute `what` by
#' @param metrics character. See \code{\link{vital_metrics}} for the available options. Note, all metrics are calculated-- this argument just specifies which one gets returned
#' @param per integer. The denominator when "rate" or "adjusted-rate" are selected as the metric.
#'
#'
#' @return a data.table containing the results
#' @details
#' This function calculates `metrics` for each variable in `what` from rows meeting the conditions specified by `where` for each grouping implied by `by`.
#'
#' @importFrom rlang quos
#' @import 
#' @import data.table
#' @export
#'
#' @examples
#' test.data <- get_data_birth(year = 2015:2017)
#' 
#' test.results <- vital_tabulate(test.data, 
#'                                what = c("kotelchuck", "fetal_pres"), 
#'                                "chi_year == 2016 & chi_sex %in% c('Male', 'Female')", 
#'                                by = c("chi_year", "chi_sex"), 
#'                                metrics = c("mean", "numerator", "denominator", "missing", "total", "lower", "upper", "se", "mising.prop"))
#'
#'
record_tabulate = function(my.dt, what, ..., by = NULL, metrics = c('mean', "numerator", "denominator", "missing", "total"), per = NULL){
  # copy data.table to prevent changing the underlying data
  temp.dt <- copy(my.dt)
  
  opts = vital_metrics()
  
  #validate 'what'
  if(!is.character(what))
    stop(paste0("The `what` argument must be submitted as a character (i.e., in quotes)"))
  
  what_check <- check_names('what', 'temp.dt', names(temp.dt), what)
  if(what_check != '') stop(what_check)
  
  #if 'what' is not binary (0,1), convert it to a series of binary columns
    # identify the factor columns
      binary.columns <- sapply(temp.dt,function(x) { all(na.omit(x) %in% 0:1) }) # logical vector 
      binary.columns <- names(temp.dt[, ..binary.columns]) # character vector 
      what.factors <- setdiff(what, binary.columns)
      names.before <- names(copy(temp.dt))
      
    # convert factors to series of binary columns (xxx_prefix is to identify the expanded data below)
      for(i in 1:length(what.factors)){
        temp.dt[, paste0(what.factors[i], "_SPLIT_HERE_", levels(temp.dt[[what.factors[i]]]) ) := 
             lapply(levels( get(what.factors[i]) ), function(x) as.integer(x == get(what.factors[i]) ))]
      }
      
    # update 'what' to reflect all binaries
      what <- c(setdiff(what, what.factors), setdiff(names(temp.dt), names.before) )
      
  
  #validate '...' (i.e., where)
  where <- NULL
  if(!missing(...)){
    where <- tryCatch(parse(text = paste0(list(...))),  error = function (e) parse(text = paste0(list(bquote(...))))) # convert 'where' into an expression
  }

  #validate 'by'
  if(!missing(by)){
    if(!is.character(by))
      stop(paste0("The `by` argument must be submitted as a character (i.e., in quotes)"))
    
    by_check <- check_names('by', 'svy', names(temp.dt), by)
    if(by_check != '') stop(by_check)
  }

  #validate 'per'
  if("rate" %in% metrics & is.null(per)){
    per <- 1000 # default denominator of 1000
  }
  if("rate" %in% metrics & !is.null(per) & all.equal(per, as.integer(per))!=T ){
    stop("The 'per' argument must be an integer")
  }

  #limits metrics to those that have been pre-specified, i.e., non-starndard metrics are dropped
  metrics <- match.arg(metrics, opts, several.ok = T)

  #subset temp.dt to only the rows needed
  if(!is.null(where)){
    temp.dt <- temp.dt[eval(where), ]
  }

  # function to calculate metrics
  res <- data.table() # create empty data.table for appending results
  for(i in 1:length(what)){
    res <- rbind(res, 
            temp.dt[, .(
              variable = as.character(what[i]), 
              mean = mean(get(what[i]), na.rm = T),
              numerator = sum(get(what[i]), na.rm = T),
              denominator = sum(!is.na( get(what[i]) )),
              total = .N, 
              years = format.years(list(sort(unique(chi_year)))),
              missing = sum(is.na( get(what[i]) )),
              missing.prop = sum(is.na( get(what[i]) ) / .N) 
              ), 
            by = by
            ], 
            fill = TRUE
          )
  }
  
  # Calculate lower, upper, & se
  numerator <- res$numerator
  denominator <- res$denominator
  lower <- rep(NA, nrow(res)) # create empty vector to hold results
  upper <- rep(NA, nrow(res)) # create empty vector to hold results
  for(i in 1:nrow(res)){
    lower[i] <- prop.test(numerator[i], denominator[i], conf.level = 0.95, correct = F)[[6]][1] # the score method ... suggested by DOH & others
    upper[i] <- prop.test(numerator[i], denominator[i], conf.level = 0.95, correct = F)[[6]][2]
  }
  res[, lower := lower]
  res[, upper := upper]
  res[, se := sqrt((mean*(1-mean))/denominator) ]
  #res[, se.alt := (((upper-mean) + (mean-lower)) / 2) / qnorm(0.975) ] # splitting difference of non-symetrical MOE ... same to 5 decimal places
  
  # apply the 'per' if rate was specified in metric
  if("rate" %in% metrics){
    res[, rate := mean * per]
    res[, rate_per := per]
    res[, c("se", "lower", "upper") := lapply(.SD, function(x){x*per}), .SDcols = c("se", "lower", "upper")] 
    metrics <- c(metrics, "rate_per")
  } else{res[, rate := NA]}
  
  
  # clean up results
  res[, c("variable", "level") := tstrsplit(variable, "_SPLIT_HERE_", fixed=TRUE)] 
  setcolorder(res, c("variable", "level", "years", by))

  # drop columns no longer needed
  metrics <- c(metrics, "years")
  if(length(opts[!opts %in% metrics]) >0){
    res[, opts[!opts %in% metrics] := NULL]
  }

  return(res)

}
