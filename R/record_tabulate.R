#' Compute metrics from record level data (e.g., vital stats)
#'
#' @param my.dt data.table with count data
#' @param what character vector. Variable to tabulate "over". Must match a column name in my.dt
#' @param ... expressions to be passed to \code{\link{filter}}
#' @param by character vector. Must refer to variables within my.dt. The variables within my.dt to compute `what` by
#' @param metrics character. See \code{\link{record_metrics}} for the available options. Note, all metrics are calculated
#' -- this argument just specifies which one gets returned
#' @param per integer. The denominator when "rate" or "adjusted-rate" are selected as the metric.
#' @param digits integer. The number of places to which the data should be rounded
#' @param win integer. The number of units of time [e.g., years, months, etc.] over which the metrics will be calculated, 
#' i.e., the 'window' for a rolling average, sum, etc. 
#'
#' @return a data.table containing the results
#' @details
#' This function calculates `metrics` for each variable in `what` from rows meeting the conditions specified 
#' by `...` (i.e., where), for each grouping implied by `by`.
#'
#' @importFrom rlang quos
#' @import 
#' @import data.table
#' @export
#'
#' @examples
#' test.data <- get_data_birth(year = 2015:2017)
#' 
#' test.results <- record_tabulate(test.data, 
#'                                what = c("kotelchuck", "fetal_pres"), 
#'                                "chi_year == 2016 & chi_sex %in% c('Male', 'Female')", 
#'                                by = c("chi_year", "chi_sex"), 
#'                                metrics = c("mean", "numerator", "denominator", "missing", "total", "lower", "upper", "se", "mising.prop"))
#'
#'
record_tabulate = function(my.dt, what, ..., by = NULL, metrics = c('mean', "numerator", "denominator", "missing", "total"), per = NULL, digits = NULL, win = NULL){
  # copy data.table to prevent changing the underlying data
  temp.dt <- copy(my.dt)
  
  opts = record_metrics()
  
  #validate 'what'
  if(!is.character(what))
    stop(paste0("The `what` argument must be submitted as a character (i.e., in quotes)"))
  
  what_check <- check_names('what', 'temp.dt', names(temp.dt), what)
  if(what_check != '') stop(what_check)
  
  #identify when 'what' is binary (0, 1), other numerics, or a factor. When a factor, convert it to a series of binary columns
    # binary columns
      binary.col <- sapply(temp.dt[, ..what],function(x) { all(na.omit(x) %in% 0:1) }) # logical vector 
      binary.col <- what[binary.col]  # character vector 
      
    # numeric columns
      numeric.col <- sapply(temp.dt[, ..what], is.numeric) # logical vector
      numeric.col <- setdiff(what[numeric.col], binary.col)
      
    # factor columns
      factor.col <- sapply(temp.dt[, ..what], is.factor) # logical vector
      factor.col <- what[factor.col]

      names.before <- names(copy(temp.dt))
      
    # convert factors to series of binary columns (xxx_prefix is to identify the expanded data below)
      for(i in 1:length(factor.col)){
        temp.dt[, paste0(factor.col[i], "_SPLIT_HERE_", levels(temp.dt[[factor.col[i]]]) ) := 
             lapply(levels( get(factor.col[i]) ), function(x) as.integer(x == get(factor.col[i]) ))]
      }
      
    # update 'what' to reflect all binaries
      what <- c(setdiff(what, factor.col), setdiff(names(temp.dt), names.before) )
      
  
  #validate '...' (i.e., where)
  where <- NULL
  if(!missing(...)){
    where <- tryCatch(parse(text = paste0(list(...))),  error = function (e) parse(text = paste0(list(bquote(...))))) # convert 'where' into an expression
      if(nrow(temp.dt[eval(where), ]) <1 ){
        stop(paste0("Your '...' (i.e., ", where, ") filters out all rows of data. Please revise and submit again"))
      }
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
    stop("If specified, the 'per' argument must be an integer")
  }

  #validate 'digits'
  if(!is.null(digits) & all.equal(digits, as.integer(digits))!=T){
    stop("If specified, the 'digits' argument must be an integer")
  }
  
  #validate 'win'
  if(!is.null(win) & all.equal(win, as.integer(win))!=T){
    stop("If specified, the 'win' argument must be an integer")
  }

  #limits metrics to those that have been pre-specified, i.e., non-standard metrics are dropped
  metrics <- match.arg(metrics, opts, several.ok = T)

  #subset temp.dt to only the rows needed
  if(!is.null(where)){
    temp.dt <- temp.dt[eval(where), ]
  }

  # function to calculate metrics
  calc.metrics <- function(raw.dt){
    results.dt <- data.table() # create empty data.table for appending results
    for(i in 1:length(what)){
      results.dt <- rbind(results.dt, 
                          raw.dt[, .(
                            years = format.years(list(sort(unique(chi_year)))),
                            variable = as.character(what[i]), 
                            mean = mean(get(what[i]), na.rm = T),
                            median = as.numeric(median(get(what[i]), na.rm = T)),
                            sum = sum(get(what[i]), na.rm = T),
                            numerator = sum(get(what[i]), na.rm = T),
                            denominator = sum(!is.na( get(what[i]) )),
                            se = sqrt(var(get(what[i]), na.rm = T)/sum(!is.na( get(what[i]) )) ), 
                            total = .N, 
                            missing = sum(is.na( get(what[i]) )),
                            missing.prop = sum(is.na( get(what[i]) ) / .N), 
                            unique.years = length(unique(chi_year))
                          ), 
                          by = by
                          ], 
                          fill = TRUE
      )
    }
    results.dt <- unique(results.dt)
    return(results.dt)
  }
  
  # apply the calc.metrics function
  if(is.null(win)){
    res <- calc.metrics(temp.dt)
  } 
  
  if(!is.null(win)){
    res <- data.table() # empty table for appending results
    for(yr in min(temp.dt$chi_year):(max(temp.dt$chi_year)-win+1) ){
      sub.temp.dt <- copy(temp.dt[chi_year %in% yr:(yr+win-1)])
      temp.results <- calc.metrics(sub.temp.dt)
      res <- rbind(res, temp.results, fill = TRUE)
    }
    rm(sub.temp.dt, temp.results)
  }

  
  # split names for factor columns  
  res[, c("variable", "level") := tstrsplit(variable, "_SPLIT_HERE_", fixed=TRUE)] 
  
  # Calculate lower, upper, se, rse
    # PROPORTIONS : Binary (& factor) variables will use prop.test function for CI. This uses the score method ... suggested by DOH & literature
        res.prop <- res[variable %in% c(binary.col, factor.col)] # split off just binary/factor data
        numerator <- res.prop$numerator
        denominator <- res.prop$denominator
        lower <- rep(NA, nrow(res.prop)) # create empty vector to hold results
        upper <- rep(NA, nrow(res.prop)) # create empty vector to hold results
        for(i in 1:nrow(res.prop)){
          lower[i] <- suppressWarnings(prop.test(numerator[i], denominator[i], conf.level = 0.95, correct = F)$conf.int[1]) # the score method ... suggested by DOH & others
          upper[i] <- suppressWarnings(prop.test(numerator[i], denominator[i], conf.level = 0.95, correct = F)$conf.int[2])
        }  
        res.prop[, lower := lower]
        res.prop[, upper := upper]
        # res.prop[, se := sqrt((mean*(1-mean))/denominator) ] # calculated the SE empirically above. Confirmed that results are ~same as from this formula
        # the calculation based on variance differened from this forumla when samples were tiny. In those cases, the empirical ones were larger and therefore more conservative
        
    # MEANS: Numeric/non-binary need to have their CI calculated separately
        res.mean <- res[variable %in% c(numeric.col)]
        res.mean[denominator>30, lower := mean - qnorm(0.975)*se] # when n>30, central limit theorm states distribution is normal & can use Z-scores 
        res.mean[denominator>30, upper := mean + qnorm(0.975)*se] # when n>30, central limit theorm states distribution is normal & can use Z-scores 
        res.mean[denominator<=30, lower := mean - qt(0.975,df=denominator-1)*se] # when n<=30, use t-distribution which accounts for smaller n having greater spread (assumes underlying data is normally distributed) 
        res.mean[denominator<=30, upper := mean + qt(0.975,df=denominator-1)*se] # when n<=30, use t-distribution which accounts for smaller n having greater spread (assumes underlying data is normally distributed)
        res.mean[lower < 0, lower := 0] # prevent negative values for confidence interval
        
    # Append data for proportions and means
        res <- rbind(res.prop, res.mean)
  
    # Calculate RSE
        res[, rse := se / mean]
        res[rse >0.3, caution := "!"]
  
  # apply the 'per' if rate was specified in metric (rates are only applicable to proportions)
  if("rate" %in% metrics){
    res[variable %in% unique(res.prop$variable), rate := mean * per] 
    res[variable %in% unique(res.prop$variable), rate_per := per]
    res[variable %in% unique(res.prop$variable), c("se", "lower", "upper") := lapply(.SD, function(x){x*per}), .SDcols = c("se", "lower", "upper")] 
    metrics <- c(metrics, "rate_per")
  } else{res[, rate := NA]}
  
  # apply rounding using 'digits' (if specified)
  if(is.null(digits)){
    res[, c("rate", "mean", "lower", "upper", "rse", "missing.prop") := lapply(.SD, round2, 3), .SDcols = c("rate", "mean", "lower", "upper", "rse", "missing.prop")]
    res[, se := round2(se, 4)]
  } else {
    res[, c("rate", "mean", "lower", "upper", "se", "rse", "missing.prop") := lapply(.SD, round2, digits), .SDcols = c("rate", "mean", "lower", "upper", "se", "rse", "missing.prop")]
  }
  
  # clean up results
  setorder(res, variable, level, years)
  setcolorder(res, c("variable", "level", "years", by, "median", "mean", "rate", "lower", "upper", "se", "rse", "caution", "sum", "numerator", "denominator", "missing", "total", "missing.prop", "unique.years"))
  
  # drop columns no longer needed
  metrics <- c(metrics, "years", "caution")
  if(length(opts[!opts %in% metrics]) >0){
    res[, opts[!opts %in% metrics] := NULL]
  }

  return(res)

}
