#' Compute metrics from record level data (e.g., vital stats)
#'
#' @param my.dt data.table with count data
#' @param what character vector. Variable to tabulate "over". Must match a column name in my.dt
#' @param ... expressions to be passed to \code{\link{filter}}
#' @param by character vector. Must refer to variables within my.dt. The variables within my.dt to compute `what` by
#' @param metrics character. See \code{\link{record_metrics}} for the available options. Note, except when 'distinct' is 
#' selected, all metrics are calculated -- this argument just specifies which one gets returned
#' @param per integer. The denominator when "rate" or "adjusted-rate" are selected as the metric.
#' @param digits integer. The number of places to which the data should be rounded
#' @param suppress logical. Select whether suppression should [T] or should not [T] be applied.
#' @param suppress_range integer vector of length 2. They specify the minimum and maximum range for suppression.
#' @param win integer. The number of units of time [e.g., years, months, etc.] over which the metrics will be calculated, 
#' i.e., the 'window' for a rolling average, sum, etc. 
#' @param distinct logical. Select whether function shoulld [T]calculte distinct counts for all combinations of 'what' and 'by'
#' or not [F]. Note, when distinct==T, no other calculations will be performed. 
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
record_tabulate = function(my.dt, 
                           what, 
                           ..., 
                           by = NULL, 
                           metrics = c('mean', "numerator", "denominator", "missing", "total"), 
                           per = NULL, 
                           digits = NULL, 
                           suppress = T, 
                           suppress_range = NULL, 
                           win = NULL, 
                           distinct = F){
  # copy data.table to prevent changing the underlying data
  temp.dt <- copy(my.dt)
  
  # Harmonize when metric contains 'distinct' or distinct==T
  if(distinct==T & sum(metrics %in% "distinct")==0){metrics <- c(metrics, "distinct")}
  if(sum(metrics %in% "distinct")>0){distinct <- T}
  
  
  #### VALIDATION ####
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
          if(length(factor.col) > 0){ # sometimes there are no factors, so this shoudl be conditional
            for(i in 1:length(factor.col)){
              temp.dt[, paste0(factor.col[i], "_SPLIT_HERE_", levels(temp.dt[[factor.col[i]]]) ) := 
                   lapply(levels( get(factor.col[i]) ), function(x) as.integer(x == get(factor.col[i]) ))]
            }
    
          }
    
        # update 'what' to reflect all binaries, including those made from factors
          what.distinct <- copy(what)
          what.metrics <- c(setdiff(what, factor.col), setdiff(names(temp.dt), names.before) )  
              
    #validate '...' (i.e., where)
      where <- NULL
      if(!missing(...)){
        where <- tryCatch(parse(text = paste0(list(...))),  error = function (e) parse(text = paste0(list(bquote(...))))) # convert 'where' into an expression
          if(nrow(temp.dt[eval(where), ]) <1 ){
            stop(paste0("Your '...' (i.e., ", where, ") filters out all rows of data. Please revise and submit again"))
          }
      }
    
    #validate 'by'
      if(!missing(by) & !is.null(by)){
        if(!is.character(by))
          stop(paste0("The `by` argument must be submitted as a character (i.e., in quotes)"))
        
        by_check <- check_names('by', 'svy', names(temp.dt), by)
        if(by_check != '') stop(by_check)
      }
    
    #validate 'metrics'
      # pull list of standard available metrics 
      opts <- record_metrics()
      
      # limits metrics to those that have been pre-specified, i.e., non-standard metrics are dropped
      metrics <- match.arg(metrics, opts, several.ok = T)
      
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
      
    #validate 'suppress'
      if(!is.logical(suppress)){
        stop("If specified, the 'suppress' argument must be a logical (i.e., T or F, without quotes)")
      }
      
    #validate 'suppress_range'
      if(is.null(suppress_range)){suppress_range <- c(0, 10)} # fixed, but eventually dependent upon data source?
      
      if(length(suppress_range) !=2){
          stop("If specified, the 'suppress_range' argument must be a numeric vector of length 2 (e.g., c(0, 10)")
      }
      
      if(!is.numeric(suppress_range) |
         suppress_range[1] != round2(suppress_range[1]) | 
         suppress_range[2] != round2(suppress_range[2])){
        stop("If specified, the 'suppress_range' argument must only contain non-negative integers  
             (e.g., c(0, 10)") 
        } 

    #validate 'distinct'
      if(!is.logical(distinct)){
        stop("If specified, the 'distinct' argument must be a logical (i.e., T or F, without quotes)")
      }
      

  #### CREATE CALC FUNCTIONS ####
    #subset temp.dt to only the rows needed
      if(!is.null(where)){
        temp.dt <- temp.dt[eval(where), ]
      }
  
    # function to calculate metrics
      calc.metrics <- function(raw.dt){
        results.dt <- data.table() # create empty data.table for appending results
        for(i in 1:length(what.metrics)){
          results.dt <- rbind(results.dt, 
                              raw.dt[, .(
                                years = format.years(list(sort(unique(chi_year)))),
                                variable = as.character(what.metrics[i]), 
                                mean = mean(get(what.metrics[i]), na.rm = T),
                                median = as.numeric(median(get(what.metrics[i]), na.rm = T)),
                                sum = sum(get(what.metrics[i]), na.rm = T),
                                numerator = sum(get(what.metrics[i]), na.rm = T),
                                denominator = sum(!is.na( get(what.metrics[i]) )),
                                se = sqrt(var(get(what.metrics[i]), na.rm = T)/sum(!is.na( get(what.metrics[i]) )) ), 
                                total = .N, 
                                missing = sum(is.na( get(what.metrics[i]) )),
                                missing.prop = sum(is.na( get(what.metrics[i]) ) / .N), 
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
    
      # function to calculate distinct combinations of 'what' and 'by' 
      calc.distinct <- function(raw.dt){
        results.dt <- data.table() # create empty data.table for appending results
        for(i in 1:length(what.distinct)){
          results.dt <- rbind(results.dt, 
                              raw.dt[, .(
                                variable = as.character(what.distinct[i]),
                                level = get(what.distinct[i]), 
                                years = format.years(list(sort(unique(chi_year)))),
                                distinct = .N
                                ), 
                              by = c(what.distinct[i], by)], 
                              fill = TRUE
          )
          results.dt[, what.distinct[i] := NULL]
        }
        return(results.dt)
      }
      
  #### APPLY CALC FUNCTION ####
    # apply the calc.metrics or calc.distinct functions
      if(is.null(win)){
          res.distinct <- calc.distinct(temp.dt)
          res.metrics <- calc.metrics(temp.dt)
      } 
      
      if(!is.null(win)){
        res.distinct <- data.table() # empty table for appending results
            for(yr in min(temp.dt$chi_year):(max(temp.dt$chi_year)-win+1) ){
            sub.temp.dt <- copy(temp.dt[chi_year %in% yr:(yr+win-1)])
            temp.results <- calc.distinct(sub.temp.dt)
            res.distinct <- rbind(res.distinct, temp.results, fill = TRUE)

        res.metrics <- data.table() # empty table for appending results
          for(yr in min(temp.dt$chi_year):(max(temp.dt$chi_year)-win+1) ){
            sub.temp.dt <- copy(temp.dt[chi_year %in% yr:(yr+win-1)])
            temp.results <- calc.metrics(sub.temp.dt)
            res.metrics <- rbind(res.metrics, temp.results, fill = TRUE)
          }
        }
        rm(sub.temp.dt, temp.results)
      }
      

  #### ADDITIONAL CALCULATIONS & DATA PREP ####

    # Split names for factor columns
        res.metrics[, c("variable", "level") := tstrsplit(variable, "_SPLIT_HERE_", fixed=TRUE)] 
      
    # Calculate lower, upper, se, rse
        # PROPORTIONS : Binary (& factor) variables will use prop.test function for CI. This uses the score method ... suggested by DOH & literature
            res.metrics.prop <- res.metrics[variable %in% c(binary.col, factor.col)] # split off just binary/factor data
            numerator <- res.metrics.prop$numerator
            denominator <- res.metrics.prop$denominator
            lower <- rep(NA, nrow(res.metrics.prop)) # create empty vector to hold res.metricsults
            upper <- rep(NA, nrow(res.metrics.prop)) # create empty vector to hold res.metricsults
            for(i in 1:nrow(res.metrics.prop)){
              lower[i] <- suppressWarnings(prop.test(numerator[i], denominator[i], conf.level = 0.95, correct = F)$conf.int[1]) # the score method ... suggested by DOH & others
              upper[i] <- suppressWarnings(prop.test(numerator[i], denominator[i], conf.level = 0.95, correct = F)$conf.int[2])
            }  
            res.metrics.prop[, lower := lower]
            res.metrics.prop[, upper := upper]
            # res.metrics.prop[, se := sqrt((mean*(1-mean))/denominator) ] # calculated the SE empirically above. Confirmed that res.metricsults are ~same as from this formula
            # the calculation based on variance differened from this forumla when samples were tiny. In those cases, the empirical ones were larger and therefore more conservative
            
        # MEANS: Numeric/non-binary need to have their CI calculated separately
            res.metrics.mean <- res.metrics[variable %in% c(numeric.col)]
            res.metrics.mean[denominator>30, lower := mean - qnorm(0.975)*se] # when n>30, central limit theorm states distribution is normal & can use Z-scores.metrics 
            res.metrics.mean[denominator>30, upper := mean + qnorm(0.975)*se] # when n>30, central limit theorm states distribution is normal & can use Z-scores.metrics 
            suppressWarnings(res.metrics.mean[denominator<=30, lower := mean - qt(0.975,df=denominator-1)*se]) # when n<=30, use t-distribution which accounts for smaller n having greater spread (assumes underlying data is normally distributed) 
            suppressWarnings(res.metrics.mean[denominator<=30, upper := mean + qt(0.975,df=denominator-1)*se]) # when n<=30, use t-distribution which accounts for smaller n having greater spread (assumes underlying data is normally distributed)
            res.metrics.mean[lower < 0, lower := 0] # prevent negative values for confidence interval
            
        # Append data for proportions and means
            res.metrics <- rbind(res.metrics.prop, res.metrics.mean)
      
        # Calculate RSE
            res.metrics[, rse := se / mean]
            res.metrics[rse >0.3, caution := "!"]
            
    # apply the 'per' if rate was specified in metric (rates are only applicable to proportions)
      if("rate" %in% metrics){
        res.metrics[variable %in% unique(res.metrics.prop$variable), rate := mean * per] 
        res.metrics[variable %in% unique(res.metrics.prop$variable), rate_per := per]
        res.metrics[variable %in% unique(res.metrics.prop$variable), c("se", "lower", "upper") := lapply(.SD, function(x){x*per}), .SDcols = c("se", "lower", "upper")] 
        metrics <- c(metrics, "rate_per")
      } else{res.metrics[, rate := NA]}
      
    # apply rounding using 'digits' (if specified)
      if(is.null(digits)){
        res.metrics[, c("rate", "mean", "lower", "upper", "rse", "missing.prop") := lapply(.SD, round2, 3), .SDcols = c("rate", "mean", "lower", "upper", "rse", "missing.prop")]
        res.metrics[, se := round2(se, 4)]
      } else {
        res.metrics[, c("rate", "mean", "lower", "upper", "se", "rse", "missing.prop") := lapply(.SD, round2, digits), .SDcols = c("rate", "mean", "lower", "upper", "se", "rse", "missing.prop")]
      }
            
    # Apply suppression
      if(suppress == T){
        res.metrics[numerator %in% suppress_range[1]:suppress_range[2], suppression := "^"]
        res.metrics[suppression=="^", c("mean", "median", "rate", "lower", "upper", "se", "rse", "numerator", "denominator") := NA]
      } else{res.metrics[, suppression := NA]}


  #### CLEAN UP ####    
            
    # select proper results to submit based on whether 'distinct' was requested
      if(sum(metrics %in% "distinct")>0 & length(metrics)==1){ # this means distinct is the only metric
        res <- res.distinct
        setcolorder(res, c("variable", "level", "years", setdiff(by, "chi_year"), "distinct"))
      }      
            
      if(sum(metrics %in% "distinct")==0 ){ # this means distinct was not requested
        res <- res.metrics
        setcolorder(res, c("variable", "level", "years", setdiff(by, "chi_year"), "median", "mean", "rate", "lower", "upper", "se", "rse", "caution", "sum", "numerator", "denominator", "missing", "total", "missing.prop", "unique.years"))
      }
      
      if(sum(metrics %in% "distinct")>0 & length(metrics)>1){ # 'distinct' + other metrics selected, so have to merge
        res <- merge(res.metrics, res.distinct, by = c("variable", "level", "years", setdiff(by, "chi_year")), all.x = T, all.y = T)    
        setcolorder(res, c("variable", "level", "years", setdiff(by, "chi_year"), "distinct", "median", "mean", "rate", "lower", "upper", "se", "rse", "caution", "sum", "numerator", "denominator", "missing", "total", "missing.prop", "unique.years"))
        res <- res[!(is.na(numerator) & is.na(distinct) ), ] # these rows are NA because they unique combinations do not exist, so dropping. 
      }            
            
    # Sort / order results
      setorder(res, variable, level, years)

    # drop columns no longer needed
        metrics <- c(metrics, "years", "caution", "suppression")
        if(length(opts[!opts %in% metrics]) >0){
          suppressWarnings(res[, opts[!opts %in% metrics] := NULL])
        }
      

  #### CLOSE ####
  return(res)

}
