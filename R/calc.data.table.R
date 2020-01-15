#' @rdname calc
#' @importFrom data.table copy data.table rbindlist tstrsplit .N
#' @importFrom stats median na.omit prop.test qnorm qt var na.omit
#' @export
calc.data.table = function(ph.data,
                           what,
                           ...,
                           by = NULL,
                           metrics = c('mean', "numerator", "denominator", "missing", "obs"),
                           per = NULL,
                           win = NULL,
                           time_var = "chi_year",
                           proportion = FALSE,
                           verbose = FALSE){

  #global variables used by data.table declared as NULL here to play nice with devtools::check()
  se <- rse <- caution <- rate <- rate_per <- level <- years <- variable <- NULL

  # copy data.table to prevent changing the underlying data
  temp.dt <- data.table::copy(ph.data)

  #### VALIDATION ####
    #validate 'what'
      if(!is.character(what))
        stop(paste0("The `what` argument must be submitted as a character (i.e., in quotes)"))

      what_check <- check_names('what', 'temp.dt', names(temp.dt), what)
      if(what_check != '') stop(what_check)

    #identify when 'what' is binary (0, 1), other numerics, or a factor. When a factor, convert it to a series of binary columns
        # binary columns
          binary.col <- vapply(temp.dt[, ..what],function(x) { all(stats::na.omit(x) %in% 0:1) }, FUN.VALUE=logical(1)) # logical vector
          binary.col <- what[binary.col]  # character vector

        # numeric columns
          numeric.col <- vapply(temp.dt[, ..what], is.numeric, FUN.VALUE=logical(1)) # logical vector
          numeric.col <- setdiff(what[numeric.col], binary.col)

        # factor columns
          factor.col <- vapply(temp.dt[, ..what], is.factor, FUN.VALUE=logical(1)) # logical vector
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

    #validate 'win'
      if(!is.null(win)){
        if(win %% 1 != 0){
        stop("If specified, the 'win' argument must be an integer")
        }
      }

  #### CREATE CALC FUNCTIONS ####
    #subset temp.dt to only the rows needed
      if(!is.null(where)){
        temp.dt <- temp.dt[eval(where), ]
      }

    # function to calculate metrics
      calc_metrics <- function(X, DT){
        . <- NULL
        DT[, .(
          years = format_years(get(time_var)),
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
          unique.years = length(unique( get(time_var) )),
          ndistinct = length(unique(na.omit(get(X))))
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
        res.metrics[, c("variable", "level") := data.table::tstrsplit(variable, "_SPLIT_HERE_", fixed=TRUE)]

    # Calculate lower, upper, se, rse
        # PROPORTIONS : Binary (& factor) variables will use prop.test function for CI. This uses the score method ... suggested by DOH & literature
            res.metrics.prop <- res.metrics[variable %in% c(binary.col, factor.col) & denominator!=0] # split off just binary/factor data. Undefined when denominator == 0, so drop
            if(length(binary.col) + length(factor.col) > 0){
              numerator <- res.metrics.prop$numerator
              denominator <- res.metrics.prop$denominator
              lower <- rep(NA, nrow(res.metrics.prop)) # create empty vector to hold res.metricsults
              upper <- rep(NA, nrow(res.metrics.prop)) # create empty vector to hold res.metricsults
              for(i in 1:nrow(res.metrics.prop)){
                lower[i] <- suppressWarnings(stats::prop.test(x = numerator[i], n = denominator[i], conf.level = 0.95, correct = F)$conf.int[1]) # the score method ... suggested by DOH & others
                upper[i] <- suppressWarnings(stats::prop.test(x = numerator[i], n = denominator[i], conf.level = 0.95, correct = F)$conf.int[2])
              }
              res.metrics.prop[, lower := lower]
              res.metrics.prop[, upper := upper]
              # res.metrics.prop[, se := sqrt((mean*(1-mean))/denominator) ] # calculated the SE empirically above. Confirmed that res.metricsults are ~same as from this formula
              # the calculation based on variance differed from this forumla when samples were tiny. In those cases, the empirical ones were larger and therefore more conservative
            }

        # MEANS: Numeric/non-binary need to have their CI calculated separately
            res.metrics.mean <- res.metrics[variable %in% c(numeric.col)]
            if(length(numeric.col) > 0){
              res.metrics.mean[denominator>30, lower := mean - stats::qnorm(0.975)*se] # when n>30, central limit theorm states distribution is normal & can use Z-scores.metrics
              res.metrics.mean[denominator>30, upper := mean + stats::qnorm(0.975)*se] # when n>30, central limit theorm states distribution is normal & can use Z-scores.metrics
              suppressWarnings(res.metrics.mean[denominator<=30, lower := mean - qt(0.975,df=denominator-1)*se]) # when n<=30, use t-distribution which accounts for smaller n having greater spread (assumes underlying data is normally distributed)
              suppressWarnings(res.metrics.mean[denominator<=30, upper := mean + qt(0.975,df=denominator-1)*se]) # when n<=30, use t-distribution which accounts for smaller n having greater spread (assumes underlying data is normally distributed)
              res.metrics.mean[lower < 0, lower := 0] # prevent negative values for confidence interval
            }

        # Append data for proportions and means
            res.metrics <- rbind(res.metrics.prop, res.metrics.mean, fill = T)

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

  #### CLEAN UP ####
        res <- res.metrics
        data.table::setcolorder(res, c("variable", "level", "years", setdiff(by, "chi_year"), "median", "mean", "rate", "lower", "upper", "se", "rse", "caution", "total", "obs", "numerator", "denominator", "missing", "missing.prop", "unique.years"))

    # Sort / order results
      data.table::setorder(res, variable, level, years)

    # drop columns no longer needed
        metrics <- c(metrics, "years", "caution", "suppression")
        if(length(opts[!opts %in% metrics]) >0){
          suppressWarnings(res[, opts[!opts %in% metrics] := NULL])
        }


  #### CLOSE ####
  return(res)

}
