#' A wrapper to compute metrics (specifically relative fraction and totals) from factor/character variables
#'
#' @param svy survey.design2 (or tbl_svy). Survey from which metrics will be computed
#' @param what character or symbol.
#' @param time_var character or symbol
calc_factor <- function(svy, what, by, time_var){
  # what <- enquo(what)
  # by <- enquo(by)
  # time_var <- enquo(time_var)

  #TODO: check for implicitly reserved words

  #create a formula to tabulate over (what the survey package requires)
  form = as.formula(paste0('~', as.character(what)))
  if(!is.null(by)){
    bys = as.formula(paste0('~', paste(as.character(by),collapse = '+')))
  }else{
    svy %>% mutate(holdby = 1)
    bys = ~holdby
  }

  #have to use svymean here because svyciprop doesn't neatly handle non-binary inputs
  #and I don't really feel like making another split
  #do both total and mean
  res1 <- lapply(c('total', 'mean'), function(x){

    if(x == 'mean'){
      f <- survey::svymean
    }else{
      f <- survey::svytotal
    }

    #calculate the results
    imed <- survey::svyby(form, bys, svy, na.rm = T, FUN = f)
    cis = confint(imed)

    #format imed
    imed = data.table::as.data.table(imed)
    means = grep(what, names(imed), value = T)
    ses = means[substr(means, 1,3) == 'se.']
    means = setdiff(means, ses)
    var_vals = substr(means, nchar(as.character(what))+1, nchar(means))
    imed = melt(imed,
                id.vars = as.character(by),
                measure.vars = list(means, ses),
                value.name = c('mean', 'se'), variable.factor = F)
    imed[, as.character(what) := var_vals[as.numeric(variable)]]
    imed[, c('variable') := NULL]

    #format cis
    cis = data.table(lower = cis[,1], upper = cis[,2], label = rownames(cis))
    cis[, as.character(by) := lapply(by, function(x){

      #get the unique values
      vals = unique(svy$variables[[as.character(x)]])

      #repeat
      rep(vals, nrow(cis)/length(vals))

    }) ]

    #confirm the by categories have been passed along
    cis[, `_header` := do.call(paste, c(.SD, sep = '.')), .SDcols = as.character(by)]
    stopifnot(all(substr(cis[,label], 1, nchar(cis[, `_header`])) == cis[,cis[, `_header`]]))

    strts = as.numeric(gregexpr(as.character(what), cis[, label]))
    cis[, as.character(what) := substr(label, strts + nchar(as.character(what)), nchar(label))]
    cis[, c('label', '_header') := NULL]

    #combine and return
    imed = merge(imed, cis, all.x = T, by = c(as.character(by), as.character(what)))
    data.table::setnames(imed, as.character(what), 'level')

    if(x == 'total'){
      data.table::setnames(imed, c('mean', 'se', 'lower', 'upper'), c('total', 'total_se', 'total_lower', 'total_upper'))
    }

    return(imed)

  })

  #Combine the results
  res1 <- merge(res1[[1]], res1[[2]], by = c('level', as.character(by)))

  #compute the other metrics:
  #"numerator"    "denominator"  "missing"      "rse"          "missing.prop" "ndistinct"
  res2 <- svy %>% group_by(!!what, add = T) %>%
    srvyr::summarize(
      numerator = unweighted(dplyr::n()),
      missing = srvyr::unweighted(sum(is.na(!!what))),
      time = srvyr::unweighted(format_time(!!time_var)),
      ndistinct = srvyr::unweighted(length(na.omit(unique(!!what)))),
      unique.time = srvyr::unweighted(length(unique(!!time_var)))
    ) %>% setDT

  #compute denominator
  by_vars = as.character(by)
  res2[!is.na(get(as.character(what))), denominator := sum(numerator), by = by_vars]
  res2[, missing := max(missing), by = by_vars]
  res2 = res2[!is.na(get(as.character(what)))]
  data.table::setnames(res2, as.character(what), 'level')
  res = merge(res1,res2, all.x = T, by = c('level', as.character(by)))

  return(res)

}

