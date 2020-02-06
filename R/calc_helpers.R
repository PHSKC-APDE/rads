#' A wrapper to compute metrics (specifically relative fraction and totals) from factor/character variables
#'
#' @param svy survey.design2 (or tbl_svy). Survey from which metrics will be computed
#' @param what character or symbol.
#' @param by character or symbol vector
#' @param time_var character or symbol
#' @importFrom survey svymean svytotal
#' @importFrom data.table melt setnames
#' @importFrom srvyr filter group_by %>% select summarize unweighted
#' @importFrom dplyr n
#' @importFrom data.table ":=" setnames
#' @importFrom rlang quos !! !!! syms
#' @importFrom methods as
#'
#' @details
#' Under the hood, \code{\link[survey]{svyciprop}} and \code{\link[survey]{svytotal}} do the heavy lifting.
#'
calc_factor <- function(svy, what, by, time_var){
  # what <- enquo(what)
  # by <- enquo(by)
  # time_var <- enquo(time_var)
  #TODO: check for implicitly reserved words

  #create a formula to tabulate over (what the survey package requires)
  form = as.formula(paste0('~', as.character(what)))
  rmholdby = F
  if(!is.null(by)){
    bys = as.formula(paste0('~', paste(as.character(by),collapse = '+')))
  }else{
    svy = svy %>% mutate(holdby = 1) %>% group_by(holdby)
    bys = ~holdby
    by = 'holdby'
    rmholdby = T
  }

  #have to use svymean here because svyciprop doesn't neatly handle non-binary inputs
  #and I don't really feel like making another split
  #do both total and mean
  res1 <- lapply(c('mean', 'total'), function(x){

    if(x == 'mean'){
      #for each value in `what`, calculte the survey ci prop for that group
      uq_cats = na.omit(unique(svy$variables[[as.character(what)]]))

      imed <- lapply(uq_cats, function(ccc){
          ccc = as.character(ccc)
          svy <- srvyr::mutate(svy, `__dv__` = !!what == !!ccc)
          r = svyby(as.formula(paste0("~`__dv__`")), bys, svy, svyciprop, na.rm = T)
          r = cbind(r, confint(r))
          names(r)[(length(names(r))-3):length(names(r))] = c('mean' ,'mean_se', 'mean_lower', 'mean_upper')
          r$level = ccc
          return(r)
      })
      imed = rbindlist(imed)

    }else{
      #calculate the results
      imed <- survey::svyby(form, bys, svy, na.rm = T, FUN = survey::svytotal)
      cis = confint(imed)

      #format imed
      imed = data.table::as.data.table(imed)
      means = grep(what, names(imed), value = T)

      if(inherits(svy, 'svyrep.design')){
        ses = paste0('se', seq(means))#possible names
      }else{
        ses = means[substr(means, 1,3) == 'se.']
        means = setdiff(means,ses)
        data.table::setnames(imed, ses, paste0('se', seq(ses)))
        ses = paste0('se', seq(ses))
      }

      means = means[substr(means, 1, nchar(what)) == what]

      var_vals = substr(means, nchar(as.character(what))+1, nchar(means))

      imed = data.table::melt(imed,
                  id.vars = as.character(by),
                  measure.vars = list(means, ses),
                  value.name = c('mean', 'se'), variable.factor = F)

      imed[, as.character(what) := var_vals[as.numeric(variable)]]
      imed[, c('variable') := NULL]

      #create label to merge with the cis
      imed[, `____label____` := do.call(paste, c(.SD, sep = '.')), .SDcols = as.character(by)]
      imed[, `____label____` := paste0(`____label____`, ':', as.character(what), get(as.character(what)))]

      #format cis
      cis = data.table(lower = cis[,1], upper = cis[,2], label = rownames(cis))

      #merge on cis
      imed = merge(imed, cis, all.x = T, by.x = '____label____', by.y = 'label')
      imed[, `____label____` := NULL]

      data.table::setnames(imed, as.character(what), 'level')

      data.table::setnames(imed, c('mean', 'se', 'lower', 'upper'), c(x, paste0(x, c('_se', '_lower', '_upper'))))
    }

    return(imed)

  })

  #Combine the results
  res1 <- merge(res1[[1]], res1[[2]], by = c('level', as.character(by)))

  #convert to the proper class
  convert = match.fun(paste0('as.',class(svy$variables[[as.character(what)]])[1]))
  res1[, level := convert(level)]

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

  if(rmholdby){
    res[, holdby := NULL]
  }

  return(res)

}

