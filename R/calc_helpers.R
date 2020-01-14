#' A wrapper to compute metrics (specifically relative fraction and totals) from factor/character variables
#'
#' @param svy survey.design2 (or tbl_svy). Survey from which metrics will be computed
#' @param what character or symbol. Variable to be
calc_factor <- function(svy, what, by){
  what <- enquo(what)
  by <- enquo(by)

  #check for implicitly reserved words
  #TODO

  #create a formula to tabulate over (what the survey package requires)
  form = as.formula(paste0('~', as.character(what)))
  if(!is.null(by)){
    bys = as.formula(paste0('~', paste(as.character(by),collapse = '+')))
  }else{
    a %>% mutate(holdby = 1)
    bys = ~holdby
  }

  #have to use svymean here because svyciprop doesn't neatly handle non-binary inputs
  #and I don't really feel like making another split
  #do both total and mean
  res <- lapply(c('total', 'mean'), function(x){

    #calculate the results
    imed <- svyby(form, bys, svy, na.rm = T, FUN = match.fun(paste0('svy',x)))
    cis = confint(imed)

    #format imed
    imed = as.data.table(imed)
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
      vals = unique(ret$variables[[as.character(x)]])

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
    setnames(imed, as.character(what), 'level')

    if(x == 'total'){
      setnames(imed, c('mean', 'se', 'lower', 'upper'), c('total', 'total_se', 'total_lower', 'total_upper'))
    }

    return(imed)

  })

  #Combine the results
  res <- merge(res[[1]], res[[2]], by = c('level', as.character(by)))

  #compute the other metrics:
  #"numerator"    "denominator"  "missing"      "rse"          "missing.prop" "ndistinct"


}


# #compute the relative fractions using the survey package
# #at the moment, I'm not sure why svymean * N != svytotal for both the main estimate and the SE

#
# #extract and format the confidence intervals
# cis = confint(imed)
# cis = data.table(lower = cis[,1], upper = cis[,2], label = rownames(cis))
# cis[, as.character(by) := lapply(by, function(x){
#
#   #get the unique values
#   vals = unique(ret$variables[[as.character(x)]])
#
#   #repeat
#   rep(vals, nrow(cis)/length(vals))
#
# }) ]
#
# #confirm the by categories have been passed along



# cis[, `_header` := do.call(paste, c(.SD, sep = '.')), .SDcols = as.character(by)]
# stopifnot(all(substr(cis[,label], 1, nchar(cis[, `_header`])) == cis[,cis[, `_header`]]))
#
# strts = as.numeric(gregexpr(as.character(what), cis[, label]))
# cis[, as.character(what) := substr(label, strts + nchar(as.character(what)), nchar(label))]
# cis[, c('label', '_header') := NULL]
#
# #reshape
# imed = as.data.table(imed)
# means = grep(what, names(imed), value = T)
# ses = means[substr(means, 1,3) == 'se.']
# means = setdiff(means, ses)
# var_vals = substr(means, nchar(as.character(what))+1, nchar(means))
# imed = melt(imed,
#             id.vars = as.character(by),
#             measure.vars = list(means, ses),
#             value.name = c('mean', 'se'), variable.factor = F)
# imed[, as.character(what) := var_vals[as.numeric(variable)]]
# imed[, c('variable') := NULL]
#
# imed = merge(imed, cis, all.x = T, by = c(as.character(by), as.character(what)))
# setnames(imed, as.character(what), 'level')
#
# #add the rest of the items
# srvyr::summarize(
#   numerator = srvyr::unweighted(sum(!!what, na.rm = T)), #only relevant for binary variables
#   denominator = srvyr::unweighted(dplyr::n()),
#   missing = srvyr::unweighted(sum(is.na(!!what))),
#   time = srvyr::unweighted(paste(sort(unique(!!time_var)), collapse = ', ')),
#   ndistinct = srvyr::unweighted(length(na.omit(unique(!!what))))
# ) %>% setDT
