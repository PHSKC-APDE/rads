# extract all the various combinations
# Set up
holdres = lapply(res, copy)

what = 'ysw_test_novar'
by = c('random3', 'stype_test_novar')
metrics = c('mean', 'numerator')
# replace calc
res = copy(holdres)
isfactor = !all(is.na(res[[1]][,level]))
res = lapply(seq_along(res), function(i) res[[i]][, `_miiter` := i])
res = rbindlist(res)


mi_res = lapply(intersect(c('mean', 'total'), names(res)), function(vvv){

  # Convert to the format required
  lhs = paste(paste(by, collapse = ' + '), 'level', 'variable', sep = ' + ')
  mform = as.formula(paste(lhs, '~', '`_miiter`'))
  r = dcast(res[, .SD, .SDcols = c(by, 'level', 'variable', vvv, paste0(vvv, '_vcov'), '_miiter')],
            mform,
            value.var = c(vvv, paste0(vvv, '_vcov')))

  # Fix NULL VCOVs
  vcov_col = grep('vcov', names(r), value = T)
  val_col = grep(paste0(vvv,'_[0-9]'), names(r), value = T)
  for(vc in vcov_col){
    vcol = r[[vc]]
    mis_chk = sapply(vcol, length)
    vcol[which(mis_chk == 0)] <- list(list(var = matrix(NA_real_)))
    r[[vc]] <- vcol
  }

  # Remelt things, and split
  r = melt(r,
           id.vars = c(by, 'level', 'variable'),
           measure.vars = list(val_col, vcov_col),
           variable.name = '_miiter',
           value.name = c(vvv, paste0(vvv, '_vcov')))

  res2 = split(r, by = c('_miiter'))

  # extract and organize the estimates and their variances
  r = lapply(res2, function(x){

    if(isfactor){

      y = x[, list(ests = list(get(vvv)),
                   varz = list(make_vcov(get(paste0(vvv, '_vcov')))),
                   levels = list(level)), keyby = by]
    }else{
      y = x[, list(ests = list(get(vvv)),
                   varz = list(make_vcov(get(paste0(vvv, '_vcov')))),
                   levels = list(level))]
    }

    y
  })

  # organize them by "by variables"
  r = rbindlist(r)
  if(isfactor && !is.null(by)){
    r = split(r, by = by)
  }else{
    r = list(r)
  }

  # compute estimates
  # This is borrowed/adapted from mitools
  mi = lapply(r, function(a){
    # if(!isfactor) a = list(ests = list(a$ests[[1]]), varz = list(a$varz[[1]]))
    m = mitools::MIcombine(a$ests, a$varz)
    mdt = data.table(coef = coef(m), se = survey::SE(m))
    crit <- qt(alpha/2, m$df, lower.tail = FALSE)
    mdt[, lower := coef - crit * se]
    mdt[, upper := coef + crit * se]
    mdt[, level := a$levels[1]]
    if(isfactor & !is.null(by)) mdt = cbind(mdt, a[1,.SD,.SDcols = by])
    mdt
  })

  # combine results
  mi = rbindlist(mi)
  updateme = c(vvv, paste0(vvv,'_se'), paste0(vvv, '_lower'), paste0(vvv, '_upper'))
  setnames(mi,
           c('coef', 'se', 'lower', 'upper'),
           updateme
  )

  # # Clean up
  # if(!isfactor && !is.null(by)) mi[, (by) := ans[, .SD, .SDcols = c(by)]]

  return(cbind(res2[[1]][, .SD, .SDcols = c(by, 'variable')], mi))

})

if(length(mi_res) == 1){
  mi_res = mi_res[[1]]
}else{
  mi_res = merge(mi_res[[1]], mi_res[[2]], by = intersect(names(mi_res[[1]]), names(mi_res[[2]])))
}


# For the variables that don't go through MI routines (e.g numerator, take the average)
vars = setdiff(metrics, c('mean', 'total', 'mean_vcov', 'total_vcov'))
non_mi = res[, lapply(vars, function(v) mean(get(v))), keyby = c(by, 'variable', 'level')]
setnames(non_mi, c(c(by, 'variable', 'level'), vars))

ans = merge(mi_res, non_mi, all.x = T, by = c(by, 'variable', 'level'))

if(!is.null(by)) data.table::setorderv(ans, cols = c(by, 'level'))
