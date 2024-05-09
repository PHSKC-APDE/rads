library('survey')
library('data.table')
library('spatagg')
library('mitools')
library('dtsurvey')
library('rads')
library('ggplot2')

options(survey.lonely.psu="adjust")
brfs = readRDS("//dphcifs/APDE-CDIP/BRFSS/prog_all/2022/kc2022.rds")
set.seed(98104)

# create some different variations for MI
alts = lapply(1:1000, function(i){

  z2h = assign_cases(brfs, 'zipcode', rads.data::spatial_zip_to_hra20_pop)
  r = copy(brfs)
  r[, hra_code := z2h]
  r[, by1 := sample(c(0,1), .N, replace = T)]
  r[, by2 := sample(c(0,1), .N, replace = T)]
  dtsurvey::dtsurvey(r[, .(seqno, x_ststr, finalwt1, zipcode, hra_code, diab2, age4, by1, by2)],'seqno', 'x_ststr', 'finalwt1')
  # convert to survey
  # survey::svydesign(ids = ~seqno, strata = ~x_ststr, weights = ~finalwt1, data = r)

})

ns = c(1,5,10, 25, 50,100,250)
nsz = lapply(ns, function(n) sample(seq_along(alts), n))

res_init = lapply(nsz, function(n){

  start = Sys.time()

  if(length(n) == 1){
    ilist = alts[[n]]
  } else{
    ilist = imputationList(alts[n])
  }

  a = calc(ilist, what = 'diab2', by = 'hra_code', metrics = c('mean', 'total', 'vcov'))
  a[, niter := length(n)]
  a[, end := Sys.time()]
  a[, start := start]
  a
})

res = lapply(res_init, function(x) x[, .(hra_code, level, variable, mean, mean_se, mean_lower, mean_upper, niter, start, end)])

res = rbindlist(res)
times = unique(res[, .(niter, start,end)])
times[, elapsed := end-start]

hids = sample(unique(res$hra_code), 8)


g = ggplot(res[hra_code %in% hids], aes(x = log(niter), y = mean, color = factor(niter))) + geom_point() + facet_wrap(~hra_code) +
  geom_errorbar(aes(ymin = mean_lower, ymax = mean_upper)) +
  geom_hline(data = res[hra_code %in% hids & niter == 1000], aes(yintercept = mean)) +
  scale_color_brewer(type = 'qual', name = '# Iter') +
  theme_dark() +
  ggtitle('MI estimates by number of permutations/iterations', 'by HRA')
g

mi_svy = svydesign(id = ~seqno, strata = ~x_ststr, weights = ~finalwt1, data = mitools::imputationList(alts), nest = T)
ddiab_mi_svy = with(mi_svy, svyby(~diab2,~hra_code,FUN = svymean, na.rm = T))
ddiab_mi_svy_comp = mitools::MIcombine(ddiab_mi_svy)

compa = data.table(hra20_id = as.numeric(names(ddiab_mi_svy_comp$coefficients)), mean = ddiab_mi_svy_comp$coefficients,
               se = sqrt(diag(ddiab_mi_svy_comp$variance)))
r_a = merge(a[, .(hra20_id = hra_code, rads_est = mean, rads_se = mean_se)], compa, all.x = T, by = 'hra20_id')
r_a[!is.na(hra20_id), .(sum(rads_est - mean), sum(rads_se - se))]


# Try with age
age_mi_svy = with(mi_svy, svyby(~age4,~hra_code,FUN = svymean, na.rm = T))
age_mi_svy_comp = mitools::MIcombine(age_mi_svy)

b = calc(ilist, what = 'age4', by = 'hra_code', metrics = c('mean', 'total', 'vcov'))
compb =  data.table(id = names(age_mi_svy_comp$coefficients), mean = age_mi_svy_comp$coefficients,
                       se = sqrt(diag(age_mi_svy_comp$variance)), version = 'SVY MI')
compb[, c('hra_code', 'agecat') := tstrsplit(id, ':', fixed = T)]
compb[, level := substr(agecat, 5, nchar(agecat))]
compb[, hra_code := as.numeric(hra_code)]
r_b = merge(b[, .(hra_code, level, rads_mean = mean, rads_se = mean_se)], compb[, .(comp_mean = mean, comp_se = se, hra_code, level)], all.x = T, by = c('hra_code', 'level'))
r_b[!is.na(hra_code), .(sum(rads_mean - comp_mean), sum(rads_se - comp_se))]


a2 = calc(ilist, what = 'diab2', by = c('hra_code', 'by2'), metrics = c('mean', 'total', 'vcov'))

mi_svy = svydesign(id = ~seqno, strata = ~x_ststr, weights = ~finalwt1, data = mitools::imputationList(alts), nest = T)
by2 = with(mi_svy, svyby(~diab2,~hra_code+by2,FUN = svymean, na.rm = T))
by2comp = mitools::MIcombine(by2)

compa2 = data.table(id = (names(by2comp$coefficients)), mean = by2comp$coefficients,
                   se = sqrt(diag(by2comp$variance)))
compa2[, c('hra20_id', 'by2') := tstrsplit(id, split = '.', fixed = T)]
r_a2 = merge(a2[, .(hra20_id = as.character(hra_code), by2 = as.character(by2), rads_est = mean, rads_se = mean_se)], compa2, all.x = T, by = c('hra20_id', 'by2'))
r_a2[!is.na(hra20_id), .(sum(rads_est - mean), sum(rads_se - se))]
