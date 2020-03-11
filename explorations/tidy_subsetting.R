library('rads')
library('survey')
library('srvyr')
library('data.table')
data(api)

rec = copy(as.data.table(apiclus1))
rectbl =apiclus1
svy = as_survey_design(svydesign(id=~dnum, weights=~pw, data=apiclus1, fpc=~fpc))


time_var = rlang::sym('api00')
what = rlang::sym('api99')

svy %>% summarize(b = unweighted(sum({{time_var}}[!{{what}}>700])))

