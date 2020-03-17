library('rads')
library('srvyr')
library('survey')
library('dplyr')
library('data.table')

data(api) #from the survey package
setDT(apisrs)
apisrs[stype != 'E', bytest := stype]
sur = apisrs %>% as_survey_design(ids = 1, fpc = fpc)
set.seed(98104)

r3 = sur %>% group_by(bytest) %>% summarize(a = survey_mean(api00))
r4 = svyby(~api00, ~bytest, sur, svymean)
r1[]

r2[]
