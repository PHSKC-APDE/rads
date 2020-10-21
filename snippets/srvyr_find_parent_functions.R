suppressPackageStartupMessages({
library('srvyr')
library('dplyr')
library('survey')
})

data(api) #from the survey package
apisrs = apisrs %>% mutate(yyy = sample(1:4, nrow(apisrs), replace = T))
sur = apisrs %>% as_survey_design(ids = 1, fpc = fpc)
set.seed(98104)

hello = function(x) x*0

a = function(svy){
  b <- hello
  svy %>% summarize(srvyr::unweighted(b(yyy)))
}
a(sur) #is unhappy


a2 = function(dat){
  b <- hello
  dat %>% summarize(b(yyy))
}

head(a2(apisrs)) #Native dplyr does not error

a3 = function(svy){
  svy %>% summarize(srvyr::unweighted(hello(yyy)))
}
a3(sur)
