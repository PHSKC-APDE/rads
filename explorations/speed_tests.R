library('rads')
library('srvyr')
library('data.table')
library('dtsurvey')

acs = fread("//phshare01/epe_share/WORK/surveys/ACS/PUMS_data/2018_2014_5_year/raw_unzipped_downloads/psam_p53.csv")

dat = data.table(id = 1:10000000)
dat[, v1 := runif(.N)]
dat[, v2 := sample(1:3, .N, replace = T)]
dat[, v3 := sample(1:3, .N, replace = T)]
dat[, weight := 1]
dat[, time := 10]
format(object.size(dat), 'Mb')


att1 = profvis::profvis({survey::svydesign(~1, data = dat, weights = ~weight)})
att2 = profvis::profvis({dtsurvey(dat, weight = 'weight')})
att3 = profvis::profvis({
  as_survey_rep(acs, weights = PWGTP,
                combined.weights = TRUE ,
                repweights = grep('PWGTP[0-9]+', names(acs), value  = T),
                scale = 4 / 80 ,
                rscales = rep( 1 , 80 ) ,
                mse = TRUE ,
                type = "JK1")
})


