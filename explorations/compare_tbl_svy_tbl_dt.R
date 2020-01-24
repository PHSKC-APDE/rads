library('rads')
library('srvyr')
test.data <- get_data_birth(year = 2015:2017)

test.results <- calc(test.data,
                     what = c("mother_weight_gain", "fetal_pres"),
                     "chi_year == 2016 & chi_sex %in% c('Male', 'Female')",
                      by = c("chi_year", "chi_sex"),
                      metrics = record_metrics(),
                     per = 1)
test.data[, weight := 1]
a <- as_survey_design(test.data, probs = weight)

svy.results <- calc(a,
                    what = c('mother_weight_gain', "fetal_pres"),
                    chi_year == 2016, chi_sex %in% c('Male', 'Female'),
                    by = c("chi_year", "chi_sex"),
                    metrics = survey_metrics())

blah = merge(svy.results[, .(variable, level, chi_year, chi_sex, sur = mean, sur_se = mean_se)],
             test.results[, .(variable, level, chi_year, chi_sex, dt = mean, dt_se = se)],
             by = c('variable', 'level', 'chi_year','chi_sex'))

blah[, sur - dt]
blah[, sur_se - dt_se]



#
# load('C:/Users/dcasey/Downloads/blah.Rdata')
#
# ret <- svy %>% mutate(blah = runif(dplyr::n()))
