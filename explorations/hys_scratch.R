library('rads')
library('magrittr')
library('srvyr')
library('dplyr')
library('data.table')
what.prop.vars <- c("hopescale", "s15", "s16")

by.vars <- c("a_aian_aic", "a_as_cambodian_aic", "a_as_chinese_aic", "a_as_filipino_aic",
             "a_as_indian_aic", "a_as_japanese_aic", "a_as_korean_aic", "a_as_other_aic",
             "a_as_vietnamese_aic", "a_asian_aic", "a_black_aic", "a_grade", "a_his_aic",
             "a_nhpi_aic", "a_oth_aic", "a_race8", "a_sex", "a_sexorien3", "a_wht_aic", "kc4reg")

hys <- get_data_hys(year = c(2008:2018), kingco = T)

hys <- hys %>%
  mutate(hopescale = case_when(hopescale %in% c(1:3) ~ 0L, hopescale == 4 ~ 1L)) %>%
  mutate_at(vars(s15, s16), list(~ case_when(. %in% c(1, 2) ~ 0L, . %in% c(3, 4) ~ 1L)))

res <- calc(hys, what = "s15", by = c("a_sex"), metrics = c("mean", "rse"), win = 1, time_var = "year", proportion = F)



blah = calc(hys, what = "s16", by = c("a_sex"), metrics = c("mean", "rse"), win = 2, time_var = "year")


stupid_cw = hys$variables$s15
stupid_cw[hys$variables$year<=2010] = NA
hys <- hys %>% mutate(testvar = stupid_cw)
chk_test = calc(hys, what = 'testvar', by = c('year', 'a_sex'), metrics = 'mean', proportion = T)

chk <- calc(hys, what = "s15", by = c("year", "a_grade", "a_sex"), metrics = c("mean", "rse"), proportion = T)

what.prop.vars <- c("hopescale", "s15", "s16")
chk2 <- calc(hys, what = what.prop.vars, by = c("year", "a_sex"), metrics = c("mean", "rse"), proportion = T)


chk2.1 <- calc(hys, what = 'hopescale', by = c("year", "a_sex"), metrics = c("mean", "rse"), proportion = F)
chk2.2 <- calc(hys, what = 's15', by = c("year", "a_sex"), metrics = c("mean", "rse"), proportion = T)
chk2.3 <- calc(hys, what = 's16', by = c("year", "a_sex"), metrics = c("mean", "rse"), proportion = T)

chk2.1 <- calc(hys, what = 'hopescale', by = c("a_sex"), metrics = c("mean", "rse"), time_var = 'year', win = 1, proportion = T)




hys %>% group_by(year, a_sex) %>% summarize(a = survey_mean(hopescale, proportion = F, na.rm = T))

b = svyby(~hopescale, ~year+a_sex, hys, svymean, na.rm = T)


trend.vars <- c("kingco","a_grade", "a_sex", "a_aian_aic", "a_asian_aic", "a_black_aic",
                "a_his_aic","a_nhpi_aic", "a_wht_aic", "a_oth_aic", "a_race8", "kc4reg")
chi_1_year <- rbindlist(lapply(trend.vars, function(by.var){
  print(by.var)
  temp.results <- calc(hys,
                       what = what.prop.vars,
                       by = c("year", by.var),
                       metrics = c("mean", "rse"),
                       proportion = TRUE)
  temp.results[, cat1_varname := by.var]
  setnames(temp.results, by.var, "cat1_group")
}))


ph.data = hys
what = 's15'
by = c("year", "a_grade", "a_sex")
metrics = c("mean", "rse")
proportion = T
per = NULL
win = NULL
time_var = NULL
verbose = F
