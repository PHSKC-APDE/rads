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

# hys <- get_data_hys(year = c(2008:2018), kingco = T)
# hys <- hys %>% mutate(hopescale = case_when(hopescale %in% c(1:3) ~ 0L, hopescale == 4 ~ 1L))
# calc(hys, what = 'hopescale', by = c("a_sex"), metrics = c("mean", "rse"), time_var = 'year', proportion = T)
# calc(hys, what = 'hopescale', by = c("a_sex"), metrics = c("mean", "rse"), time_var = 'year', fancy_time = F, proportion = T)
#
#
# blah = as.data.table(hys$variables)
# calc(blah, what = 'hopescale', by = c("a_sex"), metrics = c("mean", "rse"), time_var = 'year', fancy_time = F, proportion = T)
# calc(blah, what = 'hopescale', by = c("a_sex"), metrics = c("mean", "rse"), time_var = 'year', fancy_time = T, proportion = T)


hys <- get_data_hys(year = c(2008:2018), kingco = T)

hys <- hys %>%
  mutate(hopescale = case_when(hopescale %in% c(1:3) ~ 0L, hopescale == 4 ~ 1L)) %>%
  mutate_at(vars(s15, s16), list(~ case_when(. %in% c(1, 2) ~ 0L, . %in% c(3, 4) ~ 1L)))

chk2.1 <- calc(hys, what = 'hopescale', by = c("year", "a_sex"), metrics = c("mean", "rse"), proportion = F)
chk2.2 <- calc(hys, what = 'hopescale', by = c("year", "a_sex"), metrics = c("mean", "rse"), proportion = T)
chk2.3 <- calc(hys$variables, what = 'hopescale', by = c("year", "a_sex"), metrics = c("mean", "rse"), proportion = F)
chk2.4 <- calc(hys$variables, what = 'hopescale', by = c("year", "a_sex"), metrics = c("mean", "rse"), proportion = T)
herp = as.data.table(hys$variables)

