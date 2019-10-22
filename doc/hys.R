## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, warning = FALSE, message = FALSE-----------------------------
library('data.table')
library(rads)
library('srvyr') #analyzing surveys
library('dplyr') #data manipulation and stress

## ----listdatasets--------------------------------------------------------
rads::list_apde_data()

## ----listvariables, warning = FALSE--------------------------------------
rads::list_dataset_columns(dataset = 'hys', analytic_only = F)

## ----loaddata------------------------------------------------------------
hys <- get_data('hys', year = c(2016, 2018))

## ----checkclass----------------------------------------------------------
print(paste0('The classes are: ', paste(class(hys), collapse = ', ')))

d = dim(hys)
print(paste('The dataset has', d[1], 'respondents and', d[2], 'columns loaded'))


## ----babysfirstcomputation, warning=FALSE--------------------------------

res <- hys %>% 
  tabulate_variable(variable = 'ecig_vape',
                    metrics = c('mean', 'se', 'numerator', 'denominator'),
                    sex = 'both',
                    grade = NULL, #all grades
                    region = NULL, #all regions
                    race = NULL, #a_race8,
                    year = c(2016, 2018), #2016 and 2018 grouped
                    sexual_orientation = NULL,
                    na.rm = TRUE
                   )

#fix some things for display
#note, the output of tabulate_variable is a data.table
res[, .Sexual_Orientation := 'All']
res[, .Race := 'All']
res[, .Region := 'King County']
res[, .Grade := 'All']
res[, .Year := '2016 - 2018']

#Print the results
res[]


## ----babyssecondcomputation, warning = F---------------------------------

res <- hys %>% 
  tabulate_variable(variable = 'ecig_vape',
                    metrics = 'mean', #only the mean this time
                    sex = 'both',
                    grade = list(Tiny = c(6,8), Big = c(10,12)), #two grade groups
                    region = NULL, #all regions
                    race = 'all', #a_race8,
                    year = c(2016, 2018), #each year seperately
                    sexual_orientation = NULL,
                    na.rm = TRUE
                   )

res = res[.Race != 'Race: NA', .(.Race, .Grade, Percent = round(mean * 100))]
res[]


## ----aic-----------------------------------------------------------------

res <- hys %>% 
  tabulate_variable(variable = 'ecig_vape',
                    metrics = 'mean', #only the mean this time
                    sex = 'both',
                    grade = NULL, #two grade groups
                    region = NULL, #all regions
                    race = 'aic', #all alone or in combination groups,
                    year = c(2016, 2018), #each year seperately
                    sexual_orientation = NULL,
                    na.rm = TRUE
                   )

res = res[!grepl('Not', .Race, fixed = T), .(.Race, Percent = round(mean * 100))]
res[]


## ----aicsubset-----------------------------------------------------------

res <- hys %>% 
  tabulate_variable(variable = 'ecig_vape',
                    metrics = 'mean', #only the mean this time
                    sex = 'both',
                    grade = NULL, #two grade groups
                    region = NULL, #all regions
                    race = c('a_black_aic', 'a_asian_aic'), #all alone or in combination groups,
                    year = c(2016, 2018), #each year seperately
                    sexual_orientation = NULL,
                    na.rm = TRUE
                   )

res = res[!grepl('Not', .Race, fixed = T), .(.Race, Percent = round(mean * 100))]
res[]


## ----racecustomgroup-----------------------------------------------------
res <- hys %>% 
  tabulate_variable(variable = 'ecig_vape',
                    metrics = 'mean', #only the mean this time
                    sex = 'both',
                    grade = NULL, #two grade groups
                    region = NULL, #all regions
                    race = list(A = c('Asian', 'AIAN'), H = 'Hispanic'), #custom grouping!
                    race_var = 'a_race8', # this will probably be renamed at some point
                    year = c(2016, 2018), #each year seperately
                    sexual_orientation = NULL,
                    na.rm = TRUE
                   )

res = res[, .(.Race, Percent = round(mean * 100))]
res[]


## ----seperatesex---------------------------------------------------------

res <- hys %>% 
  tabulate_variable(variable = 'ecig_vape',
                    metrics = 'mean', #only the mean this time
                    sex = 'seperate',
                    grade = NULL, #two grade groups
                    region = NULL, #all regions
                    race = NULL, #custom grouping! Everything else goes to NA and is dropped
                    year = c(2016, 2018), #each year seperately
                    sexual_orientation = NULL,
                    na.rm = TRUE
                   )

res = res[, .(.Sex, Percent = round(mean * 100))]
res[]


## ----customtab-----------------------------------------------------------

res <- hys %>% survey_tabulate(what = 'ecig_vape',
                               kingco == 1, !is.na(ecig_vape), kcfinalwt>0, year == 2018,
                               by = 'ever_sex',
                               metrics = c('mean', 'numerator', 'denominator'),
                               proportion = T)
res[]


