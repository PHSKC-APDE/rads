## ---- echo = F, message=FALSE-------------------------------------------------
library(rads)
library(dtsurvey)

## -----------------------------------------------------------------------------
data(mtcars)
mtcars = dtadmin(mtcars) #make it work with calc.dtsurvey
calc(ph.data = mtcars, what = c("mpg"))[]

## -----------------------------------------------------------------------------
birth <- get_data_birth(cols = c("chi_year", "chi_sex", "chi_race_eth8", 
                                 "preterm", "birth_weight_grams", "mother_birthplace_state"), 
                        year = c(2013:2019), 
                        kingco = T)

## -----------------------------------------------------------------------------
calc(ph.data = birth, 
     what = c("preterm"), 
     metrics = c("mean", "rse", "numerator", "denominator"), 
     time_var = "chi_year")[]

## -----------------------------------------------------------------------------
birth[, cy := chi_year]
calc(ph.data = birth, 
     what = c("preterm"), 
     metrics = c("mean", "rse", "numerator", "denominator"), 
     time_var = "chi_year", 
     by = "cy")[]

## -----------------------------------------------------------------------------
calc(ph.data = birth, 
     what = c("preterm"), 
     metrics = c("mean", "rse", "numerator", "denominator"), 
     time_var = "chi_year", 
     win = 3)[]

## -----------------------------------------------------------------------------
calc(ph.data = birth, 
     what = c("birth_weight_grams"), 
     metrics = c("mean", "rse"), 
     time_var = "chi_year", 
     win = 3)[]

## -----------------------------------------------------------------------------
calc(ph.data = birth, 
     what = c("birth_weight_grams"), 
     chi_year == 2019,
     metrics = c("mean", "rse"), 
     by = c("chi_race_eth8", "chi_sex"))[]

## -----------------------------------------------------------------------------
calc(ph.data = birth, 
     what = c("chi_race_eth8"), 
     chi_year %in% 2017:2019,
     metrics = c("mean", "rse", "obs", "numerator", "denominator"))[]

## -----------------------------------------------------------------------------
calc(ph.data = birth, 
     what = c("chi_race_eth8"), 
     chi_year %in% 2017:2019,
     metrics = c("obs", "numerator", "denominator", "rate"), 
     per = 100000)[]

## -----------------------------------------------------------------------------
calc(ph.data = birth, 
     what = c("chi_sex"), 
     chi_year %in% 2017:2019,
     metrics = c("obs", "missing", "missing.prop"), 
     by = "chi_year")[]

## ---- warning=FALSE, message=FALSE--------------------------------------------
library(survey)

load("//phshare01/epe_share/WORK/surveys/ACS/PUMS_data/2019_1_year/prepped_R_files/2019_1_year_data.RData")

  pums <-     
    survey::svrepdesign(
      weight = ~pwgtp ,
      combined.weights = TRUE ,
      repweights = 'pwgtp[0-9]+' ,
      scale = 4 / 80 ,
      rscales = rep( 1 , 80 ) ,
      mse = TRUE ,
      type = "JK1" ,
      data = person.wa
    )
  
  #New for version 1.0.0
  pums =dtsurvey::dtrepsurvey(pums)

## ---- warning=FALSE-----------------------------------------------------------
pums[, constant :=1]

# WA State
calc(ph.data = pums, 
     what = c("constant"), 
     metrics = c("numerator", "total"), 
     proportion = T)[]

# King County
calc(ph.data = pums, 
     what = c("constant"), 
     chi_geo_kc ==1,
     metrics = c("numerator", "total"), 
     proportion = T)[]

# Seattle
calc(ph.data = pums, 
     what = c("constant"), 
     chi_geo_seattle ==1,
     metrics = c("numerator", "total"), 
     proportion = T)[]

## ---- warning=FALSE-----------------------------------------------------------
calc(ph.data = pums, 
     what = c("disability", "GEpov200"), 
     metrics = c("mean", "rse", "obs", "numerator", "denominator"), 
     proportion = T, 
     by = "chi_geo_kc")[]

## ---- warning=FALSE-----------------------------------------------------------
calc(ph.data = pums, 
     what = c("agechna"), 
     metrics = c("mean", "rse", "obs", "numerator", "denominator"), 
     proportion = F, 
     by = "disability")[]

## ---- warning=FALSE-----------------------------------------------------------
calc(ph.data = pums, 
     what = c("agep"),
     chi_geo_kc == 1,
     metrics = c("mean", "median", "rse", "obs", "numerator", "denominator"), 
     by = "disability")[]

## ---- warning=FALSE, message=FALSE--------------------------------------------
library(data.table)
set.seed(98121) 

mydt <- data.table(
  school = as.factor(sample(c("Alpha", "Beta", "Gamma", "Delta"), 2000, replace = T)),
  grades = as.factor(sample(c("A", "B", "C", "D"), 2000, replace = T)), 
  year = sample(2016:2021, 2000, replace = T))

#New for version 1.0.0
mydt = dtsurvey::dtadmin(mydt)

mydt[]

## ---- warning=FALSE, message=FALSE--------------------------------------------
grades.distribution <- calc(
  ph.data = mydt, 
  school %in% c("Alpha", "Beta"), 
  what = "grades", 
  by = "school", 
  time_var = "year", 
  metrics = c("numerator", "denominator", "mean"), proportion = F)

grades.distribution[1:4]

## ---- warning=FALSE, message=FALSE--------------------------------------------
# create weights
set.seed(98121)
mydt[, mywghts := sample(50:1300, 2000, replace = T)]

# survey set the data
mysvy <-dtsurvey::dtsurvey(data.table(mydt[, `_id` := NULL]), weight = 'mywghts')

## ---- warning=FALSE, message=FALSE--------------------------------------------
grades.distribution2 <- calc(
  ph.data = mysvy, 
  school %in% c("Alpha", "Beta"), 
  what = "grades", 
  by = "school", 
  time_var = "year", 
  metrics = c("numerator", "denominator", "mean"), proportion = FALSE)

grades.distribution2[1:4]

