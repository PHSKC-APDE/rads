## ----echo = F, message=FALSE--------------------------------------------------
library(rads)
library(dtsurvey)

## -----------------------------------------------------------------------------
data(mtcars)
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
calc(ph.data = birth, 
     what = c("preterm"), 
     where = chi_sex == "Male",
     metrics = c("mean", "rse", "numerator", "denominator"), 
     time_var = "chi_year")[]

## -----------------------------------------------------------------------------
calc(ph.data = birth, 
     what = c("preterm"), 
     where = chi_sex == "Male" & chi_race_eth8 == "Hispanic",
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

## ----warning=FALSE------------------------------------------------------------
calc(ph.data = birth, 
     what = c("chi_race_eth8"), 
     chi_year %in% 2017:2019,
     metrics = c("mean", "rse", "obs", "numerator", "denominator"))[]

## ----warning=FALSE------------------------------------------------------------
calc(ph.data = birth, 
     what = c("chi_race_eth8"), 
     chi_year %in% 2017:2019,
     metrics = c("obs", "numerator", "denominator", "rate"), 
     per = 100000)[]

## ----warning=FALSE------------------------------------------------------------
calc(ph.data = birth, 
     what = c("chi_sex"), 
     chi_year %in% 2017:2019,
     metrics = c("obs", "missing", "missing.prop"), 
     by = "chi_year")[]

## ----warning=FALSE, message=FALSE---------------------------------------------
library(survey)
library(data.table)

load("//dphcifs/APDE-CDIP/ACS/PUMS_data/2021_1_year/prepped_R_files/2021_1_year_data.RData")

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
  
  # New for version 1.0.0
  # This allows for the use of data.table syntax for data cleaning
  # users who prefer dplyr syntax should review the srvyr package
  pums = dtsurvey::dtrepsurvey(pums)

## ----warning=FALSE------------------------------------------------------------
test1 <- calc(ph.data = pums, 
             what = "chi_geo_seattle", 
             metrics = c('mean', 'numerator', 'denominator', 'obs', 'total'), 
             where = chi_geo_kc == 1)
print(test1)

## ----warning=FALSE------------------------------------------------------------
pums2 <- copy(pums)
pums2 <- pums2[chi_geo_seattle == 1, chi_geo_seattle := ifelse(rowid(chi_geo_seattle) <= 100, NA, chi_geo_seattle)]
test2 <- calc(ph.data = pums2, 
             what = "chi_geo_seattle", 
             metrics = c('mean', 'numerator', 'denominator', 'obs', 'total'), 
             where = chi_geo_kc == 1)
print(test2)

## ----warning=FALSE------------------------------------------------------------
# WA State
calc(ph.data = pums, 
     what = c('chi_geo_wastate'), 
     metrics = c("numerator", "total"), 
     proportion = F)[]

# King County
calc(ph.data = pums, 
     what = c("chi_geo_kc"), 
     metrics = c("numerator", "total"), 
     proportion = F)[]

# Seattle
calc(ph.data = pums, 
     what = c("chi_geo_seattle"), 
     metrics = c("numerator", "total"), 
     proportion = F)[]

## ----warning=FALSE------------------------------------------------------------
calc(ph.data = pums, 
     what = c("disability", "GEpov200"), 
     metrics = c("mean", "rse", "obs", "numerator", "denominator"), 
     proportion = T, 
     by = "chi_geo_kc")[]

## ----warning=FALSE------------------------------------------------------------
calc(ph.data = pums, 
     what = c("age6"), 
     metrics = c("mean", "rse", "obs", "numerator", "denominator"), 
     proportion = F, 
     by = "disability")[]

## ----warning=FALSE------------------------------------------------------------
calc(ph.data = pums, 
     what = c("agep"),
     chi_geo_kc == 1,
     metrics = c("mean", "median", "rse", "obs", "numerator", "denominator"), 
     by = "disability")[]

## ----warning=FALSE, message=FALSE---------------------------------------------
library(data.table)
set.seed(98121) 

mydt <- data.table(
  school = as.factor(sample(c("Alpha", "Beta", "Gamma", "Delta"), 2000, replace = T)),
  grades = as.factor(sample(c("A", "B", "C", "D"), 2000, replace = T)), 
  year = sample(2016:2021, 2000, replace = T))

mydt[]

## ----warning=FALSE, message=FALSE---------------------------------------------
grades.distribution <- calc(
  ph.data = mydt, 
  school %in% c("Alpha", "Beta"), 
  what = "grades", 
  by = "school", 
  time_var = "year", 
  metrics = c("numerator", "denominator", "mean"), proportion = F)

grades.distribution[level %in% c("A", "B")]

## ----warning=FALSE, message=FALSE---------------------------------------------
# create weights
set.seed(98121)
mydt[, mywghts := sample(50:1300, 2000, replace = T)]

# survey set the data
# This uses the dtsurvey package
# similar logic applies for survey and srvyr package
mydt[, `_id` := NULL] # remove id to make things play nice
mysvy <-dtsurvey::dtsurvey(data.table(mydt), weight = 'mywghts')

## ----warning=FALSE, message=FALSE---------------------------------------------
grades.distribution2 <- calc(
  ph.data = mysvy, 
  school %in% c("Alpha", "Beta"), 
  what = "grades", 
  by = "school", 
  time_var = "year", 
  metrics = c("numerator", "denominator", "mean"), proportion = FALSE)

grades.distribution2[level %in% c("A", "B")]

