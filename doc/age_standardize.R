## ---- echo = F, message=FALSE-------------------------------------------------
library(rads)

## -----------------------------------------------------------------------------
list_ref_pop()[1:5]

## -----------------------------------------------------------------------------
get_ref_pop("2000 U.S. Std Population (11 age groups)")[, 1:4]

## ---- warning=FALSE, message=FALSE--------------------------------------------
library(data.table)
set.seed(98121)

temp1 <- data.table(
  age = rep(51:60, 100), 
  disease = sample(0:1, 1000, replace = T), 
  pop = rep(c(seq(1000, 910, -10)), 100))

temp1[]

## ---- warning=FALSE, message=FALSE--------------------------------------------
age_standardize(ph.data = temp1,
                ref.popname = "2000 U.S. Std Population (11 age groups)", 
                collapse = T,
                my.count = "disease", 
                my.pop = "pop", 
                per = 1000, 
                conf.level = 0.95)[]

## ---- warning=FALSE, message=FALSE--------------------------------------------
temp1 <- temp1[, .(disease = sum(disease)), by = c("age", "pop")]
temp1[]

## ---- warning=FALSE, message=FALSE--------------------------------------------
ex1.1 <- age_standardize(ph.data = temp1,
                      ref.popname = "2000 U.S. Std Population (11 age groups)", 
                      collapse = T,
                      my.count = "disease", 
                      my.pop = "pop", 
                      per = 1000, 
                      conf.level = 0.95)
ex1.1[]

## ---- warning=FALSE, message=FALSE--------------------------------------------
ex1.2 <- age_standardize(ph.data = temp1,
                      ref.popname = list_ref_pop()[36], 
                      collapse = T,
                      my.count = "disease", 
                      my.pop = "pop", 
                      per = 1000, 
                      conf.level = 0.95)
ex1.2[]

## ---- warning=FALSE, message=FALSE--------------------------------------------
set.seed(98121)
temp2 <- data.table(
  gender = c(rep("F", 20), rep("M", 20)), 
  age = rep(46:65, 2),
  disease = c(sample(25:46, 20, replace = T), sample(25:35, 20, replace = T)), 
  pop = c(sample(2500:3500, 20, replace = T), sample(2200:3300, 20, replace = T)))

head(temp2)

## ---- warning = FALSE, message = FALSE----------------------------------------
ex2.1 <- age_standardize(ph.data = temp2,
                       collapse = T,
                       my.count = "disease", 
                       my.pop = "pop", 
                       per = 1000, 
                       conf.level = 0.95)

ex2.1[]

## ---- warning = FALSE, message = FALSE----------------------------------------
ex2.2 <- age_standardize(ph.data = temp2,
                       collapse = T,
                       my.count = "disease", 
                       my.pop = "pop", 
                       per = 1000, 
                       conf.level = 0.95, 
                       group_by = "gender")

ex2.2[]

## ---- warning=FALSE, message=FALSE--------------------------------------------
set.seed(98121)
new.standard <- data.table(
  gender = c(rep("M", 20), rep("F", 20)), 
  age = rep(46:65, 2),
  stdpop = c(sample(7800:16000, 20, replace = T), sample(10000:20000, 20, replace = T)))

head(new.standard)

## ---- warning=FALSE, message=FALSE--------------------------------------------
temp3 <- merge(temp2, new.standard, by = c("age", "gender"), all = T)

head(temp3)

## ---- warning = FALSE, message = FALSE----------------------------------------
ex3.1 <- age_standardize(ph.data = temp3,
                       ref.popname = "none",
                       collapse = F,
                       my.count = "disease", 
                       my.pop = "pop", 
                       per = 1000, 
                       conf.level = 0.95, 
                       group_by = "gender")

ex3.1[]

## ---- warning=FALSE, message=FALSE--------------------------------------------
set.seed(98121)
temp4 <- data.table(
  gender = c(rep("M", 20), rep("F", 20)), 
  age = rep(46:65, 2),
  disease = c(sample(25:46, 20, replace = T), sample(25:35, 20, replace = T)), 
  pop = c(sample(2500:3500, 20, replace = T), sample(2200:3300, 20, replace = T)))

head(temp4)

## ---- warning=FALSE, message=FALSE--------------------------------------------
temp4[age %in% 45:49, agecat := "45-49 years"]
temp4[age %in% 50:54, agecat := "50-54 years"]
temp4[age %in% 55:59, agecat := "55-59 years"]
temp4[age %in% 60:64, agecat := "60-64 years"]
temp4[age %in% 65:69, agecat := "65-69 years"]
temp4 <- temp4[, .(pop = sum(pop), disease = sum(disease)), by = c("agecat", "gender")]

temp4[]

## ---- warning=FALSE, message=FALSE--------------------------------------------
ex4.1 <- age_standardize(ph.data = temp4,
                       collapse = F,
                       my.count = "disease", 
                       my.pop = "pop", 
                       per = 1000, 
                       conf.level = 0.95, 
                       group_by = "gender")

ex4.1[]

## ---- warning=FALSE, message=FALSE--------------------------------------------
  kcbirth <- get_data_birth(cols = c("chi_age", "chi_year"), year = 2019, kingco = T)
  wabirth <- get_data_birth(cols = c("chi_age", "chi_year"), year = 2019, kingco = F)
  
  births <- rbind(kcbirth[, geo := "King County"], wabirth[, geo := "WA State"])
  births <- births[chi_age %in% 13:19]
  
  # collapse / aggregate
  births <- births[, .(births = .N), by = c("chi_age", "geo")] 
  setorder(births, geo, chi_age)
  setnames(births, "chi_age", "age")
  births[]

## ---- warning=FALSE, message=FALSE--------------------------------------------
  kcpop <- get_population(kingco = T, years = 2019, ages = 13:19, 
                          genders = "Female", group_by = "ages", geo_vintage = 2020, census_vintage = 2020)
  kcpop <- kcpop[, .(age, geo = geo_id, pop)] 
  
  wapop <- get_population(kingco = F, years = 2019, ages = 13:19, 
                          genders = "Female", group_by = "ages", geo_type = "zip", geo_vintage = 2020, census_vintage = 2020)
  wapop <- wapop[, .(pop = sum(pop), geo = "WA State"), by = "age"]
  
  pop <- rbind(kcpop, wapop)
  pop[]


## ---- warning=FALSE, message=FALSE--------------------------------------------
  temp5 <- merge(births, pop, by = c("age", "geo"), all = T)
  
  temp5[]

## ---- warning=FALSE, message=FALSE--------------------------------------------
  ex5.1 <- age_standardize(ph.data = temp5,
                           ref.popname = "World (WHO 2000-2025) Std Million (single ages to 84)", 
                           collapse = T,
                           my.count = "births", 
                           my.pop = "pop", 
                           per = 1000, 
                           conf.level = 0.95, 
                           group_by = "geo")
  ex5.1[]

