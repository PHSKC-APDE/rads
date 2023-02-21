## ---- echo = F, message=FALSE-------------------------------------------------
library(rads)

## ---- warning = FALSE, message = FALSE----------------------------------------
args(get_population)

## ---- warning = FALSE, message = FALSE----------------------------------------
get_population()[]

## ---- warning = FALSE, message = FALSE----------------------------------------
get_population(geo_type = 'wa', round = TRUE)[]

## ---- warning = FALSE, message = FALSE----------------------------------------
get_population(round = T)[]

## ---- warning = FALSE, message = FALSE----------------------------------------
get_population(geo_type = c("region"),
               group_by = c("geo_id"),
               round = TRUE)[]

## ---- warning = FALSE, message = FALSE----------------------------------------
rads::get_population(geo_type = 'region', 
                     round = F)[]

## ---- warning = FALSE, message = FALSE----------------------------------------
head(get_population(geo_type = c("hra"), group_by = c("geo_id"))[])  

## ---- warning = FALSE, message = FALSE----------------------------------------
head(get_population(geo_type = c("zip"), group_by = c("geo_id"))[])  

## ---- warning = FALSE, message = FALSE----------------------------------------
head(get_population(geo_type = c("tract"), group_by = c("geo_id"), ages = 18, census_vintage = 2020, geo_vintage = 2020)[])  

## ---- warning = FALSE, message = FALSE----------------------------------------
head(get_population(geo_type = c("blkgrp"), group_by = c("geo_id"), ages = 18,census_vintage = 2020, geo_vintage = 2020)[])  

## ---- warning = FALSE, message = FALSE----------------------------------------
#ages added to make things go faster
head(get_population(geo_type = c("blk"), group_by = c("geo_id"), ages = 18, census_vintage = 2020, geo_vintage = 2020)[])  

## ---- warning = FALSE, message = FALSE----------------------------------------
get_population(years = 2017:2019)[]

## ---- warning = FALSE, message = FALSE----------------------------------------
get_population(years = 2017:2019, 
               group_by = "years")[]

## ---- warning = FALSE, message = FALSE----------------------------------------
get_population(ages = 65:70)[]

## ---- warning = FALSE, message = FALSE----------------------------------------
get_population(ages = 65:70, group_by = "ages")[]

## ---- warning = FALSE, message = FALSE----------------------------------------
get_population(genders = "F")[]

## ---- warning = FALSE, message = FALSE----------------------------------------
get_population(group_by = "genders")[]

## ---- warning = FALSE, message = FALSE----------------------------------------
get_population(races = "aian", race_type = "race_eth")[]

## ---- warning = FALSE, message = FALSE----------------------------------------
get_population(races = "aian", race_type = "race", group_by = 'race')[]

## ---- warning = FALSE, message = FALSE----------------------------------------
get_population(race_type = "race_eth", group_by = "race_eth")[]

## ---- warning = FALSE, message = FALSE----------------------------------------
get_population(race_type = "race", group_by = "race")[]

## ---- warning = FALSE, message = FALSE----------------------------------------
get_population(geo_type = "region", 
               years = 2017:2019, 
               group_by = c("geo_id", "years", "genders"))[]

## ---- warning = FALSE, message = FALSE----------------------------------------
get_population(ages = 16:25, 
               genders = "F", 
               years = 2017:2019, 
               races = c("hispanic", "asian"), 
               geo_type = "region", 
               race_type = "race_eth", 
               group_by = c("geo_id", "years", "race_eth"), 
               round = F)[]

