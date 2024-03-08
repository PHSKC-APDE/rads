## ----echo = F, message=FALSE--------------------------------------------------
library(rads)

## ----warning = FALSE, message = FALSE-----------------------------------------
args(get_population)

## ----warning = FALSE, message = FALSE-----------------------------------------
get_population()[]

## ----warning = FALSE, message = FALSE-----------------------------------------
get_population(geo_type = 'wa', round = TRUE)[]

## ----warning = FALSE, message = FALSE-----------------------------------------
get_population(round = T)[]

## ----warning = FALSE, message = FALSE-----------------------------------------
get_population(geo_type = c("region"),
               group_by = c("geo_id"),
               round = TRUE)[]

## ----warning = FALSE, message = FALSE-----------------------------------------
rads::get_population(geo_type = 'region', 
                     round = F)[]

## ----warning = FALSE, message = FALSE-----------------------------------------
head(get_population(geo_type = c("hra"), group_by = c("geo_id"))[])  

## ----warning = FALSE, message = FALSE-----------------------------------------
head(get_population(geo_type = c("zip"), group_by = c("geo_id"))[])  

## ----warning = FALSE, message = FALSE-----------------------------------------
head(get_population(geo_type = c("tract"), group_by = c("geo_id"), ages = 18, census_vintage = 2020, geo_vintage = 2020)[])  

## ----warning = FALSE, message = FALSE-----------------------------------------
head(get_population(geo_type = c("blkgrp"), group_by = c("geo_id"), ages = 18,census_vintage = 2020, geo_vintage = 2020)[])  

## ----warning = FALSE, message = FALSE-----------------------------------------
#ages added to make things go faster
head(get_population(geo_type = c("blk"), group_by = c("geo_id"), ages = 18, census_vintage = 2020, geo_vintage = 2020)[])  

## ----warning = FALSE, message = FALSE-----------------------------------------
get_population(years = 2017:2019)[]

## ----warning = FALSE, message = FALSE-----------------------------------------
get_population(years = 2017:2019, 
               group_by = "years")[]

## ----warning = FALSE, message = FALSE-----------------------------------------
get_population(ages = 65:70)[]

## ----warning = FALSE, message = FALSE-----------------------------------------
get_population(ages = 65:70, group_by = "ages")[]

## ----warning = FALSE, message = FALSE-----------------------------------------
get_population(genders = "F")[]

## ----warning = FALSE, message = FALSE-----------------------------------------
get_population(group_by = "genders")[]

## ----warning = FALSE, message = FALSE-----------------------------------------
get_population(races = "aian", race_type = "race_eth")[]

## ----warning = FALSE, message = FALSE-----------------------------------------
get_population(races = "aian", race_type = "race", group_by = 'race')[]

## ----warning = FALSE, message = FALSE-----------------------------------------
get_population(race_type = "race_eth", group_by = "race_eth")[]

## ----warning = FALSE, message = FALSE-----------------------------------------
get_population(race_type = "race", group_by = "race")[]

## ----warning = FALSE, message = FALSE-----------------------------------------
# pull in data stratified by race/eth and region
reg_hisp_nonhisp <- get_population(geo_type = 'region', group_by = c('race_eth'))

# explicitly label all non-Hispanic individuals as 'Non-Hispanic'
reg_hisp_nonhisp[race_eth != 'Hispanic', race_eth := 'Non-Hispanic']

# aggregate the population by region and race_eth
reg_hisp_nonhisp <- reg_hisp_nonhisp[, .(pop = sum(pop)), .(region = geo_id, race_eth)]
print(reg_hisp_nonhisp)

## ----warning = FALSE, message = FALSE-----------------------------------------
get_population(geo_type = "region", 
               years = 2017:2019, 
               group_by = c("geo_id", "years", "genders"))[]

## ----warning = FALSE, message = FALSE-----------------------------------------
get_population(ages = 16:25, 
               genders = "F", 
               years = 2017:2019, 
               races = c("hispanic", "asian"), 
               geo_type = "region", 
               race_type = "race_eth", 
               group_by = c("geo_id", "years", "race_eth"), 
               round = F)[]

## ----warning = FALSE, message = FALSE-----------------------------------------
# Return all race X eth combos
get_population(race_type = 'race_eth', group_by = c('race_eth', 'hispanic'))

# Population of white people, by ethnicity
get_population(race_type = 'race_eth', races = 'white', group_by = c('race_eth', 'hispanic'))


## ----warning = FALSE, message = FALSE-----------------------------------------

# Via autoconnect
r1 = get_population()

mycon <- DBI::dbConnect(
  odbc::odbc(), 
  driver = getOption("rads.odbc_version"), 
  server = "kcitazrhpasqlprp16.azds.kingcounty.gov", 
  database = "hhs_analytics_workspace",
  uid = keyring::key_list('hhsaw')[["username"]], 
  pwd = keyring::key_get('hhsaw', keyring::key_list('hhsaw')[["username"]]), 
  Encrypt = "yes", 
  TrustServerCertificate = "yes", 
  Authentication = "ActiveDirectoryPassword")

r2 = get_population(mykey = mycon)

print(all.equal(r1,r2))


