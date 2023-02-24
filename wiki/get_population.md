---
title: "get_population()"
output:
  rmarkdown::html_vignette: default
  github_document: default
  pdf_document: default
  urlcolor: blue
vignette: |
  %\VignetteEngine{knitr::knitr} %\VignetteIndexEntry{get_population}
---



## Introduction

This vignette will provide some examples of ways to pull population data into R from the Azure cloud (thank you Jeremy!). 

As of 02/2023, there are two sets of population estimates. 

The first, [frankenpop/Population Interim Estimates (PIE)](https://github.com/PHSKC-APDE/frankenpop) were created by APDE. They are based on 2020 geographies for 2000 - 2022+.

The second (and older/outdated ones), between 2000 and 2020 derive are Population numbers are estimated by the [WA Office of Financial Management (OFM) population unit](https://ofm.wa.gov/washington-data-research/population-demographics). OFM produces two sets of estimates: (1) [April 1 official population estimates](https://ofm.wa.gov/washington-data-research/population-demographics/population-estimates/april-1-official-population-estimates) for cities and towns and (2) [Small Area Estimates (SAE)](https://ofm.wa.gov/washington-data-research/population-demographics/population-estimates/small-area-estimates-program) for smaller geographies. The `get_population()` function pulls the SAE numbers and, when `round = T`, should be the same as those in [CHAT](https://secureaccess.wa.gov/doh/chat/Entry.mvc).

**NOTE!!** To get the most out of this vignette, I highly recommend that you actually type each and every bit of code into R. Doing so will almost definitely help you learn the syntax much faster than just reading the vignette or copying and pasting the code.

## `get_population` arguments

Arguments are the values that we send to a function when it is called. Generally, typing `args(my_function_of_interest)` will return the possible arguments including any defaults. For example,


```r
args(get_population)
```

```
## function (kingco = T, years = NA, ages = c(0:100), genders = c("f", 
##     "m"), races = c("aian", "asian", "black", "hispanic", "multiple", 
##     "nhpi", "white"), race_type = c("race_eth"), geo_type = c("kc"), 
##     group_by = NULL, round = FALSE, mykey = "hhsaw", census_vintage = 2020, 
##     geo_vintage = 2010, schema = "ref", table_prefix = "pop_geo_", 
##     return_query = FALSE) 
## NULL
```

The standard arguments for get_population() are:

1) `kingco` \<\< Logical vector of length 1. Identifies whether you want population estimates limited to King County. Only impacts results for geo_type in c('blk', blkgrp', 'lgd', 'scd', 'tract', 'zip'). Default == TRUE.

2) `years` \<\< Numeric vector. Identifies which year(s) of data should be pulled. Default == c(2020).

3) `ages` \<\< Numeric vector. Identifies which age(s) should be pulled. Default == c(0:100), with 100 being the top coded value for 100:120.

4) `genders` \<\< Character vector of length 1 or 2. Identifies gender(s) should be pulled. The acceptable values are 'f', 'female', 'm', and 'male'. Default == c('f', 'm').

5) `races` \<\< Character vector of length 1 to 7. Identifies which race(s) or ethnicity should be pulled. The acceptable values are "aian", "asian", "black", "hispanic", "multiple", "nhpi", and "white". Default == all the possible values.

6) `race_type` \<\< Character vector of length 1. Identifies whether to pull race data with Hispanic as an ethnicity ("race") or Hispanic as a race ("race_eth"). Default == c("race_eth").

7) `geo_type` \<\< Character vector of length 1. Identifies the geographic level for which you want population estimates. The acceptable values are: 'blk', 'blkgrp', 'county', 'hra', 'kc', 'lgd' (WA State legislative districts), 'region', 'seattle', 'scd' (school districts), 'tract', and 'zip'. Default == "kc".

8) `group_by` \<\< Character vector of length 0 to 7. Identifies how you would like the data 'grouped' (i.e., stratified). Valid options are limited to: "years", "ages", "genders", "race", "race_eth", "fips_co", and "geo_id". Default == NULL, i.e., estimates are only grouped / aggregated by geography.

9) `round` \<\< Logical vector of length 1. Identifies whether or not population estimates should be returned as whole numbers. Default == TRUE.

10) `mykey` \<\< a character vector with the name of the `keyring::` key that provides access to the Health and Human Services Analytic Workspace (HHSAW). If you have never set your keyring before and or do not know what this is refering to, just type `keyring::key_set('hhsaw', username = 'ALastname@kingcounty.gov')` into your R console (making sure to replace the username). The default is 'hhsaw'.

11) census_vintage \<\< Either 2010 or 2020. Specifies the anchor census of the desired estimates. 

12) geo_vintage \<\< Either 2010 or 2020. Specifies the anchor census for geographies. For example, 2020 will return geographies based on 2020 blocks.

13) schema \<\< Unless you are a power user, don't mess with this

14) table_prefix \<\< unless you are a power user, don't mess with this

15) return_query \<\< logical. Rather than returning results, the query/queries used to fetch the results are provided

There is no need to specify any or all of the arguments listed above. As the following example shows, the default arguments for `get_population` provide the overall most recent year's estimated King County population.


```r
get_population()[]
```

```
##        pop geo_type      geo_id year   age       gender                                                 race_eth
## 1: 2317700       kc King County 2022 0-100 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White
```

------------------------------------------------------------------------

## Example analyses

***Note 1**: The use of `head()` below is not necessary. It is a convenience function that displays the first 6 rows of data and was used to keep the output in this vignette tidy.*

***Note 2**: The use of `[]` after get_population() is used to print the output to the console. Typically, you would not print the results but would save them as an object. E.g., `my.pop.est <- get_population()`.*

### Geographic estimates

**WA**


```r
get_population(geo_type = 'wa', round = TRUE)[]
```

```
##        pop geo_type           geo_id year   age       gender                                                 race_eth geo_id_code
## 1: 7864400       wa Washington State 2022 0-100 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White          53
```

**King County**


```r
get_population(round = T)[]
```

```
##        pop geo_type      geo_id year   age       gender                                                 race_eth
## 1: 2317700       kc King County 2022 0-100 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White
```

**King County Regions**


```r
get_population(geo_type = c("region"),
               group_by = c("geo_id"),
               round = TRUE)[]
```

```
##       pop geo_type  geo_id year   age       gender                                                 race_eth geo_id_code
## 1: 609680   region    East 2022 0-100 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White           1
## 2: 808130   region   South 2022 0-100 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White           4
## 3: 759393   region Seattle 2022 0-100 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White           3
## 4: 140498   region   North 2022 0-100 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White           2
```

**King County Regions with `round=FALSE`**

Turn off rounding to get the exact (fractional) number of people estimated.


```r
rads::get_population(geo_type = 'region', 
                     round = F)[]
```

```
##         pop geo_type  geo_id year   age       gender                                                 race_eth geo_id_code
## 1: 609679.7   region    East 2022 0-100 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White           1
## 2: 808129.6   region   South 2022 0-100 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White           4
## 3: 759392.8   region Seattle 2022 0-100 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White           3
## 4: 140497.9   region   North 2022 0-100 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White           2
```

**King County HRAs**


```r
head(get_population(geo_type = c("hra"), group_by = c("geo_id"))[])  
```

```
##         pop geo_type         geo_id year   age       gender                                                 race_eth geo_id_code
## 1: 35903.43      hra    Renton-East 2022 0-100 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White       20000
## 2: 58272.26      hra   Renton-South 2022 0-100 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White       20200
## 3: 26382.31      hra       Fairwood 2022 0-100 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White       11000
## 4: 75916.72      hra       Downtown 2022 0-100 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White        2500
## 5: 11075.33      hra  Vashon Island 2022 0-100 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White       25000
## 6: 36069.47      hra Bellevue-South 2022 0-100 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White        4200
```

**King County Zip codes**


```r
head(get_population(geo_type = c("zip"), group_by = c("geo_id"))[])  
```

```
##           pop geo_type geo_id year   age       gender                                                 race_eth
## 1: 20781.8687      zip  98177 2022 0-100 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White
## 2: 54830.4750      zip  98115 2022 0-100 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White
## 3: 49713.0127      zip  98042 2022 0-100 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White
## 4: 13399.5357      zip  98077 2022 0-100 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White
## 5: 16164.6587      zip  98045 2022 0-100 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White
## 6:   295.7078      zip  98288 2022 0-100 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White
```

**King County Census Tracts**


```r
head(get_population(geo_type = c("tract"), group_by = c("geo_id"), ages = 18, census_vintage = 2020, geo_vintage = 2020)[])  
```

```
##         pop geo_type      geo_id year age       gender                                                 race_eth
## 1: 92.52864    tract 53033030005 2022  18 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White
## 2: 44.59747    tract 53033004301 2022  18 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White
## 3: 56.32746    tract 53033027600 2022  18 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White
## 4: 74.62527    tract 53033028500 2022  18 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White
## 5: 12.55822    tract 53033008003 2022  18 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White
## 6: 47.36526    tract 53033023603 2022  18 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White
```

**King County Census Block Groups**


```r
head(get_population(geo_type = c("blkgrp"), group_by = c("geo_id"), ages = 18,census_vintage = 2020, geo_vintage = 2020)[])  
```

```
##         pop geo_type       geo_id year age       gender                                                 race_eth
## 1: 11.30792   blkgrp 530330222041 2022  18 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White
## 2: 22.45077   blkgrp 530330308013 2022  18 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White
## 3: 18.96018   blkgrp 530330117001 2022  18 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White
## 4: 10.75898   blkgrp 530330231001 2022  18 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White
## 5: 20.78780   blkgrp 530330217011 2022  18 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White
## 6: 17.38798   blkgrp 530330243013 2022  18 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White
```

**King County Census Blocks**


```r
#ages added to make things go faster
head(get_population(geo_type = c("blk"), group_by = c("geo_id"), ages = 18, census_vintage = 2020, geo_vintage = 2020)[])  
```

```
##           pop geo_type          geo_id year age       gender                                                 race_eth
## 1: 0.06800519      blk 530330069003029 2022  18 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White
## 2: 4.30892860      blk 530330323244000 2022  18 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White
## 3: 0.87651670      blk 530330226063003 2022  18 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White
## 4: 0.15797633      blk 530330324011004 2022  18 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White
## 5: 0.02169579      blk 530330059023002 2022  18 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White
## 6: 0.08496733      blk 530330319062019 2022  18 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White
```

### Other simple arguments

**King County multiple years combined**


```r
get_population(years = 2017:2019)[]
```

```
##        pop geo_type      geo_id      year   age       gender                                                 race_eth
## 1: 6590714       kc King County 2017-2019 0-100 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White
```

**King County multiple years stratified**


```r
get_population(years = 2017:2019, 
               group_by = "years")[]
```

```
##        pop geo_type      geo_id year   age       gender                                                 race_eth
## 1: 2159597       kc King County 2017 0-100 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White
## 2: 2234092       kc King County 2019 0-100 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White
## 3: 2197025       kc King County 2018 0-100 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White
```

**King County multiple ages combined**


```r
get_population(ages = 65:70)[]
```

```
##         pop geo_type      geo_id year   age       gender                                                 race_eth
## 1: 127610.9       kc King County 2022 65-70 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White
```

**King County multiple ages stratified**


```r
get_population(ages = 65:70, group_by = "ages")[]
```

```
##         pop geo_type      geo_id year age       gender                                                 race_eth
## 1: 19089.56       kc King County 2022  68 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White
## 2: 23812.15       kc King County 2022  66 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White
## 3: 25375.36       kc King County 2022  65 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White
## 4: 17479.23       kc King County 2022  69 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White
## 5: 19311.89       kc King County 2022  70 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White
## 6: 22542.67       kc King County 2022  67 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White
```

**King County female only**


```r
get_population(genders = "F")[]
```

```
##        pop geo_type      geo_id year   age       gender                                                 race_eth
## 1: 1149276       kc King County 2022 0-100 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White
```

**King County gender stratified**


```r
get_population(group_by = "genders")[]
```

```
##        pop geo_type      geo_id year   age gender                                                 race_eth
## 1: 1168424       kc King County 2022 0-100 Female AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White
## 2: 1149276       kc King County 2022 0-100   Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White
```

**King County AIAN (not Hispanic)**


```r
get_population(races = "aian", race_type = "race_eth")[]
```

```
##      pop geo_type      geo_id year   age       gender race_eth
## 1: 11936       kc King County 2022 0-100 Female, Male     AIAN
```

**King County AIAN (regardless of Hispanic ethnicity)**


```r
get_population(races = "aian", race_type = "race", group_by = 'race')[]
```

```
##         pop geo_type      geo_id year   age       gender race
## 1: 44829.39       kc King County 2022 0-100 Female, Male AIAN
```

**King County stratified by Hispanic as race**


```r
get_population(race_type = "race_eth", group_by = "race_eth")[]
```

```
##        pop geo_type      geo_id year   age       gender      race_eth
## 1: 1229997       kc King County 2022 0-100 Female, Male         White
## 2:   11936       kc King County 2022 0-100 Female, Male          AIAN
## 3:  149831       kc King County 2022 0-100 Female, Male Multiple race
## 4:  158385       kc King County 2022 0-100 Female, Male         Black
## 5:   20712       kc King County 2022 0-100 Female, Male          NHPI
## 6:  259077       kc King County 2022 0-100 Female, Male      Hispanic
## 7:  487762       kc King County 2022 0-100 Female, Male         Asian
```

**King County stratified by race (Hispanic as ethnicity)**


```r
get_population(race_type = "race", group_by = "race")[]
```

```
##           pop geo_type      geo_id year   age       gender          race
## 1: 1396462.08       kc King County 2022 0-100 Female, Male         White
## 2:   44829.39       kc King County 2022 0-100 Female, Male          AIAN
## 3:  181480.94       kc King County 2022 0-100 Female, Male Multiple race
## 4:  172776.59       kc King County 2022 0-100 Female, Male         Black
## 5:   23542.04       kc King County 2022 0-100 Female, Male          NHPI
## 6:  498609.00       kc King County 2022 0-100 Female, Male         Asian
```

### Complex arguments

**King County regions stratified by year and gender**


```r
get_population(geo_type = "region", 
               years = 2017:2019, 
               group_by = c("geo_id", "years", "genders"))[]
```

```
##           pop geo_type  geo_id year   age gender                                                 race_eth geo_id_code
##  1: 364571.19   region Seattle 2019 0-100 Female AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White           3
##  2:  66887.49   region   North 2019 0-100 Female AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White           2
##  3: 343862.72   region Seattle 2017 0-100   Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White           3
##  4: 277311.28   region    East 2017 0-100   Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White           1
##  5: 356830.04   region Seattle 2018 0-100 Female AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White           3
##  6: 397742.98   region   South 2018 0-100 Female AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White           4
##  7: 390957.46   region   South 2017 0-100 Female AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White           4
##  8:  68564.12   region   North 2019 0-100   Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White           2
##  9:  65460.59   region   North 2018 0-100 Female AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White           2
## 10: 284277.76   region    East 2018 0-100 Female AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White           1
## 11: 387960.01   region   South 2017 0-100   Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White           4
## 12: 281510.87   region    East 2018 0-100   Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White           1
## 13: 349335.78   region Seattle 2017 0-100 Female AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White           3
## 14: 398585.93   region   South 2019 0-100   Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White           4
## 15:  64375.15   region   North 2017 0-100 Female AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White           2
## 16: 393787.33   region   South 2018 0-100   Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White           4
## 17:  67145.06   region   North 2018 0-100   Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White           2
## 18: 350270.60   region Seattle 2018 0-100   Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White           3
## 19: 356805.39   region Seattle 2019 0-100   Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White           3
## 20: 403511.62   region   South 2019 0-100 Female AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White           4
## 21: 289201.81   region    East 2019 0-100 Female AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White           1
## 22:  66206.60   region   North 2017 0-100   Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White           2
## 23: 285964.66   region    East 2019 0-100   Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White           1
## 24: 279587.96   region    East 2017 0-100 Female AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White           1
##           pop geo_type  geo_id year   age gender                                                 race_eth geo_id_code
```

**King County regions stratified by year -- Female Hispanic and Asian-NH residents aged 16-25 only -- not rounded**

```r
get_population(ages = 16:25, 
               genders = "F", 
               years = 2017:2019, 
               races = c("hispanic", "asian"), 
               geo_type = "region", 
               race_type = "race_eth", 
               group_by = c("geo_id", "years", "race_eth"), 
               round = F)[]
```

```
##            pop geo_type  geo_id year   age       gender race_eth geo_id_code
##  1:  2646.1429   region    East 2018 16-25 Female, Male Hispanic           1
##  2:  2690.0698   region    East 2019 16-25 Female, Male Hispanic           1
##  3:  7538.5131   region   South 2017 16-25 Female, Male    Asian           4
##  4:  5669.8845   region    East 2017 16-25 Female, Male    Asian           1
##  5:  1175.0308   region   North 2017 16-25 Female, Male    Asian           2
##  6:  6596.0261   region Seattle 2017 16-25 Female, Male Hispanic           3
##  7:  2583.8136   region    East 2017 16-25 Female, Male Hispanic           1
##  8: 12111.4091   region Seattle 2019 16-25 Female, Male    Asian           3
##  9: 11465.2493   region Seattle 2018 16-25 Female, Male    Asian           3
## 10:   937.2483   region   North 2019 16-25 Female, Male Hispanic           2
## 11:  7800.0988   region   South 2018 16-25 Female, Male    Asian           4
## 12:   911.7558   region   North 2018 16-25 Female, Male Hispanic           2
## 13:  5978.1064   region    East 2018 16-25 Female, Male    Asian           1
## 14:  8858.9849   region   South 2017 16-25 Female, Male Hispanic           4
## 15:  8832.6460   region   South 2019 16-25 Female, Male Hispanic           4
## 16:  8040.1918   region   South 2019 16-25 Female, Male    Asian           4
## 17:  8872.2499   region   South 2018 16-25 Female, Male Hispanic           4
## 18:  6344.4286   region    East 2019 16-25 Female, Male    Asian           1
## 19:  1292.0299   region   North 2019 16-25 Female, Male    Asian           2
## 20:  1221.6790   region   North 2018 16-25 Female, Male    Asian           2
## 21:  7321.8006   region Seattle 2019 16-25 Female, Male Hispanic           3
## 22:  6923.3890   region Seattle 2018 16-25 Female, Male Hispanic           3
## 23: 10845.6264   region Seattle 2017 16-25 Female, Male    Asian           3
## 24:   887.0854   region   North 2017 16-25 Female, Male Hispanic           2
##            pop geo_type  geo_id year   age       gender race_eth geo_id_code
```
