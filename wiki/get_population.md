---
title: "get_population()"
output:
  github_document: default
  rmarkdown::html_vignette: default
  pdf_document: default
  urlcolor: blue
vignette: |
  %\VignetteEngine{knitr::knitr} %\VignetteIndexEntry{get_population}
---

## Introduction

This vignette will provide some examples of ways to pull population data into R from the Azure cloud (thank you Jeremy!). Population numbers are estimated by the [WA Office of Financial Management (OFM) population unit](https://ofm.wa.gov/washington-data-research/population-demographics). OFM produces two sets of estimates: (1) [April 1 official population estimates](https://ofm.wa.gov/washington-data-research/population-demographics/population-estimates/april-1-official-population-estimates) for cities and towns and (2) [Small Area Estimates (SAE)](https://ofm.wa.gov/washington-data-research/population-demographics/population-estimates/small-area-estimates-program) for smaller geographies. The `get_population()` function pulls the SAE numbers and should be the same as those in [CHAT](https://secureaccess.wa.gov/doh/chat/Entry.mvc). 

**NOTE!!** To get the most out of this vignette, I highly recommend that you actually type each and every bit of code into R. Doing so will almost definitely help you learn the syntax much faster than just reading the vignette or copying and pasting the code.

## `get_population` arguments
Arguments are the values that we send to a function when it is called. Generally, typing `args(my_function_of_interest)` will return the possible arguments including any defaults. For example, 

```r
args(get_population)
```

```
## function (kingco = T, years = c(2020), ages = c(0:100), genders = c("F", 
##     "M"), races = c("aian", "asian", "black", "hispanic", "multiple", 
##     "nhpi", "white"), race_type = c("race_eth"), geo_type = c("kc"), 
##     group_by = NULL, round = T, mykey = "hhsaw") 
## NULL
```
The standard arguments for get_population() are:
 
1) `kingco` << a logical vector. Do you want the estimates limited to King County? The default is TRUE.

2) `years` << a numeric vector of the year(s) of interest. Currently only provides estimates for 2010+. The default is 2020.

3) `ages` << a numeric vector of the age(s) of interest. The acceptable range and current default is 0 to 100. (Note! Age 100 is actually the top coding for ages 100-120.)

4) `genders` << a character vector of the gender(s) of interest. The acceptable values are “f”, “female”, “m”, and “male”. The default is both female and male. 

5) `races` << a character vector of the racial/ethnic group(s) of interest. The acceptable values are "aian", "asian", "black", "hispanic", "multiple", "nhpi", and "white". The default is all the possible values. 

6) `race_type` << a character vector limited to “race” (Hispanic as an ethnicity) or “race_eth” (Hispanic as a race). The default is “race_eth”.

7) `geo_type` << a character vector describing the geographic level for which you want population estimates. Possible values are “kc”, "seattle, “blk”, “blkgrp”, “hra”, “region”, “tract”, and “zip”. Note that all these geo_types except "zip" are available for King, Pierce, and Snohomish counties only. The default geo_type is “kc”.

8) `group_by` << a character vector describing the how you would like to have the estimates grouped (i.e., stratified). For example, if you set the years argument to (2017:2019) and set the `group_by` argument to “years”, you would receive estimates for 2017, 2018, and 2019. Otherwise you would receive one estimate for 2017 through 2019. Valid options are limited to: "years", "ages", "genders", "race", "race_eth", "fips_co", and "geo_id". The default is NULL, meaning estimates are not grouped / stratified.

9) `round` << a logical vector. Do you want to round your population estimates to whole numbers? Default is TRUE. 

10) `mykey` << a character vector with the name of the `keyring::` key that provides access to the Health and Human Services Analytic Workspace (HHSAW). If you have never set your keyring before and or do not know what this is refering to, just type `keyring::key_set('hhsaw', username = 'ALastname@kingcounty.gov')` into your R console (making sure to replace the username). The default is 'hhsaw'. 

There is no need to specify any or all of the arguments listed above. As the following example shows, the default arguments for `get_population` provide the overall 2020 estimated King County population.

```r
get_population()[]
```

```
##        pop geo_type      geo_id year   age       gender
## 1: 2260800       kc King County 2020 0-100 Female, Male
##                                               race_eth
## 1: aian, asian, black, hispanic, multiple, nhpi, white
```

***

## Example analyses
***Note 1**: The use of `head()` below is not necessary. It is a convenience function that displays the first 6 rows of data and was used to keep the output in this vignette tidy.*

***Note 2**: The use of `[]` after get_population() is used to print the output to the console. Typically, you would not print the results but would save them as an object. E.g., `my.pop.est <- get_population()`.*

### Geographic estimates
**WA**

```r
get_population(kingco = F, geo_type = "zip")[]
```

```
##        pop geo_type geo_id year   age       gender
##   1:    21      zip  00072 2020 0-100 Female, Male
##   2:     2      zip  00073 2020 0-100 Female, Male
##   3:    46      zip  00074 2020 0-100 Female, Male
##   4:    21      zip  00076 2020 0-100 Female, Male
##   5: 35718      zip  98001 2020 0-100 Female, Male
##  ---                                              
## 577:   115      zip  99363 2020 0-100 Female, Male
## 578:   364      zip  99371 2020 0-100 Female, Male
## 579:   244      zip  99401 2020 0-100 Female, Male
## 580:  1740      zip  99402 2020 0-100 Female, Male
## 581: 20729      zip  99403 2020 0-100 Female, Male
##                                                 race_eth
##   1: aian, asian, black, hispanic, multiple, nhpi, white
##   2: aian, asian, black, hispanic, multiple, nhpi, white
##   3: aian, asian, black, hispanic, multiple, nhpi, white
##   4: aian, asian, black, hispanic, multiple, nhpi, white
##   5: aian, asian, black, hispanic, multiple, nhpi, white
##  ---                                                    
## 577: aian, asian, black, hispanic, multiple, nhpi, white
## 578: aian, asian, black, hispanic, multiple, nhpi, white
## 579: aian, asian, black, hispanic, multiple, nhpi, white
## 580: aian, asian, black, hispanic, multiple, nhpi, white
## 581: aian, asian, black, hispanic, multiple, nhpi, white
```

**King County**

```r
get_population()[]
```

```
##        pop geo_type      geo_id year   age       gender
## 1: 2260800       kc King County 2020 0-100 Female, Male
##                                               race_eth
## 1: aian, asian, black, hispanic, multiple, nhpi, white
```

**King County Regions**

```r
get_population(geo_type = c("region"),
                   group_by = c("geo_id"))[]
```

```
##       pop geo_type  geo_id year   age       gender
## 1: 761624   region Seattle 2020 0-100 Female, Male
## 2: 776087   region   South 2020 0-100 Female, Male
## 3: 136071   region   North 2020 0-100 Female, Male
## 4: 587018   region    East 2020 0-100 Female, Male
##                                               race_eth
## 1: aian, asian, black, hispanic, multiple, nhpi, white
## 2: aian, asian, black, hispanic, multiple, nhpi, white
## 3: aian, asian, black, hispanic, multiple, nhpi, white
## 4: aian, asian, black, hispanic, multiple, nhpi, white
```

**King County HRAs**

```r
    head(get_population(geo_type = c("hra"),
                   group_by = c("geo_id"))[])  
```

```
##      pop geo_type            geo_id year   age       gender
## 1: 51121      hra     North Seattle 2020 0-100 Female, Male
## 2: 48761      hra        NW Seattle 2020 0-100 Female, Male
## 3: 77617      hra        NE Seattle 2020 0-100 Female, Male
## 4: 63248      hra           Ballard 2020 0-100 Female, Male
## 5: 65646      hra Fremont/Greenlake 2020 0-100 Female, Male
## 6: 73652      hra       QA/Magnolia 2020 0-100 Female, Male
##                                               race_eth
## 1: aian, asian, black, hispanic, multiple, nhpi, white
## 2: aian, asian, black, hispanic, multiple, nhpi, white
## 3: aian, asian, black, hispanic, multiple, nhpi, white
## 4: aian, asian, black, hispanic, multiple, nhpi, white
## 5: aian, asian, black, hispanic, multiple, nhpi, white
## 6: aian, asian, black, hispanic, multiple, nhpi, white
```

**King County Zip codes**

```r
    head(get_population(geo_type = c("zip"),
                   group_by = c("geo_id"))[])  
```

```
##      pop geo_type geo_id year   age       gender
## 1: 35718      zip  98001 2020 0-100 Female, Male
## 2: 35292      zip  98002 2020 0-100 Female, Male
## 3: 49731      zip  98003 2020 0-100 Female, Male
## 4: 36244      zip  98004 2020 0-100 Female, Male
## 5: 20140      zip  98005 2020 0-100 Female, Male
## 6: 40395      zip  98006 2020 0-100 Female, Male
##                                               race_eth
## 1: aian, asian, black, hispanic, multiple, nhpi, white
## 2: aian, asian, black, hispanic, multiple, nhpi, white
## 3: aian, asian, black, hispanic, multiple, nhpi, white
## 4: aian, asian, black, hispanic, multiple, nhpi, white
## 5: aian, asian, black, hispanic, multiple, nhpi, white
## 6: aian, asian, black, hispanic, multiple, nhpi, white
```

**King County Census Tracts**

```r
    head(get_population(geo_type = c("tract"),
                   group_by = c("geo_id"))[])  
```

```
##     pop geo_type      geo_id year   age       gender
## 1: 8157    tract 53033000100 2020 0-100 Female, Male
## 2: 8511    tract 53033000200 2020 0-100 Female, Male
## 3: 2861    tract 53033000300 2020 0-100 Female, Male
## 4: 6613    tract 53033000401 2020 0-100 Female, Male
## 5: 5375    tract 53033000402 2020 0-100 Female, Male
## 6: 3367    tract 53033000500 2020 0-100 Female, Male
##                                               race_eth
## 1: aian, asian, black, hispanic, multiple, nhpi, white
## 2: aian, asian, black, hispanic, multiple, nhpi, white
## 3: aian, asian, black, hispanic, multiple, nhpi, white
## 4: aian, asian, black, hispanic, multiple, nhpi, white
## 5: aian, asian, black, hispanic, multiple, nhpi, white
## 6: aian, asian, black, hispanic, multiple, nhpi, white
```

**King County Census Block Groups**

```r
    head(get_population(geo_type = c("blkgrp"),
                   group_by = c("geo_id"))[])  
```

```
##     pop geo_type       geo_id year   age       gender
## 1: 1341   blkgrp 530330001001 2020 0-100 Female, Male
## 2: 1782   blkgrp 530330001002 2020 0-100 Female, Male
## 3: 1630   blkgrp 530330001003 2020 0-100 Female, Male
## 4: 2172   blkgrp 530330001004 2020 0-100 Female, Male
## 5: 1231   blkgrp 530330001005 2020 0-100 Female, Male
## 6: 1159   blkgrp 530330002001 2020 0-100 Female, Male
##                                               race_eth
## 1: aian, asian, black, hispanic, multiple, nhpi, white
## 2: aian, asian, black, hispanic, multiple, nhpi, white
## 3: aian, asian, black, hispanic, multiple, nhpi, white
## 4: aian, asian, black, hispanic, multiple, nhpi, white
## 5: aian, asian, black, hispanic, multiple, nhpi, white
## 6: aian, asian, black, hispanic, multiple, nhpi, white
```

**King County Census Blocks**

```r
    head(get_population(geo_type = c("blk"),
                   group_by = c("geo_id"))[])  
```

```
##    pop geo_type          geo_id year   age       gender
## 1:   1      blk 530330001001001 2020 0-100 Female, Male
## 2:  70      blk 530330001001002 2020 0-100 Female, Male
## 3:  81      blk 530330001001006 2020 0-100 Female, Male
## 4:  19      blk 530330001001009 2020 0-100 Female, Male
## 5:  25      blk 530330001001010 2020 0-100 Female, Male
## 6:  66      blk 530330001001011 2020 0-100 Female, Male
##                                               race_eth
## 1: aian, asian, black, hispanic, multiple, nhpi, white
## 2: aian, asian, black, hispanic, multiple, nhpi, white
## 3: aian, asian, black, hispanic, multiple, nhpi, white
## 4: aian, asian, black, hispanic, multiple, nhpi, white
## 5: aian, asian, black, hispanic, multiple, nhpi, white
## 6: aian, asian, black, hispanic, multiple, nhpi, white
```

### Other simple arguments
**King County multiple years combined**

```r
get_population(years = 2017:2019)[]
```

```
##        pop geo_type      geo_id      year   age       gender
## 1: 6570200       kc King County 2017-2019 0-100 Female, Male
##                                               race_eth
## 1: aian, asian, black, hispanic, multiple, nhpi, white
```

**King County multiple years stratified**

```r
get_population(years = 2017:2019, group_by = "years")[]
```

```
##        pop geo_type      geo_id year   age       gender
## 1: 2153700       kc King County 2017 0-100 Female, Male
## 2: 2190200       kc King County 2018 0-100 Female, Male
## 3: 2226300       kc King County 2019 0-100 Female, Male
##                                               race_eth
## 1: aian, asian, black, hispanic, multiple, nhpi, white
## 2: aian, asian, black, hispanic, multiple, nhpi, white
## 3: aian, asian, black, hispanic, multiple, nhpi, white
```

**King County multiple ages combined**

```r
get_population(ages = 65:70)[]
```

```
##       pop geo_type      geo_id year   age       gender
## 1: 115111       kc King County 2020 65-70 Female, Male
##                                               race_eth
## 1: aian, asian, black, hispanic, multiple, nhpi, white
```

**King County multiple ages stratified**

```r
get_population(ages = 65:70, group_by = "ages")[]
```

```
##      pop geo_type      geo_id year age       gender
## 1: 22873       kc King County 2020  65 Female, Male
## 2: 21652       kc King County 2020  66 Female, Male
## 3: 20599       kc King County 2020  67 Female, Male
## 4: 17291       kc King County 2020  68 Female, Male
## 5: 15884       kc King County 2020  69 Female, Male
## 6: 16812       kc King County 2020  70 Female, Male
##                                               race_eth
## 1: aian, asian, black, hispanic, multiple, nhpi, white
## 2: aian, asian, black, hispanic, multiple, nhpi, white
## 3: aian, asian, black, hispanic, multiple, nhpi, white
## 4: aian, asian, black, hispanic, multiple, nhpi, white
## 5: aian, asian, black, hispanic, multiple, nhpi, white
## 6: aian, asian, black, hispanic, multiple, nhpi, white
```

**King County female only**

```r
get_population(genders = "F")[]
```

```
##        pop geo_type      geo_id year   age gender
## 1: 1129225       kc King County 2020 0-100 Female
##                                               race_eth
## 1: aian, asian, black, hispanic, multiple, nhpi, white
```

**King County gender stratified**

```r
get_population(group_by = "genders")[]
```

```
##        pop geo_type      geo_id year   age gender
## 1: 1131575       kc King County 2020 0-100   Male
## 2: 1129225       kc King County 2020 0-100 Female
##                                               race_eth
## 1: aian, asian, black, hispanic, multiple, nhpi, white
## 2: aian, asian, black, hispanic, multiple, nhpi, white
```

**King County AIAN-NH**

```r
get_population(races = "aian", race_type = "race_eth")[]
```

```
##      pop geo_type      geo_id year   age       gender race_eth
## 1: 13703       kc King County 2020 0-100 Female, Male     aian
```

**King County AIAN regardless of Hispanic ethnicity**

```r
get_population(races = "aian", race_type = "race")[]
```

```
##      pop geo_type      geo_id year   age       gender race
## 1: 23056       kc King County 2020 0-100 Female, Male aian
```

**King County stratified by Hispanic as race**

```r
get_population(race_type = "race_eth", group_by = "race_eth")[]
```

```
##        pop geo_type      geo_id year   age       gender
## 1: 1303935       kc King County 2020 0-100 Female, Male
## 2:  152999       kc King County 2020 0-100 Female, Male
## 3:   13703       kc King County 2020 0-100 Female, Male
## 4:  107040       kc King County 2020 0-100 Female, Male
## 5:  233923       kc King County 2020 0-100 Female, Male
## 6:  429652       kc King County 2020 0-100 Female, Male
## 7:   19548       kc King County 2020 0-100 Female, Male
##         race_eth
## 1:         White
## 2:         Black
## 3:          AIAN
## 4: Multiple race
## 5:      Hispanic
## 6:         Asian
## 7:          NHPI
```

**King County stratified by race-NH**

```r
get_population(race_type = "race", group_by = "race")[]
```

```
##        pop geo_type      geo_id year   age       gender
## 1: 1495057       kc King County 2020 0-100 Female, Male
## 2:  162296       kc King County 2020 0-100 Female, Male
## 3:   23056       kc King County 2020 0-100 Female, Male
## 4:  124087       kc King County 2020 0-100 Female, Male
## 5:  435386       kc King County 2020 0-100 Female, Male
## 6:   20918       kc King County 2020 0-100 Female, Male
##             race
## 1:         White
## 2:         Black
## 3:          AIAN
## 4: Multiple race
## 5:         Asian
## 6:          NHPI
```

### Complex arguments
**King County regions stratified by year and gender**

```r
get_population(geo_type = "region", years = 2017:2019, group_by = c("geo_id", "years", "genders"))[]
```

```
##        pop geo_type  geo_id year   age gender
##  1: 355807   region Seattle 2017 0-100 Female
##  2: 363819   region Seattle 2018 0-100 Female
##  3: 371891   region Seattle 2019 0-100 Female
##  4: 358410   region Seattle 2017 0-100   Male
##  5: 367101   region Seattle 2018 0-100   Male
##  6: 375933   region Seattle 2019 0-100   Male
##  7: 376381   region   South 2017 0-100   Male
##  8: 377281   region   South 2017 0-100 Female
##  9: 381035   region   South 2018 0-100   Male
## 10: 381608   region   South 2018 0-100 Female
## 11: 384809   region   South 2019 0-100   Male
## 12: 384875   region   South 2019 0-100 Female
## 13:  63264   region   North 2017 0-100   Male
## 14:  65730   region   North 2017 0-100 Female
## 15:  64237   region   North 2018 0-100   Male
## 16:  66616   region   North 2018 0-100 Female
## 17:  65646   region   North 2019 0-100   Male
## 18:  68115   region   North 2019 0-100 Female
## 19: 278340   region    East 2017 0-100   Male
## 20: 278486   region    East 2017 0-100 Female
## 21: 282887   region    East 2018 0-100   Male
## 22: 282896   region    East 2018 0-100 Female
## 23: 287591   region    East 2019 0-100   Male
## 24: 287441   region    East 2019 0-100 Female
##        pop geo_type  geo_id year   age gender
##                                                race_eth
##  1: aian, asian, black, hispanic, multiple, nhpi, white
##  2: aian, asian, black, hispanic, multiple, nhpi, white
##  3: aian, asian, black, hispanic, multiple, nhpi, white
##  4: aian, asian, black, hispanic, multiple, nhpi, white
##  5: aian, asian, black, hispanic, multiple, nhpi, white
##  6: aian, asian, black, hispanic, multiple, nhpi, white
##  7: aian, asian, black, hispanic, multiple, nhpi, white
##  8: aian, asian, black, hispanic, multiple, nhpi, white
##  9: aian, asian, black, hispanic, multiple, nhpi, white
## 10: aian, asian, black, hispanic, multiple, nhpi, white
## 11: aian, asian, black, hispanic, multiple, nhpi, white
## 12: aian, asian, black, hispanic, multiple, nhpi, white
## 13: aian, asian, black, hispanic, multiple, nhpi, white
## 14: aian, asian, black, hispanic, multiple, nhpi, white
## 15: aian, asian, black, hispanic, multiple, nhpi, white
## 16: aian, asian, black, hispanic, multiple, nhpi, white
## 17: aian, asian, black, hispanic, multiple, nhpi, white
## 18: aian, asian, black, hispanic, multiple, nhpi, white
## 19: aian, asian, black, hispanic, multiple, nhpi, white
## 20: aian, asian, black, hispanic, multiple, nhpi, white
## 21: aian, asian, black, hispanic, multiple, nhpi, white
## 22: aian, asian, black, hispanic, multiple, nhpi, white
## 23: aian, asian, black, hispanic, multiple, nhpi, white
## 24: aian, asian, black, hispanic, multiple, nhpi, white
##                                                race_eth
```

**King County regions stratified by year -- Female Hispanic and Asian-NH residents aged 16-25 only**

```r
get_population(ages = 16:25, genders = "F", years = 2017:2019, races = c("hispanic", "asian"), geo_type = "region", race_type = "race_eth", group_by = c("geo_id", "years", "race_eth"))[]
```

```
##       pop geo_type  geo_id year   age gender race_eth
##  1:  5167   region Seattle 2017 16-25 Female Hispanic
##  2: 12051   region Seattle 2017 16-25 Female    Asian
##  3:  5304   region Seattle 2018 16-25 Female Hispanic
##  4: 12853   region Seattle 2018 16-25 Female    Asian
##  5:  5462   region Seattle 2019 16-25 Female Hispanic
##  6: 13681   region Seattle 2019 16-25 Female    Asian
##  7:  9086   region   South 2017 16-25 Female Hispanic
##  8:  9135   region   South 2018 16-25 Female Hispanic
##  9:  9167   region   South 2019 16-25 Female Hispanic
## 10:  1344   region   North 2017 16-25 Female    Asian
## 11:  1416   region   North 2018 16-25 Female    Asian
## 12:  1504   region   North 2019 16-25 Female    Asian
## 13:   813   region   North 2017 16-25 Female Hispanic
## 14:   835   region   North 2018 16-25 Female Hispanic
## 15:   857   region   North 2019 16-25 Female Hispanic
## 16:  2464   region    East 2017 16-25 Female Hispanic
## 17:  5345   region    East 2017 16-25 Female    Asian
## 18:  2507   region    East 2018 16-25 Female Hispanic
## 19:  5602   region    East 2018 16-25 Female    Asian
## 20:  2550   region    East 2019 16-25 Female Hispanic
## 21:  5913   region    East 2019 16-25 Female    Asian
## 22:  7803   region   South 2017 16-25 Female    Asian
## 23:  8125   region   South 2018 16-25 Female    Asian
## 24:  8451   region   South 2019 16-25 Female    Asian
##       pop geo_type  geo_id year   age gender race_eth
```
