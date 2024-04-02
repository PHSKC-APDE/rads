


## Introduction

This vignette will provide some examples of ways to pull population data into R from the Azure cloud (thank you Jeremy!).

As of 02/2023, there are two sets of population estimates.

The first, [frankenpop/Population Interim Estimates (PIE)](https://github.com/PHSKC-APDE/frankenpop) were created by APDE. They are based on 2020 geographies for 2000 - 2022+.

The second (and older/outdated ones) are Population numbers estimated by the [WA Office of Financial Management (OFM) population unit](https://ofm.wa.gov/washington-data-research/population-demographics) for 2000-2020. OFM produces two sets of estimates: (1) [April 1 official population estimates](https://ofm.wa.gov/washington-data-research/population-demographics/population-estimates/april-1-official-population-estimates) for cities and towns and (2) [Small Area Estimates (SAE)](https://ofm.wa.gov/washington-data-research/population-demographics/population-estimates/small-area-estimates-program) for smaller geographies. The `get_population()` function pulls the SAE numbers and, when `round = T`, should be the same as those in [CHAT](https://secureaccess.wa.gov/doh/chat/Entry.mvc).

**NOTE!!** To get the most out of this vignette, we highly recommend that you actually type each and every bit of code into R. Doing so will almost definitely help you learn the syntax much faster than just reading the vignette or copying and pasting the code.

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
##     geo_vintage = 2020, schema = "ref", table_prefix = "pop_geo_", 
##     return_query = FALSE) 
## NULL
```

The standard arguments for `get_population()` are found in the its help file (`?get_population`), and summarized here for your convenience:

1)  `kingco` \<\< Logical vector of length 1. Identifies whether you want population estimates limited to King County. Only impacts results for geo_type in c('blk', blkgrp', 'lgd', 'scd', 'tract', 'zip'). Default == TRUE.

2)  `years` \<\< Numeric vector. Identifies which year(s) of data should be pulled. Default == 2022.

3)  `ages` \<\< Numeric vector. Identifies which age(s) should be pulled. Default == c(0:100), with 100 being the top coded value for 100:120.

4)  `genders` \<\< Character vector of length 1 or 2. Identifies gender(s) should be pulled. The acceptable values are 'f', 'female', 'm', and 'male'. Default == c('f', 'm').

5)  `races` \<\< Character vector of length 1 to 7. Identifies which race(s) or ethnicity should be pulled. The acceptable values are "aian", "asian", "black", "hispanic", "multiple", "nhpi", and "white". Default == all the possible values.

6)  `race_type` \<\< Character vector of length 1. Identifies whether to pull race data with Hispanic as an ethnicity ("race") or Hispanic as a race ("race_eth"). Default == c("race_eth").

7)  `geo_type` \<\< Character vector of length 1. Identifies the geographic level for which you want population estimates. The acceptable values are: 'blk', 'blkgrp', 'county', 'hra', 'kc', 'lgd' (WA State legislative districts), 'region', 'seattle', 'scd' (school districts), 'tract', and 'zip'. Default == "kc".

8)  `group_by` \<\< Character vector of length 0 to 7. Identifies how you would like the data 'grouped' (i.e., stratified). Valid options are limited to: "years", "ages", "genders", "race", "race_eth", "fips_co", and "geo_id". Default == NULL, i.e., estimates are only grouped / aggregated by geography (e.g. geo_id is always included).

9)  `round` \<\< Logical vector of length 1. Identifies whether or not population estimates should be returned as whole numbers. Default == FALSE.

10) `mykey` \<\< a character vector with the name of the `keyring::` key that provides access to the Health and Human Services Analytic Workspace (HHSAW). If you have never set your keyring before and or do not know what this is referring to, just type `keyring::key_set('hhsaw', username = 'ALastname@kingcounty.gov')` into your R console (making sure to replace the username). The default is 'hhsaw'. Note that it can also take the name of a live database connection.

11) census_vintage \<\< Either 2010 or 2020. Specifies the anchor census of the desired estimates. Default is 2020

12) geo_vintage \<\< Either 2010 or 2020. Specifies the anchor census for geographies. For example, 2020 will return geographies based on 2020 blocks. Default is 2020

13) schema \<\< Unless you are a power user, don't mess with this

14) table_prefix \<\< Unless you are a power user, don't mess with this

15) return_query \<\< logical. Rather than returning results, the query/queries used to fetch the results are provided

There is no need to specify any or all of the arguments listed above. As the following example shows, the default arguments for `get_population` provide the overall most recent year's estimated King County population.


```r
get_population()[]
```

```
##        pop geo_type      geo_id   year    age       gender                                                 race_eth
##      <num>   <char>      <char> <char> <char>       <char>                                                   <char>
## 1: 2317700       kc King County   2022  0-100 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White
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
##        pop geo_type           geo_id   year    age       gender                                                 race_eth geo_id_code
##      <num>   <char>           <char> <char> <char>       <char>                                                   <char>       <num>
## 1: 7864400       wa Washington State   2022  0-100 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White          53
```

**King County**


```r
get_population(round = T)[]
```

```
##        pop geo_type      geo_id   year    age       gender                                                 race_eth
##      <num>   <char>      <char> <char> <char>       <char>                                                   <char>
## 1: 2317700       kc King County   2022  0-100 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White
```

**King County Regions**


```r
get_population(geo_type = c("region"),
               group_by = c("geo_id"),
               round = TRUE)[]
```

```
##       pop geo_type  geo_id   year    age       gender                                                 race_eth geo_id_code
##     <num>   <char>  <char> <char> <char>       <char>                                                   <char>      <char>
## 1: 604181   region    East   2022  0-100 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White           1
## 2: 813899   region   South   2022  0-100 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White           4
## 3: 758964   region Seattle   2022  0-100 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White           3
## 4: 140656   region   North   2022  0-100 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White           2
```

**King County Regions with `round=FALSE`**

Turn off rounding to get the exact (fractional) number of people estimated.


```r
rads::get_population(geo_type = 'region', 
                     round = F)[]
```

```
##         pop geo_type  geo_id   year    age       gender                                                 race_eth geo_id_code
##       <num>   <char>  <char> <char> <char>       <char>                                                   <char>      <char>
## 1: 604181.4   region    East   2022  0-100 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White           1
## 2: 813898.8   region   South   2022  0-100 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White           4
## 3: 758963.9   region Seattle   2022  0-100 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White           3
## 4: 140656.0   region   North   2022  0-100 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White           2
```

**King County HRAs**


```r
head(get_population(geo_type = c("hra"), group_by = c("geo_id"))[])  
```

```
##         pop geo_type                                                  geo_id   year    age       gender                                                 race_eth geo_id_code
##       <num>   <char>                                                  <char> <char> <char>       <char>                                                   <char>      <char>
## 1: 40643.61      hra                                    Bellevue - Northeast   2022  0-100 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White          40
## 2: 40730.46      hra                                      Bellevue - Central   2022  0-100 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White          36
## 3: 34799.45      hra Seattle - South Beacon Hill, Georgetown, and South Park   2022  0-100 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White          30
## 4: 21183.65      hra                                               Covington   2022  0-100 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White          11
## 5: 60406.00      hra                                               Shoreline   2022  0-100 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White          61
## 6: 50169.19      hra                        Bear Creek and Greater Sammamish   2022  0-100 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White          54
```

**King County Zip codes**


```r
head(get_population(geo_type = c("zip"), group_by = c("geo_id"))[])  
```

```
##         pop geo_type geo_id   year    age       gender                                                 race_eth
##       <num>   <char> <char> <char> <char>       <char>                                                   <char>
## 1: 20783.07      zip  98177   2022  0-100 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White
## 2: 54830.36      zip  98115   2022  0-100 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White
## 3: 49718.95      zip  98042   2022  0-100 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White
## 4: 13398.45      zip  98077   2022  0-100 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White
## 5: 16440.64      zip  98065   2022  0-100 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White
## 6: 16165.71      zip  98045   2022  0-100 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White
```

**King County Census Tracts**


```r
head(get_population(geo_type = c("tract"), group_by = c("geo_id"), ages = 18, census_vintage = 2020, geo_vintage = 2020)[])  
```

```
##         pop geo_type      geo_id   year    age       gender                                                 race_eth
##       <num>   <char>      <char> <char> <char>       <char>                                                   <char>
## 1: 41.83393    tract 53033004301   2022     18 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White
## 2: 48.44064    tract 53033001300   2022     18 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White
## 3: 51.54287    tract 53033023603   2022     18 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White
## 4: 27.55609    tract 53033029207   2022     18 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White
## 5: 96.24008    tract 53033030005   2022     18 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White
## 6: 61.28889    tract 53033025806   2022     18 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White
```

**King County Census Block Groups**


```r
head(get_population(geo_type = c("blkgrp"), group_by = c("geo_id"), ages = 18,census_vintage = 2020, geo_vintage = 2020)[])  
```

```
##          pop geo_type       geo_id   year    age       gender                                                 race_eth
##        <num>   <char>       <char> <char> <char>       <char>                                                   <char>
## 1:  8.605506   blkgrp 530330237011   2022     18 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White
## 2: 12.283464   blkgrp 530330250073   2022     18 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White
## 3: 13.386648   blkgrp 530330252012   2022     18 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White
## 4: 19.333246   blkgrp 530330309024   2022     18 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White
## 5: 28.158430   blkgrp 530330323113   2022     18 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White
## 6: 19.494452   blkgrp 530330078001   2022     18 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White
```

**King County Census Blocks**


```r
#ages added to make things go faster
head(get_population(geo_type = c("blk"), group_by = c("geo_id"), ages = 18, census_vintage = 2020, geo_vintage = 2020)[])  
```

```
##            pop geo_type          geo_id   year    age       gender                                                 race_eth
##          <num>   <char>          <char> <char> <char>       <char>                                                   <char>
## 1:  0.76053594      blk 530330254023018   2022     18 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White
## 2:  0.10637681      blk 530330322212009   2022     18 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White
## 3:  0.02683813      blk 530330323111021   2022     18 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White
## 4: 21.59007906      blk 530330294083001   2022     18 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White
## 5:  1.11197142      blk 530330317081012   2022     18 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White
## 6:  0.08375210      blk 530330098011012   2022     18 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White
```

### Other simple arguments

**King County multiple years combined**


```r
get_population(years = 2017:2019)[]
```

```
##        pop geo_type      geo_id      year    age       gender                                                 race_eth
##      <num>   <char>      <char>    <char> <char>       <char>                                                   <char>
## 1: 6593269       kc King County 2017-2019  0-100 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White
```

**King County multiple years stratified**


```r
get_population(years = 2017:2019, 
               group_by = "years")[]
```

```
##        pop geo_type      geo_id   year    age       gender                                                 race_eth
##      <num>   <char>      <char> <char> <char>       <char>                                                   <char>
## 1: 2160624       kc King County   2017  0-100 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White
## 2: 2234581       kc King County   2019  0-100 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White
## 3: 2198064       kc King County   2018  0-100 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White
```

**King County multiple ages combined**


```r
get_population(ages = 65:70)[]
```

```
##         pop geo_type      geo_id   year    age       gender                                                 race_eth
##       <num>   <char>      <char> <char> <char>       <char>                                                   <char>
## 1: 127562.7       kc King County   2022  65-70 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White
```

**King County multiple ages stratified**


```r
get_population(ages = 65:70, group_by = "ages")[]
```

```
##         pop geo_type      geo_id   year   age       gender                                                 race_eth
##       <num>   <char>      <char> <char> <int>       <char>                                                   <char>
## 1: 19021.57       kc King County   2022    68 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White
## 2: 23769.82       kc King County   2022    66 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White
## 3: 25441.42       kc King County   2022    65 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White
## 4: 17518.13       kc King County   2022    69 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White
## 5: 19263.77       kc King County   2022    70 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White
## 6: 22548.02       kc King County   2022    67 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White
```

**King County female only**


```r
get_population(genders = "F")[]
```

```
##        pop geo_type      geo_id   year    age gender                                                 race_eth
##      <num>   <char>      <char> <char> <char> <char>                                                   <char>
## 1: 1149276       kc King County   2022  0-100 Female AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White
```

**King County gender stratified**


```r
get_population(group_by = "genders")[]
```

```
##        pop geo_type      geo_id   year    age gender                                                 race_eth
##      <num>   <char>      <char> <char> <char> <char>                                                   <char>
## 1: 1168424       kc King County   2022  0-100   Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White
## 2: 1149276       kc King County   2022  0-100 Female AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White
```

**King County AIAN (not Hispanic)**


```r
get_population(races = "aian", race_type = "race_eth")[]
```

```
##      pop geo_type      geo_id   year    age       gender race_eth
##    <num>   <char>      <char> <char> <char>       <char>   <char>
## 1: 11936       kc King County   2022  0-100 Female, Male     AIAN
```

**King County AIAN (regardless of Hispanic ethnicity)**


```r
get_population(races = "aian", race_type = "race", group_by = 'race')[]
```

```
##      pop geo_type      geo_id   year    age       gender   race
##    <num>   <char>      <char> <char> <char>       <char> <fctr>
## 1: 26637       kc King County   2022  0-100 Female, Male   AIAN
```

**King County stratified by Hispanic as race**


```r
get_population(race_type = "race_eth", group_by = "race_eth")[]
```

```
##        pop geo_type      geo_id   year    age       gender      race_eth
##      <num>   <char>      <char> <char> <char>       <char>        <fctr>
## 1: 1229997       kc King County   2022  0-100 Female, Male         White
## 2:   11936       kc King County   2022  0-100 Female, Male          AIAN
## 3:  149831       kc King County   2022  0-100 Female, Male Multiple race
## 4:  158385       kc King County   2022  0-100 Female, Male         Black
## 5:   20712       kc King County   2022  0-100 Female, Male          NHPI
## 6:  259077       kc King County   2022  0-100 Female, Male      Hispanic
## 7:  487762       kc King County   2022  0-100 Female, Male         Asian
```

**King County stratified by race (Hispanic as ethnicity)**


```r
get_population(race_type = "race", group_by = "race")[]
```

```
##        pop geo_type      geo_id   year    age       gender          race
##      <num>   <char>      <char> <char> <char>       <char>        <fctr>
## 1: 1383396       kc King County   2022  0-100 Female, Male         White
## 2:   26637       kc King County   2022  0-100 Female, Male          AIAN
## 3:  221561       kc King County   2022  0-100 Female, Male Multiple race
## 4:  168952       kc King County   2022  0-100 Female, Male         Black
## 5:   22518       kc King County   2022  0-100 Female, Male          NHPI
## 6:  494636       kc King County   2022  0-100 Female, Male         Asian
```

### Complex arguments

**King County regions stratified by year and gender**


```r
reg_yr_gen <- get_population(geo_type = "region",
                            years = 2017:2019, 
                            group_by = c("geo_id", "years", "genders"))
reg_yr_gen <- reg_yr_gen[, .(region = geo_id, year, gender, pop)]
print(setorder(reg_yr_gen, region, year, gender)[1:12])
```

```
##     region   year gender       pop
##     <char> <char> <char>     <num>
##  1:   East   2017 Female 274982.55
##  2:   East   2017   Male 277288.06
##  3:   East   2018 Female 279199.69
##  4:   East   2018   Male 282024.51
##  5:   East   2019 Female 283640.64
##  6:   East   2019   Male 286928.22
##  7:  North   2017 Female  66448.40
##  8:  North   2017   Male  64750.70
##  9:  North   2018 Female  67395.89
## 10:  North   2018   Male  65876.75
## 11:  North   2019 Female  68811.32
## 12:  North   2019   Male  67314.55
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
               round = F)[1:12]
```

```
##            pop geo_type  geo_id   year    age gender race_eth geo_id_code
##          <num>   <char>  <char> <char> <char> <char>   <fctr>      <char>
##  1:  2885.5149   region    East   2019  16-25 Female Hispanic           1
##  2:  2823.1282   region    East   2018  16-25 Female Hispanic           1
##  3:  7538.3639   region   South   2017  16-25 Female    Asian           4
##  4:  1172.9659   region   North   2017  16-25 Female    Asian           2
##  5:  5648.5303   region    East   2017  16-25 Female    Asian           1
##  6:  2738.7459   region    East   2017  16-25 Female Hispanic           1
##  7:  6785.1092   region Seattle   2017  16-25 Female Hispanic           3
##  8: 11430.2769   region Seattle   2018  16-25 Female    Asian           3
##  9: 12072.5280   region Seattle   2019  16-25 Female    Asian           3
## 10:  1018.6905   region   North   2019  16-25 Female Hispanic           2
## 11:  7797.9899   region   South   2018  16-25 Female    Asian           4
## 12:   987.4472   region   North   2018  16-25 Female Hispanic           2
```

## 'hispanic' as a `group_by` value

Sometimes a user might want to access population data by Hispanic ethnicity. To get population values by race X ethnicity, users should include 'hispanic' in the `group_by` argument. This option only works in conjunction when `race_type = 'race_eth'`. Several combinations (e.g. adding 'hispanic' to the `races` argument) will not work and will throw some (hopefully) informative errors. Other options (as demonstrated above) will continue to work.


**King County regions stratified by Hispanic/Non-Hispanic**


```r
# pull in data stratified by race/eth and region
  reg_hisp_nonhisp <- get_population(geo_type = 'region', group_by = 'hispanic')
  
  # print select columns 
  reg_hisp_nonhisp <- reg_hisp_nonhisp[, .(region = geo_id, hispanic, pop)]
  print(setorder(reg_hisp_nonhisp, region, hispanic))
```

```
##     region     hispanic       pop
##     <char>       <char>     <num>
## 1:    East     Hispanic  45644.87
## 2:    East Not Hispanic 558536.48
## 3:   North     Hispanic  14505.45
## 4:   North Not Hispanic 126150.56
## 5: Seattle     Hispanic  82759.69
## 6: Seattle Not Hispanic 676204.23
## 7:   South     Hispanic 116166.99
## 8:   South Not Hispanic 697731.77
```

**Return all race x Hispanic ethnicity combinations**


```r
race_x_eth <- get_population(race_type = 'race_eth', 
                             group_by = c('race_eth', 'hispanic'))
race_x_eth <- race_x_eth[, .(year, race_eth, hispanic, pop)]
print(setorder(race_x_eth, race_eth, hispanic))
```

```
##       year      race_eth     hispanic     pop
##     <char>        <fctr>       <char>   <num>
##  1:   2022          AIAN     Hispanic   14701
##  2:   2022          AIAN Not Hispanic   11936
##  3:   2022         Asian     Hispanic    6874
##  4:   2022         Asian Not Hispanic  487762
##  5:   2022         Black     Hispanic   10567
##  6:   2022         Black Not Hispanic  158385
##  7:   2022 Multiple race     Hispanic   71730
##  8:   2022 Multiple race Not Hispanic  149831
##  9:   2022          NHPI     Hispanic    1806
## 10:   2022          NHPI Not Hispanic   20712
## 11:   2022         White     Hispanic  153399
## 12:   2022         White Not Hispanic 1229997
```

**Return population of White residents by Hispanic ethnicity**


```r
race_x_eth <- get_population(race_type = 'race_eth', 
                             races = 'white', 
                             group_by = c('race_eth', 'hispanic'))
race_x_eth <- race_x_eth[, .(year, race_eth, hispanic, pop)]
print(setorder(race_x_eth, race_eth, hispanic))
```

```
##      year race_eth     hispanic     pop
##    <char>   <fctr>       <char>   <num>
## 1:   2022    White     Hispanic  153399
## 2:   2022    White Not Hispanic 1229997
```

## Using get_population with a pre-existing connection to hhsaw

Some users may not need/want to rely on `get_population`'s auto-connection to HHSAW via keyring. Users can instead pass an existing database connection through the mykey argument. The example below still uses keyring (since most `get_population` users are on the PH domain), but it can be replaced by `ActiveDirectoryIntegrated` type authentications to HHSAW for the KC lucky ducks.


```r
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
```

```
## [1] TRUE
```

-- *Updated by dcasey, 2024-04-01*
