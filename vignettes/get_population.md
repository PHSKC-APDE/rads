Introduction to get\_population()
================

This vignette will provide some examples of ways to pull population data
into R from the Azure cloud (thank you Jeremy!). Population numbers are
estimated by the [WA Office of Financial Management (OFM) population
unit](https://ofm.wa.gov/washington-data-research/population-demographics).
OFM produces two sets of estimates: (1) [April 1 official population
estimates](https://ofm.wa.gov/washington-data-research/population-demographics/population-estimates/april-1-official-population-estimates)
for cities and towns and (2) [Small Area Estimates
(SAE)](https://ofm.wa.gov/washington-data-research/population-demographics/population-estimates/small-area-estimates-program)
for smaller geographies. The `get_population()` function pulls the SAE
numbers and should be the same as those in
[CHAT](https://secureaccess.wa.gov/doh/chat/Entry.mvc).

**NOTE!!** To get the most out of this vignette, I highly recommend that
you actually type each and every bit of code into R. Doing so will
almost definitely help you learn the syntax much faster than just
reading the vignette or copying and pasting the code.

## `get_population` arguments

Arguments are the values that we send to a function when it is called.
Generally, typing `args(my_function_of_interest)` will return the
possible arguments including any defaults. For example,

``` r
args(get_population)
```

    ## function (kingco = T, years = c(2019), ages = c(0:120), genders = c("F", 
    ##     "M"), races = c("aian", "asian", "black", "hispanic", "multiple", 
    ##     "nhpi", "white"), race_type = c("race_eth"), geo_type = c("kc"), 
    ##     group_by = NULL, round = T, mykey = "hhsaw") 
    ## NULL

The standard arguments for get\_population() are:

1.  `kingco` &lt;&lt; a logical vector. Do you want the estimates
    limited to King County? The default is TRUE.

2.  `years` &lt;&lt; a numeric vector of the year(s) of interest.
    Currently only provides estimates for 2010+. The default is 2019.

3.  `ages` &lt;&lt; a numeric vector of the age(s) of interest. The
    acceptable range and current default is 0 to 120.

4.  `genders` &lt;&lt; a character vector of the gender(s) of interest.
    The acceptable values are “f”, “female”, “m”, and “male”. The
    default is both female and male.

5.  `races` &lt;&lt; a character vector of the racial/ethnic group(s) of
    interest. The acceptable values are “aian”, “asian”, “black”,
    “hispanic”, “multiple”, “nhpi”, and “white”. The default is all the
    possible values.

6.  `race_type` &lt;&lt; a character vector limited to “race” (Hispanic
    as an ethnicity) or “race\_eth” (Hispanic as a race). The default is
    “race\_eth”.

7.  `geo_type` &lt;&lt; a character vector describing the geographic
    level for which you want population estimates. Possible values are
    “kc”, “seattle, “blk”, “blkgrp”, “hra”, “region”, “tract”, and
    “zip”. Note that all these geo\_types except “zip” are available for
    King, Pierce, and Snohomish counties only. The default geo\_type is
    “kc”.

8.  `group_by` &lt;&lt; a character vector describing the how you would
    like to have the estimates grouped (i.e., stratified). For example,
    if you set the years argument to (2017:2019) and set the `group_by`
    argument to “years”, you would receive estimates for 2017, 2018,
    and 2019. Otherwise you would receive one estimate for 2017
    through 2019. Valid options are limited to: “years”, “ages”,
    “genders”, “race”, “race\_eth”, “fips\_co”, and “geo\_id”. The
    default is NULL, meaning estimates are not grouped / stratified.

9.  `round` &lt;&lt; a logical vector. Do you want to round your
    population estimates to whole numbers? Default is TRUE.

10. `mykey` &lt;&lt; a character vector with the name of the `keyring::`
    key that provides access to the Health and Human Services Analytic
    Workspace (HHSAW). If you have never set your keyring before and or
    do not know what this is refering to, just type
    `keyring::key_set('hhsaw', username = 'ALastname@kingcounty.gov')`
    into your R console (making sure to replace the username). The
    default is ‘hhsaw’.

There is no need to specify any or all of the arguments listed above. As
the following example shows, the default arguments for `get_population`
provide the overall 2019 estimated King County population.

``` r
get_population()[]
```

    ##        pop geo_type      geo_id year   age       gender
    ## 1: 2226300       kc King County 2019 0-120 Female, Male
    ##                                               race_eth
    ## 1: aian, asian, black, hispanic, multiple, nhpi, white

------------------------------------------------------------------------

## Example analyses

***Note 1**: The use of `head()` below is not necessary. It is a
convenience function that displays the first 6 rows of data and was used
to keep the output in this vignette tidy.*

***Note 2**: The use of `[]` after get\_population() is used to print
the output to the console. Typically, you would not print the results
but would save them as an object. E.g.,
`my.pop.est <- get_population()`.*

### Geographic estimates

**WA**

``` r
get_population(kingco = F, geo_type = "zip")[]
```

    ##        pop geo_type geo_id year   age       gender
    ##   1:    21      zip     72 2019 0-120 Female, Male
    ##   2:     2      zip     73 2019 0-120 Female, Male
    ##   3:    46      zip     74 2019 0-120 Female, Male
    ##   4:    21      zip     76 2019 0-120 Female, Male
    ##   5: 35393      zip  98001 2019 0-120 Female, Male
    ##  ---                                              
    ## 573:   115      zip  99363 2019 0-120 Female, Male
    ## 574:   356      zip  99371 2019 0-120 Female, Male
    ## 575:   243      zip  99401 2019 0-120 Female, Male
    ## 576:  1746      zip  99402 2019 0-120 Female, Male
    ## 577: 20604      zip  99403 2019 0-120 Female, Male
    ##                                                 race_eth
    ##   1: aian, asian, black, hispanic, multiple, nhpi, white
    ##   2: aian, asian, black, hispanic, multiple, nhpi, white
    ##   3: aian, asian, black, hispanic, multiple, nhpi, white
    ##   4: aian, asian, black, hispanic, multiple, nhpi, white
    ##   5: aian, asian, black, hispanic, multiple, nhpi, white
    ##  ---                                                    
    ## 573: aian, asian, black, hispanic, multiple, nhpi, white
    ## 574: aian, asian, black, hispanic, multiple, nhpi, white
    ## 575: aian, asian, black, hispanic, multiple, nhpi, white
    ## 576: aian, asian, black, hispanic, multiple, nhpi, white
    ## 577: aian, asian, black, hispanic, multiple, nhpi, white

**King County**

``` r
get_population()[]
```

    ##        pop geo_type      geo_id year   age       gender
    ## 1: 2226300       kc King County 2019 0-120 Female, Male
    ##                                               race_eth
    ## 1: aian, asian, black, hispanic, multiple, nhpi, white

**King County Regions**

``` r
get_population(geo_type = c("region"),
                   group_by = c("geo_id"))[]
```

    ##       pop geo_type  geo_id year   age       gender
    ## 1: 747824   region Seattle 2019 0-120 Female, Male
    ## 2: 769684   region   South 2019 0-120 Female, Male
    ## 3: 133761   region   North 2019 0-120 Female, Male
    ## 4: 575032   region    East 2019 0-120 Female, Male
    ##                                               race_eth
    ## 1: aian, asian, black, hispanic, multiple, nhpi, white
    ## 2: aian, asian, black, hispanic, multiple, nhpi, white
    ## 3: aian, asian, black, hispanic, multiple, nhpi, white
    ## 4: aian, asian, black, hispanic, multiple, nhpi, white

**King County HRAs**

``` r
    head(get_population(geo_type = c("hra"),
                   group_by = c("geo_id"))[])  
```

    ##      pop geo_type            geo_id year   age       gender
    ## 1: 50900      hra     North Seattle 2019 0-120 Female, Male
    ## 2: 48532      hra        NW Seattle 2019 0-120 Female, Male
    ## 3: 76891      hra        NE Seattle 2019 0-120 Female, Male
    ## 4: 62548      hra           Ballard 2019 0-120 Female, Male
    ## 5: 64276      hra Fremont/Greenlake 2019 0-120 Female, Male
    ## 6: 72147      hra       QA/Magnolia 2019 0-120 Female, Male
    ##                                               race_eth
    ## 1: aian, asian, black, hispanic, multiple, nhpi, white
    ## 2: aian, asian, black, hispanic, multiple, nhpi, white
    ## 3: aian, asian, black, hispanic, multiple, nhpi, white
    ## 4: aian, asian, black, hispanic, multiple, nhpi, white
    ## 5: aian, asian, black, hispanic, multiple, nhpi, white
    ## 6: aian, asian, black, hispanic, multiple, nhpi, white

**King County Zip codes**

``` r
    head(get_population(geo_type = c("zip"),
                   group_by = c("geo_id"))[])  
```

    ##      pop geo_type geo_id year   age       gender
    ## 1: 35393      zip  98001 2019 0-120 Female, Male
    ## 2: 35217      zip  98002 2019 0-120 Female, Male
    ## 3: 49449      zip  98003 2019 0-120 Female, Male
    ## 4: 34724      zip  98004 2019 0-120 Female, Male
    ## 5: 19684      zip  98005 2019 0-120 Female, Male
    ## 6: 40273      zip  98006 2019 0-120 Female, Male
    ##                                               race_eth
    ## 1: aian, asian, black, hispanic, multiple, nhpi, white
    ## 2: aian, asian, black, hispanic, multiple, nhpi, white
    ## 3: aian, asian, black, hispanic, multiple, nhpi, white
    ## 4: aian, asian, black, hispanic, multiple, nhpi, white
    ## 5: aian, asian, black, hispanic, multiple, nhpi, white
    ## 6: aian, asian, black, hispanic, multiple, nhpi, white

**King County Census Tracts**

``` r
    head(get_population(geo_type = c("tract"),
                   group_by = c("geo_id"))[])  
```

    ##     pop geo_type      geo_id year   age       gender
    ## 1: 8121    tract 53033000100 2019 0-120 Female, Male
    ## 2: 8475    tract 53033000200 2019 0-120 Female, Male
    ## 3: 2850    tract 53033000300 2019 0-120 Female, Male
    ## 4: 6618    tract 53033000401 2019 0-120 Female, Male
    ## 5: 5367    tract 53033000402 2019 0-120 Female, Male
    ## 6: 3365    tract 53033000500 2019 0-120 Female, Male
    ##                                               race_eth
    ## 1: aian, asian, black, hispanic, multiple, nhpi, white
    ## 2: aian, asian, black, hispanic, multiple, nhpi, white
    ## 3: aian, asian, black, hispanic, multiple, nhpi, white
    ## 4: aian, asian, black, hispanic, multiple, nhpi, white
    ## 5: aian, asian, black, hispanic, multiple, nhpi, white
    ## 6: aian, asian, black, hispanic, multiple, nhpi, white

**King County Census Block Groups**

``` r
    head(get_population(geo_type = c("blkgrp"),
                   group_by = c("geo_id"))[])  
```

    ##     pop geo_type       geo_id year   age       gender
    ## 1: 1338   blkgrp 530330001001 2019 0-120 Female, Male
    ## 2: 1779   blkgrp 530330001002 2019 0-120 Female, Male
    ## 3: 1619   blkgrp 530330001003 2019 0-120 Female, Male
    ## 4: 2176   blkgrp 530330001004 2019 0-120 Female, Male
    ## 5: 1208   blkgrp 530330001005 2019 0-120 Female, Male
    ## 6: 1156   blkgrp 530330002001 2019 0-120 Female, Male
    ##                                               race_eth
    ## 1: aian, asian, black, hispanic, multiple, nhpi, white
    ## 2: aian, asian, black, hispanic, multiple, nhpi, white
    ## 3: aian, asian, black, hispanic, multiple, nhpi, white
    ## 4: aian, asian, black, hispanic, multiple, nhpi, white
    ## 5: aian, asian, black, hispanic, multiple, nhpi, white
    ## 6: aian, asian, black, hispanic, multiple, nhpi, white

**King County Census Blocks**

``` r
    head(get_population(geo_type = c("blk"),
                   group_by = c("geo_id"))[])  
```

    ##    pop geo_type          geo_id year   age       gender
    ## 1:   1      blk 530330001001001 2019 0-120 Female, Male
    ## 2:  70      blk 530330001001002 2019 0-120 Female, Male
    ## 3:  81      blk 530330001001006 2019 0-120 Female, Male
    ## 4:  19      blk 530330001001009 2019 0-120 Female, Male
    ## 5:  25      blk 530330001001010 2019 0-120 Female, Male
    ## 6:  66      blk 530330001001011 2019 0-120 Female, Male
    ##                                               race_eth
    ## 1: aian, asian, black, hispanic, multiple, nhpi, white
    ## 2: aian, asian, black, hispanic, multiple, nhpi, white
    ## 3: aian, asian, black, hispanic, multiple, nhpi, white
    ## 4: aian, asian, black, hispanic, multiple, nhpi, white
    ## 5: aian, asian, black, hispanic, multiple, nhpi, white
    ## 6: aian, asian, black, hispanic, multiple, nhpi, white

### Other simple arguments

**King County multiple years combined**

``` r
get_population(years = 2017:2019)[]
```

    ##        pop geo_type      geo_id      year   age       gender
    ## 1: 6570200       kc King County 2017-2019 0-120 Female, Male
    ##                                               race_eth
    ## 1: aian, asian, black, hispanic, multiple, nhpi, white

**King County multiple years stratified**

``` r
get_population(years = 2017:2019, group_by = "years")[]
```

    ##        pop geo_type      geo_id year   age       gender
    ## 1: 2153700       kc King County 2017 0-120 Female, Male
    ## 2: 2190200       kc King County 2018 0-120 Female, Male
    ## 3: 2226300       kc King County 2019 0-120 Female, Male
    ##                                               race_eth
    ## 1: aian, asian, black, hispanic, multiple, nhpi, white
    ## 2: aian, asian, black, hispanic, multiple, nhpi, white
    ## 3: aian, asian, black, hispanic, multiple, nhpi, white

**King County multiple ages combined**

``` r
get_population(ages = 65:70)[]
```

    ##       pop geo_type      geo_id year   age       gender
    ## 1: 111815       kc King County 2019 65-70 Female, Male
    ##                                               race_eth
    ## 1: aian, asian, black, hispanic, multiple, nhpi, white

**King County multiple ages stratified**

``` r
get_population(ages = 65:70, group_by = "ages")[]
```

    ##      pop geo_type      geo_id year age       gender
    ## 1: 22274       kc King County 2019  65 Female, Male
    ## 2: 21108       kc King County 2019  66 Female, Male
    ## 3: 20075       kc King County 2019  67 Female, Male
    ## 4: 16836       kc King County 2019  68 Female, Male
    ## 5: 15454       kc King County 2019  69 Female, Male
    ## 6: 16068       kc King County 2019  70 Female, Male
    ##                                               race_eth
    ## 1: aian, asian, black, hispanic, multiple, nhpi, white
    ## 2: aian, asian, black, hispanic, multiple, nhpi, white
    ## 3: aian, asian, black, hispanic, multiple, nhpi, white
    ## 4: aian, asian, black, hispanic, multiple, nhpi, white
    ## 5: aian, asian, black, hispanic, multiple, nhpi, white
    ## 6: aian, asian, black, hispanic, multiple, nhpi, white

**King County female only**

``` r
get_population(genders = "F")[]
```

    ##        pop geo_type      geo_id year   age gender
    ## 1: 1112321       kc King County 2019 0-120 Female
    ##                                               race_eth
    ## 1: aian, asian, black, hispanic, multiple, nhpi, white

**King County gender stratified**

``` r
get_population(group_by = "genders")[]
```

    ##        pop geo_type      geo_id year   age gender
    ## 1: 1113979       kc King County 2019 0-120   Male
    ## 2: 1112321       kc King County 2019 0-120 Female
    ##                                               race_eth
    ## 1: aian, asian, black, hispanic, multiple, nhpi, white
    ## 2: aian, asian, black, hispanic, multiple, nhpi, white

**King County AIAN-NH**

``` r
get_population(races = "aian", race_type = "race_eth")[]
```

    ##      pop geo_type      geo_id year   age       gender race_eth
    ## 1: 13617       kc King County 2019 0-120 Female, Male     aian

**King County AIAN regardless of Hispanic ethnicity**

``` r
get_population(races = "aian", race_type = "race")[]
```

    ##      pop geo_type      geo_id year   age       gender race
    ## 1: 22828       kc King County 2019 0-120 Female, Male aian

**King County stratified by Hispanic as race**

``` r
get_population(race_type = "race_eth", group_by = "race_eth")[]
```

    ##        pop geo_type      geo_id year   age       gender      race_eth
    ## 1: 1304696       kc King County 2019 0-120 Female, Male         White
    ## 2:  148063       kc King County 2019 0-120 Female, Male         Black
    ## 3:   13617       kc King County 2019 0-120 Female, Male          AIAN
    ## 4:  104651       kc King County 2019 0-120 Female, Male Multiple race
    ## 5:  228558       kc King County 2019 0-120 Female, Male      Hispanic
    ## 6:  407684       kc King County 2019 0-120 Female, Male         Asian
    ## 7:   19031       kc King County 2019 0-120 Female, Male          NHPI

**King County stratified by race-NH**

``` r
get_population(race_type = "race", group_by = "race")[]
```

    ##        pop geo_type      geo_id year   age       gender          race
    ## 1: 1491379       kc King County 2019 0-120 Female, Male         White
    ## 2:  157093       kc King County 2019 0-120 Female, Male         Black
    ## 3:   22828       kc King County 2019 0-120 Female, Male          AIAN
    ## 4:  121418       kc King County 2019 0-120 Female, Male Multiple race
    ## 5:  413214       kc King County 2019 0-120 Female, Male         Asian
    ## 6:   20368       kc King County 2019 0-120 Female, Male          NHPI

### Complex arguments

**King County regions stratified by year and gender**

``` r
get_population(geo_type = "region", years = 2017:2019, group_by = c("geo_id", "years", "genders"))[]
```

    ##        pop geo_type  geo_id year   age gender
    ##  1: 355807   region Seattle 2017 0-120 Female
    ##  2: 363819   region Seattle 2018 0-120 Female
    ##  3: 371891   region Seattle 2019 0-120 Female
    ##  4: 358410   region Seattle 2017 0-120   Male
    ##  5: 367101   region Seattle 2018 0-120   Male
    ##  6: 375933   region Seattle 2019 0-120   Male
    ##  7: 376381   region   South 2017 0-120   Male
    ##  8: 377281   region   South 2017 0-120 Female
    ##  9: 381035   region   South 2018 0-120   Male
    ## 10: 381608   region   South 2018 0-120 Female
    ## 11: 384809   region   South 2019 0-120   Male
    ## 12: 384875   region   South 2019 0-120 Female
    ## 13:  63264   region   North 2017 0-120   Male
    ## 14:  65730   region   North 2017 0-120 Female
    ## 15:  64237   region   North 2018 0-120   Male
    ## 16:  66616   region   North 2018 0-120 Female
    ## 17:  65646   region   North 2019 0-120   Male
    ## 18:  68115   region   North 2019 0-120 Female
    ## 19: 278340   region    East 2017 0-120   Male
    ## 20: 278486   region    East 2017 0-120 Female
    ## 21: 282887   region    East 2018 0-120   Male
    ## 22: 282896   region    East 2018 0-120 Female
    ## 23: 287591   region    East 2019 0-120   Male
    ## 24: 287441   region    East 2019 0-120 Female
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

**King County regions stratified by year – Female Hispanic and Asian-NH
residents aged 16-25 only**

``` r
get_population(ages = 16:25, genders = "F", years = 2017:2019, races = c("hispanic", "asian"), geo_type = "region", race_type = "race_eth", group_by = c("geo_id", "years", "race_eth"))[]
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
