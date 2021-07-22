Exploring Birth Data with RADS
================

## Introduction

This vignette will provide some examples of ways to explore and analyze
birth data using [`rads`](https://github.com/PHSKC-APDE/rads), APDE’s ‘R
Automated Data System’. [`rads`](https://github.com/PHSKC-APDE/rads) is
a suite custom tools and utilities that can be used to analyze survey
and record level data for [CHI](https://www.kingcounty.gov/chi), [data
requests](https://kingcounty.gov/depts/health/data/data-request-service.aspx),
and other APDE projects. Spending the time getting familiar with
[`rads`](https://github.com/PHSKC-APDE/rads) now should save you many
hours of coding if you will need to calculate standard metrics such as
means and rates in the future.

**NOTE!!** To get the most out of this vignette, I highly recommend that
you actually type each and every bit of code into R. Doing so will
almost definitely help you learn the syntax much faster than just
reading the vignette or copying and pasting the code.

## Getting data

Begin by loading the [`rads`](https://github.com/PHSKC-APDE/rads)
package/library by typing the following in R:

``` r
library(rads)
```

The analytic ready birth data is stored in on KCIT SQL Server 50
(`[PH_APDEStore].[final].[bir_wa]`). The `get_data_birth()` function
will allow you to pull data from the SQL server with minimal fuss. To
see the possible arguments that you can pass, use the `args()` function
by typing the following:

``` r
args(get_data_birth)
```

    ## function (cols = NA, year = c(2017), kingco = T) 
    ## NULL

You can see that `get_data_birth` takes three possible arguments:

1.  `cols` &lt;- a vector of the specific columns that you want to load
    into memory, e.g., `c("chi_year", "chi_geo_regions_4")`. If it is
    not specified, the default is `NA`, which will pull all available
    columns.
    -   In the future, standardized documentation, including a data
        dictionary, will be available to help you identify the birth
        columns of interest. For now, manually [open the data
        dictionary](https://github.com/PHSKC-APDE/DOHdata/blob/master/ETL/birth/ref/ref_bir_user_dictionary_final.csv)
        to see what is available.
2.  `year` &lt;- a vector of the year or years of data that you want to
    load, e.g., c(2011, 2015:2018). Note that the default is to load
    2017 data only.
3.  `kingco` &lt;- a logical argument (i.e., `T` or `F` only, without
    quotes) denoting whether or not the data should be limited to King
    County. The default is King County only.

Let’s try the function to see how it works by loading the year and King
County columns for WA State in 2018:

``` r
birth <- get_data_birth(cols = c("chi_year", "chi_geo_kc"), year = c(2018), kingco = F)
```

We can confirm the `birth` object is in our environment by typing `ls()`

``` r
ls() 
```

    ## [1] "birth"

To identify the class of the `birth` object, we can type `class(birth)`

``` r
class(birth) 
```

    ## [1] "data.table" "data.frame"

The `dim()` function tells us the dimensions of the `birth` table. In
this case, it has 87249 rows and 2 columns

``` r
dim(birth) 
```

    ## [1] 87249     2

Use the `head()` command to take a peak at the first 6 lines of the
`birth` table

``` r
head(birth) 
```

    ##    chi_geo_kc chi_year
    ## 1:          0     2018
    ## 2:          0     2018
    ## 3:          0     2018
    ## 4:          0     2018
    ## 5:          0     2018
    ## 6:          0     2018

## Introducing the `calc` function

`calc` is the analytic workhorse of `rads`. It provides a standardized
method for obtaining most of what we usually want to calculate: means,
medians, counts, confidence intervals, standard errors, relative
standard errors (RSE), numerators, denominators, the number missing, and
the proportion missing. In this case, using the `args()` function will
not be very helpful:

``` r
args(calc)
```

    ## function (ph.data, ...) 
    ## NULL

This is because calc is a front end for two engines:
[`calc.data.frame`](https://github.com/PHSKC-APDE/rads/blob/master/R/calc.data.frame.R)
for record level data &
[`calc.tbl_svy`](https://github.com/PHSKC-APDE/rads/blob/master/R/calc.tbl_svy.R)
for survey data. The good news is that you can ignore this and use the
following standard set of arguments:

1.  `ph.data` &lt;- the name of the data.table or survey object that you
    want to analyze. In our case, `birth`.
2.  `what` &lt;- a character vector of the variables for which you want
    to calculate the metrics. E.g., `what = c("preterm")`
3.  `...` &lt;- think of this as a “where” statement, i.e., a filter or
    subsetting of data. E.g., `chi_geo_zip5 %in% c(98001:98010)`.
    **NOTE** do not type `...` !
4.  `by` &lt;- a character vector of the variables that you want to
    computer the `what` by, i.e., the cross-tab variable(s). E.g.,
    `by = c("mother_birthplace_foreign")`
5.  `metrics` &lt;- a character vector of the metrics that you want
    returned. E.g., `metrics = c("mean", "rse")`. You can see a complete
    list of available metrics by typing `metrics()`
6.  `per` &lt;- an integer, which is the denominator when `rate` is
    selected as the metric. Metrics will be multiplied by this value.
    E.g., `per = 1000`
7.  `win` &lt;- an integer, which is the number of consecutive units of
    time (e.g., years, months, etc.) over which the metrics will be
    calculated, i.e., the ‘window’ for a rolling average, sum, etc. E.g.
    `win = 5` will perform calculations over every 5 time unit window.
8.  `time_var` &lt;- a character, which is the name of the time variable
    in the dataset. Used in combination with the `win` argument to
    generate time windowed calculations.
9.  `proportion` &lt;- a logical (i.e., `T` or `F`) flag determining
    whether the metrics should be calculated as a proportion. Currently
    only relevant for survey data.
10. `verbose` &lt;- a logical used to toggle on/off printed warnings

There is no need to specify all of the arguments listed above. As the
following example shows, the `calc` function simply needs a specified
dataset (i.e., `pha.data`) and at least one `what` variable to return an
intelliglbe result. In this case it will provide the proportion of all
WA State births that took place in King County in 2018.

``` r
result.1 <- calc(birth, what = c("chi_geo_kc") )
head(result.1)
```

    ##      variable level    mean numerator denominator     mean_se mean_lower
    ## 1: chi_geo_kc    NA 0.27911     24337       87195 0.001519074  0.2761425
    ##    mean_upper
    ## 1:   0.282097

## Introducing helper functions

The many helper functions and utilities that have been created for RADS
will be documented in detail in the future. We limit ourselves to the
few immediately relevant to post-processing of data analyzed with the
`calc` function.

-   `digits` &lt;- This function rounds data following APDE/CHI generic
    standards or custom requests. By default, this function expects data
    that has already been formatted for CHI, i.e., it contains `result`,
    `lower_bound`, `upper_bound`, `se`, `rse` columns. However, the
    variables and the number of digits to round can be readily
    specified. Typing `args(digits)` reveals that there are 5 possible
    arguments:
    -   `digit_data` &lt;- a data.table or data.frame containing the
        data to be rounded
    -   `vars_1` &lt;- a character vector of indeterminate length. It
        specifies the variables to be rounded to the number of digits
        specified by `digits_1`. If not specified it defaults to
        `c("result", "lower_bound", "upper_bound", "rse")`, which are
        the relevant columns in CHI Tableau Ready data.
    -   `digits_1` &lt;- an integer representing the number of decimal
        places to round variables specified in vars\_1. The default is
        digits\_1 = 3, i.e., 0.123456 &gt;&gt; 0.123.
    -   `vars_2` &lt;- a character vector of indeterminate length. It
        specifies the variables to be rounded to the number of digits
        specified by `digits_2`. If not specified it defaults to
        `c("se")`
    -   `digits_2` &lt;- an integer representing the number of decimal
        places to round variables specified in vars\_1. The default is
        digits\_1 = 4, i.e., 0.123456 &gt;&gt; 0.1235.
-   `round2` &lt;- R’s built-in `round` function doesn’t act the way
    most of us might expect. `round2` rounds the way you were taught in
    elementary school, i.e., &gt;=5 rounds up. E.g., `round2(0.135, 2)`
    == 0.14 whereas `round2(0.134, 2)` == 0.13.
-   `suppress` &lt;- This function suppresses data output by the `calc`
    function according to APDE standards or custom requests and adds a
    caution flag for large RSEs. When the reference sheet of all
    suppression guidelines is complete, this code will reference that
    sheet to apply the relevant suppression algorithm. In the meantime,
    the default suppression algorithm is as described by the
    [DOH](https://www.doh.wa.gov/Portals/1/Documents/1500/SmallNumbers.pdf).
    At present, this functon takes two arguments:
    -   `sup_data` &lt;- a data.table or data.frame containing the data
        to be suppressed with the standard CHI Tableau metric names
        (e.g., mean, median, numerator, etc.).
    -   `suppress_range` &lt;- an integer vector of length 2, which
        specifies the minimum and maximum range for suppression.
-   `joinpoint` &lt;- this function runs NCI’s
    [JoinPoint](https://surveillance.cancer.gov/joinpoint/) from
    within R. Its inputs, outputs, and arguments are somewhat complex
    and will be described in full in a future stand-alone vignette.

We will not use any of the above post-processing helper functions in
this vignette. For now, just know that they exist and wait expectantly
for future vignettes that demostrate their utility.

------------------------------------------------------------------------

## Example analyses

### Preterm births in KC 2013-2018

**get data**

``` r
birth <- get_data_birth(cols = c("chi_year", "preterm"), year = c(2013:2018), kingco = T)
```

**calc all years**

``` r
birth_all <- calc(birth, what = c("preterm"), metrics = c("mean", "rse", "numerator", "denominator") )
head(birth_all)
```

    ##    variable level       mean       rse numerator denominator      mean_se
    ## 1:  preterm    NA 0.08992366 0.8196318     13547      150650 0.0007370429
    ##    mean_lower mean_upper
    ## 1: 0.08848953 0.09137871

**calc by single years**

``` r
birth_single <- calc(birth, what = c("preterm"), metrics = c("mean", "rse", "numerator", "denominator"), 
                  by = c("chi_year"))
head(birth_single)
```

    ##    variable level chi_year       mean      rse numerator denominator
    ## 1:  preterm    NA     2013 0.09259783 1.989329      2293       24763
    ## 2:  preterm    NA     2014 0.08847557 2.021357      2231       25216
    ## 3:  preterm    NA     2015 0.08829787 2.017038      2241       25380
    ## 4:  preterm    NA     2016 0.08956837 1.981016      2320       25902
    ## 5:  preterm    NA     2017 0.09120521 1.989553      2296       25174
    ## 6:  preterm    NA     2018 0.08944869 2.050369      2166       24215
    ##        mean_se mean_lower mean_upper
    ## 1: 0.001842076 0.08905042 0.09627162
    ## 2: 0.001788407 0.08503282 0.09204369
    ## 3: 0.001781002 0.08486925 0.09185110
    ## 4: 0.001774364 0.08615134 0.09310713
    ## 5: 0.001814576 0.08771087 0.09482429
    ## 6: 0.001834028 0.08591895 0.09310867

**calc 3 year window**

``` r
birth_window <- calc(birth, what = c("preterm"), metrics = c("mean", "rse", "numerator", "denominator"), 
                  win = 3)
head(birth_window)
```

    ##    variable level       mean       rse numerator denominator      mean_se
    ## 1:  preterm    NA 0.08992366 0.8196318     13547      150650 0.0007370429
    ## 2:  preterm    NA 0.08992366 0.8196318     13547      150650 0.0007370429
    ## 3:  preterm    NA 0.08992366 0.8196318     13547      150650 0.0007370429
    ##    mean_lower mean_upper
    ## 1: 0.08848953 0.09137871
    ## 2: 0.08848953 0.09137871
    ## 3: 0.08848953 0.09137871

**calc 3 year window (per 1000)**

``` r
birth_window <- calc(birth, what = c("preterm"), metrics = c("rate", "rse", "numerator", "denominator"), 
                  win = 3, per = 1000)
head(birth_window)
```

    ##    variable level     rate       rse numerator denominator   rate_se rate_lower
    ## 1:  preterm    NA 89.92366 0.8196318     13547      150650 0.7370429   88.48953
    ## 2:  preterm    NA 89.92366 0.8196318     13547      150650 0.7370429   88.48953
    ## 3:  preterm    NA 89.92366 0.8196318     13547      150650 0.7370429   88.48953
    ##    rate_upper rate_per
    ## 1:   91.37871     1000
    ## 2:   91.37871     1000
    ## 3:   91.37871     1000

### Intersection of Low Birth Weight & Prematurity in KC (last 5 years combined)

**get data**

``` r
birth <- get_data_birth(year = c(2014:2018), cols = c("chi_year", "preterm", "bw_low"), kingco = T)
```

**recode data **

``` r
birth[, preterm_lbw := 0][preterm == 1 & bw_low == 1, preterm_lbw := 1]
```

**calc 5 year**

``` r
birth_5y <- calc(birth, what = c("preterm_lbw"), 
                 metrics = c("mean", "rse", "numerator", "denominator", "missing") )
head(birth_5y)
```

    ##       variable level       mean      rse numerator denominator missing
    ## 1: preterm_lbw    NA 0.04123141 1.356042      5214      126457       0
    ##         mean_se mean_lower mean_upper
    ## 1: 0.0005591151 0.04014943 0.04234126

**calc 5 year (per 100,000)**

``` r
birth_5yrate <- calc(birth, what = c("preterm_lbw"), per = 100000, 
              metrics = c("rate", "rse", "numerator", "denominator", "missing") )
head(birth_5yrate)
```

    ##       variable level     rate      rse numerator denominator missing  rate_se
    ## 1: preterm_lbw    NA 4123.141 1.356042      5214      126457       0 55.91151
    ##    rate_lower rate_upper rate_per
    ## 1:   4014.943   4234.126    1e+05

### Premature by race and foreign born status in KC (singe year and multiple years)

**get data**

``` r
birth <- get_data_birth(year = c(2014:2018), 
                        cols = c("chi_year", "preterm", "chi_race_eth8", "mother_birthplace_foreign"), 
                        kingco = T)
```

**calc 5 year**

``` r
birth_5y <- calc(birth, what = "preterm", by = c("chi_race_eth8", "mother_birthplace_foreign"), 
                 metrics = c("mean", "rse", "numerator", "denominator"))
head(birth_5y)
```

    ##    variable level chi_race_eth8 mother_birthplace_foreign       mean       rse
    ## 1:  preterm    NA         White                         1 0.06886866  3.619026
    ## 2:  preterm    NA         White                         0 0.07938262  1.499581
    ## 3:  preterm    NA       Oth/unk                         0 0.12931034 10.783898
    ## 4:  preterm    NA       Oth/unk                         1 0.11079137 10.753978
    ## 5:  preterm    NA         Black                         1 0.09894095  3.635096
    ## 6:  preterm    NA      Hispanic                         0 0.09745194  3.715170
    ##    numerator denominator     mean_se mean_lower mean_upper
    ## 1:       711       10324 0.002492375 0.06414256 0.07391547
    ## 2:      4094       51573 0.001190407 0.07708070 0.08174721
    ## 3:        75         580 0.013944696 0.10442274 0.15907595
    ## 4:        77         695 0.011914479 0.08956175 0.13629987
    ## 5:       682        6893 0.003596599 0.09211407 0.10621461
    ## 6:       654        6711 0.003620505 0.09058500 0.10477947

**calc by single year**

``` r
birth_1y <- calc(birth, what = "preterm", 
                 by = c("chi_race_eth8", "mother_birthplace_foreign", "chi_year"), 
                 metrics = c("mean", "rse", "numerator", "denominator"))
head(birth_1y)
```

    ##    variable level chi_race_eth8 mother_birthplace_foreign chi_year       mean
    ## 1:  preterm    NA         White                         1     2014 0.06225869
    ## 2:  preterm    NA         White                         0     2014 0.08389354
    ## 3:  preterm    NA       Oth/unk                         0     2014 0.14814815
    ## 4:  preterm    NA       Oth/unk                         1     2014 0.07352941
    ## 5:  preterm    NA         Black                         1     2014 0.08623298
    ## 6:  preterm    NA      Hispanic                         0     2014 0.10183639
    ##          rse numerator denominator     mean_se mean_lower mean_upper
    ## 1:  8.528085       129        2072 0.005309473 0.05264300 0.07349450
    ## 2:  3.171274       911       10859 0.002660494 0.07882531 0.08925608
    ## 3: 32.937906         8          54 0.048796898 0.07703063 0.26600115
    ## 4: 43.365851         5          68 0.031886655 0.03181484 0.16085188
    ## 5:  8.956321       114        1322 0.007723302 0.07227460 0.10258903
    ## 6:  8.583791       122        1198 0.008741423 0.08596342 0.12025468

Note that this only shows the earliest years. By using the `tail()`
function, we can view the end of the more recent years

``` r
tail(birth_1y)
```

    ##    variable level chi_race_eth8 mother_birthplace_foreign chi_year      mean
    ## 1:  preterm    NA          NHPI                         1     2016 0.1954887
    ## 2:  preterm    NA          NHPI                         0     2018 0.1553398
    ## 3:  preterm    NA          AIAN                         1     2014 0.2500000
    ## 4:  preterm    NA          AIAN                         1     2016 0.2000000
    ## 5:  preterm    NA          AIAN                         1     2015 0.2500000
    ## 6:  preterm    NA          AIAN                         1     2018 0.0000000
    ##          rse numerator denominator    mean_se mean_lower mean_upper
    ## 1:  12.46184        52         266 0.02436150 0.15230820  0.2473393
    ## 2:  16.28631        32         206 0.02529912 0.11223483  0.2110638
    ## 3: 100.00000         1           4 0.25000000 0.04558726  0.6993582
    ## 4: 100.00000         1           5 0.20000000 0.03622411  0.6244654
    ## 5: 100.00000         1           4 0.25000000 0.04558726  0.6993582
    ## 6:       NaN         0           2 0.00000000 0.00000000  0.6576198

### Premature by race in specific geographies

**get data**

``` r
birth <- get_data_birth(year = c(2014:2018), 
                        cols = c("chi_year", "preterm", "chi_race_eth8", "chi_geo_big_cities", "chi_geo_hra_short", "chi_geo_zip5"))
```

**calc by each “big city”**

``` r
birth_big <-  calc(birth, what = "preterm", by = c("chi_race_eth8", "chi_geo_big_cities"), 
                   metrics = c("mean", "rse"))
head(birth_big)
```

    ##    variable level chi_race_eth8 chi_geo_big_cities       mean       rse
    ## 1:  preterm    NA         White               <NA> 0.07768814  2.251192
    ## 2:  preterm    NA       Oth/unk               <NA> 0.14893617 10.074576
    ## 3:  preterm    NA         Black               <NA> 0.10314740  5.206121
    ## 4:  preterm    NA      Hispanic               <NA> 0.10275013  3.838705
    ## 5:  preterm    NA         Asian               <NA> 0.08599717  3.165897
    ## 6:  preterm    NA          NHPI               <NA> 0.14948454  9.895891
    ##        mean_se mean_lower mean_upper
    ## 1: 0.001748909 0.07432923 0.08118552
    ## 2: 0.015004688 0.12193196 0.18069028
    ## 3: 0.005369978 0.09309417 0.11414963
    ## 4: 0.003944274 0.09527567 0.11073919
    ## 5: 0.002722582 0.08081003 0.09148413
    ## 6: 0.014792827 0.12281823 0.18074761

**calc within a limited set of zip codes**

``` r
birth_zip <- calc(birth, what = "preterm", chi_geo_zip5 %in% c(98001:98010), by = c("chi_race_eth8"), 
                  metrics = c("mean", "rse"))
head(birth_zip)
```

    ##    variable level chi_race_eth8       mean       rse     mean_se mean_lower
    ## 1:  preterm    NA         White 0.08265905  4.196440 0.003468737 0.07611251
    ## 2:  preterm    NA       Oth/unk 0.10493827 23.016888 0.024153524 0.06655616
    ## 3:  preterm    NA      Hispanic 0.10657846  5.551483 0.005916685 0.09553365
    ## 4:  preterm    NA         Asian 0.08140856  4.622406 0.003763034 0.07433446
    ## 5:  preterm    NA      Multiple 0.11658456 11.163752 0.013015212 0.09346612
    ## 6:  preterm    NA         Black 0.13276596  7.459174 0.009903244 0.11455562
    ##    mean_upper
    ## 1: 0.08971399
    ## 2: 0.16162236
    ## 3: 0.11873256
    ## 4: 0.08909107
    ## 5: 0.14450971
    ## 6: 0.15336969

**calc by HRAs like “Auburn”**

``` r
birth_hra <-  calc(birth, 
                   what = "preterm", 
                   chi_geo_hra_short %like% c("Auburn"), 
                   by = c("chi_race_eth8", "chi_geo_hra_short"), 
                   metrics = c("mean", "rse"))
head(birth_hra)      
```

    ##    variable level chi_race_eth8 chi_geo_hra_short       mean        rse
    ## 1:  preterm    NA      Multiple      Auburn-North 0.12500000  21.530819
    ## 2:  preterm    NA         White      Auburn-North 0.09210526   8.264991
    ## 3:  preterm    NA         Asian      Auburn-North 0.10182768  15.195508
    ## 4:  preterm    NA      Hispanic      Auburn-North 0.10099010  13.290079
    ## 5:  preterm    NA          AIAN      Auburn-North 0.11538462  55.377492
    ## 6:  preterm    NA       Oth/unk      Auburn-North 0.07142857 100.000000
    ##        mean_se mean_lower mean_upper
    ## 1: 0.026913524 0.08150359  0.1869837
    ## 2: 0.007612491 0.07825300  0.1081220
    ## 3: 0.015473233 0.07538677  0.1361765
    ## 4: 0.013421664 0.07764913  0.1303557
    ## 5: 0.063897107 0.04003245  0.2897590
    ## 6: 0.071428571 0.01272222  0.3146870

### Custom sub-populations

**get data**

``` r
birth <- get_data_birth(year = c(2014:2018), 
                        cols = c("chi_year", "chi_age", "chi_geo_hra_long", "chi_race_eth8", "dlp_medicaid", "prior_live_births_living", "prior_live_births_deceased"))
```

**calc Medicaid births, first time moms, under 23, by HRA and by race**

``` r
birth_custom1 <- calc(birth, what = "dlp_medicaid", 
                      (prior_live_births_living + prior_live_births_deceased == 0) & chi_age < 23, 
                      by = c("chi_geo_hra_long", "chi_race_eth8"))
head(birth_custom1)
```

    ##        variable level chi_geo_hra_long chi_race_eth8      mean numerator
    ## 1: dlp_medicaid    NA             <NA>      Multiple 0.5833333         7
    ## 2: dlp_medicaid    NA             <NA>         White 0.5897436        23
    ## 3: dlp_medicaid    NA             <NA>      Hispanic 0.8500000        17
    ## 4: dlp_medicaid    NA             <NA>       Oth/unk 0.6666667         2
    ## 5: dlp_medicaid    NA             <NA>         Black 0.7500000         9
    ## 6: dlp_medicaid    NA             <NA>          AIAN 1.0000000         2
    ##    denominator   mean_se mean_lower mean_upper
    ## 1:          12 0.1486471  0.3195113  0.8067397
    ## 2:          39 0.0797935  0.4341862  0.7292069
    ## 3:          20 0.0819178  0.6395811  0.9476313
    ## 4:           3 0.3333333  0.2076596  0.9385081
    ## 5:          12 0.1305582  0.4676947  0.9110583
    ## 6:           2 0.0000000  0.3423802  1.0000000

### Custom geographies

**get data**

``` r
birth <- get_data_birth(year = c(2014:2018), cols = c("chi_year", "chi_geo_hra_long"))
```

**recode data**

``` r
birth[, Duamish := 0]
birth[chi_geo_hra_long %in% c("Beacon Hill/Georgetown/South Park", "Delridge"), Duamish := 1]
```

**calc % of all births that were in Duamish, by year**

``` r
birth_custom_2 <- calc(birth, what = "Duamish", metrics = c("mean", "rse"), by = c("chi_year"))
head(birth_custom_2)
```

    ##    variable level chi_year       mean      rse     mean_se mean_lower
    ## 1:  Duamish    NA     2014 0.03956920 3.094505 0.001224471 0.03723826
    ## 2:  Duamish    NA     2015 0.03962805 3.083668 0.001221997 0.03730158
    ## 3:  Duamish    NA     2016 0.04171312 2.971948 0.001239692 0.03935033
    ## 4:  Duamish    NA     2017 0.03889372 3.126926 0.001216178 0.03657933
    ## 5:  Duamish    NA     2018 0.03841887 3.206979 0.001232085 0.03607601
    ##    mean_upper
    ## 1: 0.04203967
    ## 2: 0.04209327
    ## 3: 0.04421126
    ## 4: 0.04134827
    ## 5: 0.04090742

**calc \# of births in Duamish, by year**

``` r
birth_custom_3 <- calc(birth, Duamish == 1, what = "Duamish", metrics = c("numerator", "denominator"), by = c("chi_year"))
head(birth_custom_3)
```

    ##    variable level chi_year numerator denominator
    ## 1:  Duamish    NA     2014      1003        1003
    ## 2:  Duamish    NA     2015      1010        1010
    ## 3:  Duamish    NA     2016      1085        1085
    ## 4:  Duamish    NA     2017       983         983
    ## 5:  Duamish    NA     2018       935         935
