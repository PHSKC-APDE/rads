Introduction to calc()
================

## Introducing the `calc()` function

`calc()` is the analytic workhorse of `rads`. It provides a standardized
method for obtaining most of what we usually want to calculate: means,
medians, counts, confidence intervals, standard errors, relative
standard errors (RSE), numerators, denominators, the number missing, and
the proportion missing. `calc()` can be used with record data (e.g.,
vital status, census, enrollment numbers, etc.) as well as survey data
(e.g., [BRFSS](https://www.cdc.gov/brfss/index.html), [ACS
PUMS](https://github.com/PHSKC-APDE/svy_acs), etc.). `calc()` is built
on top of common R packages and was created to allow APDE staff the
convenience of using a common syntax across various data sources. This
means that everything `calc` can do can be done with other packages …
sometimes in a more efficient manner.

This vignette will provide some examples to introduce the `calc()`
function by walking through basic analyses with vital status (birth
data) and survey data (ACS PUMS). To get the most out of this vignette,
we recommend that you type each and every bit of code into R. Doing so
will almost definitely help you learn the syntax much faster than just
reading the vignette or copying and pasting the code.

## `calc()` arguments

Arguments are the values that we send to a function when it is called.
The standard arguments for `calc()` are:

1.  `ph.data` &lt;- the name of the data.table/data.frame or survey
    object that you want to analyze.

2.  `what` &lt;- a character vector of the variable(s) for which you
    want to calculate the metrics. E.g., `what = c("uninsured")`

3.  `...` &lt;- think of this as a “where” statement, i.e., a filter or
    subsetting of data. E.g., `ages %in% c(0:17) & gender == "Female"`.
    **NOTE** do not type `...` !

4.  `by` &lt;- a character vector of the variables that you want to
    computer the `what` by, i.e., the cross-tab variable(s). E.g.,
    `by = c("gender")` would stratify results by gender

5.  `metrics` &lt;- a character vector of the metrics that you want
    returned. E.g., `metrics = c("mean", "rse")`. You can see a complete
    list of available metrics by typing `metrics()`

6.  `per` &lt;- an integer, which is the denominator when `rate` is
    selected as a metric. Metrics will be multiplied by this value.
    E.g., `per = 1000`. **NOTE** this is just a scalar. At present this
    does not calculate a true rate (i.e., the relevant population
    denominator is not used).

7.  `win` &lt;- an integer, which is the number of consecutive units of
    time (e.g., years, months, etc.) over which the metrics will be
    calculated, i.e., the ‘window’ for a rolling average, sum, etc. E.g.
    `win = 5` will perform calculations over every 5 time unit window.

8.  `time_var` &lt;- a character, which is the name of the time variable
    in the dataset. Used in combination with the `win` argument to
    generate time windowed calculations.

9.  `proportion` &lt;- a logical (i.e., `T` or `F`) flag determining
    whether the metrics should be calculated as a proportion. Currently
    only relevant for survey data. The default is FALSE.

10. `verbose` &lt;- a logical used to toggle on/off printed warnings

There is no need to specify all of the arguments listed above. As you
can see in the following example, the `calc` function simply needs a
specified dataset (i.e., `ph.data`) and at least one `what` variable to
return an intelligible result.

``` r
data(mtcars)
calc(ph.data = mtcars, what = c("mpg"))[]
```

    ##    variable level     mean numerator denominator  mean_se mean_lower mean_upper
    ## 1:      mpg    NA 20.09062     642.9          32 1.065424   18.00243   22.17882

**Note **: The use of `[]` after `calc()` is used to print the output to
the console. Typically, you would not print the results but would save
them as an object. E.g., `my.est <- calc()`.

------------------------------------------------------------------------

## Example vital status analyses

**First get the birth data (cf. [`get_data_birth()`
vignette](https://github.com/PHSKC-APDE/rads/wiki/Vignette:-get_data_birth()))**

``` r
birth <- get_data_birth(cols = c("chi_year", "chi_sex", "chi_race_eth8", 
                                 "preterm", "birth_weight_grams", "mother_birthplace_state"), 
                        year = c(2013:2019), 
                        kingco = T)
```

**mean (proportion) for a binary over multiple years**

``` r
calc(ph.data = birth, 
     what = c("preterm"), 
     metrics = c("mean", "rse", "numerator", "denominator"), 
     time_var = "chi_year")[]
```

    ##    variable level  chi_year       mean       rse numerator denominator
    ## 1:  preterm    NA 2013-2019 0.09052329 0.7585532     15806      174607
    ##         mean_se mean_lower mean_upper
    ## 1: 0.0006866673 0.08918644 0.09187815

**mean (proportion) for a binary over individual years**

``` r
calc(ph.data = birth, 
     what = c("preterm"), 
     metrics = c("mean", "rse", "numerator", "denominator"), 
     time_var = "chi_year", 
     by = "chi_year")[]
```

    ##    variable level chi_year chi_year       mean      rse numerator denominator
    ## 1:  preterm    NA     2013     2013 0.09259783 1.989329      2293       24763
    ## 2:  preterm    NA     2014     2014 0.08847557 2.021357      2231       25216
    ## 3:  preterm    NA     2015     2015 0.08829787 2.017038      2241       25380
    ## 4:  preterm    NA     2016     2016 0.08956837 1.981016      2320       25902
    ## 5:  preterm    NA     2017     2017 0.09120521 1.989553      2296       25174
    ## 6:  preterm    NA     2018     2018 0.08944869 2.050369      2166       24215
    ## 7:  preterm    NA     2019     2019 0.09429394 2.002371      2259       23957
    ##        mean_se mean_lower mean_upper
    ## 1: 0.001842076 0.08905042 0.09627162
    ## 2: 0.001788407 0.08503282 0.09204369
    ## 3: 0.001781002 0.08486925 0.09185110
    ## 4: 0.001774364 0.08615134 0.09310713
    ## 5: 0.001814576 0.08771087 0.09482429
    ## 6: 0.001834028 0.08591895 0.09310867
    ## 7: 0.001888115 0.09065815 0.09805982

**mean (proportion) for a binary over windowed years**

``` r
calc(ph.data = birth, 
     what = c("preterm"), 
     metrics = c("mean", "rse", "numerator", "denominator"), 
     time_var = "chi_year", 
     win = 3)[]
```

    ##    variable level  chi_year       mean      rse numerator denominator
    ## 1:  preterm    NA 2013-2015 0.08977030 1.159964      6765       75359
    ## 2:  preterm    NA 2014-2016 0.08878663 1.158281      6792       76498
    ## 3:  preterm    NA 2015-2017 0.08968557 1.152210      6857       76456
    ## 4:  preterm    NA 2016-2018 0.09007717 1.158314      6782       75291
    ## 5:  preterm    NA 2017-2019 0.09163417 1.162563      6721       73346
    ##        mean_se mean_lower mean_upper
    ## 1: 0.001041303 0.08775025 0.09183217
    ## 2: 0.001028399 0.08679162 0.09082295
    ## 3: 0.001033366 0.08768079 0.09173159
    ## 4: 0.001043376 0.08805306 0.09214310
    ## 5: 0.001065305 0.08956756 0.09374356

**mean for a continuous over windowed years**

``` r
calc(ph.data = birth, 
     what = c("birth_weight_grams"), 
     metrics = c("mean", "rse"), 
     time_var = "chi_year", 
     win = 3)[]
```

    ##              variable level  chi_year     mean        rse  mean_se mean_lower
    ## 1: birth_weight_grams    NA 2013-2015 3339.469 0.06354375 2.122024   3335.310
    ## 2: birth_weight_grams    NA 2014-2016 3336.555 0.06255302 2.087116   3332.465
    ## 3: birth_weight_grams    NA 2015-2017 3333.507 0.06205880 2.068735   3329.452
    ## 4: birth_weight_grams    NA 2016-2018 3327.129 0.06233269 2.073889   3323.065
    ## 5: birth_weight_grams    NA 2017-2019 3320.961 0.06288689 2.088449   3316.868
    ##    mean_upper
    ## 1:   3343.628
    ## 2:   3340.646
    ## 3:   3337.562
    ## 4:   3331.194
    ## 5:   3325.055

**mean for a continuous in 2019, by gender and race/eth**

``` r
calc(ph.data = birth, 
     what = c("birth_weight_grams"), 
     chi_year == 2019,
     metrics = c("mean", "rse"), 
     by = c("chi_race_eth8", "chi_sex"))[]
```

    ##               variable level chi_race_eth8 chi_sex     mean        rse
    ##  1: birth_weight_grams    NA         White  Female 3340.295  0.2179622
    ##  2: birth_weight_grams    NA         Asian    Male 3231.952  0.3027408
    ##  3: birth_weight_grams    NA         White    Male 3460.298  0.2164470
    ##  4: birth_weight_grams    NA          NHPI  Female 3287.541  1.5002905
    ##  5: birth_weight_grams    NA      Hispanic    Male 3301.784  0.4334156
    ##  6: birth_weight_grams    NA      Multiple  Female 3251.312  0.7494198
    ##  7: birth_weight_grams    NA         Asian  Female 3145.902  0.2994104
    ##  8: birth_weight_grams    NA         Black    Male 3306.213  0.5986925
    ##  9: birth_weight_grams    NA      Multiple    Male 3369.178  0.6933292
    ## 10: birth_weight_grams    NA         Black  Female 3196.872  0.5844288
    ## 11: birth_weight_grams    NA      Hispanic  Female 3270.074  0.4190681
    ## 12: birth_weight_grams    NA          AIAN  Female 3251.965  2.4314716
    ## 13: birth_weight_grams    NA       Oth/unk  Female 3223.599  0.9514328
    ## 14: birth_weight_grams    NA          NHPI    Male 3444.178  1.2579021
    ## 15: birth_weight_grams    NA       Oth/unk    Male 3315.557  1.0060823
    ## 16: birth_weight_grams    NA          AIAN    Male 3438.641  2.4517252
    ## 17: birth_weight_grams    NA         White    <NA> 1873.500 70.5364291
    ##         mean_se mean_lower mean_upper
    ##  1:    7.280580   3326.026   3354.565
    ##  2:    9.784437   3212.775   3251.129
    ##  3:    7.489711   3445.618   3474.977
    ##  4:   49.322668   3190.871   3384.212
    ##  5:   14.310447   3273.736   3329.832
    ##  6:   24.365976   3203.556   3299.069
    ##  7:    9.419157   3127.441   3164.363
    ##  8:   19.794051   3267.418   3345.009
    ##  9:   23.359494   3323.394   3414.962
    ## 10:   18.683442   3160.253   3233.491
    ## 11:   13.703835   3243.215   3296.933
    ## 12:   79.070604   3096.989   3406.940
    ## 13:   30.670381   3163.487   3283.712
    ## 14:   43.324387   3359.264   3529.092
    ## 15:   33.357232   3250.178   3380.936
    ## 16:   84.306018   3273.404   3603.877
    ## 17: 1321.500000      0.000  18664.750

**Proportion of 2017-2019 births among each race/eth group**

``` r
calc(ph.data = birth, 
     what = c("chi_race_eth8"), 
     chi_year %in% 2017:2019,
     metrics = c("mean", "rse", "obs", "numerator", "denominator"))[]
```

    ##         variable    level       mean       rse   obs numerator denominator
    ## 1: chi_race_eth8     AIAN 0.00530522 5.0438189 73701       391       73701
    ## 2: chi_race_eth8    Asian 0.22911494 0.6756696 73701     16886       73701
    ## 3: chi_race_eth8    Black 0.09036512 1.1686901 73701      6660       73701
    ## 4: chi_race_eth8 Hispanic 0.13007965 0.9525797 73701      9587       73701
    ## 5: chi_race_eth8 Multiple 0.04203471 1.7584788 73701      3098       73701
    ## 6: chi_race_eth8     NHPI 0.01629557 2.8619613 73701      1201       73701
    ## 7: chi_race_eth8  Oth/unk 0.01867003 2.6705534 73701      1376       73701
    ## 8: chi_race_eth8    White 0.46813476 0.3926283 73701     34502       73701
    ##         mean_se  mean_lower  mean_upper
    ## 1: 0.0002675857 0.004805929 0.005856077
    ## 2: 0.0015480599 0.226094981 0.232163131
    ## 3: 0.0010560883 0.088316537 0.092456411
    ## 4: 0.0012391123 0.127670314 0.132527538
    ## 5: 0.0007391714 0.040609678 0.043507475
    ## 6: 0.0004663730 0.015406391 0.017235175
    ## 7: 0.0004985932 0.017717604 0.019672633
    ## 8: 0.0018380296 0.464534068 0.471738775

**2017-2019 rate per 100k of births among each race/eth group**

``` r
calc(ph.data = birth, 
     what = c("chi_race_eth8"), 
     chi_year %in% 2017:2019,
     metrics = c("obs", "numerator", "denominator", "rate"), 
     per = 100000)[]
```

    ##         variable    level   obs numerator denominator      rate   rate_se
    ## 1: chi_race_eth8     AIAN 73701       391       73701   530.522  26.75857
    ## 2: chi_race_eth8    Asian 73701     16886       73701 22911.494 154.80599
    ## 3: chi_race_eth8    Black 73701      6660       73701  9036.512 105.60883
    ## 4: chi_race_eth8 Hispanic 73701      9587       73701 13007.965 123.91123
    ## 5: chi_race_eth8 Multiple 73701      3098       73701  4203.471  73.91714
    ## 6: chi_race_eth8     NHPI 73701      1201       73701  1629.557  46.63730
    ## 7: chi_race_eth8  Oth/unk 73701      1376       73701  1867.003  49.85932
    ## 8: chi_race_eth8    White 73701     34502       73701 46813.476 183.80296
    ##    rate_lower rate_upper rate_per
    ## 1:   480.5929   585.6077    1e+05
    ## 2: 22609.4981 23216.3131    1e+05
    ## 3:  8831.6537  9245.6411    1e+05
    ## 4: 12767.0314 13252.7538    1e+05
    ## 5:  4060.9678  4350.7475    1e+05
    ## 6:  1540.6391  1723.5175    1e+05
    ## 7:  1771.7604  1967.2633    1e+05
    ## 8: 46453.4068 47173.8775    1e+05

**Number and proportion of missing gender by year**

``` r
calc(ph.data = birth, 
     what = c("chi_sex"), 
     chi_year %in% 2017:2019,
     metrics = c("obs", "missing", "missing.prop"), 
     by = "chi_year")[]
```

    ## Warning in `[.data.table`(res, is.na(numerator), `:=`((na_mets), NA)):
    ## length(LHS)==0; no columns to delete or assign RHS to.

    ##    variable  level chi_year   obs missing missing.prop
    ## 1:  chi_sex   <NA>     2019 24090       2   8.3022e-05
    ## 2:  chi_sex Female     2017 25274       0   0.0000e+00
    ## 3:  chi_sex Female     2018 24337       0   0.0000e+00
    ## 4:  chi_sex Female     2019 24090       0   0.0000e+00
    ## 5:  chi_sex   Male     2017 25274       0   0.0000e+00
    ## 6:  chi_sex   Male     2018 24337       0   0.0000e+00
    ## 7:  chi_sex   Male     2019 24090       0   0.0000e+00

## Example survey analyses

Before using `rads::calc()` with survey data, the user must survey set
the data while properly specifying the survey design. Here is an example
of how to set [ACS PUMS](https://github.com/PHSKC-APDE/svy_acs) person
level data:

``` r
library(srvyr)

load("//phshare01/epe_share/WORK/surveys/ACS/PUMS data & code all years/2019_1_year/prepped_R_files/2019_1_year_data.RData")

  pums <-     
    srvyr::as_survey_rep(
      person.wa ,
      weights = pwgtp ,
      combined.weights = TRUE ,
      repweights = grep('pwgtp[0-9]+', names(person.wa), value  = T) ,
      scale = 4 / 80 ,
      rscales = rep( 1 , 80 ) ,
      mse = TRUE ,
      type = "JK1"
    )
```

Once the data has been set as a survey, we can use the `calc()` function
the same way that we used it for vital status (birth) data.

**Mean (proportion) of those near poverty or disabled, by King County
(vs. remainder of WA)**

``` r
calc(ph.data = pums, 
     what = c("disability", "GEpov200"), 
     metrics = c("mean", "rse", "obs", "numerator", "denominator"), 
     proportion = T, 
     by = "chi_geo_kc")[]
```

    ## Adding missing grouping variables: `chi_geo_kc`
    ## Adding missing grouping variables: `chi_geo_kc`

    ##      variable level chi_geo_kc       mean       rse   obs numerator denominator
    ## 1:   GEpov200    NA          0 0.73832042 0.5462199 57112     41951       55069
    ## 2:   GEpov200    NA          1 0.83492701 0.5927687 20767     17250       20149
    ## 3: disability    NA          0 0.14337089 1.2832777 57112      9038       57112
    ## 4: disability    NA          1 0.09965467 2.9098684 20767      2310       20767
    ##        mean_se mean_lower mean_upper
    ## 1: 0.004032853 0.73019251  0.7462886
    ## 2: 0.004949186 0.82483426  0.8445478
    ## 3: 0.001839847 0.13974736  0.1470723
    ## 4: 0.002899820 0.09403037  0.1055762

**Proportion in each CHNA age group, by disability status**

``` r
calc(ph.data = pums, 
     what = c("agechna"), 
     metrics = c("mean", "rse", "obs", "numerator", "denominator"), 
     proportion = T, 
     by = "disability")[]
```

    ##     variable level disability       mean       rse   obs numerator denominator
    ##  1:  agechna 18-24          0 0.09226662 0.7796531 66531      5771       66531
    ##  2:  agechna 18-24          1 0.04838122 6.2568562 11348       482       11348
    ##  3:  agechna 25-44          0 0.30776057 0.3613000 66531     18128       66531
    ##  4:  agechna 25-44          1 0.16410713 2.9390524 11348      1592       11348
    ##  5:  agechna 45-64          0 0.24204020 0.3674029 66531     17535       66531
    ##  6:  agechna 45-64          1 0.28470479 1.4983446 11348      3089       11348
    ##  7:  agechna 65-74          0 0.08320689 0.7995977 66531      7137       66531
    ##  8:  agechna 65-74          1 0.18640300 2.2250358 11348      2338       11348
    ##  9:  agechna   75+          0 0.03492094 1.6831876 66531      2996       66531
    ## 10:  agechna   75+          1 0.24249002 1.6895181 11348      3173       11348
    ## 11:  agechna   <18          0 0.23980479 0.2619719 66531     14964       66531
    ## 12:  agechna   <18          1 0.07391385 4.6569890 11348       674       11348
    ##          mean_se mean_lower mean_upper
    ##  1: 0.0007193595 0.09084462 0.09370857
    ##  2: 0.0030271433 0.04273527 0.05473043
    ##  3: 0.0011119388 0.30555210 0.30997787
    ##  4: 0.0048231944 0.15472290 0.17394340
    ##  5: 0.0008892628 0.24027539 0.24381381
    ##  6: 0.0042658589 0.27628975 0.29327226
    ##  7: 0.0006653204 0.08189291 0.08454001
    ##  8: 0.0041475334 0.17826549 0.19482390
    ##  9: 0.0005877849 0.03376989 0.03610975
    ## 10: 0.0040969128 0.23442309 0.25074362
    ## 11: 0.0006282212 0.23855619 0.24105785
    ## 12: 0.0034421598 0.06734098 0.08107250

**Proportion disabled, by CHNA age group**

``` r
calc(ph.data = pums, 
     what = c("disability"), 
     metrics = c("mean", "rse", "obs", "numerator", "denominator"), 
     proportion = T, 
     by = "agechna")[]
```

    ## Adding missing grouping variables: `agechna`

    ##      variable level agechna       mean      rse   obs numerator denominator
    ## 1: disability    NA     <18 0.04419218 4.883924 15638       674       15638
    ## 2: disability    NA   18-24 0.07292150 6.447607  6253       482        6253
    ## 3: disability    NA   25-44 0.07406324 3.314274 19720      1592       19720
    ## 4: disability    NA   45-64 0.14998292 1.928818 20624      3089       20624
    ## 5: disability    NA   65-74 0.25152354 2.202846  9475      2338        9475
    ## 6: disability    NA     75+ 0.51019608 1.690506  6169      3173        6169
    ##        mean_se mean_lower mean_upper
    ## 1: 0.002158313 0.04008902 0.04869400
    ## 2: 0.004701692 0.06413084 0.08281052
    ## 3: 0.002454659 0.06932856 0.07909378
    ## 4: 0.002892898 0.14431679 0.15583101
    ## 5: 0.005540677 0.24063949 0.26272955
    ## 6: 0.008624897 0.49302208 0.52734605

**Mean & median age in King County, by disability status**

``` r
calc(ph.data = pums, 
     what = c("agep"),
     chi_geo_kc == 1,
     metrics = c("mean", "median", "rse", "obs", "numerator", "denominator"), 
     by = "disability")[]
```

    ## Adding missing grouping variables: `disability`

    ##    variable level disability     mean median       rse   obs numerator
    ## 1:     agep    NA          0 36.04553     37 0.2326545 18457    694645
    ## 2:     agep    NA          1 57.92030     65 1.1450027  2310    139502
    ##    denominator    mean_se mean_lower mean_upper
    ## 1:       18457 0.08386155   35.87861   36.21246
    ## 2:        2310 0.66318899   56.60026   59.24035
