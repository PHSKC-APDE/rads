---
title: "calc()"
output:
  rmarkdown::html_vignette: default
  github_document: default
  pdf_document: default
  urlcolor: blue
  keep_md: true
vignette: |
  %\VignetteEngine{knitr::knitr} %\VignetteIndexEntry{calc}
---



## Introduction

`calc()` is the analytic workhorse of `rads`. It provides a standardized method for obtaining most of what we usually want to calculate: means, medians, counts, confidence intervals, standard errors, relative standard errors (RSE), numerators, denominators, the number missing, and the proportion missing. `calc()` can be used with record data (e.g., vital statistics, census, enrollment numbers, etc.) as well as survey data (e.g., [BRFSS](https://www.cdc.gov/brfss/index.html), [ACS PUMS](https://github.com/PHSKC-APDE/svy_acs), etc.). `calc()` is built on top of common R packages and was created to allow APDE staff the convenience of using a common syntax across various data sources. This means that everything `calc` can do can be done with other packages ... sometimes in a more efficient manner.

This vignette will provide some examples to introduce the `calc()` function by walking through basic analyses with vital statistics (birth data) and survey data (ACS PUMS). To get the most out of this vignette, we recommend that you type each and every bit of code into R. Doing so will almost definitely help you learn the syntax much faster than just reading the vignette or copying and pasting the code.

## `calc()` arguments

Arguments are the values that we send to a function when it is called. The standard arguments for `calc()` are:

1\) `ph.data` \<- the name of the data.table/data.frame or survey object that you want to analyze.

2\) `what` \<- a character vector of the variable(s) for which you want to calculate the metrics. E.g., `what = c("uninsured")`

3\) `where` \<- think of this as a SQL `WHERE` clause, i.e., a filtering or subsetting of data. E.g., `ages %in% c(0:17) & gender == "Female"`

4\) `by` \<- a character vector of the variables that you want to computer the `what` by, i.e., the cross-tab variable(s). E.g., `by = c("gender")` would stratify results by gender

5\) `metrics` \<- a character vector of the metrics that you want returned. E.g., `metrics = c("mean", "rse")`. You can see a complete list of available metrics by typing `metrics()` and get detailed descriptions of what each metric means by typing `?metrics()`.

6\) `per` \<- an integer, which is the denominator when `rate` is selected as a metric. Metrics will be multiplied by this value. E.g., `per = 1000`. **NOTE** this is just a scalar. At present this does not calculate a true rate (i.e., the relevant population denominator is not used).

7\) `win` \<- an integer, which is the number of consecutive units of time (e.g., years, months, etc.) over which the metrics will be calculated, i.e., the 'window' for a rolling average, sum, etc. E.g. `win = 5` will perform calculations over every 5 time unit window.

8\) `time_var` \<- a character, which is the name of the time variable in the dataset. Used in combination with the `win` argument to generate time windowed calculations.

9\) `fancy_time` \<- a logical (i.e., `T` or `F`) flag. If TRUE, a record of all the years going into the data is returned. If FALSE, just a simple range (where certain years within the range might not be represented in your data).

10\) `proportion` \<- a logical (i.e., `T` or `F`) flag determining whether the metrics should be calculated as a proportion. Currently only relevant for survey data. The default is FALSE.

11\) `ci` \<- a numeric value between 0 & 1 for the confidence level returned in the estimates. E.g., 0.95 == 95% confidence interval

12\) `verbose` \<- a logical used to toggle on/off printed warnings

There is no need to specify all of the arguments listed above. As you can see in the following example, the `calc` function simply needs a specified dataset (i.e., `ph.data`) and at least one `what` variable to return an intelligible result.


```r
data(mtcars)
calc(ph.data = mtcars, what = c("mpg"))[]
```

```
##    variable     mean numerator denominator level  mean_se mean_lower mean_upper
## 1:      mpg 20.09062     642.9          32    NA 1.065424   18.00243   22.17882
```

**Note:** *The use of `[]` after `calc()` is used to print the output to the console. Typically, you would not print the results but would save them as an object. E.g., `my.est <- calc()`.*

------------------------------------------------------------------------

## Example vital statistics analyses

**First get the birth data (cf. [get_data vignette](https://github.com/PHSKC-APDE/rads/wiki/get_data))**


```r
birth <- get_data_birth(cols = c("chi_year", "chi_sex", "chi_race_eth8", 
                                 "preterm", "birth_weight_grams", "mother_birthplace_state"), 
                        year = c(2013:2019), 
                        kingco = T)
```

**mean (proportion) for a binary over multiple years**


```r
calc(ph.data = birth, 
     what = c("preterm"), 
     metrics = c("mean", "rse", "numerator", "denominator"), 
     time_var = "chi_year")[]
```

```
##     chi_year variable       mean numerator denominator level      mean_se mean_lower mean_upper       rse
## 1: 2013-2019  preterm 0.09052329     15806      174607    NA 0.0006866673 0.08917745 0.09186913 0.7585532
```

**mean (proportion) for a binary over multiple years -- `where` newborn is male**


```r
calc(ph.data = birth, 
     what = c("preterm"), 
     where = chi_sex == "Male",
     metrics = c("mean", "rse", "numerator", "denominator"), 
     time_var = "chi_year")[]
```

```
##     chi_year variable       mean numerator denominator level      mean_se mean_lower mean_upper      rse
## 1: 2013-2019  preterm 0.09716898      8694       89473    NA 0.0009902013 0.09522822 0.09910974 1.019051
```

**mean (proportion) for a binary over multiple years -- `where` newborn is male born to a Hispanic mother**


```r
calc(ph.data = birth, 
     what = c("preterm"), 
     where = chi_sex == "Male" & chi_race_eth8 == "Hispanic",
     metrics = c("mean", "rse", "numerator", "denominator"), 
     time_var = "chi_year")[]
```

```
##     chi_year variable      mean numerator denominator level     mean_se mean_lower mean_upper      rse
## 1: 2013-2019  preterm 0.1105628      1277       11550    NA 0.002918031  0.1048435   0.116282 2.639253
```

**mean (proportion) for a binary over individual years**


```r
birth[, cy := chi_year]
calc(ph.data = birth, 
     what = c("preterm"), 
     metrics = c("mean", "rse", "numerator", "denominator"), 
     time_var = "chi_year", 
     by = "cy")[]
```

```
##      cy chi_year variable       mean numerator denominator level     mean_se mean_lower mean_upper      rse
## 1: 2013     2013  preterm 0.09259783      2293       24763    NA 0.001842076 0.08898743 0.09620823 1.989329
## 2: 2014     2014  preterm 0.08847557      2231       25216    NA 0.001788407 0.08497036 0.09198078 2.021357
## 3: 2015     2015  preterm 0.08829787      2241       25380    NA 0.001781002 0.08480717 0.09178857 2.017038
## 4: 2016     2016  preterm 0.08956837      2320       25902    NA 0.001774364 0.08609068 0.09304606 1.981016
## 5: 2017     2017  preterm 0.09120521      2296       25174    NA 0.001814576 0.08764871 0.09476172 1.989553
## 6: 2018     2018  preterm 0.08944869      2166       24215    NA 0.001834028 0.08585406 0.09304332 2.050369
## 7: 2019     2019  preterm 0.09429394      2259       23957    NA 0.001888115 0.09059331 0.09799458 2.002371
```

**mean (proportion) for a binary over windowed years**


```r
calc(ph.data = birth, 
     what = c("preterm"), 
     metrics = c("mean", "rse", "numerator", "denominator"), 
     time_var = "chi_year", 
     win = 3)[]
```

```
##     chi_year variable       mean numerator denominator level     mean_se mean_lower mean_upper      rse
## 1: 2013-2015  preterm 0.08977030      6765       75359    NA 0.001041303 0.08772938 0.09181122 1.159964
## 2: 2014-2016  preterm 0.08878663      6792       76498    NA 0.001028399 0.08677101 0.09080226 1.158281
## 3: 2015-2017  preterm 0.08968557      6857       76456    NA 0.001033366 0.08766021 0.09171093 1.152210
## 4: 2016-2018  preterm 0.09007717      6782       75291    NA 0.001043376 0.08803219 0.09212215 1.158314
## 5: 2017-2019  preterm 0.09163417      6721       73346    NA 0.001065305 0.08954621 0.09372213 1.162563
```

**mean for a continuous over windowed years**


```r
calc(ph.data = birth, 
     what = c("birth_weight_grams"), 
     metrics = c("mean", "rse"), 
     time_var = "chi_year", 
     win = 3)[]
```

```
##     chi_year           variable     mean level  mean_se mean_lower mean_upper        rse
## 1: 2013-2015 birth_weight_grams 3339.469    NA 2.122024   3335.310   3343.628 0.06354375
## 2: 2014-2016 birth_weight_grams 3336.555    NA 2.087116   3332.465   3340.646 0.06255302
## 3: 2015-2017 birth_weight_grams 3333.507    NA 2.068735   3329.452   3337.562 0.06205880
## 4: 2016-2018 birth_weight_grams 3327.129    NA 2.073889   3323.065   3331.194 0.06233269
## 5: 2017-2019 birth_weight_grams 3320.961    NA 2.088449   3316.868   3325.055 0.06288689
```

**mean for a continuous in 2019, by gender and race/eth**


```r
calc(ph.data = birth, 
     what = c("birth_weight_grams"), 
     chi_year == 2019,
     metrics = c("mean", "rse"), 
     by = c("chi_race_eth8", "chi_sex"))[]
```

```
##     chi_race_eth8 chi_sex           variable     mean level     mean_se mean_lower mean_upper        rse
##  1:         White  Female birth_weight_grams 3340.295    NA    7.280580   3326.026   3354.565  0.2179622
##  2:         White    Male birth_weight_grams 3460.298    NA    7.489711   3445.618   3474.977  0.2164470
##  3:      Hispanic  Female birth_weight_grams 3270.074    NA   13.703835   3243.215   3296.933  0.4190681
##  4:      Hispanic    Male birth_weight_grams 3301.784    NA   14.310447   3273.736   3329.832  0.4334156
##  5:      Multiple    Male birth_weight_grams 3369.178    NA   23.359494   3323.394   3414.962  0.6933292
##  6:         Black  Female birth_weight_grams 3196.872    NA   18.683442   3160.253   3233.491  0.5844288
##  7:         Asian    Male birth_weight_grams 3231.952    NA    9.784437   3212.775   3251.129  0.3027408
##  8:         Black    Male birth_weight_grams 3306.213    NA   19.794051   3267.418   3345.009  0.5986925
##  9:         Asian  Female birth_weight_grams 3145.902    NA    9.419157   3127.441   3164.363  0.2994104
## 10:      Multiple  Female birth_weight_grams 3251.312    NA   24.365976   3203.556   3299.069  0.7494198
## 11:       Oth/unk  Female birth_weight_grams 3223.599    NA   30.670381   3163.487   3283.712  0.9514328
## 12:          NHPI    Male birth_weight_grams 3444.178    NA   43.324387   3359.264   3529.092  1.2579021
## 13:          NHPI  Female birth_weight_grams 3287.541    NA   49.322668   3190.871   3384.212  1.5002905
## 14:          AIAN  Female birth_weight_grams 3251.965    NA   79.070604   3096.989   3406.940  2.4314716
## 15:       Oth/unk    Male birth_weight_grams 3315.557    NA   33.357232   3250.178   3380.936  1.0060823
## 16:          AIAN    Male birth_weight_grams 3438.641    NA   84.306018   3273.404   3603.877  2.4517252
## 17:         White    <NA> birth_weight_grams 1873.500    NA 1321.500000 -14917.750  18664.750 70.5364291
```

**Proportion of 2017-2019 births among each race/eth group**


```r
calc(ph.data = birth, 
     what = c("chi_race_eth8"), 
     chi_year %in% 2017:2019,
     metrics = c("mean", "rse", "obs", "numerator", "denominator"))[]
```

```
##       level      variable denominator   obs       mean      mean_se  mean_lower  mean_upper numerator       rse
## 1:     AIAN chi_race_eth8       73701 73701 0.00530522 0.0002675857 0.004805929 0.005856077       391 5.0438189
## 2:    Asian chi_race_eth8       73701 73701 0.22911494 0.0015480599 0.226094981 0.232163131     16886 0.6756696
## 3:    Black chi_race_eth8       73701 73701 0.09036512 0.0010560883 0.088316537 0.092456411      6660 1.1686901
## 4: Hispanic chi_race_eth8       73701 73701 0.13007965 0.0012391123 0.127670314 0.132527538      9587 0.9525797
## 5: Multiple chi_race_eth8       73701 73701 0.04203471 0.0007391714 0.040609678 0.043507475      3098 1.7584788
## 6:     NHPI chi_race_eth8       73701 73701 0.01629557 0.0004663730 0.015406391 0.017235175      1201 2.8619613
## 7:  Oth/unk chi_race_eth8       73701 73701 0.01867003 0.0004985932 0.017717604 0.019672633      1376 2.6705534
## 8:    White chi_race_eth8       73701 73701 0.46813476 0.0018380296 0.464534068 0.471738775     34502 0.3926283
```

**2017-2019 rate per 100k of births among each race/eth group**


```r
calc(ph.data = birth, 
     what = c("chi_race_eth8"), 
     chi_year %in% 2017:2019,
     metrics = c("obs", "numerator", "denominator", "rate"), 
     per = 100000)[]
```

```
##       level      variable denominator   obs numerator      rate   rate_se rate_lower rate_upper rate_per
## 1:     AIAN chi_race_eth8       73701 73701       391   530.522  26.75857   480.5929   585.6077   100000
## 2:    Asian chi_race_eth8       73701 73701     16886 22911.494 154.80599 22609.4981 23216.3131   100000
## 3:    Black chi_race_eth8       73701 73701      6660  9036.512 105.60883  8831.6537  9245.6411   100000
## 4: Hispanic chi_race_eth8       73701 73701      9587 13007.965 123.91123 12767.0314 13252.7538   100000
## 5: Multiple chi_race_eth8       73701 73701      3098  4203.471  73.91714  4060.9678  4350.7475   100000
## 6:     NHPI chi_race_eth8       73701 73701      1201  1629.557  46.63730  1540.6391  1723.5175   100000
## 7:  Oth/unk chi_race_eth8       73701 73701      1376  1867.003  49.85932  1771.7604  1967.2633   100000
## 8:    White chi_race_eth8       73701 73701     34502 46813.476 183.80296 46453.4068 47173.8775   100000
```

**Number and proportion of missing gender by year**


```r
calc(ph.data = birth, 
     what = c("chi_sex"), 
     chi_year %in% 2017:2019,
     metrics = c("obs", "missing", "missing.prop"), 
     by = "chi_year")[]
```

```
##    chi_year variable   obs missing missing.prop  level
## 1:     2017  chi_sex 25274       0  0.000000000 Female
## 2:     2017  chi_sex 25274       0  0.000000000   Male
## 3:     2018  chi_sex 24337       0  0.000000000   Male
## 4:     2018  chi_sex 24337       0  0.000000000 Female
## 5:     2019  chi_sex 24090       2  0.000083022 Female
## 6:     2019  chi_sex 24090       2  0.000083022   Male
## 7:     2019  chi_sex 24090       2  0.000083022   <NA>
```

------------------------------------------------------------------------

## Example survey analyses

Before using `calc()` with survey data, the user must survey set the data while properly specifying the survey design. Here is an example of how to set [ACS PUMS](https://github.com/PHSKC-APDE/svy_acs) person level data:


```r
library(survey)

load("//dphcifs/APDE-CDIP/ACS/PUMS_data/2019_1_year/prepped_R_files/2019_1_year_data.RData")

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
```

Once the data has been set as a survey, we can use the `calc()` function the same way that we used it for vital statistics (birth) data.

**Quick sanity check before proceeding.**

To confirm that the data has been survey set properly, it would be wise to check if the survey weighted population is more or less what you would expect.

When `calc` analyzes survey data, the `numerator` is the number of observations in that category and the `total` is the survey weighted value for those observations. We know that the approximate 2019 populations of WA State, King County, and Seattle were \~7.6 million, \~2.2 million, and 750,000, respectively. How do the following 2019 ACS estimates (i.e., `total` values) compare?


```r
pums[, constant :=1]

# WA State
calc(ph.data = pums, 
     what = c("constant"), 
     metrics = c("numerator", "total"), 
     proportion = F)[]
```

```
##    variable   total numerator level total_se total_lower total_upper
## 1: constant 7614893     77879    NA        0     7614893     7614893
```

```r
# King County
calc(ph.data = pums, 
     what = c("constant"), 
     chi_geo_kc ==1,
     metrics = c("numerator", "total"), 
     proportion = F)[]
```

```
##    variable   total numerator level total_se total_lower total_upper
## 1: constant 2252619     20767    NA  1102.38     2250425     2254813
```

```r
# Seattle
calc(ph.data = pums, 
     what = c("constant"), 
     chi_geo_seattle ==1,
     metrics = c("numerator", "total"), 
     proportion = F)[]
```

```
##    variable  total numerator level total_se total_lower total_upper
## 1: constant 752953      6526    NA 1077.184    750808.9    755097.1
```

Now that we've established that our survey data is reasonable, we can continue with our analyses.

**Mean (proportion) of those near poverty or disabled, by King County (vs. remainder of WA)**


```r
calc(ph.data = pums, 
     what = c("disability", "GEpov200"), 
     metrics = c("mean", "rse", "obs", "numerator", "denominator"), 
     proportion = T, 
     by = "chi_geo_kc")[]
```

```
##    chi_geo_kc   variable       mean numerator denominator   obs level     mean_se mean_lower mean_upper       rse
## 1:          0 disability 0.14337089      9038       57112 57112    NA 0.001839847 0.13974757  0.1470721 1.2832777
## 2:          1 disability 0.09965467      2310       20767 20767    NA 0.002899820 0.09402953  0.1055771 2.9098684
## 3:          0   GEpov200 0.73832042     41951       55069 57112    NA 0.004032853 0.73021413  0.7462678 0.5462199
## 4:          1   GEpov200 0.83492701     17250       20149 20767    NA 0.004949186 0.82483869  0.8445438 0.5927687
```

**Proportion in each CHNA age group, by disability status**


```r
calc(ph.data = pums, 
     what = c("agechna"), 
     metrics = c("mean", "rse", "obs", "numerator", "denominator"), 
     proportion = F, 
     by = "disability")[]
```

```
##     disability level variable denominator   obs       mean      mean_se mean_lower mean_upper numerator       rse
##  1:          0 18-24  agechna       66531 66531 0.09226661 0.0007193595 0.09083477 0.09369846      5771 0.7796531
##  2:          0 25-44  agechna       66531 66531 0.30776057 0.0011119388 0.30554731 0.30997383     18128 0.3613000
##  3:          0 45-64  agechna       66531 66531 0.24204020 0.0008892628 0.24027016 0.24381023     17535 0.3674029
##  4:          0 65-74  agechna       66531 66531 0.08320689 0.0006653204 0.08188260 0.08453118      7137 0.7995977
##  5:          0   75+  agechna       66531 66531 0.03492094 0.0005877849 0.03375098 0.03609089      2996 1.6831876
##  6:          0   <18  agechna       66531 66531 0.23980479 0.0006282212 0.23855435 0.24105523     14964 0.2619719
##  7:          1 18-24  agechna       11348 11348 0.04838122 0.0030271433 0.04235584 0.05440660       482 6.2568562
##  8:          1 25-44  agechna       11348 11348 0.16410713 0.0048231944 0.15450680 0.17370746      1592 2.9390524
##  9:          1 45-64  agechna       11348 11348 0.28470479 0.0042658589 0.27621381 0.29319577      3089 1.4983446
## 10:          1 65-74  agechna       11348 11348 0.18640300 0.0041475334 0.17814754 0.19465846      2338 2.2250358
## 11:          1   75+  agechna       11348 11348 0.24249002 0.0040969128 0.23433532 0.25064472      3173 1.6895181
## 12:          1   <18  agechna       11348 11348 0.07391385 0.0034421598 0.06706240 0.08076530       674 4.6569890
```

**Mean & median age in King County, by disability status**


```r
calc(ph.data = pums, 
     what = c("agep"),
     chi_geo_kc == 1,
     metrics = c("mean", "median", "rse", "obs", "numerator", "denominator"), 
     by = "disability")[]
```

```
##    disability variable     mean median numerator denominator   obs level    mean_se mean_lower mean_upper       rse
## 1:          1     agep 57.92030     65    139502        2310  2310    NA 0.66318899   56.60026   59.24035 1.1450027
## 2:          0     agep 36.04553     37    694645       18457 18457    NA 0.08386155   35.87861   36.21246 0.2326545
```

------------------------------------------------------------------------

## Example analyses with non-standard data

In the examples above we used standard public health data (birth vital statistics and the Census Bureau's ACS survey). However, since `calc()` is a generalized function, you can use it with nearly any dataset, as long as it is a data.frame, a data.table, or a survey object. To demonstrate this, we will use `calc()` with synthetic data that we will generate below.

**Create the dataset**


```r
library(data.table)
set.seed(98121) 

mydt <- data.table(
  school = as.factor(sample(c("Alpha", "Beta", "Gamma", "Delta"), 2000, replace = T)),
  grades = as.factor(sample(c("A", "B", "C", "D"), 2000, replace = T)), 
  year = sample(2016:2021, 2000, replace = T))

mydt[]
```

```
##       school grades year
##    1:  Alpha      A 2017
##    2:  Delta      B 2019
##    3:  Gamma      C 2017
##    4:   Beta      A 2016
##    5:  Delta      C 2018
##   ---                   
## 1996:  Alpha      C 2018
## 1997:   Beta      B 2021
## 1998:  Delta      C 2020
## 1999:  Delta      D 2019
## 2000:  Delta      D 2018
```

We see that we created a dataset of 2000 rows with grades in four schools between with 2016 and 2021.

**Calculate the proportion of A's and B's in the Alpha and Beta schools**


```r
grades.distribution <- calc(
  ph.data = mydt, 
  school %in% c("Alpha", "Beta"), 
  what = "grades", 
  by = "school", 
  time_var = "year", 
  metrics = c("numerator", "denominator", "mean"), proportion = F)

grades.distribution[level %in% c("A", "B")]
```

```
##    school level      year variable denominator      mean    mean_se mean_lower mean_upper numerator
## 1:  Alpha     A 2016-2021   grades         497 0.2857143 0.02028435  0.2477598  0.3269560       142
## 2:  Alpha     B 2016-2021   grades         497 0.2555332 0.01958418  0.2191639  0.2956526       127
## 3:   Beta     A 2016-2021   grades         491 0.2484725 0.01952152  0.2123012  0.2885490       122
## 4:   Beta     B 2016-2021   grades         491 0.2566191 0.01973114  0.2199795  0.2970375       126
```

These results show that the Alpha School has a higher proportion of A's (0.286 vs 0.248) and a similar proportion of B's (0.256 vs 0.257).

**Wait! We forgot the survey weights!**

A colleague just reminded you that you forgot to add the survey weights. Let's add weights and survey set the data.


```r
# create weights
set.seed(98121)
mydt[, mywghts := sample(50:1300, 2000, replace = T)]

# survey set the data
# This uses the dtsurvey package
# similar logic applies for survey and srvyr package
mydt[, `_id` := NULL] # remove id to make things play nice
mysvy <-dtsurvey::dtsurvey(data.table(mydt), weight = 'mywghts')
```

**Using the survey information, again calculate the proportion of A's and B's in the Alpha and Beta schools**


```r
grades.distribution2 <- calc(
  ph.data = mysvy, 
  school %in% c("Alpha", "Beta"), 
  what = "grades", 
  by = "school", 
  time_var = "year", 
  metrics = c("numerator", "denominator", "mean"), proportion = FALSE)

grades.distribution2[level %in% c("A", "B")]
```

```
##    school level      year variable denominator      mean    mean_se mean_lower mean_upper numerator
## 1:  Alpha     A 2016-2021   grades         497 0.2819952 0.02280603  0.2371869  0.3268036       142
## 2:  Alpha     B 2016-2021   grades         497 0.2460211 0.02157805  0.2036254  0.2884167       127
## 3:   Beta     A 2016-2021   grades         491 0.2361366 0.02142621  0.1940380  0.2782351       122
## 4:   Beta     B 2016-2021   grades         491 0.2685865 0.02295556  0.2234831  0.3136900       126
```

You'll note that using the survey design caused small changes in the results. For examples, the proportion of A's in the Alpha school changed from 0.286 to 0.282.

------------------------------------------------------------------------

## Knowing is half the battle ... but only half

You've been introduced to `calc()`, but you'll only become competent at using it by using it. Try it out with your favorite dataset. Use it to answer data requests. Play with it. Try to break it and, if you're successful, submit a [GitHub issue](https://github.com/PHSKC-APDE/rads/issues/new). Enjoy!
