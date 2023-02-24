---
title: "age_standardize()"
output:
  pdf_document: default
  rmarkdown::html_vignette: default
  github_document: default
  urlcolor: blue
vignette: |
  %\VignetteEngine{knitr::knitr} %\VignetteIndexEntry{age_standardize}
---

## Introduction


The comparison of crude health indicator rates across populations is often confounded by different age structures. For example, in 2019 the mean age of Hispanic King County residents was ~12.5 years younger than that of white residents (27.5 vs 40). Given this age discrepancy, we should expect higher mortality rates among white residents. However, you might be thinking, *“Is there a way to compare Hispanic and white mortality rates while fully accounting for their different age structures? I.e., is there a way to standardize or adjust for age?”* 

While you can use Poisson regression or other advanced statistical techniques to calculate age adjusted rates, for simplicity and broader comparison with the work of others, it is often beneficial to adjust your data using a standard reference population. This process is called ***age standardization***. Age standardization answers the question, *“What rates would I observe if my data had the same age structure as a different population?”*

There are 35 reference populations baked into `rads` (via it's dependency on [`rads.data`](https://github.com/PHSKC-APDE/rads.data)). Their names are visible when you type `list_ref_pop()`. Here are the first five as an example:

```r
list_ref_pop()[1:5]
```

```
## [1] "2000 U.S. Std Population (11 age groups)"                       "2000 U.S. Std Population (18 age groups - Census P25-1130)"    
## [3] "2000 U.S. Std Population (19 age groups - Census P25-1130)"     "2000 U.S. Std Population (single ages to 84 - Census P25-1130)"
## [5] "2000 U.S. Std Population (single ages to 99 - Census P25-1130)"
```

The first one is the default reference population used by APDE and WA DOH. Taking a peek behind the curtain shows that each reference standard is a simple table of age ranges with their corresponding populations. 


```r
get_ref_pop("2000 U.S. Std Population (11 age groups)")[, 1:4]
```

```
##          agecat age_start age_end      pop
##  1:           0         0       0  3795000
##  2:   1-4 years         1       4 15192000
##  3:  5-14 years         5      14 39977000
##  4: 15-24 years        15      24 38077000
##  5: 25-34 years        25      34 37233000
##  6: 35-44 years        35      44 44659000
##  7: 45-54 years        45      54 37030000
##  8: 55-64 years        55      64 23961000
##  9: 65-74 years        65      74 18136000
## 10: 75-84 years        75      84 12315000
## 11:   85+ years        85     120  4259000
```


The ` age_standardize()` function provides a simple way to use reference populations to calculate age standardized rates. This vignette will provide some examples of how to use ` age_standardize()` and how to understand its output. To get the most out of this vignette, we recommend that you type each and every bit of code into R. Doing so will almost definitely help you learn the syntax much faster than just reading the vignette or copying and pasting the code.

## `age_standardize()` arguments
Arguments are the values that we send to a function when it is called. The standard arguments for `age_standardize()` are:

1) `ph.data` <- the name of the data.table/data.frame with the data that you want to age standardize. **Note:** *ph.data must contain a numeric column named 'age'. The sole exception is if it already has a standard population merged onto the dataset, in which case it must contain a numeric column named 'stdpop'.*

2) `ref.popname` <- a character vector whose only valid options are "none" (used when ph.data has a 'stdpop' column) or one of the reference standarards contained in `list_ref_pop()`

3) `collapse` <- a logical vector (T|F), determines whether or not to to collapse the 'age' column in ph.data to match those in ref.popname. If your data is already collapsed / aggregated into the same age bins as your reference population, and if that variable is named "agecat", then you can set collapse to F. The default is T

4) `my.count` <- a character vector, identifies the column name for the **aggregated** count data in ph.data. The default is "count"

5) `my.pop` <- a character vector, identifies the column name for the population number corresponding to the count. The default is "pop"

6) `per` <- an integer, used as multiplier for all rates and CI, e.g., when per = 1000, the rates are per 1000 persons. The default is 100,000

7) `conf.level` <- a numeric value between 0.00 & 0.99, designates the confidence level (i.e., 1 - alpha) to be used in the calculations. The default is 0.95 

8) `group_by` <- a character, specifies the variable(s) by which to stratify the rate results (if any). The default is NULL (i.e., no stratification)


## Preparing your datset
Your dataset must have the following three columns ... 

1) 'age' or 'agecat': 'age' in single years (if collapse = T) or 'agecat' with the same age bins as your selected reference population (if collapse = F).

2) a count for the event (e.g., disease) for which you want to find an age standardized rate.

3) the population corresponding to the age or agecat in your original data

***

## Example #1: The Basics
**Create synthetic line level data for a cohort 51 to 60 years of age with a binary indicator for disease status**

```r
library(data.table)
set.seed(98121)

temp1 <- data.table(
  age = rep(51:60, 100), 
  disease = sample(0:1, 1000, replace = T), 
  pop = rep(c(seq(1000, 910, -10)), 100))

temp1[]
```

```
##       age disease  pop
##    1:  51       0 1000
##    2:  52       1  990
##    3:  53       0  980
##    4:  54       1  970
##    5:  55       1  960
##   ---                 
##  996:  56       1  950
##  997:  57       1  940
##  998:  58       1  930
##  999:  59       1  920
## 1000:  60       0  910
```

**Run age_standardize with APDE's default reference population**

```r
age_standardize(ph.data = temp1,
                ref.popname = "2000 U.S. Std Population (11 age groups)", 
                collapse = T,
                my.count = "disease", 
                my.pop = "pop", 
                per = 1000, 
                conf.level = 0.95)[]
```

```
##    count    pop crude.rate crude.lci crude.uci adj.rate adj.lci adj.uci                            reference_pop
## 1:   511 955000       0.54      0.49      0.58     0.52    0.47    0.57 2000 U.S. Std Population (11 age groups)
```
*Does anything seem fishy? Think for a moment ...* 

* **count** is the number of events that occured (i.e., the numerator)

* **pop** is the population corresponding to the count (i.e., the denominator)

* **crude.rate, crude.lci, & crude.uci** are the crude (i.e., observed) rate and CI

* **adj.rate, adj.lci, & adj.uci** are the age standardized rate and CI

*Did you find the problem?*

The problem is that the population is too big. Eyeballing the output when we created the dataset above, we see that the total population for those 51 to 60 would have to be less than 10,000 (10 age groups with max 1,000 population for each). In this case it is almost 1 million! In the description of the arguments above, it specifies that we need to *use aggregated count data*. Now you see why. Ignoring that detail caused us to inflate the population (and therefore deflate the rate and CI) 1000x.

**Aggregate (collapse) the line level data**

```r
temp1 <- temp1[, .(disease = sum(disease)), by = c("age", "pop")]
temp1[]
```

```
##     age  pop disease
##  1:  51 1000      49
##  2:  52  990      46
##  3:  53  980      50
##  4:  54  970      47
##  5:  55  960      60
##  6:  56  950      50
##  7:  57  940      61
##  8:  58  930      50
##  9:  59  920      52
## 10:  60  910      46
```

**Run age_standardize again << this time with aggregated disease events**

```r
ex1.1 <- age_standardize(ph.data = temp1,
                      ref.popname = "2000 U.S. Std Population (11 age groups)", 
                      collapse = T,
                      my.count = "disease", 
                      my.pop = "pop", 
                      per = 1000, 
                      conf.level = 0.95)
ex1.1[]
```

```
##    count  pop crude.rate crude.lci crude.uci adj.rate adj.lci adj.uci                            reference_pop
## 1:   511 9550      53.51     48.97     58.35    51.93   47.19   57.05 2000 U.S. Std Population (11 age groups)
```
Now that count, pop, and rates seem reasonable, let's see what happens if we change the reference population. We'll arbitrarily set the references population to the 36th in the list provided by `list_ref_pop()` (i.e., 'World (WHO 2000-2025) Std Million (single ages to 99)'). 


```r
ex1.2 <- age_standardize(ph.data = temp1,
                      ref.popname = list_ref_pop()[36], 
                      collapse = T,
                      my.count = "disease", 
                      my.pop = "pop", 
                      per = 1000, 
                      conf.level = 0.95)
ex1.2[]
```

```
##    count  pop crude.rate crude.lci crude.uci adj.rate adj.lci adj.uci                                         reference_pop
## 1:   511 9550      53.51     48.97     58.35    53.34   48.81    58.2 World (WHO 2000-2025) Std Million (single ages to 99)
```
As we'd expect, the crude rates are identical since the reference population is irrelevant for those calculations (53.51 vs 53.51). However, the age-standardized rates changed from 51.93 to 53.34. Remember, if you want to compare your age-adjusted rates to those published by other health jurisdictions, it's important that you use the same reference population.

***

## Example #2: Stratifcation
**Let's create a new dataset with disease counts aggregated by age (46 to 64) and gender (F|M)**

```r
set.seed(98121)
temp2 <- data.table(
  gender = c(rep("F", 20), rep("M", 20)), 
  age = rep(46:65, 2),
  disease = c(sample(25:46, 20, replace = T), sample(25:35, 20, replace = T)), 
  pop = c(sample(2500:3500, 20, replace = T), sample(2200:3300, 20, replace = T)))

head(temp2)
```

```
##    gender age disease  pop
## 1:      F  46      32 3134
## 2:      F  47      36 2930
## 3:      F  48      29 3289
## 4:      F  49      27 3198
## 5:      F  50      27 3480
## 6:      F  51      33 2679
```

**Let's examine the overall rates**

```r
ex2.1 <- age_standardize(ph.data = temp2,
                       collapse = T,
                       my.count = "disease", 
                       my.pop = "pop", 
                       per = 1000, 
                       conf.level = 0.95)

ex2.1[]
```

```
##    count    pop crude.rate crude.lci crude.uci adj.rate adj.lci adj.uci                            reference_pop
## 1:  1333 115448      11.55     10.93     12.18    11.68   10.87   12.57 2000 U.S. Std Population (11 age groups)
```
In this case, the crude rate and age standardized rate are the same. This doesn't mean there is a mistake. Notice that the confidence intervals differ -- as would be expected. 

Also notice that we didn't include the `ref.popname` argument. When it is not specified, `age_standardize()` uses the default which is `list_ref_pop()[1]` (i.e, 2000 U.S. Std Population (11 age groups)).

**Now let's run the same analysis, but stratified by gender**

```r
ex2.2 <- age_standardize(ph.data = temp2,
                       collapse = T,
                       my.count = "disease", 
                       my.pop = "pop", 
                       per = 1000, 
                       conf.level = 0.95, 
                       group_by = "gender")

ex2.2[]
```

```
##    gender count   pop crude.rate crude.lci crude.uci adj.rate adj.lci adj.uci                            reference_pop
## 1:      F   731 60827      12.02     11.16     12.92    12.22   11.07   13.50 2000 U.S. Std Population (11 age groups)
## 2:      M   602 54621      11.02     10.16     11.94    11.09    9.97   12.36 2000 U.S. Std Population (11 age groups)
```
Here we see that the crude and age standardized rates are higher among females when compared to males. 

***

## Example #3: Using your own standard population
**Create a reference population for the gendered dataset above**

To keep things simple, we will create a reference population based on single ages rather than age bins. As specified in the arguments description above, we will name the standard population column 'stdpop'.

```r
set.seed(98121)
new.standard <- data.table(
  gender = c(rep("M", 20), rep("F", 20)), 
  age = rep(46:65, 2),
  stdpop = c(sample(7800:16000, 20, replace = T), sample(10000:20000, 20, replace = T)))

head(new.standard)
```

```
##    gender age stdpop
## 1:      M  46  10160
## 2:      M  47   8515
## 3:      M  48   9204
## 4:      M  49   7827
## 5:      M  50   9756
## 6:      M  51   9178
```

**Merge the standard population onto the data**

```r
temp3 <- merge(temp2, new.standard, by = c("age", "gender"), all = T)

head(temp3)
```

```
##    age gender disease  pop stdpop
## 1:  46      F      32 3134  14506
## 2:  46      M      26 3055  10160
## 3:  47      F      36 2930  18955
## 4:  47      M      35 2954   8515
## 5:  48      F      29 3289  17813
## 6:  48      M      35 2268   9204
```

**Calculate the rates when `ref.pop = "none"`**

Note that I need to specify `collapse = F` because the function expects data to be pre-aggregated for the custom reference population contained in `stdpop`.

```r
ex3.1 <- age_standardize(ph.data = temp3,
                       ref.popname = "none",
                       collapse = F,
                       my.count = "disease", 
                       my.pop = "pop", 
                       per = 1000, 
                       conf.level = 0.95, 
                       group_by = "gender")

ex3.1[]
```

```
##    gender count   pop crude.rate crude.lci crude.uci adj.rate adj.lci adj.uci            reference_pop
## 1:      F   731 60827      12.02     11.16     12.92    12.02   11.15   12.95 stdpop column in `temp3`
## 2:      M   602 54621      11.02     10.16     11.94    11.15   10.25   12.12 stdpop column in `temp3`
```

***

## Example #4: When to specify `collapse = F`
In example #3 above, we specified `collapse = F` because `age_standardardize()` expects the data to be pre-aggregated when you provide a `stdpop` column. The other time when you will want to set `collapse = F` is if you have data that has already been collapsed down to the reference population's age bins along with the proper labels in a column called `agecat`. **This is uncommon.** It isn't worth your time and energy to manually collapse the data -- so don't do it! This functionality is here just in case you receive data that has already been structured this way.

**Let's recreate the dataset used in example 2 above**

```r
set.seed(98121)
temp4 <- data.table(
  gender = c(rep("M", 20), rep("F", 20)), 
  age = rep(46:65, 2),
  disease = c(sample(25:46, 20, replace = T), sample(25:35, 20, replace = T)), 
  pop = c(sample(2500:3500, 20, replace = T), sample(2200:3300, 20, replace = T)))

head(temp4)
```

```
##    gender age disease  pop
## 1:      M  46      32 3134
## 2:      M  47      36 2930
## 3:      M  48      29 3289
## 4:      M  49      27 3198
## 5:      M  50      27 3480
## 6:      M  51      33 2679
```

**Collapse the data down to the same age bins as those used in the APDE standard reference population**

```r
temp4[age %in% 45:49, agecat := "45-49 years"]
temp4[age %in% 50:54, agecat := "50-54 years"]
temp4[age %in% 55:59, agecat := "55-59 years"]
temp4[age %in% 60:64, agecat := "60-64 years"]
temp4[age %in% 65:69, agecat := "65-69 years"]
temp4 <- temp4[, .(pop = sum(pop), disease = sum(disease)), by = c("agecat", "gender")]

temp4[]
```

```
##          agecat gender   pop disease
##  1: 45-49 years      M 12551     124
##  2: 50-54 years      M 15192     169
##  3: 55-59 years      M 15466     198
##  4: 60-64 years      M 14425     194
##  5: 65-69 years      M  3193      46
##  6: 45-49 years      F 11302     130
##  7: 50-54 years      F 13392     149
##  8: 55-59 years      F 12631     159
##  9: 60-64 years      F 14498     133
## 10: 65-69 years      F  2798      31
```

**Now you are able to run age_standardize with `collapse = F`**

```r
ex4.1 <- age_standardize(ph.data = temp4,
                       collapse = F,
                       my.count = "disease", 
                       my.pop = "pop", 
                       per = 1000, 
                       conf.level = 0.95, 
                       group_by = "gender")

ex4.1[]
```

```
## Empty data.table (0 rows and 10 cols): gender,count,pop,crude.rate,crude.lci,crude.uci...
```
Note the results in ex4.1 are exactly the same as those in ex2.2. This makes sense since `age_standardize()` collapsed the data in a similar (but more efficient manner) when creating ex2.2.

***

## Example #5: Real world analysis

You're staffing data requests today and receive the following message: "*Hi! For a class, I'd like to get the 2019 teen (13 to 19) birth rate for King County and WA State as a whole. Since the age distribution in the county may differ from the rest of the state, it would be appreciated if you could provide crude and age-standardized rates. If you could standardize to "World (WHO 2000-2025) Std Million (single ages to 84)", that would be great! Thank you, S. Capstone*" 

You quickly pull up [CHAT](https://www.doh.wa.gov/ForPublicHealthandHealthcareProviders/PublicHealthSystemResourcesandServices/CommunityHealthAssessmentandImprovement/CHAT) only to find that this specific indicator doesn't exist. You remember that [CHI](https://kingcounty.gov/chi/) has an adolescent birth rate indicator, but are downcast when you find that it applies to those 15 to 17. In desperation, you resign yourself to using `rads`. 

**Get birth counts for 13 to 19 year olds in 2019**


```r
  kcbirth <- get_data_birth(cols = c("chi_age", "chi_year"), year = 2019, kingco = T)
  wabirth <- get_data_birth(cols = c("chi_age", "chi_year"), year = 2019, kingco = F)
  
  births <- rbind(kcbirth[, geo := "King County"], wabirth[, geo := "WA State"])
  births <- births[chi_age %in% 13:19]
  
  # collapse / aggregate
  births <- births[, .(births = .N), by = c("chi_age", "geo")] 
  setorder(births, geo, chi_age)
  setnames(births, "chi_age", "age")
  births[]
```

```
##     age         geo births
##  1:  13 King County      1
##  2:  14 King County      4
##  3:  15 King County     10
##  4:  16 King County     23
##  5:  17 King County     44
##  6:  18 King County    111
##  7:  19 King County    227
##  8:  13    WA State      6
##  9:  14    WA State     16
## 10:  15    WA State     69
## 11:  16    WA State    190
## 12:  17    WA State    348
## 13:  18    WA State    751
## 14:  19    WA State   1481
```

**Get the female populations corresponding to the birth counts**

```r
  kcpop <- get_population(kingco = T, years = 2019, ages = 13:19, 
                          genders = "Female", group_by = "ages", geo_vintage = 2020, census_vintage = 2020)
  kcpop <- kcpop[, .(age, geo = geo_id, pop)] 
  
  wapop <- get_population(kingco = F, years = 2019, ages = 13:19, 
                          genders = "Female", group_by = "ages", geo_type = "zip", geo_vintage = 2020, census_vintage = 2020)
  wapop <- wapop[, .(pop = sum(pop), geo = "WA State"), by = "age"]
  
  pop <- rbind(kcpop, wapop)
  pop[]
```

```
##     age         geo      pop
##  1:  18 King County 12658.91
##  2:  19 King County 13378.52
##  3:  13 King County 12506.60
##  4:  15 King County 11090.13
##  5:  16 King County 11462.55
##  6:  17 King County 11566.49
##  7:  14 King County 11991.38
##  8:  16    WA State 44501.15
##  9:  17    WA State 43450.77
## 10:  15    WA State 44360.21
## 11:  19    WA State 47588.43
## 12:  14    WA State 46092.01
## 13:  13    WA State 46647.21
## 14:  18    WA State 46040.50
```

**Merge population onto to birth counts**

```r
  temp5 <- merge(births, pop, by = c("age", "geo"), all = T)
  
  temp5[]
```

```
##     age         geo births      pop
##  1:  13 King County      1 12506.60
##  2:  13    WA State      6 46647.21
##  3:  14 King County      4 11991.38
##  4:  14    WA State     16 46092.01
##  5:  15 King County     10 11090.13
##  6:  15    WA State     69 44360.21
##  7:  16 King County     23 11462.55
##  8:  16    WA State    190 44501.15
##  9:  17 King County     44 11566.49
## 10:  17    WA State    348 43450.77
## 11:  18 King County    111 12658.91
## 12:  18    WA State    751 46040.50
## 13:  19 King County    227 13378.52
## 14:  19    WA State   1481 47588.43
```
  
  
**Run `age_standardize()`**

```r
  ex5.1 <- age_standardize(ph.data = temp5,
                           ref.popname = "World (WHO 2000-2025) Std Million (single ages to 84)", 
                           collapse = T,
                           my.count = "births", 
                           my.pop = "pop", 
                           per = 1000, 
                           conf.level = 0.95, 
                           group_by = "geo")
  ex5.1[]
```

```
##            geo count       pop crude.rate crude.lci crude.uci adj.rate adj.lci adj.uci                                         reference_pop
## 1: King County   420  84654.57       4.96      4.50      5.46     4.65    4.22    5.12 World (WHO 2000-2025) Std Million (single ages to 84)
## 2:    WA State  2861 318680.29       8.98      8.65      9.31     8.74    8.43    9.07 World (WHO 2000-2025) Std Million (single ages to 84)
```
According to your analysis, the crude King County teen birth rate is approximately half (4.96 / 8.98 = 0.552) of the WA State teen birth rate. This relationship remains largely unchanged after age-standardization  (4.65 / 8.74 = 0.532).
  
***

## Conclusion

If you've walked through this vignette and more or less understood what's going on, you're in good shape! If you're still confused, please walk through it again and then reach out if you still have questions. Good luck!
