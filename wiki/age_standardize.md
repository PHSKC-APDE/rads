
## Introduction


The comparison of crude health indicator rates across populations is often confounded by different age structures. For example, in 2019 the mean age of Hispanic King County residents was ~12.5 years younger than that of white residents (27.5 vs 40). Given this age discrepancy, we should expect higher mortality rates among white residents. However, you might be thinking, *“Is there a way to compare Hispanic and white mortality rates while fully accounting for their different age structures? I.e., is there a way to standardize or adjust for age?”* 

While you can use Poisson regression or other advanced statistical techniques to calculate age adjusted rates, for simplicity and broader comparison with the work of others, it is often beneficial to adjust your data using a standard reference population. This process is called ***age standardization***. Age standardization answers the question, *“What rates would I observe if my data had the same age structure as a different population?”*

There are 35 reference populations baked into `rads` (via its dependency on [`rads.data`](https://github.com/PHSKC-APDE/rads.data)). Their names are visible when you type `list_ref_pop()`. Here are the first five as an example:

``` r
list_ref_pop()[1:5]
```

```
## [1] "2000 U.S. Std Population (11 age groups)"                       "2000 U.S. Std Population (18 age groups - Census P25-1130)"     "2000 U.S. Std Population (19 age groups - Census P25-1130)"    
## [4] "2000 U.S. Std Population (single ages to 84 - Census P25-1130)" "2000 U.S. Std Population (single ages to 99 - Census P25-1130)"
```

The first one is the default reference population used by APDE and WA DOH. Taking a peek behind the curtain shows that each reference standard is a simple table of age ranges with their corresponding populations. 


``` r
get_ref_pop("2000 U.S. Std Population (11 age groups)")[, 1:4]
```

```
##          agecat age_start age_end      pop
##          <char>     <int>   <int>    <int>
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

1) `ph.data`: a data.table or data.frame containing the data to be age-standardized. At a minimum, it must include the following columns:
    - an integer column named 'age' representing individual ages, OR a column named 'agecat' containing pre-aggregated age categories that match the bins in the reference population
    - a numeric column of event counts (see `my.count` below)
    - a numeric column of population counts (see `my.pop` below)

2) `ref.popname` <- a character vector whose only valid options are "none" (used when ph.data has a 'stdpop' column) or one of the reference standards contained in `list_ref_pop()`

3) `collapse` <- a logical vector (T|F), determines whether or not to to collapse the 'age' column in ph.data to match those in ref.popname. If your data is already collapsed / aggregated into the same age bins as your reference population, and if that variable is named "agecat", then you can set collapse to F. The default is T

4) `my.count` <- a character vector, identifies the column name for the **aggregated** event count data in ph.data. The default is "count"

5) `my.pop` <- a character vector, identifies the column name for the population number corresponding to the count. The default is "pop"

6) `per` <- an integer, used as multiplier for all rates and CI, e.g., when per = 1000, the rates are per 1000 persons. The default is 100,000

7) `conf.level` <- a numeric value between 0.00 & 0.99, designates the confidence level (i.e., 1 - alpha) to be used in the calculations. The default is 0.95 

8) `group_by` <- a character vector, specifies the variable(s) by which to stratify the rate results (if any). The default is NULL (i.e., no stratification)


## Preparing your dataset
Your dataset must have the following three columns ... 

1) 'age' or 'agecat': 'age' in single years (if collapse = T) or 'agecat' with the same age bins as your selected reference population (if collapse = F)

2) an **aggregated** count for the event (e.g., disease) for which you want to find an age standardized rate

3) the population corresponding to the age or agecat in your original data

***

## Example #1: The Basics
**Create synthetic line level data for a cohort 51 to 60 years of age with a binary indicator for disease status**

``` r
library(data.table)
set.seed(98104)

temp1 <- data.table(
  age = rep(51:60, 100), 
  disease = sample(0:1, 1000, replace = T), 
  pop = rep(c(seq(1000, 910, -10)), 100))

head(temp1)
```

```
##      age disease   pop
##    <int>   <int> <num>
## 1:    51       0  1000
## 2:    52       1   990
## 3:    53       1   980
## 4:    54       1   970
## 5:    55       0   960
## 6:    56       1   950
```

**Run age_standardize with APDE's default reference population**

``` r
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
##    <num>  <num>      <num>     <num>     <num>    <num>   <num>   <num>                                   <char>
## 1:   491 955000       0.51      0.47      0.56     0.51    0.47    0.56 2000 U.S. Std Population (11 age groups)
```
*Does anything seem fishy? Think for a moment ...* 

* **count** is the number of events that occured (i.e., the numerator)

* **pop** is the population corresponding to the count (i.e., the denominator)

* **crude.rate, crude.lci, & crude.uci** are the crude (i.e., observed) rate and CI

* **adj.rate, adj.lci, & adj.uci** are the age standardized rate and CI

*Did you find the problem?*

The problem is that the population is too big. Eyeballing the output when we created the dataset above, we see that the total population for those 51 to 60 would have to be less than 10,000 (10 age groups with max 1,000 population for each). In this case it is almost 1 million! In the description of the arguments above, it specifies that we need to ***use aggregated count data***. Now you see why. Ignoring that detail caused us to inflate the population (and therefore deflate the rate and CI) by 1000x.

**Aggregate (sum) the line level data**

``` r
temp1 <- temp1[, .(disease = sum(disease)), by = c("age", "pop")]
head(temp1)
```

```
##      age   pop disease
##    <int> <num>   <int>
## 1:    51  1000      45
## 2:    52   990      52
## 3:    53   980      57
## 4:    54   970      47
## 5:    55   960      54
## 6:    56   950      46
```

**Run age_standardize again << this time with aggregated disease events**

``` r
ex1.1 <- age_standardize(ph.data = temp1,
                      ref.popname = "2000 U.S. Std Population (11 age groups)", 
                      collapse = T,
                      my.count = "disease", 
                      my.pop = "pop", 
                      per = 1000, 
                      conf.level = 0.95)
head(ex1.1)
```

```
##    count   pop crude.rate crude.lci crude.uci adj.rate adj.lci adj.uci                            reference_pop
##    <num> <num>      <num>     <num>     <num>    <num>   <num>   <num>                                   <char>
## 1:   491  9550      51.41     46.97     56.17    51.28   46.52   56.44 2000 U.S. Std Population (11 age groups)
```

Now that count, pop, and rates seem reasonable, let's see what happens if we change the reference population. We'll arbitrarily set the references population to the 36th in the list provided by `list_ref_pop()` (i.e., 'World (WHO 2000-2025) Std Million (single ages to 99)'). 


``` r
ex1.2 <- age_standardize(ph.data = temp1,
                      ref.popname = list_ref_pop()[36], 
                      collapse = T,
                      my.count = "disease", 
                      my.pop = "pop", 
                      per = 1000, 
                      conf.level = 0.95)
head(ex1.2)
```

```
##    count   pop crude.rate crude.lci crude.uci adj.rate adj.lci adj.uci                                         reference_pop
##    <num> <num>      <num>     <num>     <num>    <num>   <num>   <num>                                                <char>
## 1:   491  9550      51.41     46.97     56.17    51.34   46.89   56.12 World (WHO 2000-2025) Std Million (single ages to 99)
```
As we'd expect, the crude rates are identical since the reference population is irrelevant for those calculations (51.41 vs 51.41). However, the age-standardized rates changed from 51.28 to 51.34. Remember, if you want to compare your age-adjusted rates to those published by other health jurisdictions, it's important that you use the same reference population.

***

## Example #2: Stratification
**Let's create a new dataset, with counts aggregated by age and gender (F|M)**

``` r
set.seed(98104)
temp2 <- data.table(
  gender = sample(c('F', 'M'), 20000, replace = T), 
  age = sample(0:100, 20000, replace = T),
  disease = sample(0:1, 20000, replace = T))
temp2 <- temp2[, .(pop = .N, disease = sum(disease)), .(gender, age)]
setorder(temp2, age, gender)

head(temp2)
```

```
##    gender   age   pop disease
##    <char> <int> <int>   <int>
## 1:      F     0    96      45
## 2:      M     0   106      47
## 3:      F     1    89      40
## 4:      M     1   106      51
## 5:      F     2    91      42
## 6:      M     2    95      47
```

**Let's examine the overall rates**

``` r
ex2.1 <- age_standardize(ph.data = temp2,
                       collapse = T,
                       my.count = "disease", 
                       my.pop = "pop", 
                       per = 1000, 
                       conf.level = 0.95)

head(ex2.1)
```

```
##    count   pop crude.rate crude.lci crude.uci adj.rate adj.lci adj.uci                            reference_pop
##    <num> <num>      <num>     <num>     <num>    <num>   <num>   <num>                                   <char>
## 1: 10157 20000     507.85    498.02    517.82   511.68  500.49  523.07 2000 U.S. Std Population (11 age groups)
```

Notice that we didn't include the `ref.popname` argument. When it is not specified, `age_standardize()` uses the default which is `list_ref_pop()[1]` (i.e, 2000 U.S. Std Population (11 age groups)).

**Now let's run the same analysis, but stratified by gender**

``` r
ex2.2 <- age_standardize(ph.data = temp2,
                       collapse = T,
                       my.count = "disease", 
                       my.pop = "pop", 
                       per = 1000, 
                       conf.level = 0.95, 
                       group_by = "gender")

head(ex2.2)
```

```
##    gender count   pop crude.rate crude.lci crude.uci adj.rate adj.lci adj.uci                            reference_pop
##    <char> <num> <num>      <num>     <num>     <num>    <num>   <num>   <num>                                   <char>
## 1:      F  5050  9946     507.74    493.83    521.94   513.35  497.55  529.56 2000 U.S. Std Population (11 age groups)
## 2:      M  5107 10054     507.96    494.12    522.08   509.97  494.20  526.15 2000 U.S. Std Population (11 age groups)
```
Here we see that the crude rates are nearly identical (507.74 vs 507.96), but the adjusted rate is higher for females (513.35 vs 509.97).

***

## Example #3: Using your own standard population
**Create a reference population for the gendered dataset above**

To keep things simple, we will create a reference population based on single ages rather than age bins. As specified in the arguments description above, we will name the standard population column 'stdpop'.

``` r
set.seed(98104)
new.standard <- unique(temp2[, .(gender, age)])
new.standard[, stdpop := fifelse(gender == 'M', 
                                 sample(7800:16000, 1, replace = T), 
                                 sample(10000:20000, 1, replace = T))]

head(new.standard)
```

```
##    gender   age stdpop
##    <char> <int>  <int>
## 1:      F     0  13030
## 2:      M     0  14754
## 3:      F     1  13030
## 4:      M     1  14754
## 5:      F     2  13030
## 6:      M     2  14754
```

**Merge the standard population onto the data**

``` r
temp3 <- merge(temp2, new.standard, by = c("age", "gender"), all = T)

head(temp3)
```

```
## Key: <age, gender>
##      age gender   pop disease stdpop
##    <int> <char> <int>   <int>  <int>
## 1:     0      F    96      45  13030
## 2:     0      M   106      47  14754
## 3:     1      F    89      40  13030
## 4:     1      M   106      51  14754
## 5:     2      F    91      42  13030
## 6:     2      M    95      47  14754
```

**Calculate the rates when `ref.pop = "none"`**

The data set already contains a `stdpop` column, so we can set `ref.popname = "none"`. Also note that I need to specify `collapse = F` because the function expects data to be pre-aggregated when I have the column `stdpop`.

``` r
ex3.1 <- age_standardize(ph.data = temp3,
                       ref.popname = "none",
                       collapse = F,
                       my.count = "disease", 
                       my.pop = "pop", 
                       per = 1000, 
                       conf.level = 0.95, 
                       group_by = "gender")

head(ex3.1)
```

```
##    gender count   pop crude.rate crude.lci crude.uci adj.rate adj.lci adj.uci            reference_pop
##    <char> <num> <num>      <num>     <num>     <num>    <num>   <num>   <num>                   <char>
## 1:      F  5050  9946     507.74    493.83    521.94   507.33  493.37  521.61 stdpop column in `temp3`
## 2:      M  5107 10054     507.96    494.12    522.08   508.91  495.00  523.14 stdpop column in `temp3`
```

Notice that the crude rates are identical to those in ex2.2, but the adjusted rates differ due to the use of our custom reference population. 

***

## Example #4: When else should I specify `collapse = F`?
In example #3 above, we specified `collapse = F` because `age_standardardize()` expects the data to be pre-aggregated when you provide a `stdpop` column. The other time when you will want to set `collapse = F` is if you have data that has already been collapsed down to a standard reference population's age bins. **This is uncommon.** It isn't worth your time and energy to manually collapse the data -- so don't do it! This functionality is here just in case you receive data that has already been structured this way.

**Let's reuse the dataset with age, disease, pop, and gender from example #2**

``` r
temp4 <- copy(temp2)
```

**Collapse the data down to the same age bins (`agecat`) as those used in the default APDE standard reference population**

``` r
temp4[age == 0, agecat := "0"]
temp4[age %in% 1:4, agecat := "1-4 years"]
temp4[age %in% 5:14, agecat := "5-14 years"]
temp4[age %in% 15:24, agecat := "15-24 years"]
temp4[age %in% 25:34, agecat := "25-34 years"]
temp4[age %in% 35:44, agecat := "35-44 years"]
temp4[age %in% 45:54, agecat := "45-54 years"]
temp4[age %in% 55:64, agecat := "55-64 years"]
temp4[age %in% 65:74, agecat := "65-74 years"]
temp4[age %in% 75:84, agecat := "75-84 years"]
temp4[age %in% 85:100, agecat := "85+ years"]

temp4 <- temp4[, .(count = sum(disease), pop = sum(pop)), .(agecat, gender)]

head(temp4[])
```

```
##        agecat gender count   pop
##        <char> <char> <int> <int>
## 1:          0      F    45    96
## 2:          0      M    47   106
## 3:  1-4 years      F   179   379
## 4:  1-4 years      M   200   397
## 5: 5-14 years      F   522   986
## 6: 5-14 years      M   500   963
```

**Now run age_standardize with `collapse = F`**

``` r
ex4.1 <- age_standardize(ph.data = copy(temp4),
                         ref.popname = "2000 U.S. Std Population (11 age groups)",
                         collapse = F, # because already collapsed
                         my.count = "count", 
                         my.pop = "pop", 
                         per = 1000, 
                         conf.level = 0.95, 
                         group_by = "gender")

head(ex4.1)
```

```
##    gender count   pop crude.rate crude.lci crude.uci adj.rate adj.lci adj.uci                            reference_pop
##    <char> <num> <num>      <num>     <num>     <num>    <num>   <num>   <num>                                   <char>
## 1:      F  5050  9946     507.74    493.83    521.94   513.35  497.55  529.56 2000 U.S. Std Population (11 age groups)
## 2:      M  5107 10054     507.96    494.12    522.08   509.97  494.20  526.15 2000 U.S. Std Population (11 age groups)
```

Note the results in ex4.1 are exactly the same as those in ex2.2. This makes sense since age_standardize() collapsed the data in a similar (but more efficient manner) when creating ex2.2.


***

## Example #5: Real world analysis

You're staffing data requests today and receive the following message: "*Hi! For a class, I'd like to get the 2019 teen (13 to 19) birth rate for King County and WA State as a whole. Since the age distribution in the county may differ from the rest of the state, it would be appreciated if you could provide crude and age-standardized rates. If you could standardize to "World (WHO 2000-2025) Std Million (single ages to 84)", that would be great! Thank you, S. Capstone*" 

You quickly pull up [CHAT](https://www.doh.wa.gov/ForPublicHealthandHealthcareProviders/PublicHealthSystemResourcesandServices/CommunityHealthAssessmentandImprovement/CHAT) only to find that this specific indicator doesn't exist. You remember that [CHI](https://kingcounty.gov/chi/) has an adolescent birth rate indicator, but are downcast when you find that it applies to those 15 to 17. In desperation, you resign yourself to using `rads`. 

**Get birth counts for 13 to 19 year olds in 2019**


``` r
  kcbirth <- get_data_birth(cols = c("chi_age", "chi_year"), year = 2019, kingco = T)
  wabirth <- get_data_birth(cols = c("chi_age", "chi_year"), year = 2019, kingco = F)
  
  births <- rbind(kcbirth[, geo := "King County"], wabirth[, geo := "WA State"])
  births <- births[chi_age %in% 13:19]
  
  # collapse / aggregate
  births <- births[, .(births = .N), by = c("chi_age", "geo")] 
  setorder(births, geo, chi_age)
  setnames(births, "chi_age", "age")
  head(births)
```

```
##      age         geo births
##    <int>      <char>  <int>
## 1:    13 King County      1
## 2:    14 King County      4
## 3:    15 King County     10
## 4:    16 King County     23
## 5:    17 King County     45
## 6:    18 King County    107
```

**Get the female populations corresponding to the birth counts**

``` r
  kcpop <- get_population(kingco = T, years = 2019, ages = 13:19, 
                          genders = "Female", group_by = "ages", 
                          geo_vintage = 2020, census_vintage = 2020)
  kcpop <- kcpop[, .(age, geo = geo_id, pop)] 
  
  wapop <- get_population(kingco = F, years = 2019, ages = 13:19, 
                          genders = "Female", group_by = "ages", 
                          geo_type = "zip", geo_vintage = 2020, 
                          census_vintage = 2020)
  wapop <- wapop[, .(pop = sum(pop), geo = "WA State"), by = "age"]
  
  pop <- rbind(kcpop, wapop)
  head(pop)
```

```
##      age         geo      pop
##    <int>      <char>    <num>
## 1:    18 King County 12835.83
## 2:    19 King County 12640.53
## 3:    13 King County 12389.62
## 4:    15 King County 12068.50
## 5:    16 King County 12049.00
## 6:    17 King County 12034.53
```

**Merge population onto to birth counts**

``` r
  temp5 <- merge(births, pop, by = c("age", "geo"), all = T)
  
  head(temp5)
```

```
## Key: <age, geo>
##      age         geo births      pop
##    <int>      <char>  <int>    <num>
## 1:    13 King County      1 12389.62
## 2:    13    WA State      6 46781.32
## 3:    14 King County      4 12186.03
## 4:    14    WA State     16 46250.60
## 5:    15 King County     10 12068.50
## 6:    15    WA State     70 45629.71
```
  
  
**Run `age_standardize()`**

``` r
  ex5.1 <- age_standardize(ph.data = temp5,
                           ref.popname = "World (WHO 2000-2025) Std Million (single ages to 84)", 
                           collapse = T,
                           my.count = "births", 
                           my.pop = "pop", 
                           per = 1000, 
                           conf.level = 0.95, 
                           group_by = "geo")
  head(ex5.1)
```

```
##            geo count       pop crude.rate crude.lci crude.uci adj.rate adj.lci adj.uci                                         reference_pop
##         <char> <num>     <num>      <num>     <num>     <num>    <num>   <num>   <num>                                                <char>
## 1: King County   420  86204.04       4.87      4.42      5.36     4.73    4.29    5.21 World (WHO 2000-2025) Std Million (single ages to 84)
## 2:    WA State  2864 322995.26       8.87      8.55      9.20     8.65    8.33    8.97 World (WHO 2000-2025) Std Million (single ages to 84)
```
According to your analysis, the crude King County teen birth rate is approximately half (4.87 / 8.87 = 0.549) of the WA State teen birth rate. This relationship remains largely unchanged after age-standardization  (4.73 / 8.65 = 0.547).
  
***

## Conclusion

If you've walked through this vignette and more or less understood what's going on, you're in good shape! If you're still confused, please walk through it again and then reach out if you still have questions. Good luck!

-- *Updated by dcolombara, 2024-07-25*
