---
title: "Calculating rates with rads"
output:
  pdf_document: default
  rmarkdown::html_vignette: default
  github_document: default
  urlcolor: blue
vignette: |
  %\VignetteEngine{knitr::knitr} %\VignetteIndexEntry{calculating_rates_with_rads}
---



# Introduction

Many PHSKC metrics are simple counts or means/proportions/fractions. The `rads::calc()` function was created to calculate these metrics from survey and line-level data. However, there are certain metrics (e.g., [adolescent birth rate](https://kingcounty.gov/depts/health/data/community-health-indicators/washington-state-vital-statistics-birth.aspx?shortname=Adolescent%20birth%20rate)) and datasets (e.g., death, CHARS, etc.) where **true rates --** **with a population denominator** -- are of interest. Most of the time these rates need to be [age-adjusted](https://github.com/PHSKC-APDE/rads/wiki/age_standardize) and most of the time DOH and APDE adjust to the US 2000 Standard Population with 11 age groups (see `rads.data::population_reference_pop_11_age_groups`). The [rads package](https://github.com/PHSKC-APDE/rads/) has a suite of tools that were developed to facilitate these types of analyses. This vignette explains how to use these tools in **four steps**.

# The four steps: an overview

**STEP 1:** Create a **summary table of event counts** (deaths, hospitalizations, etc.) by single year age groups and the strata of interest. For example, part of your your table might look like this:

| event_name | counts | age | year | region  |
|:-----------|:-------|:----|:-----|:--------|
| CVD death  | 26     | 47  | 2021 | Seattle |
| CVD death  | 17     | 47  | 2021 | North   |
| CVD death  | 28     | 47  | 2021 | East    |
| CVD death  | 36     | 47  | 2021 | South   |
| CVD death  | 23     | 48  | 2021 | Seattle |
| CVD death  | 28     | 48  | 2021 | North   |
| CVD death  | 36     | 48  | 2021 | East    |
| CVD death  | 24     | 48  | 2021 | South   |

**STEP 2:** Create a **table of population** denominators corresponding to the ages and demographics in the table created in Step 1.

| age | year | region  | population |
|:----|:-----|:--------|:-----------|
| 47  | 2021 | Seattle | 11353      |
| 47  | 2021 | North   | 347        |
| 47  | 2021 | East    | 8203       |
| 47  | 2021 | South   | 6432       |
| 48  | 2021 | Seattle | 3543       |
| 48  | 2021 | North   | 3041       |
| 48  | 2021 | East    | 425        |
| 48  | 2021 | South   | 7124       |

**STEP 3: Merge** the denominators (population) onto the table with the numerators (counts) using age and the strata of interest.

| event_name | counts | age | year | region  | population |
|:-----------|:-------|:----|:-----|:--------|:-----------|
| CVD death  | 26     | 47  | 2021 | Seattle | 11353      |
| CVD death  | 17     | 47  | 2021 | North   | 347        |
| CVD death  | 28     | 47  | 2021 | East    | 8203       |
| CVD death  | 36     | 47  | 2021 | South   | 6432       |
| CVD death  | 23     | 48  | 2021 | Seattle | 3543       |
| CVD death  | 28     | 48  | 2021 | North   | 3041       |
| CVD death  | 36     | 48  | 2021 | East    | 425        |
| CVD death  | 24     | 48  | 2021 | South   | 7124       |

**STEP 4:** Use [**rads::age_standardize()**](https://github.com/PHSKC-APDE/rads/wiki/age_standardize) to calculate the crude and age-adjusted rates

# The four steps: example 1

As a simple example, let's assess the King County death rate from falls in 2016 through 2020, by intent.

### STEP 1: Create a summary **table of event counts**

We will pull down the relevant death data using `rads::get_data_death` and use `rads::death_injury_matrix_count` to process the ICD-10 death codes into human readable mechanisms and intents.

Let's use `rads` to get our death data and check if it seems reasonable:


```r
deaths <- get_data_death(cols = c('chi_age', 'chi_year', 'chi_geo_seattle', 
                                  'underlying_cod_code'), 
                               year = c(2016:2020), 
                               kingco = T)
deaths[, `_id` := NULL] # delete _id, a column created by / for the rads package 
names(deaths) 
```

```
## [1] "underlying_cod_code" "chi_age"             "chi_year"            "chi_geo_seattle"
```

```r
nrow(deaths) # should be ~65,000 based on apriori knowledge
```

```
## [1] 67260
```

We see that the column names and the number of rows are what we expected. Now, let's identify the deaths from falls using the `rads::death_injury_matrix_count` function. To learn about this function, type `?death_injury_matrix_count` in your console. For now, just trust that this is the correct syntax:


```r
deaths <- death_injury_matrix_count(ph.data = deaths,
                               intent = "*", # all five intents
                               mechanism = "fall",
                               icdcol = 'underlying_cod_code',
                               byage = TRUE,
                               death_age_col = 'chi_age',
                               kingco = F) # False because already subset
head(deaths[deaths >9]) # only display non-suppressed data
```

```
##    mechanism        intent chi_age chi_year chi_geo_seattle deaths rank total_deaths
## 1:      Fall Unintentional      86     2016           FALSE     12    3          245
## 2:      Fall Unintentional      86     2018           FALSE     10    2          302
## 3:      Fall Unintentional      89     2016           FALSE     13    1          265
## 4:      Fall Unintentional      91     2016           FALSE     12    3          255
## 5:      Fall Unintentional      93     2018           FALSE     10    2          234
```

Glancing at the output from the `death_injury_matrix_count` function, we see the that we have **aggregated** `deaths` by `chi_age` and stratified by `intent`. This is all we really need. Let's tidy our data set and rename the two essential columns at the same time:


```r
deaths <- deaths[, .(intent, age = chi_age, count = deaths)]
head(deaths[count >9]) # only display non-suppressed data
```

```
##           intent age count
## 1: Unintentional  86    12
## 2: Unintentional  86    10
## 3: Unintentional  89    13
## 4: Unintentional  91    12
## 5: Unintentional  93    10
```

The table above confirms shows that we created a summary table of event (fall deaths) counts by age and intent. Congratulations, you've completed step 1.

### STEP 2: Create a table of **population denominators**

We can easily get the corresponding population data using [rads::get_population()](https://github.com/PHSKC-APDE/rads/wiki/get_population)


```r
population <- rads::get_population(years = c(2016:2020), 
                                   geo_type = 'kc', 
                                   group_by = c('ages'))
head(population)
```

```
##       pop geo_type      geo_id      year age       gender                                            race_eth
## 1: 128906       kc King County 2016-2020   0 Female, Male aian, asian, black, hispanic, multiple, nhpi, white
## 2: 126259       kc King County 2016-2020   1 Female, Male aian, asian, black, hispanic, multiple, nhpi, white
## 3: 128181       kc King County 2016-2020   2 Female, Male aian, asian, black, hispanic, multiple, nhpi, white
## 4: 125771       kc King County 2016-2020   3 Female, Male aian, asian, black, hispanic, multiple, nhpi, white
## 5: 121015       kc King County 2016-2020   4 Female, Male aian, asian, black, hispanic, multiple, nhpi, white
## 6: 131800       kc King County 2016-2020   5 Female, Male aian, asian, black, hispanic, multiple, nhpi, white
```

The essential columns of interest are `pop` and `age`, so let's drop the other columns.


```r
population <- population[, .(age, pop)]
head(population)
```

```
##    age    pop
## 1:   0 128906
## 2:   1 126259
## 3:   2 128181
## 4:   3 125771
## 5:   4 121015
## 6:   5 131800
```

Since we are interested in strata (i.e., `intent`), we will need a complete set of population data for each strata.


```r
population <- rbindlist(lapply(X = 1:length(unique(deaths$intent)),
                               FUN = function(X){
                                 temp <- copy(population)[, intent := unique(deaths$intent)[X]]
                                 }))
population[, .N, intent] # confirm that have 101 rows per intent
```

```
##           intent   N
## 1: Unintentional 101
## 2:       Suicide 101
## 3:  Undetermined 101
```

### STEP 3: **Merge** the denominators onto the numerators

The title says it all:


```r
deaths <- merge(deaths, 
                population, 
                by = c('age', 'intent'), 
                all.x = F, # drop if death strata do not match a population
                all.y = T) # keep population data if do not have deaths
deaths[is.na(count), count := 0] # formally set rows with zero counts to zero
head(deaths[count > 9]) # only display non-suppressed data
```

```
##    age        intent count   pop
## 1:  86 Unintentional    12 22044
## 2:  86 Unintentional    10 22044
## 3:  89 Unintentional    13 15550
## 4:  91 Unintentional    12 10621
## 5:  93 Unintentional    10  6722
```

### STEP 4: Use [rads::age_standardize()](https://github.com/PHSKC-APDE/rads/wiki/age_standardize)

The documentation tells us that `rads::age_standardize` needs a data set with either an `age` or `agecat` column. Since we have the former, we are good to go as long as we choose the correct reference population. Typing `list_ref_pop` will show you all the available reference populations, however we already know that that DOH uses the 2000 US standard population with 11 age groups, so we can plug that into the function.


```r
est <- age_standardize(ph.data = deaths,
                       ref.popname = "2000 U.S. Std Population (11 age groups)",
                       collapse = T,
                       my.count = 'count',
                       my.pop = 'pop',
                       per = 100000,
                       conf.level = 0.95, 
                       group_by = 'intent')
head(est)
```

```
##           intent count      pop crude.rate crude.lci crude.uci adj.rate adj.lci adj.uci                            reference_pop
## 1:       Suicide    70 15037011       0.47      0.36      0.59     0.42    0.33    0.55 2000 U.S. Std Population (11 age groups)
## 2:  Undetermined     9 11128975       0.08      0.04      0.15     0.07    0.03    0.15 2000 U.S. Std Population (11 age groups)
## 3: Unintentional  1209 35580177       3.40      3.21      3.60     1.62    1.51    1.74 2000 U.S. Std Population (11 age groups)
```

Here we see that the King County **crude** and **adjusted** fall death rates can differ substantially (e.g., Unintentional) or slightly (e.g., Undetermined). 

# The four steps: example 2

In this example we will calculate the adjusted rate of deaths from any cause by King County Regions in 2019. Because Regions are not baked into the death data, we will have to crosswalk from HRAs to Regions.

### STEP 1: Create a summary **table of event counts**


```r
deaths <- get_data_death(cols = c('chi_age', 
                                  'chi_year',
                                  'chi_geo_hra_short', 
                                  'underlying_cod_code'), 
                               year = c(2019), 
                               kingco = T)
```

Now let's crosswalk from King County HRAs to Regions using `rads::get_walk()`.


```r
xwalk <- rads::get_xwalk(geo1 = 'region10', geo2 = 'hra10')

deaths <- merge(deaths, xwalk, 
                by.x = "chi_geo_hra_short", by.y = "hra10", 
                all.x = T, all.y = F)

deaths <- deaths[, .(age = chi_age, underlying_cod_code, Region = region10)]

nrow(deaths[is.na(Region)]) # rows with missing Region information
```

```
## [1] 82
```

```r
head(deaths)
```

```
##    age underlying_cod_code Region
## 1:  66                K703   <NA>
## 2:  68                C329   <NA>
## 3:  63                N185   <NA>
## 4:  65                 N10   <NA>
## 5:  81                C911   <NA>
## 6:  85                 R99   <NA>
```

As you can see, there are few rows where the death data are missing a Region (due to a missing HRA). For simplicity sake, let's drop these rows.


```r
deaths <- deaths[!is.na(Region)]
```

Remember that step 1 is supposed to create a **summary** table. So, let's collapse/aggregate/sum the data to get the count of deaths by age and region:


```r
deaths <- deaths[, .(count = .N), .(age, Region)]
head(deaths)
```

```
##    age Region count
## 1:  69  South    95
## 2:  30  South    15
## 3:  59  South    60
## 4:  95  South    82
## 5:  88  South   132
## 6:  55  South    67
```

Finally, let's take a peak at the number of deaths by region:


```r
deaths[, sum(count), Region]
```

```
##     Region   V1
## 1:   South 5447
## 2: Seattle 3986
## 3:    East 2853
## 4:   North 1095
```

This table of deaths by region shows that the number of deaths in South King County is five times higher than those in North King County (5447 vs. 1095). Does that mean the death *rates* in South King County are five times higher than those in North King County? Let's get the corresponding population data and find out.

### STEP 2: Create a table of **population denominators**

We again use [rads::get_population()](https://github.com/PHSKC-APDE/rads/wiki/get_population) to the necessary population data


```r
population <- rads::get_population(years = c(2019), 
                                   geo_type = 'region', 
                                   group_by = c('ages', 'geo_id'))
head(population)
```

```
##     pop geo_type  geo_id year age       gender                                            race_eth
## 1: 2149   region Seattle 2019  80 Female, Male aian, asian, black, hispanic, multiple, nhpi, white
## 2: 2046   region Seattle 2019  81 Female, Male aian, asian, black, hispanic, multiple, nhpi, white
## 3: 2067   region Seattle 2019  82 Female, Male aian, asian, black, hispanic, multiple, nhpi, white
## 4: 2008   region Seattle 2019  83 Female, Male aian, asian, black, hispanic, multiple, nhpi, white
## 5: 1881   region Seattle 2019  84 Female, Male aian, asian, black, hispanic, multiple, nhpi, white
## 6: 7447   region Seattle 2019   0 Female, Male aian, asian, black, hispanic, multiple, nhpi, white
```

The key columns of interest are `pop`, `geo_id` (which has our regions), and `age`. The other columns are all constants, so let's just keep what we need.


```r
population <- population[, .(Region = geo_id, age, pop)]
head(population)
```

```
##     Region age  pop
## 1: Seattle  80 2149
## 2: Seattle  81 2046
## 3: Seattle  82 2067
## 4: Seattle  83 2008
## 5: Seattle  84 1881
## 6: Seattle   0 7447
```

Note that, unlike in the example above, we already have a complete population data set (ages 0 to 100) for each strata.


```r
population[, .N, Region]
```

```
##     Region   N
## 1: Seattle 101
## 2:   South 101
## 3:   North 101
## 4:    East 101
```

### STEP 3: **Merge** the denominators onto the numerators

Just do it!


```r
deaths <- merge(deaths, 
                population, 
                by = c('Region', 'age'), 
                all.x = F, 
                all.y = T) 
deaths[is.na(count), count := 0] # set rows with zero counts to zero
head(deaths)
```

```
##    Region age count  pop
## 1:   East   0    16 6753
## 2:   East   1     1 6703
## 3:   East   2     0 7042
## 4:   East   3     2 7088
## 5:   East   4     1 7122
## 6:   East   5     0 8079
```

### STEP 4: Use [rads::age_standardize()](https://github.com/PHSKC-APDE/rads/wiki/age_standardize)


```r
est <- age_standardize(ph.data = deaths,
                       ref.popname = "2000 U.S. Std Population (11 age groups)",
                       collapse = T,
                       my.count = 'count',
                       my.pop = 'pop',
                       per = 100000,
                       conf.level = 0.95, 
                       group_by = 'Region')
head(est)
```

```
##     Region count    pop crude.rate crude.lci crude.uci adj.rate adj.lci adj.uci                            reference_pop
## 1:    East  2853 575033     496.15    478.11    514.69   548.67  528.45  569.53 2000 U.S. Std Population (11 age groups)
## 2:   North  1095 133761     818.62    770.85    868.59   680.11  639.83  722.63 2000 U.S. Std Population (11 age groups)
## 3: Seattle  3986 747830     533.01    516.59    549.82   551.07  533.78  568.89 2000 U.S. Std Population (11 age groups)
## 4:   South  5447 769683     707.69    689.02    726.74   817.16  795.05  839.75 2000 U.S. Std Population (11 age groups)
```

Here we see that the the **crude rate is lower** in South King County (707.69) compared to North King County (818.62). This flips the relationship we saw with the counts (5447 vs 1095), but does not account for differences in the population structure in the two regions. In contrast, the **adjusted rate is higher** in South King County (817.16) vs. North King County(680.11) .

# Conclusion

We hope this vignettes has convinced you of two things:

1.  Age-adjusted rates are important because they can (sometimes) tell a different story compared to crude rates and counts!
2.  Calculating age-adjusted rates within the PHSKC universe is readily doable if you follow the four basic steps:
    1.  Create a summary table by age and strata of interest

    2.  Create an appropriate matching population table

    3.  Merge the population data onto the summary table

    4.  Use `rads::age_standardize()`

If you've walked through this vignette and more or less understood what's going on, you're in good shape! If you're still confused, please walk through it again and then reach out if you still have questions. Good luck!
