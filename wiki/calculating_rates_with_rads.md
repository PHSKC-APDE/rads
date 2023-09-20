---
title: "Calculating rates with rads"
output:
  html_document:
    df_print: paged
  rmarkdown::html_vignette: default
  github_document: default
  urlcolor: blue
  pdf_document: default
vignette: |
  %\VignetteEngine{knitr::knitr} %\VignetteIndexEntry{calculating_rates_with_rads}
---



# Introduction

Many PHSKC metrics are simple counts or means/proportions/fractions. The `rads::calc()` function was created to calculate these metrics from survey and line-level data. However, there are certain metrics and datasets (e.g., death & CHARS) where **true rates --** **with a population denominator** -- are of interest. Most of the time these rates need to be [age-adjusted](https://github.com/PHSKC-APDE/rads/wiki/age_standardize) and most of the time DOH and APDE adjust to the US 2000 Standard Population with 11 age groups (see `rads.data::population_reference_pop_11_age_groups`). The [rads package](https://github.com/PHSKC-APDE/rads/) has a suite of tools that were developed to facilitate these types of analyses. This vignette explains how to use these tools in **four steps**.

# The four steps: an overview

**STEP 1:** Create a **summary table of event counts** (deaths, hospitalizations, etc.) **by single year age groups and strata** of interest. For example, part of your your table might look like this:

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
names(deaths) 
```

```
## [1] "chi_age"             "chi_year"            "chi_geo_seattle"     "underlying_cod_code"
```

```r
nrow(deaths) # should be ~65,000 based on apriori knowledge
```

```
## [1] 67260
```

We see that the column names and the number of rows are what we expected. Now, let's identify the deaths from falls using the `rads::death_injury_matrix_count` function. To learn about this function, type `?death_injury_matrix_count` in your console or walk through the [death_functions wiki](https://github.com/PHSKC-APDE/rads/wiki). For now, just trust that this is the correct syntax:


```r
deaths <- death_injury_matrix_count(ph.data = deaths,
                               intent = "*", # all five intents
                               mechanism = "fall",
                               icdcol = 'underlying_cod_code',
                               kingco = F, # False because already subset
                               group_by = 'chi_age') 
head(deaths[deaths >9]) # only display non-suppressed data
```

```
##    mechanism        intent deaths chi_age
## 1:      Fall Unintentional     10      55
## 2:      Fall Unintentional     11      56
## 3:      Fall Unintentional     12      61
## 4:      Fall Unintentional     10      62
## 5:      Fall Unintentional     13      63
## 6:      Fall Unintentional     14      64
```

Glancing at the output from the `death_injury_matrix_count` function, we see the that we have **aggregated** `deaths` by `chi_age` and stratified by `intent`. This is all we really need. Let's tidy our data set and rename the two essential columns at the same time:


```r
deaths <- deaths[, .(intent, age = chi_age, count = deaths)]
head(deaths[count >9]) # only display non-suppressed data
```

```
##           intent age count
## 1: Unintentional  55    10
## 2: Unintentional  56    11
## 3: Unintentional  61    12
## 4: Unintentional  62    10
## 5: Unintentional  63    13
## 6: Unintentional  64    14
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
##           pop geo_type      geo_id      year age       gender                                                 race_eth
## 1: 126926.123       kc King County 2016-2020  18 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White
## 2:  25414.877       kc King County 2016-2020  86 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White
## 3:   7474.077       kc King County 2016-2020  93 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White
## 4:  87325.243       kc King County 2016-2020  68 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White
## 5: 131855.803       kc King County 2016-2020   0 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White
## 6: 193885.671       kc King County 2016-2020  33 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White
```

The essential columns of interest are `pop` and `age`, so let's drop the other columns.


```r
population <- population[, .(age, pop)]
head(population)
```

```
##    age        pop
## 1:  18 126926.123
## 2:  86  25414.877
## 3:  93   7474.077
## 4:  68  87325.243
## 5:   0 131855.803
## 6:  33 193885.671
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
##                    intent   N
## 1:               Homicide 101
## 2: Legal intervention/war 101
## 3:                Suicide 101
## 4:           Undetermined 101
## 5:          Unintentional 101
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
##    age        intent count       pop
## 1:  55 Unintentional    10 150673.44
## 2:  56 Unintentional    11 145840.61
## 3:  61 Unintentional    12 129379.94
## 4:  62 Unintentional    10 128243.03
## 5:  63 Unintentional    13 122568.98
## 6:  64 Unintentional    14  95319.47
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
##                    intent count      pop crude.rate crude.lci crude.uci adj.rate adj.lci adj.uci                            reference_pop
## 1:               Homicide     0 10974431       0.00      0.00      0.03     0.00    0.00    0.04 2000 U.S. Std Population (11 age groups)
## 2: Legal intervention/war     0 10974431       0.00      0.00      0.03     0.00    0.00    0.04 2000 U.S. Std Population (11 age groups)
## 3:                Suicide    70 10974431       0.64      0.50      0.81     0.61    0.47    0.78 2000 U.S. Std Population (11 age groups)
## 4:           Undetermined     9 10974431       0.08      0.04      0.16     0.08    0.04    0.16 2000 U.S. Std Population (11 age groups)
## 5:          Unintentional  1209 10974431      11.02     10.40     11.66    10.99   10.37   11.64 2000 U.S. Std Population (11 age groups)
```

Here we see that the King County **crude** and **adjusted** fall death rates can differ substantially (e.g., Unintentional) or slightly (e.g., Undetermined). 

# The four steps: example 2

In this example we will calculate the adjusted rate of deaths from any cause by King County Regions in 2019. Because Regions are not baked into the death data, we will have to crosswalk from HRAs to Regions.

### STEP 1: Create a summary **table of event counts**


```r
deaths <- get_data_death(cols = c('chi_age', 
                                  'chi_year',
                                  'chi_geo_hra2010_short', 
                                  'underlying_cod_code'), 
                               year = c(2019), 
                               kingco = T)
```

Now let's crosswalk from King County HRAs to Regions using `rads::get_walk()`.


```r
xwalk <- rads::get_xwalk(geo1 = 'region10', geo2 = 'hra10')

deaths <- merge(deaths, xwalk, 
                by.x = "chi_geo_hra2010_short", by.y = "hra10", 
                all.x = T, all.y = F)

deaths <- deaths[, .(age = chi_age, underlying_cod_code, Region = region10)]

nrow(deaths[is.na(Region)]) # rows with missing Region information
```

```
## [1] 322
```

```r
head(deaths)
```

```
##    age underlying_cod_code Region
## 1:  80                D471   <NA>
## 2:  96                F019   <NA>
## 3:  69                C259   <NA>
## 4:  88                F019   <NA>
## 5:  64                 W80   <NA>
## 6:  48                J100   <NA>
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
## 1:  63  South    77
## 2:  97  South    62
## 3:  93  South   112
## 4:  78  South   108
## 5:  95  South    80
## 6:  60  South    75
```

Finally, let's take a peak at the number of deaths by region:


```r
deaths[, sum(count), Region]
```

```
##     Region   V1
## 1:   South 5207
## 2: Seattle 3986
## 3:    East 2853
## 4:   North 1095
```

This table of deaths by region shows that the number of deaths in South King County is five times higher than those in North King County (5207 vs. 1095). Does that mean the death *rates* in South King County are five times higher than those in North King County? Let's get the corresponding population data and find out.

### STEP 2: Create a table of **population denominators**

We again use [rads::get_population()](https://github.com/PHSKC-APDE/rads/wiki/get_population) to the necessary population data


```r
population <- rads::get_population(years = c(2019), 
                                   geo_type = 'region', 
                                   group_by = c('ages', 'geo_id'))
head(population)
```

```
##          pop geo_type  geo_id year age       gender                                                 race_eth geo_id_code
## 1:  9617.317   region    East 2019  31 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White           1
## 2:  2155.107   region   North 2019  30 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White           2
## 3: 17707.450   region Seattle 2019  27 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White           3
## 4:  1723.738   region   North 2019  43 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White           2
## 5:  8190.881   region    East 2019  48 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White           1
## 6:  6742.962   region Seattle 2019   3 Female, Male AIAN, Asian, Black, Hispanic, Multiple race, NHPI, White           3
```

The key columns of interest are `pop`, `geo_id` (which has our regions), and `age`. The other columns are all constants, so let's just keep what we need.


```r
population <- population[, .(Region = geo_id, age, pop)]
head(population)
```

```
##     Region age       pop
## 1:    East  31  9617.317
## 2:   North  30  2155.107
## 3: Seattle  27 17707.450
## 4:   North  43  1723.738
## 5:    East  48  8190.881
## 6: Seattle   3  6742.962
```

Note that, unlike in the example above, we already have a complete population data set (ages 0 to 100) for each strata.


```r
population[, .N, Region]
```

```
##     Region   N
## 1:    East 101
## 2:   North 101
## 3: Seattle 101
## 4:   South 101
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
##    Region age count      pop
## 1:   East   0    16 7150.688
## 2:   East   1     1 7018.036
## 3:   East   2     0 7217.847
## 4:   East   3     2 7215.894
## 5:   East   4     1 7061.314
## 6:   East   5     0 8389.604
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
##     Region count      pop crude.rate crude.lci crude.uci adj.rate adj.lci adj.uci                            reference_pop
## 1:    East  2853 570568.9     500.03    481.85    518.72   517.25  498.19  536.93 2000 U.S. Std Population (11 age groups)
## 2:   North  1095 136125.9     804.40    757.46    853.50   625.86  588.59  665.32 2000 U.S. Std Population (11 age groups)
## 3: Seattle  3986 723214.3     551.15    534.17    568.53   531.22  514.54  548.45 2000 U.S. Std Population (11 age groups)
## 4:   South  5207 804672.4     647.10    629.64    664.92   677.37  658.74  696.42 2000 U.S. Std Population (11 age groups)
```

Here we see that the the **crude rate is lower** in South King County (647.1) compared to North King County (804.4). This flips the relationship we saw with the counts (5207 vs 1095), but does not account for differences in the population structure in the two regions. In contrast, the **adjusted rate is higher** in South King County (677.37) vs. North King County(625.86) .

# Conclusion

We hope this vignettes has convinced you of two things:

1.  Age-adjusted rates are important because they can (sometimes) tell a different story compared to crude rates and counts!
2.  Calculating age-adjusted rates within the PHSKC universe is readily doable if you follow the four basic steps:
    1.  Create a summary table by age and strata of interest

    2.  Create an appropriate matching population table

    3.  Merge the population data onto the summary table

    4.  Use `rads::age_standardize()`

If you've walked through this vignette and more or less understood what's going on, you're in good shape! If you're still confused, please walk through it again and then reach out if you still have questions. Good luck!
