# Death Functions

# Introduction

The [rads package](https://github.com/PHSKC-APDE/rads/) has a suite of
tools designed to facilitate and accelerate the analysis of standardized
death data. Combining the `rads` functions below with the clean death
data on our servers should allow APDE analysts to conduct custom
analyses with relative ease.

The core `rads` death functions are:

- `get_data_death()`: easily download standardized death data from TSQL
  into R
- `death_icd10_clean()`: clean and standardize ICD-10 codes for use with
  `rads` death functions
- `death_113_count()`: generate death counts for the CDC’s 113 Selected
  Causes of Death (PLUS COVID-19)
- `death_130_count()`: generate death counts for the CDC’s 130 Selected
  Causes of **infant** Death (PLUS COVID-19)
- `death_injury_matrix_count()`: generate counts of external cause
  (a.k.a., injury related) deaths by intent and mechanism
- `death_other_count()`: generate counts for select causes of death that
  are **not** **included** in `death_113_count()`, `death_130_count()`,
  & `death_injury_matrix_count()`, but are of interest to APDE.
- `life_table()`: generate a standard life table, where the first row is
  the life expectancy (aka ‘expectation of life’) at birth

All of these functions have detailed help files that are accessible by
typing `?function_name`, e.g. `?get_data_death`. Some examples for how
to use these functions are given below.

***A few quick notes before we begin …***

- `death_113_count()`, `death_130_count()`,
  `death_injury_matrix_count()`, & `death_other_count()` only work with
  ICD-10 cause of death codes (i.e., those used since 1999).
- If you want to create age-adjusted rates, we recommend you read the
  [age_standardize](https://github.com/PHSKC-APDE/rads/wiki/age_standardize)
  and
  [calculating_rates_with_rads](https://github.com/PHSKC-APDE/rads/wiki/calculating_rates_with_rads)
  vignettes after reading through this one.

# Set up the environment

``` r
rm(list=ls())
library(rads)
library(data.table)
```

# get_data_death()

`get_data_death()` takes four potential arguments:

- `cols`: the names of the columns that you want to download. You will
  dramatically speed up the download time if you only specify the
  columns of genuine interest.
- `year`: the year(s) of interest, from 1980 to the present.
- `kingco`: logical (T\|F) specifying whether to limit the download to
  King County.
- `topcode`: logical (T\|F) specifying whether you want to top code
  `chi_age` at 100 to match population data.

If you do not specify any of the arguments, you will get all death data
columns, for King County, for the latest year, with top coding to 100.
Top coding to 100 is our general practice because there are few people
above 100 years of age and it aligns with our population data, which is
also top coded to 100.

``` r
ex0 <- get_data_death()
ncol(ex0) # number of columns in the downloaded death data  
```

    [1] 344

``` r
names(ex0)[1:6] # names of the first 6 columns
```

    [1] "etl_id_stat"       "state_file_number" "sex"              
    [4] "age_type"          "age"               "age_years"        

``` r
unique(ex0$chi_geo_kc) # confirm data is limited to King County
```

    [1] "King County"

``` r
unique(ex0$chi_year) # check the year
```

    [1] 2022

``` r
max(ex0$chi_age, na.rm = T) # check top coding
```

    [1] 100

# death_icd10_clean()

`death_icd10_clean()` is called upon by all of the `death_***_count()`
functions, but can also be used as a standalone function for truly
custom analyses. It takes a single argument:

- `icdcol`: the name of a character vector of ICD-10 codes.

The function can be used to clean any arbitrary vector of ICD-10 codes.
Notice that it gives a warning, informing you that it cleaned the codes.

``` r
icd_codes <- c('G30.9', 'I25.1', 'I250', 'C349', 'U07-1', 'X44')
cleaned_icd_codes <- death_icd10_clean(icd_codes)
```

    Warning in death_icd10_clean(icd_codes): 
    ⚠ There is at least one row where `icdcol` contains a hyphen (-), period (.), 
    space or some other non alpha-numeric character. These characters have been deleted, 
    e.g., A85.2 will become A852. This is necessary because rads death functions expect
    pure alpha numeric ICD codes.

``` r
print(cleaned_icd_codes)
```

    [1] "G309" "I251" "I250" "C349" "U071" "X440"

The function can also be used within a data.table. Notice that this time
it gives a different warning, informing you that it had to replace one
of the values (i.e., ‘3X44’) with NA because it did not appear to be an
ICD-10 code.

``` r
mydt <- data.table(icd_codes = c('G309', 'I251', 'I250', 'C349', 'U071', '3X44'))
mydt[, cleaned_icd_codes := death_icd10_clean(icd_codes)]
```

    Warning in death_icd10_clean(icd_codes): 
    ⚠  There is/are 1 value(s) in `icdcol` that do not follow the proper 
    ICD pattern. All ICDs that do not begin with a letter and end with
    a numeric have been replaced with NA.

``` r
head(mydt)
```

       icd_codes cleaned_icd_codes
          <char>            <char>
    1:      G309              G309
    2:      I251              I251
    3:      I250              I250
    4:      C349              C349
    5:      U071              U071
    6:      3X44              <NA>

# death_113_count()

`death_113_count()` allows the user to get death counts following the
CDC’s National Center for Health Statistics (NCHS) National Vital
Statistics System’s (NVSS) list of 113 selected causes of death ([see
Table
B](https://www.cdc.gov/nchs/data/dvs/Part9InstructionManual2020-508.pdf)).
To review this list, simply type `death_113()` into your R console. You
can use this information to select information for either the `causeids`
or `cause` arguments of `death_113_count()`.

Here is a snapshot of the top 10 rows from `death_113()`

| causeid | cause.of.death                      |
|--------:|:------------------------------------|
|       1 | Salmonella infections               |
|       2 | Shigellosis and amebiasis           |
|       3 | Certain other intestinal infections |
|       4 | Respiratory tuberculosis            |
|       5 | Other tuberculosis                  |
|       6 | Whooping cough                      |
|       7 | Scarlet fever and erysipelas        |
|       8 | Meningococcal infection             |
|       9 | Septicemia                          |
|      10 | Syphilis                            |

`death_113_count()` takes eight potential arguments:

- `ph.data`: the name of a person level data.table/data.frame of death
  data with ICD codes
- `causeids`: specifies the causeid(s) for NCHS 113 Causes of Death
  (1:113)
- `cause`: optional. A character vector specifying the causes of death
  of interest. When specified, it is used in lieu of the `causeids`. It
  is case insensitive and matches partial strings.
- `icdcol`: name of the column with the ICD codes of interest. Defaults
  to “underlying_cod_code”, which is used in APDE’s data.
- `kingco`: logical (T\|F) specifying whether to limit the data analysis
  to King County. Only works if ph.data still has the `chi_geo_kc`
  column.
- `group_by`: identifies the variables by which you want to group
  (a.k.a., stratify) the results.
- `ypll_age`: optional. The age in years (an integer) used for
  calculating Years of Potential Life Lost.
- `death_age_col`: optional. Name of the age column used if `ypll_age`
  is specified.

Please refer to the help file for more details.

### example \#1: Count 2020 deaths from COVID-19 or viral hepatitis

Get the 2020 data from TSQL

``` r
ex1 <- get_data_death(year = 2020, 
                      cols = c('chi_year', 'chi_geo_kc',
                               'underlying_cod_code'))
```

Check the dimensions and column names

``` r
dim(ex1)
```

    [1] 14353     3

``` r
names(ex1)
```

    [1] "chi_year"            "chi_geo_kc"          "underlying_cod_code"

Let’s take a peek at the first 10 rows where the underlying cause of
death is not missing to see the data structure.

| chi_year | chi_geo_kc  | underlying_cod_code |
|---------:|:------------|:--------------------|
|     2020 | King County | I330                |
|     2020 | King County | J440                |
|     2020 | King County | C259                |
|     2020 | King County | J440                |
|     2020 | King County | I059                |
|     2020 | King County | E142                |
|     2020 | King County | B182                |
|     2020 | King County | I613                |
|     2020 | King County | K703                |
|     2020 | King County | A199                |

Count deaths due to COVID-19 or viral hepatitis

``` r
dt1 <- death_113_count(ph.data = ex1, 
                       cause = 'viral hep|covid')
```

| cause.of.death   | causeid | deaths |
|:-----------------|:--------|:-------|
| All causes       | NA      | 14,353 |
| COVID-19 (U07.1) | NA      | 969    |
| Viral hepatitis  | 14      | 28     |

Glancing at the table above, you’ll see that you have the two causes of
death of interest, PLUS the total of death from any cause (‘All
causes’)*.*

We could have specified ‘Viral hepatitis’ using `causeids` rather than
the `cause` text string for the same result. Also, we really can’t
specify COVID-19. Since COVID-19 is not on the official 113 causes of
death list, it does not have a code. It is however returned
automatically by this function.

``` r
dt1.alt <- death_113_count(ph.data = ex1, 
                       causeids = 14)
```

| cause.of.death   | causeid | deaths |
|:-----------------|:--------|:-------|
| All causes       | NA      | 14,353 |
| COVID-19 (U07.1) | NA      | 969    |
| Viral hepatitis  | 14      | 28     |

### example \#2: 2019 & 2020 top five causes of death in `death_113_count()`

First, let’s get 2019 & 2020 data from TSQL …

``` r
ex2 <- get_data_death(year = 2019:2020, 
                      cols = c('chi_year', 'chi_geo_kc', 
                               'underlying_cod_code'))
```

Now, let’s use `group_by` to get counts by year

``` r
dt113_19_20 <- death_113_count(ph.data = ex2, 
                               group_by = 'chi_year')
```

Finally, let’s select the top five causes of death by year using
data.table

``` r
dt113_19_20[cause.of.death != 'All causes', 
            rank := frank(-deaths), 
            by = 'chi_year']
dt113_19_20 <- dt113_19_20[rank <= 5 ]
setorder(dt113_19_20, chi_year, rank)
```

| cause.of.death                                    | causeid | deaths | chi_year | rank |
|:--------------------------------------------------|:--------|:-------|---------:|-----:|
| All other diseases (Residual)                     | 95      | 1,693  |     2019 |    1 |
| Alzheimer’s disease                               | 48      | 896    |     2019 |    2 |
| All other forms of chronic ischemic heart disease | 55      | 787    |     2019 |    3 |
| Cerebrovascular diseases                          | 61      | 688    |     2019 |    4 |
| All other forms of heart disease                  | 59      | 612    |     2019 |    5 |
| All other diseases (Residual)                     | 95      | 1,733  |     2020 |    1 |
| COVID-19 (U07.1)                                  | NA      | 969    |     2020 |    2 |
| Alzheimer’s disease                               | 48      | 904    |     2020 |    3 |
| All other forms of chronic ischemic heart disease | 55      | 771    |     2020 |    4 |
| Cerebrovascular diseases                          | 61      | 642    |     2020 |    5 |

*Note!* Calculating the leading causes of death that we report publicly
is more complicated. Please refer to the [CHI repository
README](https://github.com/PHSKC-APDE/chi/tree/main/death#leading-causes-of-death)
for details.

### example \#3: Calculating years of potential life lost due to COVID-19, assuming age of death at 65 as the standard

Again, let’s load 2020 death data.

``` r
ex3 <- get_data_death(year = 2020, 
                      cols = c('chi_age', 'underlying_cod_code'))
```

Now, identify deaths due to COVID-19. Remember COVID-19 is not a one of
the official 113 causes of death, so let’s arbitrarily get causeid \#1
(Salmonella infections), knowing that it will also give us COVID-19.

``` r
dt3 <- death_113_count(ph.data = ex3, 
                       causeids = 1,
                       kingco = FALSE,
                       ypll_age = 65)
dt3 <- dt3[cause.of.death == 'COVID-19 (U07.1)'] # limit output to COVID-19
```

| cause.of.death   | causeid | deaths | ypll_65 |
|:-----------------|:--------|:-------|:--------|
| COVID-19 (U07.1) | NA      | 969    | 1,396   |

The table above tells us that, if everyone who died from COVID-19 in
2020 had lived to at least age 65, they would have lived a total of
1,396 additional years.

# death_130_count()

`death_130()` and `death_130_count()` are used for the [NCHS 130
Selected Causes of Infant Death (Table
C)](https://www.cdc.gov/nchs/data/dvs/Part9InstructionManual2020-508.pdf).
They work in exactly the same way as `death_113()` and
`death_113_count()` and will therefore not be described in detail.

# death_injury_matrix_count()

`death_injury_matrix_count()` allows the user to get injury related
death counts by intent and mechanism. To review a table of all available
combinations of mechanism and intent, simply type
`death_injury_matrix()` into your R console. Here are the first six
lines …

``` r
  mech.n.intent <- death_injury_matrix()[1:6]
```

| mechanism  | intent                 |
|:-----------|:-----------------------|
| All injury | Unintentional          |
| All injury | Suicide                |
| All injury | Homicide               |
| All injury | Undetermined           |
| All injury | Legal intervention/war |
| Cut/pierce | Unintentional          |

`death_injury_matrix_count()` takes eight potential arguments:

- `ph.data`: the name of a person level data.table/data.frame of death
  data with ICD codes
- `intent`: specifies the intent(s) of interest as per options available
  in `death_injury_matrix`. ‘none’ will ignore intent and return ‘Any
  intent’. ‘\*’ is a wildcard for all possible intents.
- `mechanism`: specifies the mechanism(s) of interest as per options
  available in `death_injury_matrix`. ‘none’ will ignore mechanism and
  return ‘Any mechansim’. ‘\*’ is a wildcard for all possible
  mechanisms.
- `icdcol`: name of the column with the ICD codes of interest. Defaults
  to “underlying_cod_code”, which is used in APDE’s data.
- `kingco`: logical (T\|F) specifying whether to limit the data analysis
  to King County. Only works if ph.data still has the `chi_geo_kc`
  column.
- `group_by`: identifies the variables by which you want to group
  (a.k.a., stratify) the results.
- `ypll_age`: optional. The age in years (an integer) used for
  calculating Years of Potential Life Lost.
- `death_age_col`: optional. Name of the age column used if `ypll_age`
  is specified.

Please refer to the help file for more details.

### example \#4: 2020 King County injury related deaths (every combination of intent and mechanism), stratified by sex

Get the data

``` r
ex4 <- get_data_death(year = 2020, 
                      cols = c('chi_sex', 'underlying_cod_code'))
```

Run death_injury_matrix_count and show the first six rows

``` r
dt4 <- death_injury_matrix_count(ph.data = ex4, 
                                 intent = "*", 
                                 mechanism = "*",
                                 kingco = FALSE, # FALSE b/c already subset to KC when importing data
                                 group_by = 'chi_sex')[1:6]
```

| mechanism  | intent                 | deaths | chi_sex |
|:-----------|:-----------------------|:-------|:--------|
| All injury | Homicide               | 23     | Female  |
| All injury | Homicide               | 76     | Male    |
| All injury | Legal intervention/war | 1      | Female  |
| All injury | Legal intervention/war | 7      | Male    |
| All injury | Suicide                | 69     | Female  |
| All injury | Suicide                | 186    | Male    |

**What happens if you set both intent & mechanism to “none”?** The
function assumes you want the total injury related deaths (without
regard to intent or mechanism).

``` r
dt4.alt <- death_injury_matrix_count(ph.data = ex4, 
                                 intent = "none", 
                                 mechanism = "none",
                                 kingco = FALSE, 
                                 group_by = 'chi_sex')
```

| mechanism     | intent     | deaths | chi_sex |
|:--------------|:-----------|:-------|:--------|
| Any mechanism | Any intent | 416    | Female  |
| Any mechanism | Any intent | 931    | Male    |

### example \#5: 2016-2020 King County homicides (intent) by firearms (mechanism), stratified by sex

Get the data

``` r
ex5 <- get_data_death(year = 2016:2020, 
                      cols = c('chi_sex', 'underlying_cod_code'))
```

Run death_injury_matrix_count

``` r
dt5 <- death_injury_matrix_count(ph.data = ex5, 
                                 intent = "homicide", 
                                 mechanism = "firearm", 
                                 kingco = FALSE, 
                                 group_by = 'chi_sex')
```

| mechanism | intent   | deaths | chi_sex |
|:----------|:---------|-------:|:--------|
| Firearm   | Homicide |     43 | Female  |
| Firearm   | Homicide |    218 | Male    |

### example \#6: years of potential life lost due to firearm homicides 2016-2020

Get the data

``` r
ex6 <- get_data_death(year = 2016:2020, 
                      cols = c('chi_age', 'underlying_cod_code'))
```

Run death_injury_matrix_count

``` r
dt6 <- death_injury_matrix_count(ph.data = ex6, 
                                 intent = "homicide", 
                                 mechanism = "firearm", 
                                 kingco = FALSE, 
                                 ypll_age = 65)
```

| mechanism | intent   | deaths | ypll_65 |
|:----------|:---------|-------:|:--------|
| Firearm   | Homicide |    261 | 8,727   |

# death_other_count()

`death_other_count()` allows the user to get death counts for cause of
death that are NOT included in `death_113_count()`, `death_130_count()`,
and `death_injury_matrix_count()`, but that are still of general
interest to APDE. To review a list of all available ‘other’ causes of
death, simply type `death_other()` into your R console. If you want to
add additional causes of death to this list, [please submit a GitHub
issue](https://github.com/PHSKC-APDE/rads/issues/new).

``` r
death_other()
```

     [1] "Alcohol_Death"                       "CO_Death"                           
     [3] "Cancer"                              "Chronic liver disease and cirrhosis"
     [5] "Chronic lower respiratory disease"   "Drug-induced"                       
     [7] "Drug-overdose"                       "Drug_Death"                         
     [9] "Heart disease"                       "HeatStress_Death"                   
    [11] "Influenza/pneumonia"                 "Opioid_Death"                       

`death_other_count()` takes seven potential arguments:

- `ph.data`: the name of a person level data.table/data.frame of death
  data with ICD codes
- `cause`: a character vector specifying the cause(s) of death of
  interest taken from the list provided by `death_other()`. It is case
  insensitive and matches partial strings
- `icdcol`: name of the column with the ICD codes of interest. Defaults
  to “underlying_cod_code”, which is used in APDE’s data
- `kingco`: logical (T\|F) specifying whether to limit the data analysis
  to King County. Only works if ph.data still has the `chi_geo_kc`
  column.
- `group_by`: identifies the variables by which you want to group
  (a.k.a., stratify) the results.
- `ypll_age`: optional. The age in years (an integer) used for
  calculating Years of Potential Life Lost.
- `death_age_col`: optional. Name of the age column used if `ypll_age`
  is specified.

Please refer to the help file for more details.

### example \#7: Count the number of heat stress deaths for 2017-2021, stratified by year

Get the data

``` r
ex7 <- get_data_death(year = 2017:2021, 
                      cols = c('chi_year', 'underlying_cod_code'))
```

Run `death_other_count`

``` r
dt7 <- death_other_count(ph.data = ex7, 
                                 cause = "heat", 
                                 kingco = FALSE, 
                                 group_by = "chi_year")
dt7 <- dt7[cause.of.death != 'All causes']
setorder(dt7, chi_year, cause.of.death)
```

| cause.of.death   | deaths | chi_year |
|:-----------------|:-------|---------:|
| HeatStress_Death | 0      |     2017 |
| HeatStress_Death | 1      |     2018 |
| HeatStress_Death | 1      |     2019 |
| HeatStress_Death | 0      |     2020 |
| HeatStress_Death | 28     |     2021 |

### example \#8: Count the number of cancer and heart disease deaths in 2021

Get the data

``` r
ex8 <- get_data_death(year = 2021, 
                      cols = c('chi_age', 'underlying_cod_code'))
```

Run `death_other_count`

``` r
dt8 <- death_other_count(ph.data = ex8, 
                                 cause = "cancer|heart", 
                                 kingco = FALSE, 
                                 ypll_age = 65)
```

| cause.of.death | deaths | ypll_65 |
|:---------------|:-------|:--------|
| All causes     | 15,033 | 68,480  |
| Cancer         | 2,988  | 8,268   |
| Heart disease  | 2,879  | 5,440   |

# life_table()

`life_table()` generates a standard life table, where the first row is
the life expectancy (aka ‘expectation of life’) at birth. Since it needs
aggregate death data, you are **strongly encouraged to preprocess the
death data with `life_table_prep()`.** A simple example of how to use
`life_table_prep()` is embedded below.

`life_table()` takes seven potential arguments:

- `ph.data`: the name of data.table/data.frame with aggregated deaths
  and corresponding populations, as well as the age interval and the
  average fraction of years lived in the interval by those who die in
  the interval.
- `myages`: the name of a column in `ph.data` with the age intervals
  used for the life table calculations (e.g., c(‘0-1’, ‘1-5’, … ‘85+’)).
- `mydeaths`: the name of a numeric column in `ph.data` with the total
  deaths for the given age interval in the given year.
- `mypops`: the name of a numeric column in `ph.data` with the total
  population corresponding to `mydeaths`.
- `myprops`: the name of a numeric column in `ph.data` with the average
  proportion of the interval lived by those who died in the interval.
- `group_by`: identifies the variables by which you want to group
  (a.k.a., stratify) the results.
- `ci`: the confidence interval, must be \[0, 1\]. Default is 0.95

Please refer to the help file for more details.

### example \#9: Get life expectancy at birth for King County residents born 2016-2020, stratified by sex

Get the critical death data

``` r
ex9 <- get_data_death(year = 2016:2020, 
                        cols = c('date_of_birth', 'date_of_death', 'chi_sex'))
```

To preserve privacy, let’s look at a summary of the table rather than
the actual table

``` r
summary(ex9)
```

     date_of_birth        date_of_death          chi_sex     
     Min.   :1906-03-05   Min.   :2016-01-01   Female:32800  
     1st Qu.:1929-06-03   1st Qu.:2017-03-31   Male  :34442  
     Median :1939-08-04   Median :2018-07-18   NA's  :   18  
     Mean   :1943-06-18   Mean   :2018-07-14                 
     3rd Qu.:1953-05-04   3rd Qu.:2019-10-27                 
     Max.   :2020-12-28   Max.   :2020-12-31                 
     NA's   :2                                               

Use `life_table_prep()` to aggregate the age interval specific count of
deaths and the average fraction of the interval lived by those who died
in that interval, stratified by sex. **Note**, by default
`life_table_prep()` uses the standard age intervals used by the CDC and
WA DOH. These intervals can easily be customized if desired. Here we
show just the top six rows so you can see the data structure.

``` r
dt9 = life_table_prep(ph.data = ex9, 
                      group_by = 'chi_sex')
```

| chi_sex | ages | deaths |  fraction |
|:--------|:-----|-------:|----------:|
| Male    | NA   |      2 |       NaN |
| Female  | 0-1  |    212 | 0.0613904 |
| Male    | 0-1  |    268 | 0.0559866 |
| NA      | 1-5  |      1 | 0.7156164 |
| Female  | 1-5  |     28 | 0.2480545 |
| Male    | 1-5  |     34 | 0.4037292 |

Filter/delete rows with missing ages or non-binary sex because neither
would have a corresponding population.

``` r
dt9 <- dt9[!is.na(chi_sex) & !is.na(ages)]
```

Rename `chi_sex` to `gender` so that it plays nicely with the population
data below.

``` r
setnames(dt9, "chi_sex", "gender")
```

| gender | ages  | deaths |  fraction |
|:-------|:------|-------:|----------:|
| Female | 0-1   |    212 | 0.0613904 |
| Male   | 0-1   |    268 | 0.0559866 |
| Female | 1-5   |     28 | 0.2480545 |
| Male   | 1-5   |     34 | 0.4037292 |
| Female | 10-15 |     26 | 0.5178314 |
| Male   | 10-15 |     40 | 0.4695332 |

The death data is now ready for `life_table`, but we still need to merge
on the corresponding populations. Let’s start by getting the raw
population for the same time period (2016-2020) by gender and age.

``` r
pop <- get_population(kingco = T, 
                      years = 2016:2020, 
                      group_by = c('genders', 'ages'))
pop <- pop[, .(gender, age, pop)]
```

| gender | age |      pop |
|:-------|----:|---------:|
| Male   |   0 | 66699.47 |
| Female |   0 | 65156.33 |
| Male   |   1 | 65280.55 |
| Female |   1 | 62821.88 |
| Male   |   2 | 67669.05 |
| Female |   2 | 61472.62 |

Now we need to aggregate the population data to use the same age
intervals as the death data.

``` r
my.cuts <- c(0, 1, 5, 10, 15, 18, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85)
pop[, ages := cut(age, my.cuts, right = F)] # create age intervals
pop[, ages := gsub("\\[|\\]|\\)", "", ages)] # tidy
pop[, ages := gsub("\\,", "-", ages)] #tidy
pop[age >= max(my.cuts), ages := paste0(max(my.cuts), "+")] # tidy
pop <- pop[, .(pop = sum(pop)), .(gender, ages)] #tidy
```

| gender | ages |       pop |
|:-------|:-----|----------:|
| Male   | 0-1  |  66699.47 |
| Female | 0-1  |  65156.33 |
| Male   | 1-5  | 255888.56 |
| Female | 1-5  | 244458.54 |
| Male   | 5-10 | 322774.96 |
| Female | 5-10 | 310606.85 |

Merge the properly formatted death and population data together.

``` r
dt9 <- merge(dt9, 
             pop, 
             by = c('gender', 'ages'), 
             all = T)
```

| gender | ages  | deaths |  fraction |       pop |
|:-------|:------|-------:|----------:|----------:|
| Female | 0-1   |    212 | 0.0613904 |  65156.33 |
| Female | 1-5   |     28 | 0.2480545 | 244458.54 |
| Female | 10-15 |     26 | 0.5178314 | 302747.86 |
| Female | 15-18 |     42 | 0.4281800 | 171470.75 |
| Female | 18-20 |     29 | 0.3798422 | 128079.03 |
| Female | 20-25 |    102 | 0.4517549 | 340662.74 |

Run `life_table()` function to create life tables for males and females.

``` r
dt9.lifetable <- life_table(ph.data = dt9, 
                            group_by = 'gender')
```

Display the first row of the life table for each gender, which is for
children under 1. The `ex` column is the life expectancy at birth. To
see a detailed key of what all the columns mean, type `?life_table` in
your R console and read the ‘Details’ section.

``` r
dt9.lifetable[ages == '0-1',]
```

| gender | ages |      pop | deaths |  fraction |      mx |      qx |    lx |  dx |   ax |    Lx |      Tx |    ex | ex_lower | ex_upper |   ex_se |
|:-------|:-----|---------:|-------:|----------:|--------:|--------:|------:|----:|-----:|------:|--------:|------:|---------:|---------:|--------:|
| Female | 0-1  | 65156.33 |    212 | 0.0613904 | 0.00325 | 0.00324 | 1e+05 | 324 | 0.06 | 99696 | 8405505 | 84.06 |    83.94 |    84.17 | 0.05745 |
| Male   | 0-1  | 66699.47 |    268 | 0.0559866 | 0.00402 | 0.00400 | 1e+05 | 400 | 0.06 | 99622 | 7950038 | 79.50 |    79.38 |    79.62 | 0.06300 |

# Conclusion

We know this was a lot to process. The good news is that this vignette
isn’t going anywhere. If you remember (a) that this vignette exists and
(b) where to find it, you’ll be in good shape to take on standard
mortality analyses in the future.

If you’ve read through this vignette and the corresponding help files
and are still confused, please feel free to reach out for assistance.
You may have found a bug, who knows? Good luck!

– *Updated by dcolombara, 2024-06-24*
