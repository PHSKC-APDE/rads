# Death Functions

# Introduction

The [rads package](https://github.com/PHSKC-APDE/rads/) has a suite of
tools designed to facilitate and accelerate the analysis of standardized
death data. Combining the `rads` functions below with the clean death
data on our servers should allow APDE analysts to conduct custom
analyses with relative ease.

The core `rads` death function are:

-   `get_data_death()`: easily download standardized death data from SQL
    into R
-   `death_113_count()`: generate death counts for the CDC’s 113
    Selected Causes of Death (PLUS COVID-19)
-   `death_130_count()`: generate death counts for the CDC’s 130
    Selected Causes of **infant** Death (PLUS COVID-19)
-   `death_injury_matrix_count()`: generate counts of external cause
    (a.k.a., injury related) deaths by intent and mechanism
-   `death_other_count()`: generate counts for select causes of death
    that are **not** **included** in `death_113_count()`,
    `death_130_count()`, & `death_injury_matrix_count()`, but are of
    interest to APDE.
-   `life_table()`: generate a standard life table, where the first row
    is the life expectancy (aka ‘expectation of life’) at birth

All of these functions have detailed help files that are accessible by
typing `?function_name`, e.g. `?get_data_death`. Some examples for how
to use these functions are given below.

***A few quick notes before we begin …***

-   `death_113_count()`, `death_130_count()`,
    `death_injury_matrix_count()`, & `death_other_count()` only work
    with ICD-10 cause of death codes (i.e., those used since 1999).
-   If you want to create age-adjusted rates, we recommend you read the
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

-   `cols`: the names of the columns that you want to download. You will
    dramatically speed up the download time if you only specify the
    columns of genuine interest.
-   `year`: the year(s) of interest, from 1980 to the present.
-   `kingco`: logical (T|F) specifying whether to limit the download to
    King County.
-   `topcode`: logical (T|F) specifying whether you want to top code
    `chi_age` at 100 to match population data.

If you do not specify any of the arguments, you will get all death data
columns, for King County, for the latest year, with top coding to 100.

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

    [1] 2021

``` r
max(ex0$chi_age, na.rm = T) # check top coding
```

    [1] 100

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

<table>
<thead>
<tr class="header">
<th style="text-align: right;">causeid</th>
<th style="text-align: left;">cause.of.death</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: right;">1</td>
<td style="text-align: left;">Salmonella infections</td>
</tr>
<tr class="even">
<td style="text-align: right;">2</td>
<td style="text-align: left;">Shigellosis and amebiasis</td>
</tr>
<tr class="odd">
<td style="text-align: right;">3</td>
<td style="text-align: left;">Certain other intestinal infections</td>
</tr>
<tr class="even">
<td style="text-align: right;">4</td>
<td style="text-align: left;">Respiratory tuberculosis</td>
</tr>
<tr class="odd">
<td style="text-align: right;">5</td>
<td style="text-align: left;">Other tuberculosis</td>
</tr>
<tr class="even">
<td style="text-align: right;">6</td>
<td style="text-align: left;">Whooping cough</td>
</tr>
<tr class="odd">
<td style="text-align: right;">7</td>
<td style="text-align: left;">Scarlet fever and erysipelas</td>
</tr>
<tr class="even">
<td style="text-align: right;">8</td>
<td style="text-align: left;">Meningococcal infection</td>
</tr>
<tr class="odd">
<td style="text-align: right;">9</td>
<td style="text-align: left;">Septicemia</td>
</tr>
<tr class="even">
<td style="text-align: right;">10</td>
<td style="text-align: left;">Syphilis</td>
</tr>
</tbody>
</table>

`death_113_count()` takes eight potential arguments:

-   `ph.data`: the name of a person level data.table/data.frame of death
    data with ICD codes
-   `causeids`: specifies the causeid(s) for NCHS 113 Causes of Death
    (1:113)
-   `cause`: optional. A character vector specifying the causes of death
    of interest. When specified, it is used in lieu of the `causeids`.
    It is case insensitive and matches partial strings.
-   `icdcol`: name of the column with the ICD codes of interest.
    Defaults to “underlying_cod_code”, which is used in APDE’s data.
-   `kingco`: logical (T|F) specifying whether to limit the data
    analysis to King County. Only works if ph.data still has the
    `chi_geo_kc` column.
-   `group_by`: identifies the variables by which you want to group
    (a.k.a., stratify) the results.
-   `ypll_age`: optional. The age in years (an integer) used for
    calculating Years of Potential Life Lost.
-   `death_age_col`: optional. Name of the age column used if `ypll_age`
    is specified.

Please refer to the help file for more details.

### example #1: Count 2020 deaths from COVID-19 or viral hepatitis

Get the 2020 data from SQL

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

Lets take a peak at the first 10 rows where the underlying cause of
death is not missing to see the data structure.

<table>
<thead>
<tr class="header">
<th style="text-align: right;">chi_year</th>
<th style="text-align: left;">chi_geo_kc</th>
<th style="text-align: left;">underlying_cod_code</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: right;">2020</td>
<td style="text-align: left;">King County</td>
<td style="text-align: left;">K704</td>
</tr>
<tr class="even">
<td style="text-align: right;">2020</td>
<td style="text-align: left;">King County</td>
<td style="text-align: left;">I251</td>
</tr>
<tr class="odd">
<td style="text-align: right;">2020</td>
<td style="text-align: left;">King County</td>
<td style="text-align: left;">E102</td>
</tr>
<tr class="even">
<td style="text-align: right;">2020</td>
<td style="text-align: left;">King County</td>
<td style="text-align: left;">I120</td>
</tr>
<tr class="odd">
<td style="text-align: right;">2020</td>
<td style="text-align: left;">King County</td>
<td style="text-align: left;">I120</td>
</tr>
<tr class="even">
<td style="text-align: right;">2020</td>
<td style="text-align: left;">King County</td>
<td style="text-align: left;">K709</td>
</tr>
<tr class="odd">
<td style="text-align: right;">2020</td>
<td style="text-align: left;">King County</td>
<td style="text-align: left;">B232</td>
</tr>
<tr class="even">
<td style="text-align: right;">2020</td>
<td style="text-align: left;">King County</td>
<td style="text-align: left;">P072</td>
</tr>
<tr class="odd">
<td style="text-align: right;">2020</td>
<td style="text-align: left;">King County</td>
<td style="text-align: left;">C349</td>
</tr>
<tr class="even">
<td style="text-align: right;">2020</td>
<td style="text-align: left;">King County</td>
<td style="text-align: left;">F019</td>
</tr>
</tbody>
</table>

Count deaths due to COVID-19 or viral hepatitis

``` r
dt1 <- death_113_count(ph.data = ex1, 
                       cause = 'viral hep|covid')
```

<table>
<thead>
<tr class="header">
<th style="text-align: left;">cause.of.death</th>
<th style="text-align: left;">causeid</th>
<th style="text-align: left;">deaths</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">All causes</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">14,353</td>
</tr>
<tr class="even">
<td style="text-align: left;">COVID-19 (U07.1)</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">969</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Missing/Unknown</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">13,356</td>
</tr>
<tr class="even">
<td style="text-align: left;">Viral hepatitis</td>
<td style="text-align: left;">14</td>
<td style="text-align: left;">28</td>
</tr>
</tbody>
</table>

Glancing at the table above, you’ll see that you have the two causes of
death of interest, PLUS the total of death from any cause (‘All causes’)
PLUS any rows where the ICD code did not match the selected causes of
death (‘Missing/Unknown’). *Missing/Unknown = All causes - Viral
hepatitis - COVID-19.*

We could have specified ‘Viral hepatitis’ using `causeids` rather than
the `cause` text string for the same result. Also, we really can’t
specify COVID-19. Since COVID-19 is not on the official 113 causes of
death list, it does not have a code. It is however returned
automatically by this function.

``` r
dt1.alt <- death_113_count(ph.data = ex1, 
                       causeids = 14)
```

<table>
<thead>
<tr class="header">
<th style="text-align: left;">cause.of.death</th>
<th style="text-align: left;">causeid</th>
<th style="text-align: left;">deaths</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">All causes</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">14,353</td>
</tr>
<tr class="even">
<td style="text-align: left;">COVID-19 (U07.1)</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">969</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Missing/Unknown</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">13,356</td>
</tr>
<tr class="even">
<td style="text-align: left;">Viral hepatitis</td>
<td style="text-align: left;">14</td>
<td style="text-align: left;">28</td>
</tr>
</tbody>
</table>

### example #2: 2019 & 2020 top five causes of death

First, let’s get 2019 & 2020 data from SQL …

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
dt113_19_20 <- dt113_19_20[rank <=5 ]
setorder(dt113_19_20, chi_year, rank)
```

<table>
<colgroup>
<col style="width: 63%" />
<col style="width: 10%" />
<col style="width: 8%" />
<col style="width: 11%" />
<col style="width: 6%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">cause.of.death</th>
<th style="text-align: left;">causeid</th>
<th style="text-align: left;">deaths</th>
<th style="text-align: right;">chi_year</th>
<th style="text-align: right;">rank</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">All other diseases (Residual)</td>
<td style="text-align: left;">95</td>
<td style="text-align: left;">1,693</td>
<td style="text-align: right;">2019</td>
<td style="text-align: right;">1</td>
</tr>
<tr class="even">
<td style="text-align: left;">Alzheimer’s disease</td>
<td style="text-align: left;">48</td>
<td style="text-align: left;">896</td>
<td style="text-align: right;">2019</td>
<td style="text-align: right;">2</td>
</tr>
<tr class="odd">
<td style="text-align: left;">All other forms of chronic ischemic heart
disease</td>
<td style="text-align: left;">55</td>
<td style="text-align: left;">787</td>
<td style="text-align: right;">2019</td>
<td style="text-align: right;">3</td>
</tr>
<tr class="even">
<td style="text-align: left;">Cerebrovascular diseases</td>
<td style="text-align: left;">61</td>
<td style="text-align: left;">688</td>
<td style="text-align: right;">2019</td>
<td style="text-align: right;">4</td>
</tr>
<tr class="odd">
<td style="text-align: left;">All other forms of heart disease</td>
<td style="text-align: left;">59</td>
<td style="text-align: left;">612</td>
<td style="text-align: right;">2019</td>
<td style="text-align: right;">5</td>
</tr>
<tr class="even">
<td style="text-align: left;">All other diseases (Residual)</td>
<td style="text-align: left;">95</td>
<td style="text-align: left;">1,733</td>
<td style="text-align: right;">2020</td>
<td style="text-align: right;">1</td>
</tr>
<tr class="odd">
<td style="text-align: left;">COVID-19 (U07.1)</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">969</td>
<td style="text-align: right;">2020</td>
<td style="text-align: right;">2</td>
</tr>
<tr class="even">
<td style="text-align: left;">Alzheimer’s disease</td>
<td style="text-align: left;">48</td>
<td style="text-align: left;">904</td>
<td style="text-align: right;">2020</td>
<td style="text-align: right;">3</td>
</tr>
<tr class="odd">
<td style="text-align: left;">All other forms of chronic ischemic heart
disease</td>
<td style="text-align: left;">55</td>
<td style="text-align: left;">771</td>
<td style="text-align: right;">2020</td>
<td style="text-align: right;">4</td>
</tr>
<tr class="even">
<td style="text-align: left;">Cerebrovascular diseases</td>
<td style="text-align: left;">61</td>
<td style="text-align: left;">642</td>
<td style="text-align: right;">2020</td>
<td style="text-align: right;">5</td>
</tr>
</tbody>
</table>

### example #3: Calculating years of potential life lost due to COVID-19, assuming age of death at 65 as the standard

Again, let’s load 2020 death data.

``` r
ex3 <- get_data_death(year = 2020, 
                      cols = c('chi_age', 'underlying_cod_code'))
```

Now, identify deaths due to COVID-19. Remember COVID-19 is not a one of
the official 113 causes of death, so let’s arbitrarily get causeid #1
(Salmonella infections), knowing that it will also give us COVID-19.

``` r
dt3 <- death_113_count(ph.data = ex3, 
                       causeids = 1,
                       kingco = FALSE,
                       ypll_age = 65)
dt3 <- dt3[cause.of.death == 'COVID-19 (U07.1)'] # limit output to COVID-19
```

<table>
<thead>
<tr class="header">
<th style="text-align: left;">cause.of.death</th>
<th style="text-align: left;">causeid</th>
<th style="text-align: left;">deaths</th>
<th style="text-align: left;">ypll_65</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">COVID-19 (U07.1)</td>
<td style="text-align: left;">NA</td>
<td style="text-align: left;">969</td>
<td style="text-align: left;">1,396</td>
</tr>
</tbody>
</table>

The table above tells us that, if everyone who died from COVID-19 in
2020 had lived to at least age 65, they would have lived a total of
1,396 additional years.

# death_130_count()

`death_130()` and `death_130_count()` work in exactly the same way as
`death_113()` and `death_113_count()` and will therefore not be
described in detail.

# death_injury_matrix_count()

`death_injury_matrix_count()` allows the user to get injury related
death counts by intent and mechanism. To review a table of all available
combinations of mechanism and intent, simply type
`death_injury_matrix()` into your R console. Here are the first six
lines …

``` r
  mech.n.intent <- death_injury_matrix()[1:6]
```

<table>
<thead>
<tr class="header">
<th style="text-align: left;">mechanism</th>
<th style="text-align: left;">intent</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">All injury</td>
<td style="text-align: left;">Unintentional</td>
</tr>
<tr class="even">
<td style="text-align: left;">All injury</td>
<td style="text-align: left;">Suicide</td>
</tr>
<tr class="odd">
<td style="text-align: left;">All injury</td>
<td style="text-align: left;">Homicide</td>
</tr>
<tr class="even">
<td style="text-align: left;">All injury</td>
<td style="text-align: left;">Undetermined</td>
</tr>
<tr class="odd">
<td style="text-align: left;">All injury</td>
<td style="text-align: left;">Legal intervention/war</td>
</tr>
<tr class="even">
<td style="text-align: left;">Cut/pierce</td>
<td style="text-align: left;">Unintentional</td>
</tr>
</tbody>
</table>

`death_injury_matrix_count()` takes eight potential arguments:

-   `ph.data`: the name of a person level data.table/data.frame of death
    data with ICD codes
-   `intent`: specifies the intent(s) of interest as per options
    available in `death_injury_matrix`. ‘none’ will ignore intent and
    return ‘Any intent’. ‘\*’ is a wildcard for all possible intents.
-   `mechanism`: specifies the mechanism(s) of interest as per options
    available in `death_injury_matrix`. ‘none’ will ignore mechanism and
    return ‘Any mechansim’. ‘\*’ is a wildcard for all possible
    mechanisms.
-   `icdcol`: name of the column with the ICD codes of interest.
    Defaults to “underlying_cod_code”, which is used in APDE’s data.
-   `kingco`: logical (T|F) specifying whether to limit the data
    analysis to King County. Only works if ph.data still has the
    `chi_geo_kc` column.
-   `group_by`: identifies the variables by which you want to group
    (a.k.a., stratify) the results.
-   `ypll_age`: optional. The age in years (an integer) used for
    calculating Years of Potential Life Lost.
-   `death_age_col`: optional. Name of the age column used if `ypll_age`
    is specified.

Please refer to the help file for more details.

### example #4: 2020 King County injury related deaths (every combination of intent and mechanism), stratified by sex

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
                                 kingco = FALSE, 
                                 group_by = 'chi_sex')[1:6]
```

<table>
<thead>
<tr class="header">
<th style="text-align: left;">mechanism</th>
<th style="text-align: left;">intent</th>
<th style="text-align: left;">deaths</th>
<th style="text-align: left;">chi_sex</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">All injury</td>
<td style="text-align: left;">Homicide</td>
<td style="text-align: left;">23</td>
<td style="text-align: left;">Female</td>
</tr>
<tr class="even">
<td style="text-align: left;">All injury</td>
<td style="text-align: left;">Homicide</td>
<td style="text-align: left;">76</td>
<td style="text-align: left;">Male</td>
</tr>
<tr class="odd">
<td style="text-align: left;">All injury</td>
<td style="text-align: left;">Legal intervention/war</td>
<td style="text-align: left;">1</td>
<td style="text-align: left;">Female</td>
</tr>
<tr class="even">
<td style="text-align: left;">All injury</td>
<td style="text-align: left;">Legal intervention/war</td>
<td style="text-align: left;">7</td>
<td style="text-align: left;">Male</td>
</tr>
<tr class="odd">
<td style="text-align: left;">All injury</td>
<td style="text-align: left;">Suicide</td>
<td style="text-align: left;">69</td>
<td style="text-align: left;">Female</td>
</tr>
<tr class="even">
<td style="text-align: left;">All injury</td>
<td style="text-align: left;">Suicide</td>
<td style="text-align: left;">186</td>
<td style="text-align: left;">Male</td>
</tr>
</tbody>
</table>

Notice that you did not have to specify that you wanted the results
stratified by sex. The function will automatically stratify by whatever
extra columns are contained in `ph.data`. Be mindful of this and be sure
that you submit `ph.data` with the minimum necessary number of columns.

**Now, what happens if you set both intent & mechanism to “none”?** The
function assumes you want the total injury related deaths (without
regard to intent or mechanism).

``` r
dt4.alt <- death_injury_matrix_count(ph.data = ex4, 
                                 intent = "none", 
                                 mechanism = "none",
                                 kingco = FALSE, 
                                 group_by = 'chi_sex')
```

<table>
<thead>
<tr class="header">
<th style="text-align: left;">mechanism</th>
<th style="text-align: left;">intent</th>
<th style="text-align: left;">deaths</th>
<th style="text-align: left;">chi_sex</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Any mechanism</td>
<td style="text-align: left;">Any intent</td>
<td style="text-align: left;">416</td>
<td style="text-align: left;">Female</td>
</tr>
<tr class="even">
<td style="text-align: left;">Any mechanism</td>
<td style="text-align: left;">Any intent</td>
<td style="text-align: left;">931</td>
<td style="text-align: left;">Male</td>
</tr>
</tbody>
</table>

### example #5: 2016-2020 King County homicides (intent) by firearms (mechanism), stratified by sex

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

<table>
<thead>
<tr class="header">
<th style="text-align: left;">mechanism</th>
<th style="text-align: left;">intent</th>
<th style="text-align: right;">deaths</th>
<th style="text-align: left;">chi_sex</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Firearm</td>
<td style="text-align: left;">Homicide</td>
<td style="text-align: right;">43</td>
<td style="text-align: left;">Female</td>
</tr>
<tr class="even">
<td style="text-align: left;">Firearm</td>
<td style="text-align: left;">Homicide</td>
<td style="text-align: right;">218</td>
<td style="text-align: left;">Male</td>
</tr>
</tbody>
</table>

### example #6: years of potential life lost due to firearm homicides 2016-2020

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

<table>
<thead>
<tr class="header">
<th style="text-align: left;">mechanism</th>
<th style="text-align: left;">intent</th>
<th style="text-align: right;">deaths</th>
<th style="text-align: left;">ypll_65</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Firearm</td>
<td style="text-align: left;">Homicide</td>
<td style="text-align: right;">261</td>
<td style="text-align: left;">8,727</td>
</tr>
</tbody>
</table>

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

-   `ph.data`: the name of a person level data.table/data.frame of death
    data with ICD codes
-   `cause`: a character vector specifying the cause(s) of death of
    interest taken from the list provided by `death_other()`. It is case
    insensitive and matches partial strings
-   `icdcol`: name of the column with the ICD codes of interest.
    Defaults to “underlying_cod_code”, which is used in APDE’s data
-   `kingco`: logical (T|F) specifying whether to limit the data
    analysis to King County. Only works if ph.data still has the
    `chi_geo_kc` column.
-   `group_by`: identifies the variables by which you want to group
    (a.k.a., stratify) the results.
-   `ypll_age`: optional. The age in years (an integer) used for
    calculating Years of Potential Life Lost.
-   `death_age_col`: optional. Name of the age column used if `ypll_age`
    is specified.

Please refer to the help file for more details.

### example #7: Count the number of heat stress deaths for 2017-2021, stratified by year

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

<table>
<thead>
<tr class="header">
<th style="text-align: left;">cause.of.death</th>
<th style="text-align: left;">deaths</th>
<th style="text-align: right;">chi_year</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">HeatStress_Death</td>
<td style="text-align: left;">0</td>
<td style="text-align: right;">2017</td>
</tr>
<tr class="even">
<td style="text-align: left;">HeatStress_Death</td>
<td style="text-align: left;">1</td>
<td style="text-align: right;">2018</td>
</tr>
<tr class="odd">
<td style="text-align: left;">HeatStress_Death</td>
<td style="text-align: left;">1</td>
<td style="text-align: right;">2019</td>
</tr>
<tr class="even">
<td style="text-align: left;">HeatStress_Death</td>
<td style="text-align: left;">0</td>
<td style="text-align: right;">2020</td>
</tr>
<tr class="odd">
<td style="text-align: left;">HeatStress_Death</td>
<td style="text-align: left;">28</td>
<td style="text-align: right;">2021</td>
</tr>
</tbody>
</table>

### example #8: Count the number of cancer and heart disease deaths in 2021

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

<table>
<thead>
<tr class="header">
<th style="text-align: left;">cause.of.death</th>
<th style="text-align: left;">deaths</th>
<th style="text-align: left;">ypll_65</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">All causes</td>
<td style="text-align: left;">15,033</td>
<td style="text-align: left;">68,480</td>
</tr>
<tr class="even">
<td style="text-align: left;">Cancer</td>
<td style="text-align: left;">2,988</td>
<td style="text-align: left;">8,268</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Heart disease</td>
<td style="text-align: left;">2,879</td>
<td style="text-align: left;">5,440</td>
</tr>
</tbody>
</table>

# life_table()

`life_table()` generates a standard life table, where the first row is
the life expectancy (aka ‘expectation of life’) at birth. Since it needs
aggregate death data, you are **strongly encouraged to preprocess the
death data with `life_table_prep()`.**

`life_table()` takes six potential arguments:

-   `ph.data`: the name of data.table/data.frame with aggregated deaths
    and corresponding populations, as well as the age interval and the
    average fraction of years lived in the interval by those who die in
    the interval.
-   `myages`: the name of a column in `ph.data` with the age intervals
    used for the life table calculations (e.g., c(‘0-1’, ‘1-5’, …
    ‘85+’)).
-   `mydeaths`: the name of a numeric column in `ph.data` with the total
    deaths for the given age interval in the given year.
-   `mypops`: the name of a numeric column in `ph.data` with the total
    population corresponding to `mydeaths`.
-   `myprops`: the name of a numeric column in `ph.data` with the
    average proportion of the interval lived by those who died in the
    interval.
-   `ci`: the confidence interval, must be \[0, 1\]. Default is 0.95

Please refer to the help file for more details.

### example #9: Get life expectancy at birth for King County residents born 2016-2020, stratified by sex

Get the critical death data

``` r
ex9 <- get_data_death(year = 2016:2020, 
                        cols = c('date_of_birth', 'date_of_death', 'chi_sex'))
```

To preserve privacy, lets look at a summary of the table rather than the
actual table

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
dt9 = life_table_prep(DTx = ex9)
```

<table>
<thead>
<tr class="header">
<th style="text-align: left;">chi_sex</th>
<th style="text-align: left;">ages</th>
<th style="text-align: right;">deaths</th>
<th style="text-align: right;">fraction</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Male</td>
<td style="text-align: left;">NA</td>
<td style="text-align: right;">2</td>
<td style="text-align: right;">NaN</td>
</tr>
<tr class="even">
<td style="text-align: left;">Female</td>
<td style="text-align: left;">0-1</td>
<td style="text-align: right;">212</td>
<td style="text-align: right;">0.0613904</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Male</td>
<td style="text-align: left;">0-1</td>
<td style="text-align: right;">268</td>
<td style="text-align: right;">0.0559866</td>
</tr>
<tr class="even">
<td style="text-align: left;">NA</td>
<td style="text-align: left;">1-5</td>
<td style="text-align: right;">1</td>
<td style="text-align: right;">0.7156164</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Female</td>
<td style="text-align: left;">1-5</td>
<td style="text-align: right;">28</td>
<td style="text-align: right;">0.2480545</td>
</tr>
<tr class="even">
<td style="text-align: left;">Male</td>
<td style="text-align: left;">1-5</td>
<td style="text-align: right;">34</td>
<td style="text-align: right;">0.4037292</td>
</tr>
</tbody>
</table>

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

<table>
<thead>
<tr class="header">
<th style="text-align: left;">gender</th>
<th style="text-align: left;">ages</th>
<th style="text-align: right;">deaths</th>
<th style="text-align: right;">fraction</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Female</td>
<td style="text-align: left;">0-1</td>
<td style="text-align: right;">212</td>
<td style="text-align: right;">0.0613904</td>
</tr>
<tr class="even">
<td style="text-align: left;">Male</td>
<td style="text-align: left;">0-1</td>
<td style="text-align: right;">268</td>
<td style="text-align: right;">0.0559866</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Female</td>
<td style="text-align: left;">1-5</td>
<td style="text-align: right;">28</td>
<td style="text-align: right;">0.2480545</td>
</tr>
<tr class="even">
<td style="text-align: left;">Male</td>
<td style="text-align: left;">1-5</td>
<td style="text-align: right;">34</td>
<td style="text-align: right;">0.4037292</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Female</td>
<td style="text-align: left;">10-15</td>
<td style="text-align: right;">26</td>
<td style="text-align: right;">0.5178314</td>
</tr>
<tr class="even">
<td style="text-align: left;">Male</td>
<td style="text-align: left;">10-15</td>
<td style="text-align: right;">40</td>
<td style="text-align: right;">0.4695332</td>
</tr>
</tbody>
</table>

The death data is now ready for `life_table`, but we still need to merge
on the corresponding populations. Let’s start by getting the raw
population for the same time period (2016-2020) by gender and age.

``` r
pop <- get_population(kingco = T, 
                      years = 2016:2020, 
                      group_by = c('genders', 'ages'))
pop <- pop[, .(gender, age, pop)]
```

<table>
<thead>
<tr class="header">
<th style="text-align: left;">gender</th>
<th style="text-align: right;">age</th>
<th style="text-align: right;">pop</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Male</td>
<td style="text-align: right;">0</td>
<td style="text-align: right;">66699.47</td>
</tr>
<tr class="even">
<td style="text-align: left;">Female</td>
<td style="text-align: right;">0</td>
<td style="text-align: right;">65156.33</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Male</td>
<td style="text-align: right;">1</td>
<td style="text-align: right;">65280.55</td>
</tr>
<tr class="even">
<td style="text-align: left;">Female</td>
<td style="text-align: right;">1</td>
<td style="text-align: right;">62821.88</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Male</td>
<td style="text-align: right;">2</td>
<td style="text-align: right;">67669.05</td>
</tr>
<tr class="even">
<td style="text-align: left;">Female</td>
<td style="text-align: right;">2</td>
<td style="text-align: right;">61472.62</td>
</tr>
</tbody>
</table>

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

<table>
<thead>
<tr class="header">
<th style="text-align: left;">gender</th>
<th style="text-align: left;">ages</th>
<th style="text-align: right;">pop</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Male</td>
<td style="text-align: left;">0-1</td>
<td style="text-align: right;">66699.47</td>
</tr>
<tr class="even">
<td style="text-align: left;">Female</td>
<td style="text-align: left;">0-1</td>
<td style="text-align: right;">65156.33</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Male</td>
<td style="text-align: left;">1-5</td>
<td style="text-align: right;">255888.56</td>
</tr>
<tr class="even">
<td style="text-align: left;">Female</td>
<td style="text-align: left;">1-5</td>
<td style="text-align: right;">244458.54</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Male</td>
<td style="text-align: left;">5-10</td>
<td style="text-align: right;">322774.96</td>
</tr>
<tr class="even">
<td style="text-align: left;">Female</td>
<td style="text-align: left;">5-10</td>
<td style="text-align: right;">310606.85</td>
</tr>
</tbody>
</table>

Merge the properly formatted death and population data together.

``` r
dt9 <- merge(dt9, 
             pop, 
             by = c('gender', 'ages'), 
             all = T)
```

<table>
<thead>
<tr class="header">
<th style="text-align: left;">gender</th>
<th style="text-align: left;">ages</th>
<th style="text-align: right;">deaths</th>
<th style="text-align: right;">fraction</th>
<th style="text-align: right;">pop</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Female</td>
<td style="text-align: left;">0-1</td>
<td style="text-align: right;">212</td>
<td style="text-align: right;">0.0613904</td>
<td style="text-align: right;">65156.33</td>
</tr>
<tr class="even">
<td style="text-align: left;">Female</td>
<td style="text-align: left;">1-5</td>
<td style="text-align: right;">28</td>
<td style="text-align: right;">0.2480545</td>
<td style="text-align: right;">244458.54</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Female</td>
<td style="text-align: left;">10-15</td>
<td style="text-align: right;">26</td>
<td style="text-align: right;">0.5178314</td>
<td style="text-align: right;">302747.86</td>
</tr>
<tr class="even">
<td style="text-align: left;">Female</td>
<td style="text-align: left;">15-18</td>
<td style="text-align: right;">42</td>
<td style="text-align: right;">0.4281800</td>
<td style="text-align: right;">171470.75</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Female</td>
<td style="text-align: left;">18-20</td>
<td style="text-align: right;">29</td>
<td style="text-align: right;">0.3798422</td>
<td style="text-align: right;">128079.03</td>
</tr>
<tr class="even">
<td style="text-align: left;">Female</td>
<td style="text-align: left;">20-25</td>
<td style="text-align: right;">102</td>
<td style="text-align: right;">0.4517549</td>
<td style="text-align: right;">340662.74</td>
</tr>
</tbody>
</table>

Run `life_table()` function to create life tables for males and females.

``` r
dt9.male <- life_table(ph.data = dt9[gender == 'Male'])
dt9.female <- life_table(ph.data = dt9[gender == 'Female'])
```

Display the first row of the life table, which is for children under 1.
The `ex` column is the life expectancy at birth. To see a detailed key
of what all the columns mean, type `?life_table` in your R console and
read the ‘Details’ section.

<table>
<colgroup>
<col style="width: 6%" />
<col style="width: 4%" />
<col style="width: 8%" />
<col style="width: 6%" />
<col style="width: 9%" />
<col style="width: 7%" />
<col style="width: 5%" />
<col style="width: 5%" />
<col style="width: 3%" />
<col style="width: 4%" />
<col style="width: 5%" />
<col style="width: 7%" />
<col style="width: 4%" />
<col style="width: 8%" />
<col style="width: 8%" />
<col style="width: 5%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">gender</th>
<th style="text-align: left;">ages</th>
<th style="text-align: right;">pop</th>
<th style="text-align: right;">deaths</th>
<th style="text-align: right;">fraction</th>
<th style="text-align: right;">mx</th>
<th style="text-align: right;">qx</th>
<th style="text-align: right;">lx</th>
<th style="text-align: right;">dx</th>
<th style="text-align: right;">ax</th>
<th style="text-align: right;">Lx</th>
<th style="text-align: right;">Tx</th>
<th style="text-align: right;">ex</th>
<th style="text-align: right;">ex_lower</th>
<th style="text-align: right;">ex_upper</th>
<th style="text-align: right;">ex_se</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Male</td>
<td style="text-align: left;">0-1</td>
<td style="text-align: right;">66699.47</td>
<td style="text-align: right;">268</td>
<td style="text-align: right;">0.0559866</td>
<td style="text-align: right;">0.00402</td>
<td style="text-align: right;">0.004</td>
<td style="text-align: right;">1e+05</td>
<td style="text-align: right;">400</td>
<td style="text-align: right;">0.06</td>
<td style="text-align: right;">99622</td>
<td style="text-align: right;">7950038</td>
<td style="text-align: right;">79.5</td>
<td style="text-align: right;">79.38</td>
<td style="text-align: right;">79.62</td>
<td style="text-align: right;">0.063</td>
</tr>
</tbody>
</table>

<table>
<colgroup>
<col style="width: 6%" />
<col style="width: 4%" />
<col style="width: 7%" />
<col style="width: 6%" />
<col style="width: 8%" />
<col style="width: 6%" />
<col style="width: 6%" />
<col style="width: 5%" />
<col style="width: 3%" />
<col style="width: 4%" />
<col style="width: 5%" />
<col style="width: 6%" />
<col style="width: 5%" />
<col style="width: 7%" />
<col style="width: 7%" />
<col style="width: 6%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">gender</th>
<th style="text-align: left;">ages</th>
<th style="text-align: right;">pop</th>
<th style="text-align: right;">deaths</th>
<th style="text-align: right;">fraction</th>
<th style="text-align: right;">mx</th>
<th style="text-align: right;">qx</th>
<th style="text-align: right;">lx</th>
<th style="text-align: right;">dx</th>
<th style="text-align: right;">ax</th>
<th style="text-align: right;">Lx</th>
<th style="text-align: right;">Tx</th>
<th style="text-align: right;">ex</th>
<th style="text-align: right;">ex_lower</th>
<th style="text-align: right;">ex_upper</th>
<th style="text-align: right;">ex_se</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Female</td>
<td style="text-align: left;">0-1</td>
<td style="text-align: right;">65156.33</td>
<td style="text-align: right;">212</td>
<td style="text-align: right;">0.0613904</td>
<td style="text-align: right;">0.00325</td>
<td style="text-align: right;">0.00324</td>
<td style="text-align: right;">1e+05</td>
<td style="text-align: right;">324</td>
<td style="text-align: right;">0.06</td>
<td style="text-align: right;">99696</td>
<td style="text-align: right;">8405505</td>
<td style="text-align: right;">84.06</td>
<td style="text-align: right;">83.94</td>
<td style="text-align: right;">84.17</td>
<td style="text-align: right;">0.05745</td>
</tr>
</tbody>
</table>

# Conclusion

We know this was a lot to process. The good news is that this vignette
isn’t going anywhere. If you remember (a) that this vignette exists and
(b) where to find it, you’ll be in good shape to take on standard
mortality analyses in the future.

If you’ve read through this vignette and the corresponding help files
and are still confused, please feel free to reach out for assistance.
You may have found a bug, who knows? Good luck!
