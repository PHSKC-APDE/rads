---
title: "get_data()"
output:
  rmarkdown::html_vignette: default
  github_document: default
vignette: >
  %\VignetteIndexEntry{get_data}
  %\VignetteEngine{knitr::knitr}
---

## Introduction

This vignette will provide some examples of ways to explore and load [birth data](https://github.com/PHSKC-APDE/DOHdata/tree/master/ETL/birth) using the `get_data_birth()` functions in [`rads`](https://github.com/PHSKC-APDE/rads), APDE's 'R Automated Data System'. As of April 2023, `get_data_chars()`, `get_data_death()`, and `get_data_hys()` also exist. In the future, `rads` may allow users to easily load additional datasets (e.g., BRFSS). You are encouraged to read the detailed help files that exist for each data source.

**NOTE!!** To get the most out of this vignette, we recommend that you actually type each and every bit of code into R. Doing so will almost definitely help you learn the syntax much faster than just reading the vignette or copying and pasting the code.

## Loading data with get_data_birth()

Begin by loading the [`rads`](https://github.com/PHSKC-APDE/rads) package/library by typing the following in R:


```r
library(rads)
```

The analytic ready birth data is stored in on KCIT SQL Server 50 (`[PH_APDEStore].[final].[bir_wa]`). The `get_data_birth()` function will allow you to pull data from the SQL server with minimal fuss. To see the possible arguments that you can pass, use the `args()` function by typing the following:


```r
args(get_data_birth)
```

```
## function (cols = NA, year = NA, kingco = T) 
## NULL
```

You can see that `get_data_birth` takes three possible arguments:

1)  `cols` \<\< a vector of the specific columns that you want to load into memory, e.g., `c("chi_year", "chi_geo_regions_4")`. If it is not specified, the default is `NA`, which will pull all available columns.

    -   In the future, we hope to integrate standardized documentation, including a data dictionary, into `rads`. For now, manually [open the data dictionary](https://github.com/PHSKC-APDE/DOHdata/blob/master/ETL/birth/ref/ref_bir_user_dictionary_final.csv) to see the available variables.

2)  `year` \<\< a vector of the year or years of data that you want to load, e.g., c(2011, 2015:2018). Note that the default is to load 2017 data only.

3)  `kingco` \<\< a logical argument (i.e., `T` or `F` only, without quotes) denoting whether or not the data should be limited to King County. The default is King County only.

Let's try the function to see how it works by loading the year and King County columns for WA State in 2019:


```r
birth <- get_data_birth(cols = c("chi_year", "chi_geo_kc"), year = c(2019), kingco = F)
```

We can confirm the `birth` object is in our environment by typing `ls()`


```r
ls() 
```

```
##  [1] "birth"                "births"               "deaths"               "est"                  "ex1.1"                "ex1.2"               
##  [7] "ex2.1"                "ex2.2"                "ex3.1"                "ex4.1"                "ex5.1"                "grades.distribution" 
## [13] "grades.distribution2" "household.wa"         "kcbirth"              "kcpop"                "mtcars"               "mydt"                
## [19] "mysvy"                "new.standard"         "nlines"               "out"                  "person.wa"            "pop"                 
## [25] "population"           "pums"                 "pums2"                "rl"                   "rmds"                 "rrr"                 
## [31] "start_dir"            "td"                   "temp1"                "temp2"                "temp3"                "temp4"               
## [37] "temp5"                "test1"                "test2"                "wabirth"              "wapop"                "wiki"                
## [43] "xwalk"
```

To identify the class of the `birth` object and to confirm that our columns are present, we can type `str(birth)`


```r
str(birth) 
```

```
## Classes 'data.table' and 'data.frame':	86154 obs. of  2 variables:
##  $ chi_geo_kc: Factor w/ 1 level "King County": NA NA NA NA NA NA NA NA NA NA ...
##  $ chi_year  : int  2019 2019 2019 2019 2019 2019 2019 2019 2019 2019 ...
##  - attr(*, ".internal.selfref")=<externalptr>
```

We can see that 'birth' is a data.table and a data.frame and has our columns of interest. Also, in case you missed it when using `str()`, the `dim()` function tells us the dimensions of the `birth` table. In this case, it has 86154 rows and 2 columns


```r
dim(birth) 
```

```
## [1] 86154     2
```

Use the `head()` command to take a peak at the first 6 lines of the `birth` table


```r
head(birth) 
```

```
##    chi_geo_kc chi_year
## 1:       <NA>     2019
## 2:       <NA>     2019
## 3:       <NA>     2019
## 4:       <NA>     2019
## 5:       <NA>     2019
## 6:       <NA>     2019
```

## Save time by identifying columns of interest 
As mentioned and demonstrated above, you can use the `cols` argument to save time by downloading a limited data set from the SQL servers. However, to do so, you need to know the column names. This is why we've created the`list_dataset_columns()` function. The sole argument is the name of the dataset (e.g., 'birth', 'chars', etc.) and the output is a table with all of the variable names. 


```r
head(list_dataset_columns('birth'))
```

```
##              var.names analytic_ready
## 1:  birth_cert_encrypt           TRUE
## 2:     birth_cert_type           TRUE
## 3:                 sex           TRUE
## 4: date_of_birth_month           TRUE
## 5:  date_of_birth_year           TRUE
## 6:       time_of_birth           TRUE
```

## Loading data with get_data()

Note that the same dataset can be retrieved using the `get_data()` function. Since this is generalized for multiple data sources, you will need to specify 'birth' as the data set.


```r
birth2 <- get_data(dataset = "birth", c("chi_year", "chi_geo_kc"), year = c(2019), kingco = F)
dim(birth2)
```

```
## [1] 86154     2
```

We can confirm that the birth and birth2 data.tables are the same using data.table's `fsetequal()` function


```r
data.table::fsetequal(birth, birth2)
```

```
## [1] TRUE
```

`get_data()` is generic and provides a shortcut for loading datasets that have been cleaned and compiled. You will be able to identify these datasets with `list_apde_data()`


```r
list_apde_data()
```

```
## [1] "hys"   "birth" "chars" "death"
```
