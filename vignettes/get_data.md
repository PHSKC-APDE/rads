Introduction to get\_data\_birth() & get\_data()
================

## Introduction

This vignette will provide some examples of ways to explore and load
[birth
data](https://github.com/PHSKC-APDE/DOHdata/tree/master/ETL/birth) using
the `get_data_birth()` functions in
[`rads`](https://github.com/PHSKC-APDE/rads), APDE’s ‘R Automated Data
System’. In the future, `rads` will allow users to load additional
datasets (e.g., BRFSS, deaths, hospitalizations, etc.) using the generic
`get_data()` wrapper.

**NOTE!!** To get the most out of this vignette, I recommend that you
actually type each and every bit of code into R. Doing so will almost
definitely help you learn the syntax much faster than just reading the
vignette or copying and pasting the code.

## Loading data with get\_data\_birth()

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

1.  `cols` &lt;&lt; a vector of the specific columns that you want to
    load into memory, e.g., `c("chi_year", "chi_geo_regions_4")`. If it
    is not specified, the default is `NA`, which will pull all available
    columns.

    -   In the future, we hope to integrate standardized documentation,
        including a data dictionary, into `rads`. For now, manually
        [open the data
        dictionary](https://github.com/PHSKC-APDE/DOHdata/blob/master/ETL/birth/ref/ref_bir_user_dictionary_final.csv)
        to see the available variables.

2.  `year` &lt;&lt; a vector of the year or years of data that you want
    to load, e.g., c(2011, 2015:2018). Note that the default is to load
    2017 data only.

3.  `kingco` &lt;&lt; a logical argument (i.e., `T` or `F` only, without
    quotes) denoting whether or not the data should be limited to King
    County. The default is King County only.

Let’s try the function to see how it works by loading the year and King
County columns for WA State in 2019:

``` r
birth <- get_data_birth(cols = c("chi_year", "chi_geo_kc"), year = c(2019), kingco = F)
```

We can confirm the `birth` object is in our environment by typing `ls()`

``` r
ls() 
```

    ## [1] "birth"

To identify the class of the `birth` object and to confirm that our
columns are present, we can type `str(birth)`

``` r
str(birth) 
```

    ## Classes 'data.table' and 'data.frame':   86154 obs. of  2 variables:
    ##  $ chi_geo_kc: int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ chi_year  : int  2019 2019 2019 2019 2019 2019 2019 2019 2019 2019 ...
    ##  - attr(*, ".internal.selfref")=<externalptr>

We can see that ‘birth’ is a data.table and a data.frame and has our
columns of interest. Also, in case you missed it when using `str()`, the
`dim()` function tells us the dimensions of the `birth` table. In this
case, it has 86154 rows and 2 columns

``` r
dim(birth) 
```

    ## [1] 86154     2

Use the `head()` command to take a peak at the first 6 lines of the
`birth` table

``` r
head(birth) 
```

    ##    chi_geo_kc chi_year
    ## 1:          0     2019
    ## 2:          0     2019
    ## 3:          0     2019
    ## 4:          0     2019
    ## 5:          0     2019
    ## 6:          0     2019

## Loading data with get\_data()

Note that the same dataset can be retrieved using the `get_data()`
function. Since this is generalized for multiple data sources, you will
need to specify ‘birth’ as the data set.

``` r
birth2 <- get_data(dataset = "birth", c("chi_year", "chi_geo_kc"), year = c(2019), kingco = F)
dim(birth2)
```

    ## [1] 86154     2

We can confirm that the birth and birth2 data.tables are the same using
data.table’s `fsetequal()` function

``` r
data.table::fsetequal(birth, birth2)
```

    ## [1] TRUE

`get_data()` is generic and provides a shortcut for loading datasets
that have been cleaned and compiled. You will be able to identify these
datasets with `list_apde_data()`

``` r
list_apde_data()
```

    ## [1] "hys"   "birth" "bsk"