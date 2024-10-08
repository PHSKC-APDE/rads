# R Automatic Data System (RADS)

## Purpose

RADS, APDE's 'R Automated Data System, is a suite of tools written in R and designed to make standard public health analyses faster, more standardized, and less prone to error. While some tools may be applicable for different settings, the toolset has been customized to the needs of [PHSKC's](https://www.kingcounty.gov/depts/health.aspx) [APDE](https://www.kingcounty.gov/depts/health/data). While these tools have only been tested in Windows, they should work identically on a Linux or Mac OS X machine.

## Installation

If you haven't yet installed [`rads`](https://github.com/PHSKC-APDE/rads), follow these steps:

1.  Make sure remotes is installed ... `install.packages("remotes")`.

2.  RADS depends on version 17 of [Microsoft ODBC Driver for SQL Server](https://docs.microsoft.com/en-us/sql/connect/odbc/download-odbc-driver-for-sql-server?view=sql-server-ver15). You can download it [here](https://go.microsoft.com/fwlink/?linkid=2187214)

3.  Install a keyring:: 'service' for accessing HHSAW. 
    -   `install.packages("keyring")`
    
    -   `keyring::key_set(service = 'hhsaw', username = 'Your.KCUsername@kingcounty.gov')`
    
    -   In the pop-up window type in your standard King County password (the one you use to log into your laptop) and click `OK`

4.  Install [`rads`](https://github.com/PHSKC-APDE/rads) ... `remotes::install_github("PHSKC-APDE/rads", auth_token = NULL)`

    -   To install github from a particular branch, specify it with the 'ref' argument, e.g., `remotes::install_github("PHSKC-APDE/rads", ref = "dev", auth_token = NULL)`

5.  Load [`rads`](https://github.com/PHSKC-APDE/rads) ... `library(rads)`

## New for version 1.2.1.0
1. `get_population` now accepts 'ccl', 'csa', 'inc_uninc', 'puma', 'kccd', and 'tribal' as valid geography types (`geo_type`). These refer to crisis care levy areas, community service areas, incorperated and unincorperated king county, public use microdata areas, king county council districts, and tribal lands respectively.
2. `rads::calc` can now handle `imputationList` type inputs as data. See the [wiki]([https://github.com/PHSKC-APDE/rads/wiki/calc](https://github.com/PHSKC-APDE/rads/wiki/calc#example-analyses-with-resampled-datamultiple-imputation) for more information.
3. `rads::calc` now has sorted outputs. This is a a minor, potentially breaking change.

## New for version 1.2.0.0
1. `get_data_hys` updated to pull 2023 data.
2. `get_population` data updated to pull estimates by hispanic/non-hispanic within a race (as opposed to Hispanic as its own race).
3. Updated error messages in `calc`
4. Updated documentation throughout

## New for version 1.1.7.7
1. You must create a keyring:: 'service' key for HHSAW as described above. If you do not do this, you will not be able to use `get_population`, `get_data*`, and `list_dataset_columns()` functions.

## New for version 1.1.5
1. `get_population` now defaults to 2020 vintage geographies (e.g. new HRAs)

## New for version 1.1.4

1.  `get_population` has been overhauled and now is able to access [frankenpop aka Population Interim Estimates (PIE)](https://github.com/PHSKC-APDE/frankenpop_pub)**.** For the most part, existing `get_population` calls should still work-- although the estimates being pulled will differ slightly:
    1.  `round` now defaults to `FALSE`. Now you can have fractional people. We recommend rounding before publishing/sharing results so as not to confuse people.

    2.  **Frankenpop** estimates are now the default population numbers as governed by `census_vintage = 2020`. Setting the `census_vintage` argument to `2010` will return the old OFM SADE estimates based on Census 2010. These estimates are out of date and should only be used sparingly.

    3.  Users can specify the *vintage* of geography they want via the `geo_vintage` option. `geo_vintage` (current default is 2010 although **this will change to 2020 on the next release**) refers to the decennial census associate with a class of geography. For example, all census geographies change with each decennial census and so do political districts.

    4.  Queries, especially for region and HRA data should be noticiably faster.
2.  It is now easier to pull HYS data (especially staged data)
3.  `calc` no longer requires a `dtsurvey` type object as an input. `data.frame`s and other dataset types are converted on the fly based on the needs of `calc`. This means you dplyr heathens can do some data cleaning with your pipes and mutations without having to bounce back and forth between data types.

## New for version 1.0.0

Version 1.0.0 includes a major overhaul of the workhorse `calc` function and the cleaning up of some dependencies.

Potential breaking changes:

1.  `Calc` will only accept objects of type `dtsurvey`, `dtrepsurvey`, or `dtadmin`. The documentation for these types (and the functions used to create them) can be found in the [`dtsurvey`](https://github.com/PHSKC-APDE/dtsurvey) package.

2.  Subsetting a dataset via `calc` no longer makes use of a `dplyr::filter`-esque interface. Instead, via the `where` argument, a user may pass an unquoted (e.g., no " or ') expression that would be valid in the `i` part of a `DT[i,j,by]` command.

3.  The default method for computing confidence intervals for survey data has changed. The practical difference of this change is essentially nil, but interested users can fuss around with the differences between the `xlogit` (new) and `logit` methods to computing CIs via `survey::svyciprop`.

## Getting started

After installation, we highly recommend that you start by walking through a vignette on the [wiki](https://github.com/PHSKC-APDE/rads/wiki). \* [calc()](https://github.com/PHSKC-APDE/rads/wiki/calc) \* [get_population()](https://github.com/PHSKC-APDE/rads/wiki/get_population) \* [get_data()](https://github.com/PHSKC-APDE/rads/wiki/get_data) \* [age_standardize()](https://github.com/PHSKC-APDE/rads/wiki/age_standardize)

## Problems?

-   If you come across a bug or have specific suggestions for improvement, please click on ["Issues"](https://github.com/PHSKC-APDE/rads/issues) at the top of this page and then click ["New Issue"](https://github.com/PHSKC-APDE/rads/issues/new/choose) and provide the necessary details.
