# R Automatic Data System (RADS)

## Purpose
RADS, APDE’s ‘R Automated Data System, is a suite of tools written in R and designed to make standard public health analyses faster, more standardized, and less prone to error. While some tools may be applicable for different settings, the toolset has been customized to the needs of [PHSKC's](https://www.kingcounty.gov/depts/health.aspx) [APDE](https://www.kingcounty.gov/depts/health/data). While these tools have only been tested in Windows, they should work identically on a Linux or Mac OS X machine.    

## Installation

If you haven’t yet installed [`rads`](https://github.com/PHSKC-APDE/rads), follow these steps:

1. Make sure devtools is installed … `install.packages("remotes")`.

2. Install `data.table` ... `install.packages("data.table")`. Update `data.table` to use the dev package (until they release the new version):
   
   `data.table::update.dev.pkg()`

3. Install [`dtsurvey`](https://github.com/PHSKC-APDE/dtsurvey)
   `remotes::install_github("PHSKC-APDE/dtsurvey", auth_token = NULL)`

4. Install [`rads`](https://github.com/PHSKC-APDE/rads) …
    `remotes::install_github("PHSKC-APDE/rads", auth_token = NULL)`
    * To install github from a particular branch, specify it with the 'ref' argument, e.g., `remotes::install_github("PHSKC-APDE/rads", ref = "dev", auth_token = NULL)`

5. Load [`rads`](https://github.com/PHSKC-APDE/rads) … `library(rads)`

6. Exit RStudio and start it again. 

7. Confirm `rads` installed properly by typing `library(rads)` in the console.

## New for version 1.0.0
Version 1.0.0 includes a major overhaul of the workhorse `calc` function and the cleaning up of some dependencies.

Potential breaking changes:

1. `Calc` will only accept objects of type `dtsurvey`, `dtrepsurvey`, or `dtadmin`. The documentation for these types (and the functions used to create them) can be found in the [`dtsurvey`](https://github.com/PHSKC-APDE/dtsurvey) package.

2. Subsetting a dataset via `calc` no longer makes use of a `dplyr::filter`-esque interface. Instead, via the `where` argument, a user may pass an unquoted (e.g., no " or ') expression that would be valid in the `i` part of a `DT[i,j,by]` command.

3. The default method for computing confidence intervals for survey data has changed. The practical difference of this change is essentially nil, but interested users can fuss around with the differences between the `xlogit` (new) and `logit` methods to computing CIs via `survey::svyciprop`.

## Getting started
After installation, we highly recommend that you start by walking through a vignette on the [wiki](https://github.com/PHSKC-APDE/rads/wiki).
* [calc()](https://github.com/PHSKC-APDE/rads/wiki/calc)
* [get_population()](https://github.com/PHSKC-APDE/rads/wiki/get_population)
* [get_data()](https://github.com/PHSKC-APDE/rads/wiki/get_data)
* [age_standardize()](https://github.com/PHSKC-APDE/rads/wiki/age_standardize)

## Problems?
* If you come across a bug or have specific suggestions for improvement, please click on ["Issues"](https://github.com/PHSKC-APDE/rads/issues) at the top of this page and then click ["New Issue"](https://github.com/PHSKC-APDE/rads/issues/new/choose) and provide the necessary details. 

