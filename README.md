# R Automatic Data System (RADS)

## Purpose
RADS is a suite of tools written in R and designed to make standard public health analyses faster, more standardized, and less prone to error. While some tools may be applicable for different settings, the toolset has been customized to the needs of [PHSKC's](https://www.kingcounty.gov/depts/health.aspx) [APDE](https://www.kingcounty.gov/depts/health/data). While these tools have only been tested in Windows, they should work identically on a Linux or Mac OS X machine.    

## Installation
1. Install the [latest version of R from a server near you](https://cran.r-project.org/mirrors.html)
2. Install the free version of [RStudio Desktop](https://rstudio.com/products/rstudio/download/)
3. Install the `devtools` package in R by typing `install.packages("devtools")` in the R console
4.  Follow ***one*** of the following methods:
    1. Install `rads` using devtools::install_github()
        * If needed, set up a [GitHub PAT (Personal Access Token) follwing these instructions](https://github.com/PHSKC-APDE/rads/blob/master/ref/Git_PAT_setup.md)
        * type `devtools::install_github("PHSKC-APDE/rads", auth_token = Sys.getenv("GITHUB_PAT"))` in your R console   
        * Note this will always give you the latest `rads` release  
    2. Install `rads` using devtools::install_local()
        * [Download the rads_#.#.#.zip file](https://github.com/PHSKC-APDE/rads/releases) for the release that you want to install
        * Install all dependencies by typing the following into your R console: `devtools::install_local("C:/Users/[USERNAME]/Downloads/rads_#.#.#.zip")`. Be sure to update the file path to the zip file!
        * Type the following in the R console to install rads `install.packages("C:/Users/[USERNAME]/Downloads/rads_#.#.#.zip", repos = NULL, type = "win.binary")`
5. Exit RStudio and start it again. 
6. Confirm `rads` installed properly by typing `library(rads)` in the console.

## Getting started
After installation, we highly recommend that you start by walking through a vignette.
* "Exploring Birth Data with RADS" 


---
# MOVE REMAINDER ELSEWHERE?




## General workflow idea
1. Use `list_apde_data` to highlight which datasources are available for analysis
2. Use `get_data` to fetch the desired dataset into memory. Use `list_dataset_columns` to investigate which columns are available. `get_data`, given a valid dataset option, dispatches a sub-function/method which returns the data to the user.
3. The user uses `calc` which dispatches to a dataset specific method to conduct tabulations.
4. If required, results from step 3 are passed to `apply_suppression` to make things ready for public release
5. (something about computing CHI)
6. ...

Note: Workflows that are more complex (e.g. using non-standard by/grouping variables) would require custom coding after step 2.

## Design Principles
1. In general, follow the guidelines described by [ROpenSci](https://devguide.ropensci.org/building.html)
2. Primary data frame manipulation is through `data.table` and its associated syntax.
    - Any function using `set` or `:=` functions from the `data.table` package on an object passed as part of the function
    evaluation should have an option `jump_scope` that takes a logical input of length 1. If `TRUE`, the data can be modified by
    reference. If `FALSE` an explict `copy` should be taken. E.g.: `if(!jump_scope) data = copy(data)`. If a function is designed
    to modify by reference, then it should (where relevant) `return(invisible(data))` to allow for chaining.

3. Functions should only rely on explicitly described inputs and should in general, not modify the parent environment. The only
exception is using `data.table`'s modify on reference semantics as described in item #1. Or in other words, don't use `assign` or `<<-`.

4. Functions that are talkative should have a `verbose` argument. To "talk" to the console, use `message` and `warning`. DO NOT USE `cat` or `print`. See the ROpenSci site for further information.

5. Each function should be in its own `.R` file. Small groups of utility functions can be combined within the same `.R` file where relevant.

6. User-written functions should use `package::function` notation when calling functions from other packages. Or in short, we should be as R-check friendly as possible. One purposeful exception can be related to `data.table`'s (and presumably `dplyr`) non-standard evaluation (NSE) framework (e.g. selecting a column like `dt[, mycolumn]` instead of `df[, 'mycolumn']`). Because this package is unlikely to be used outside of APDE, converting the NSE to be R-check friendly seems like more trouble than its worth. See ['Dealing with “undefined global functions or variables”'](https://cran.r-project.org/web/packages/data.table/vignettes/datatable-importing.html)

7. All non-trivial functions should be accompanied by tests using the `testthat` package to ensure functionality stability.

## Work Plan
1. Define Scope
2. Identify modules
3. Create and test modules
4. Conduct "all-up" testing
5. ...

## Relevant resources for package construction
1. https://devguide.ropensci.org/building.html
2. http://r-pkgs.had.co.nz/
3. https://cran.r-project.org/web/packages/data.table/vignettes/datatable-importing.html
4. [checkmate](https://cran.r-project.org/web/packages/checkmate/checkmate.pdf) (or similar) to do argument parsing/checking
5. [validate](https://cran.r-project.org/web/packages/validate/vignettes/introduction.html) to help with dataset validity/stability.
