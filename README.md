# R Automatic Data System (RADS)

## Purpose
RADS, APDE’s ‘R Automated Data System, is a suite of tools written in R and designed to make standard public health analyses faster, more standardized, and less prone to error. While some tools may be applicable for different settings, the toolset has been customized to the needs of [PHSKC's](https://www.kingcounty.gov/depts/health.aspx) [APDE](https://www.kingcounty.gov/depts/health/data). While these tools have only been tested in Windows, they should work identically on a Linux or Mac OS X machine.    

## Installation

If you haven’t yet installed [`rads`](https://github.com/PHSKC-APDE/rads), follow these steps:

1.  Make sure devtools is installed … `install.packages("devtools")`.

2.  Install [`rads`](https://github.com/PHSKC-APDE/rads) …
    `devtools::install_github("PHSKC-APDE/rads", auth_token = NULL)`
    * To install github from a particular branch, specify it with the 'ref' argument, e.g., `devtools::install_github("PHSKC-APDE/rads", ref = "dev", auth_token = NULL)`

3.  Load [`rads`](https://github.com/PHSKC-APDE/rads) … `library(rads)`

4. Exit RStudio and start it again. 

5. Confirm `rads` installed properly by typing `library(rads)` in the console.

## Getting started
After installation, we highly recommend that you start by walking through a vignette on the [wiki](https://github.com/PHSKC-APDE/rads/wiki).
* [calc()](https://github.com/PHSKC-APDE/rads/wiki/calc)
* [get_population()](https://github.com/PHSKC-APDE/rads/wiki/get_population)
* [get_data()](https://github.com/PHSKC-APDE/rads/wiki/get_data)

## Problems?
* If you come across a bug or have specific suggestions for improvement, please click on ["Issues"](https://github.com/PHSKC-APDE/rads/issues) at the top of this page and then click ["New Issue"](https://github.com/PHSKC-APDE/rads/issues/new/choose) and provide the necessary details. 

