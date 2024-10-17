# R Automatic Data System (RADS)

## Purpose
RADS, APDE's R Automated Data System, is a suite of tools written in R and designed to make standard public health analyses faster, more standardized, and less prone to error. While some tools may be applicable for different settings, the toolset has been customized to the needs of [PHSKC's](https://www.kingcounty.gov/depts/health.aspx) [APDE](https://www.kingcounty.gov/depts/health/data). While these tools have only been tested in Windows, they should work identically on a Linux or Mac OS X machine.

## Installation
If you haven't yet installed [`rads`](https://github.com/PHSKC-APDE/rads), follow these steps:

1.  Make sure `remotes` is installed:

    ```
    install.packages("remotes")
    ```

2.  RADS depends on version 17 of [Microsoft ODBC Driver for SQL Server](https://docs.microsoft.com/en-us/sql/connect/odbc/download-odbc-driver-for-sql-server?view=sql-server-ver15). You can download it [here](https://go.microsoft.com/fwlink/?linkid=2187214).

3.  Install `keyring` for accessing HHSAW:

    ```
    install.packages("keyring")
    ```
    
    ```
    keyring::key_set(service = 'hhsaw', username = 'Your.KCUsername@kingcounty.gov')
    ```
    
    In the pop-up window, type in your standard King County password (the one you use to log into your laptop) and click `OK`.

4.  Install [`rads`](https://github.com/PHSKC-APDE/rads):

    ```
    remotes::install_github("PHSKC-APDE/rads", auth_token = NULL)
    ```
    
    To install GitHub from a particular branch, specify it with the `ref` argument, e.g.,
    
    ```
    remotes::install_github("PHSKC-APDE/rads", ref = "dev", auth_token = NULL)
    ```

5.  Load [`rads`](https://github.com/PHSKC-APDE/rads):
    
    ```
    library(rads)
    ```

## Permissions
While you do not need SQL Server or file server access for basic `rads` functionality, you will need the following permissions to utilize all functions:

On `kcitazrhpasqlprp16.azds.kingcounty.gov`, `rads` needs access to the following schema in `hhs_analytic_workspace`:

- [birth]: for `get_data_birth()`

- [chars]: for `get_data_chars()`

- [death]: for `get_data_death()`

- [ref]: for `get_population()`

- [YourPersonalSchema]: only needed by developers for unit testing

On our file server, you will need access to:

- `//dphcifs/APDE-CDIP/HYS/releases/`: for `get_data_hys()`

All other reference data are either built into `rads` or are available via [`rads.data`](https://github.com/PHSKC-APDE/rads.data), which is automatically installed when you install `rads`.

## Releases
Please refer to the [releases](https://github.com/PHSKC-APDE/rads/releases) page for details on the package history and particular releases.

## Getting started
After installation, we highly recommend that you start by walking through a vignette on the [wiki](https://github.com/PHSKC-APDE/rads/wiki).

- [calc()](https://github.com/PHSKC-APDE/rads/wiki/calc)

- [get_population()](https://github.com/PHSKC-APDE/rads/wiki/get_population)

- [get_data()](https://github.com/PHSKC-APDE/rads/wiki/get_data)

- [age_standardize()](https://github.com/PHSKC-APDE/rads/wiki/age_standardize)

- etc. 

## Problems?
If you come across a bug or have specific suggestions for improvement, please click on [Issues](https://github.com/PHSKC-APDE/rads/issues) at the top of this page and then click [New Issue](https://github.com/PHSKC-APDE/rads/issues/new/choose) and provide the necessary details.
