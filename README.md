# R Automatic Data System (RADS)

## Purpose
RADS is a suite of tools written in R and designed to make standard public health analyses faster, more standardized, and less prone to error. While we hope the tools are applicable for different settings, the toolset has been customized to the needs of [PHSKC's](https://www.kingcounty.gov/depts/health.aspx) [APDE](https://www.kingcounty.gov/depts/health/data). While this package has only been tested in Windows, it should work identically on a Linux or Mac OS machine.

## Installation
If you haven't yet installed [`rads`](https://github.com/PHSKC-APDE/rads), follow these steps:

1.  Make sure `remotes` is installed:

    ```
    install.packages("remotes")
    ```

2.  Install [`rads`](https://github.com/PHSKC-APDE/rads):

    ```
    remotes::install_github("PHSKC-APDE/rads", auth_token = NULL)
    ```
    
    To install GitHub from a particular branch, specify it with the `ref` argument, e.g.,
    
    ```
    remotes::install_github("PHSKC-APDE/rads", ref = "dev", auth_token = NULL)
    ```

3.  Load [`rads`](https://github.com/PHSKC-APDE/rads):
    
    ```
    library(rads)
    ```

## Reference Data
All reference data are either built into `rads` or are available via [`rads.data`](https://github.com/PHSKC-APDE/rads.data), which is automatically installed when you install `rads`.

## Permissions
As of version 1.5.2, all functions that need SQL Server fo file server access have been deprecated and moved to [a private package](https://github.com/PHSKC-APDE/apde.data). Therefore you should not need any permissions to run a current RADS release.

## Releases
Please refer to the [releases](https://github.com/PHSKC-APDE/rads/releases) page for details on the package history and particular releases.

## Getting started
After installation, we highly recommend that you start by walking through a vignette on the [wiki](https://github.com/PHSKC-APDE/rads/wiki), e.g.,  [calc()](https://github.com/PHSKC-APDE/rads/wiki/calc).

## Problems?
If you come across a bug or have specific suggestions for improvement, please click on [Issues](https://github.com/PHSKC-APDE/rads/issues) at the top of this page and then click [New Issue](https://github.com/PHSKC-APDE/rads/issues/new/choose) and provide the necessary details.
