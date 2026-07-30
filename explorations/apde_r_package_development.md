# APDE R Package Development Standards

## Version 1.1 (2026-07-30)

## Getting Started

-   If you are starting from scratch, consider using `usethis::create_package()` to setup a package on your laptop. E.g., `usethis::create_package("c:/code/mypkg_dir")`
-   If you created a new package, update the DESCRIPTION file as needed, specifically the Title, Version, [Authors\@R](mailto:Authors@R){.email}, Description.
-   Note that for [Authors\@R](mailto:Authors@R){.email} one of the roles must be the maintainer ('cre'). I would just leave that part alone and update your name and email and delete the part about ORCID.
-   If you are developing an existing package, clone it from GitHub and be sure to create and use a feature branch.
-   Whether it is a new or existing package, set the package license to [Apache 2.0](https://www.apache.org/licenses/LICENSE-2.0.html) by entering `usethis::use_apache_license(version = 2)`. This is the license used by CDC.
-   For additional reading on building R packages, [R Packages (2e)](https://r-pkgs.org/ "R Packages (2e) Learn how to create a package, the fundamental unit of shareable, reusable, and reproducible R code.") by Hadley Wickham and Jennifer Bryan is an authoritative and free reference.

## Package Dependencies and Configuration

-   Never use `library()` or `require()` directly in R/ files. Always use the `package::function()` syntax or proper imports via NAMESPACE.
-   If you are using another package that has not yet been used in the package, use `usethis::use_package("pkg", type = "Imports|Suggests")` to update the DESCRIPTION file (or update it manually if you like).

    -   Imports = packages needed for APDE's package to work.
    -   Suggests = optional packages used for tests and building vignettes.

-   If your package uses other APDE packages, include them in your DESCRIPTION with something like:

    ```         
    Remotes:
        github::PHSKC-APDE/rads, github::PHSKC-APDE/rads.data
    ```

## Package Documentation

- Always create package-level documentation using `usethis::use_package_doc()`. It will warn you if it already exists.
    - `usethis::use_package_doc()` creates `/R/package-name-package.R` with the "\_PACKAGE" marker.
        - **ALL `@import` and `@importFrom` declarations go ONLY in this file. Never put them in individual function documentation.**
            - Only import functions used in >50% of your package functions OR operators that can't use `::` syntax (e.g., `%>%`, `:=`).
            - Example: `usethis::use_import_from("data.table", c(":=", ".SD", ".N"))`.
- Consider using `usethis::use_readme_md()` or `usethis::use_readme_rmd()` to create a README that explains what your package does. Otherwise, you can easily code a README.md from scratch.

## Creating Functions

- **Use `packageName::functionName()` syntax for all external functions EXCEPT those imported in `/R/package-name-package.R`.**
- **Never put `@import` or `@importFrom` in individual function roxygen headers.**
- Maintain consistent naming conventions across functions (e.g., `ph.data` for datasets, `group_by` for stratification).
- Validate all function parameters and provide informative error messages.
- Write unit tests as you develop functions. Convert your scratch/development code into tests.

## R Directory Standards

-   Each `.R` file in the `R/` directory should be self-contained and focused on defining functions or documenting datasets
-   The `R/` directory should contain ONLY the following:
    -   Function definitions using `functionName <- function(...)` or `functionName = function(...)`
    -   S3 method definitions (e.g., `print.myClass <- function(...)`)
    -   Dataset documentation blocks (roxygen2 comments followed by the dataset name in quotes)
    -   Package documentation (from `usethis::use_package_doc()`)
    -   Comments (both regular `#` and roxygen2 `#'` comments)
-   The `R/` directory must NOT contain:
    -   Script-like code (e.g., `rm(list = ls())`, `options(...)`, direct variable assignments)
    -   Direct `library()` or `require()` calls outside of functions
    -   Data processing or analysis code 
    -   Direct file I/O operations outside of functions (i.e., don't use any reading or writing functions )
    -   Any code that executes when the package is built or loaded (use `R/zzz.R` for initialization)
-   If you need to run analysis code, create a vignette or put it in `inst/examples/`

## Function Documentation

-   Document your function(s) with standard roxygen2 tags, at the minimum `@description` (which, by default is the second line of your documentation), `@param`, `@return`, and `@examples`. However, *please* consider adding more information like `@details`, `@import`, `@importFrom`, `@keywords`, `@examples`, `@noRd`, etc.

-   Use `@importFrom` rather than `@import` whenever possible.

-   Always mark internal functions with `@keywords internal`.

-   For S3Methods (like `rads:::calc.imputationList`), add the following three lines to your documentation to keep your methods accessible internally while hiding them and their helpfiles from end users:

    ``` r
    #' @keywords internal
    #' @export
    #' @method functionName className
    ```

-   Note, do not be like Danny who likes to use complicated old school syntax like `\enumerate{\item ...}` and `\code{...}`. roxygen2 plays nicely with Markdown, just ensure your DESCRIPTION file has `Roxygen: list(markdown = TRUE)` and you can use standard markdown syntax.

-   Run `devtools::document()` and proof read your helpfile a bunch of times.

-   NEVER manually edit the NAMESPACE file. Use roxygen2 coding and rebuild with `devtools::document()`.

## Declare Global Variables

-   `devtools::check()`, which calls R CMD check, gets angry when there are undeclared global variables. This happens a lot with data.table.

-   In the past we often dealt with this by 'declaring' the variables `NULL`. For example, `'var1' <- 'var2' <- 'var3' <- NULL`.

-   From now on, let's move toward a more consolidated method like the following which should be saved in `/R/globals.R`:

    ``` r
    utils::globalVariables(c(
      "var1",
      "var2", 
      "var3"
      # ... all your globals
    ))
    ```

-   If you miss some declarations, it's okay, because `devtools::check()` will let you know in a later step.

## Deprecating and Removing Functions

### Lifecycle Stages

Functions go through these stages when being phased out:

- **deprecated**: Function still works but warns users to switch to the replacement
- **defunct**: Function throws an error and must be replaced  
- **removed**: Function and documentation completely deleted from the package

### Deprecating a Function

When you want to discourage use of a function but keep it working:

1. Run `usethis::use_lifecycle()` (only needed once per package). This will add lifecycle to your Imports in the DESCRIPTION file and will save some 'badges' to your `man/figures/` directory.

2. Add the deprecation warning at the start of your function:
```r
   lifecycle::deprecate_warn(
      when = "version # when deprecation occurred", 
      what = "old_function()", 
      with = "new_function()",
      details = "A specific note you want to add"
   )
```

3. Add `` #' `r lifecycle::badge("deprecated")` `` immediately under the function's `@description` marker. Note that the tick marks and opening with `r` are critical!

4. Add a meaningful comment like: `#' oldFunction() was deprecated in MyPackage 1.2.0. Please use [newFunction()] instead.`

### Making a Function Defunct (Stops Working but Documented)

When you're ready to completely stop the function from working:

1. Replace the function body with a hard error:
```r
   old_function <- function(...) {
     lifecycle::deprecate_stop(
       when = "version # when removed",
       what = "old_function()",
       with = "new_function()"
     )
   }
```

2. Update the comment to indicate that it no longer works, e.g.,   
   ``#' `oldFunction()` was made defunct in MyPackage 1.2.0. Please use [newFunction()] instead.``

3. Simplify the documentation by removing detailed @param, @return, @details, and @examples sections. Use `@param ... Deprecated arguments` as a catch-all.

4. Keep the documentation for 1-2 major versions to help users who encounter old code.

### Completely Removing a Function

After a function has been defunct for 1-2 major versions, you can completely delete the function and its documentation from the package.

## Including Datasets

-   All data processing code must be in `data-raw/`, never in `R/` (As stated above, the `R/` directory should only contain the roxygen2 documentation for datasets, not the code that creates them)
-   All datasets should be data.tables, not data.frames, unless they are shapefiles or other non-compatible file types.
-   Use three key directories: `data-raw/`, `data/`, and `R/`.
    -   The `data-raw/` directory contains code that creates or processes raw data into the final datasets that are available within your package. Use `usethis::use_data_raw("dataset_name")` to set the `data-raw/` directory and to create a `dataset_name.R` file for the code to prepare your data.
    -   The `data/` directory contains the actual `.rda` files that users will access. These are created by the code in `data-raw/` when they run `usethis::use_data(dataset_name)`.
    -   The `R/data.R` file contains all your dataset documentation using roxygen2 syntax. Each dataset gets its own documentation block with detailed `@format` descriptions, `@source` information, `@usage`, and `@name`. Additional tags like `@note`, `@reference`, `@details`, `@examples`, `@keywords`, etc. are helpful.
        -   Dataset documentation should be thorough - describe every column, include data sources, and note any special characteristics or limitations.
        -   Always end dataset documentation with the dataset name in quotes on a line by itself, e.g., `"dataset_name"`
-   Consider using `usethis::use_data(..., overwrite = TRUE)` when updating existing datasets.

## Additional Directories

-   Use the `inst/` directory for files that need to be installed with your package but aren't R data objects. E.g., configuration files, templates, or reference documents that your functions might read.
-   Use `inst/extdata/` for raw data files that are processed by scripts in `data-raw/` or for example external data files that demonstrate how your package works. These files should be small.

## Package Initialization and Setup

-   Use `R/zzz.R` (rather than `R/onAttach`, `R/onload.R`, `R/options_and_onload.R`, etc.) for package initialization code.
-   This replaces the `.onLoad()` function that runs when the package namespace is loaded.
-   It also replaces the `.onAttach()` function that runs when the package is attached via `library()`.
-   Common uses include:
    -   Setting package-specific options (like database connection defaults in the `rads` package).
    -   Registering S3 methods for other packages (as in the `dtsurvey` package).
    -   Checking system requirements or package versions (used by many APDE packages).
    -   Displaying startup messages -- only if absolutely necessary!

## Write Unit Tests

-   Use `usethis::use_testthat()` to set up testing directories and files if needed.
-   Use `usethis::use_test("function_name")` to create test file templates.
-   Write tests for all exported functions (or have a compelling reason why you can't do so).
-   Consider tests for complex or important internal functions too.
-   Try to mirror the structure of `R/` in `tests/testthat/`. E.g., if you have `R/BigFunction1.R` and `R/myutilities.R`, then aim to have `tests/testthat/test-BigFunction1.R` and `tests/testthat/test-myutilities.R`.
-   Try hard to break your code with corner cases.
-   Run `devtools::test()` and update your function(s) and/or tests as needed.

## Creating CRAN Style Vignettes / Wiki Style Documentation (optional, but helpful)

-   You could use `usethis::use_vignette("vignette-name")`, but that creates an `.Rmd` file whereas the world is moving toward `.Qmd` files. Instead, do the following:
    -   Create `vignettes/` in your package's root directory.

    -   Add `^vignettes$`to .Rbuildignore

    -   Add knitr and quarto to your DESCRIPTION under Suggests using `usethis::use_package()` or by editing the DESCRIPTION file manually.

    - Add `VignetteBuilder: quarto` to your DESCRIPTION

    -   Create a `.Qmd` file in `vignettes` and give it a header like the following which will render git flavored markdown (which you will need to post on a GitHub wiki):

        ``` yaml
        ---
        title: "My Vignette Title"
        format:
          html:
            embed-resources: true
          gfm:
            prefer-html: false
        vignette: >
          %\VignetteIndexEntry{My Vignette Index Title}
          %\VignetteEngine{quarto::html}
          %\VignetteEncoding{UTF-8}
          %\VignetteDepends{kableExtra}
        editor: source
        ---
        ```

- If you've followed this set up carefully, when you build your package, it will create CRAN stylel vignettes. In addition, you can easily render these to markdown for use in a GitHub wiki.

### Quarto Code Chunk Standards
- All Quarto vignette code chunks should use the YAML-style chunk options (`#| option: value`) rather than knitr-style options placed in the chunk header.
- This YAML style is the modern Quarto standard.
- Example of preferred syntax:

    ```text
    ```{r}
    #| warning: false
    #| message: false
    kable(myTable, format = "markdown")
    ```

- Avoid older knitr-style options:
    ```text
    ```{r, warning=FALSE, messsage=FALSE}
    kable(myTable, format = "markdown")
    ```

## Run Tests

-   Run `devtools::check()`.
-   All packages should pass R CMD check --as-cran with no ERRORs or WARNINGS (and ideally, with no NOTES).
-   Do not proceed until all tests pass.

## Issue a Pull Request into Dev

-   If you were working on an existing package, you should have been working on a feature branch as was stated above.
-   Consider rebasing your feature branch off of the dev branch. Once you do so, run `devtools::check()` again to make sure that any changes that occurred in dev do not contribute to errors or warnings on your feature branch.
-   Issue a pull request from your feature branch into the dev branch and assign it to another developer for review.

## When You're Ready to Pull from Dev into Main

-   Update the version number in the DESCRIPTION file, following standard semantic versioning (MAJOR.MINOR.PATCH).
-   Breaking changes go in MAJOR, new features go in MINOR, and bug fixes and function enhancements go in PATCH.
-   Run `devtools::check()` again and make sure it passes all tests.
-   Issue a pull request from dev into main and assign it to another developer for review.

## When Changes are Pushed to Main

-   Create release notes on GitHub for every MAJOR and MINOR release and, preferably, for every substantial PATCH too.
-   Please identify and group changes into sections, e.g., New Functions, Updated Functions, Bug Fixes, Vignette Changes, Deprecations, Other Changes.
-   Post your release notes in `https://github.com/PHSKC-APDE/MyPackage/releases`. For convenience, the Tag and Release should both be the version number, e.g., 'v1.5.13'.
