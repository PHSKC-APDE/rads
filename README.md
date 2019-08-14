# R Automatic Data System (RADS)

<img src="https://raw.githubusercontent.com/PHSKC-APDE/rads/master/Radiation_warning_symbol.png?token=AL2FPANBBXJ5MVJDGGTGDH25LXFFK" alt="drawing" width="200"/>

## Design Principles
1. In general, follow the guidelines described by [ROpenSci](https://devguide.ropensci.org/building.html)
2. Primary data frame manipulation is through `data.table` and its associated syntax.
    - Any function using `set` or `:=` functions from the `data.table` package on an object passed as part of the function
    evaluation should have an option `jump_scope` that takes a logical input of length 1. If `TRUE`, the data can be modified by
    reference. If `FALSE` an explict `copy` should be taken. E.g.: `if(!jump_scope) data = copy(data)`. If a function is designed
    to modify by reference, then it should (where relevant) `return(invisible(data))` to allow for chaining.

3. Use `<-` for assignment

4. Functions should only rely on explicitly described inputs and should in general, not modify the parent environment. The only
exception is using `data.table`'s modify on reference semantics as described in item #1. Or in other words, don't use `assign` or `<<-`.

5. Functions that are talkative should have a `verbose` argument. To "talk" to the console, use `message` and `warning`. DO NOT USE `cat` or `print`. See the ROpenSci site for further information.

6. Functions should not use `%>%` internally. Writing functions to be pipeable is fine as long as its `data.table` friendly.

7. Each function should be in its own `.R` file. Small groups of utility functions can be combined within the same `.R` file where relevant.

8. User-written functions should `package::function` notation when calling functions from other packages. Or in short, we should be as R-check friendly as possible. One purposeful exception can be related to `data.table`'s (and presumably `dplyr`) non-standard evaluation (NSE) framework (e.g. selecting a column like `dt[, mycolumn]` instead of `df[, 'mycolumn']`). Because this package is unlikely to be used outside of APDE, converting the NSE to be R-check friendly seems like more trouble than its worth. See ['Dealing with “undefined global functions or variables”'](https://cran.r-project.org/web/packages/data.table/vignettes/datatable-importing.html)

9. All non-trivial functions should be accompanied by tests using the `testthat` package to ensure functionality stability.

10. Use [checkmate](https://cran.r-project.org/web/packages/checkmate/checkmate.pdf) (or similar) to do argument parsing/checking

11. Use [validate](https://cran.r-project.org/web/packages/validate/vignettes/introduction.html) to help with dataset validity/stability.

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
