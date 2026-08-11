# **rads v2.0.0 Summary (Draft)**

**This is a major release with breaking changes.** The headline items: 

- `kingco` was removed from six `chars_*`/`death_*` counting functions plus `life_table()`/`life_table_prep()`. Previously defaulted to `TRUE` and silently filtered to King County — it no longer does anything 
- `group_by` was renamed to `by` across those same functions and `age_standardize()`
- `age_standardize()`'s positional argument order changed 
- `calc()`'s `proportion` default changed from `FALSE` to `"autodetect"`, which can silently change CI/RSE output for existing calls. 

Details below, and breaking changes are flagged with ⚠️.

## **New Functions**

* `bin_age()`
  * Binning function used internally by `age_standardize()`, now also exported for standalone use. Buckets single-year ages into `agecat` bins matching a reference population (see `get_ref_pop()`) or user-supplied `cuts`.

## **Updated Functions**

### ⚠️ Breaking changes (old code will error, or produce a different result set with no error)

* `chars_injury_matrix_count()`, `chars_icd_ccs_count()`, `death_injury_matrix_count()`, `death_multicause_count()`, `death_other_count()`, `death_xxx_count()`, `life_table()`, `life_table_prep()`
  * **`kingco` parameter removed entirely.** In v1.5.5 this defaulted to `kingco = TRUE` and silently subset `ph.data` to `chi_geo_kc == "King County"`. Existing calls that relied on the default (i.e., didn't pass `kingco` explicitly) will **not error** — they will now silently run on the *full, unfiltered* dataset instead of King County only. This is the change most likely to bite you in the butt without you noticing. Users who want King-County-only output must now filter `ph.data` themselves before calling these functions.
  * **`group_by` renamed to `by`.** Any call using `group_by = ` by name will now error with "unused argument."
* `chars_icd_ccs_count()`
  * Dropped the `...`/`mykey` deprecation notice. Passing `mykey = ` now hard-errors ("unused arguments") instead of emitting a deprecation warning.
* `age_standardize()`
  * **Positional argument order changed**:
    * Old: `(ph.data, ref.popname, collapse, my.count, my.pop, per, conf.level, group_by, diagnostic_report, event_type)`
    * New: `(ph.data, ref.popname, my.count, my.pop, collapse, line_level, per, conf.level, by, diagnostic_report, event_type)`
    * Any code calling this function positionally past the first two arguments will silently pass values to the wrong parameter.
  * `group_by` renamed to `by`.

### ⚠️ Silent behavior/output changes (no error, but different results than v1.x)

* `calc()`
  * Default for `proportion` changed from `proportion = FALSE` to `proportion = "autodetect"`. Under the new default, any `what` variable that is a factor, logical, or a binary 0/1 numeric is now automatically treated as proportion-like and gets proportion-appropriate CI methods (`unweighted_binary` for admin data, `xlogit` for survey data). Pass `proportion = FALSE` explicitly to reproduce v1.5.5 default behavior exactly.
  * Added selection of the "worst" RSE for binary `what`: new formula `RSE = 100 * mean_se / pmin(mean, 1 - mean)`.
* `age_standardize()`
  * 🐞 Bug fix to the crude rate/CI calculation: the denominator now sums the true (uncapped) population and only caps the *aggregate* total if it's less than the aggregate count — previously each stratum was capped individually before summing. The function now returns the same crude rate regardless of the reference population.
* `do_recode()`
  * 🐞 Bug fix: when recoding a factor with `update = TRUE` (which keeps the original label for any value you *didn't* explicitly recode), the function could attach the wrong label to a preserved value. I.e., labels could get mixed up with each other. This is now fixed.
* `convert_to_date()`
  * 🐞 Bug fix: Now handles `YYYYMMDD`-formatted strings correctly (previously an 8-digit string like `"20230115"` was interpreted as an Excel date and silently converted to a nonsense date).
  * Caps serial-date conversion at Jan 1, 2100 — implausibly large serial numbers are now set to `NA` instead of converted.
  * On total parse failure, now returns an all-`NA` vector instead of the original input unchanged.
* `chars_validate_data()`
  * No longer errors when `chi_geo_kc` contains values other than "King County"/`NA`.
* `multi_t_test()`
  * Output text change: the reference-row label changed from `"Group X - Referent"` to `"Group X (Reference)"`. Numeric results are unaffected.

### Other Bug fixes

* 🐞 `compare_estimate()`
  * Fixed bug where conversion to `data.table` was not saved back to the object. Silently impacted `data.frame` input.

### Enhancements

* `life_table_prep()`
  * Now returns a complete age × demographic table: groups with `NA` in a `by` column previously got dropped from the zero-death age-bin template.

* `chars_injury_matrix_count()`
  * Now returns a complete age × demographic table: groups with `NA` in a `by` column previously got dropped from the zero-death age-bin template.

* `calc()`
  * Added proportion detection and regrouped metrics.
  * Added parameter description for `vcov`.
  * Many new tests for binary RSE and CI proportion detection (see new `tests/testthat/test-calc-proportions.R`).

* `age_standardize()`
  * No longer need to pre-aggregate counts before passing data to the function.
  * New parameter `line_level = FALSE` (requires `collapse = TRUE`): when `TRUE`, duplicate rows per age/`by` group are treated as un-aggregated line-level records. Identical `my.pop` values across duplicates are no longer summed (only `my.count` is). The default `FALSE` provides a warning recommending `line_level = TRUE` when duplicates are detected.
  * Now uses the `bin_age()` internally for age binning. Just a refactoring that provides the same result.

* Various validation utilities (`check_bin`, `check_nan`, `check_version`, `depth`, `validate_list_input`)
  * Added new tests.

## **Updated Vignettes**

Major and minor revisions across (nearly?) all vignettes:

* Migrated all vignettes to qmd and ensured CRAN compliance.
* All vignettes render correctly as auto-built package vignettes and wiki pages.
* Updated vignette dependencies (`DESCRIPTION` Suggests and vignette header `%\VignetteDepends{}`).
* Updated vignette building documentation & tidied many vignette files.
* Added new **recoding vignette**.
* Updated **utilities.qmd** for `bin_age()`.
* Updated **leading COD vignette** for synthetic data.
* Updated **calc**, **chars**, **age_standardize**, **calculating_rates_with_rads**, and other vignettes to use `rads.data::synthetic_*` instead of APDE-specific functions.
* Deleted old, irrelevant vignettes (population, BRFSS, get_* functions, obsolete examples).
* Removed `/doc` RMD vignettes entirely.
* Modernized Quarto syntax & removed kableExtra.
* Corrected numerous typos.

## **Deprecations**

All SQL Server/file-server-dependent functions have been **fully removed** (not just deprecated) in v2.0.0. Complete list of removed exports, by destination:

* **APDE data-pull functions** — moved to the private [`apde.data`](https://github.com/PHSKC-APDE/apde.data) package: `get_data()`, `get_data_birth()`, `get_data_brfss()`, `get_data_chars()`, `get_data_death()`, `get_data_hys()`, `get_data_pums()`, `get_population()`.
* **Dataset-specific BRFSS functions**: `as_imputed_brfss()`, `as_table_brfss()`, `pool_brfss_weights()`.
* **Old CHI functions** — moved to `apde.chi.tools`: `chi_cols()`, `chi_compare_est()`, `chi_compare_kc()`, `chi_metadata_cols()`, `chi_qa()`, `list_apde_data()`, `list_dataset_columns()`, `sql_clean()`, `suppress()`, `validate_hhsaw_key()`, `validate_yaml_data()`.
* **YAML/SQL-type-conversion tooling** — moved to `apde.etl`: `generate_yaml()`, `tsql_validate_field_types()`, `tsql_chunk_loader()`, `tsql_convert_types()`.
* **Crosswalk functions** removed as outdated, replaced by `rads.data` crosswalks: `get_xwalk()`, `list_ref_xwalk()`.

As a result, `rads` no longer hard-depends on `DBI`, `glue`, `keyring`, `odbc`, `yaml`, or `lifecycle` (see DESCRIPTION changes below), and no longer searches for an ODBC driver or warns about a missing one when loading the package.

## **Other Changes**

Large structural and standards-related improvements:

* **License changed from GPL-3 to Apache License (>= 2)**, consistent with [apde_r_package_development.md](https://github.com/PHSKC-APDE/package-toolkit/blob/main/standards/apde_r_package_development.md).

* **Package-wide modernization to APDE R standards**, including:
  * Update `R/globals.R` and remove per-file imports.
  * Consistent `package::function()` syntax.
  * Added `.gitattributes` for LF line endings (to save my sanity!).
  * Dropped unused `R/rads.R`.

* Numerous **DESCRIPTION** updates:
  * Version bumped to **2.0.0**.
  * Fixed commas, tidied metadata, restructured `Authors@R`, added Suggests (`ggplot2`, `quarto`).
  * Dropped dependencies that are no longer needed: `DBI`, `glue`, `keyring`, `odbc`, `yaml`, `lifecycle`.
  * `VignetteBuilder` changed from `knitr` to `quarto`.

* `testthat` improvements:
  * Fixed tests affected by synthetic data changes.
  * Added tests for completeness in `chars_injury_matrix_count()`, death counting functions, and others.
  * Standardize test file naming (`test_*.R` → `test-*.R`).
  * Removed duplicate tests.
* Bug fixes across the package:
  * Encoding fixes.
  * Fixes to multi-ttest referent notation (see `multi_t_test()` above).
  * Fixed non-ASCII issues in `utilities.R`.

* Removal of King County/Seattle dependencies throughout code and documentation (see the `kingco` breaking change above).

* Various documentation updates (examples, comments), including a documentation typo pass across roxygen and vignette files.

* Updated population and synthetic-data references throughout.

* Addressed a bunch of issues, including: #475, #477, #478, #481, #489, #490, #491, #492, #493, #494.
