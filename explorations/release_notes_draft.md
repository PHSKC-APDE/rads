# **rads v2.0.0 Summary (Draft)**

## **New Functions**

* `bin_age()`
  * binning function used by `age_standardize()` and exported 

## **Updated Functions**

* `calc()`
  * Added selection of worst RSE when for binary `what`
  * Added proportion detection and regrouped metrics.
  * Added parameter description for `vcov`.
  * Many new tests for binary RSE and CI proportion detection.
* `age_standardize()`
  * No longer need to pre-aggregate counts before passing data to the function
* `compare_estimate()`
  * Fixed bug where conversion to `data.table` was not saved back to object.
* `life_table_prep()`
  * Declared missing `data.table::` qualifiers
  * Ensure it returns a complete age × demographic table
* `convert_to_date()`
  * Now handles `YYYYMMDD` correctly
  * Prevents conversion of irrationally large serial dates (> Jan 1, 2100).
* Various validation utilities (`check_bin`, `check_nan`, `check_version`, `depth`, `validate_list_input`)
  * Added new tests.

## **Updated Vignettes**

Major and minor revisions across (nearly?) all vignettes:

* Migrated all vignettes to qmd and ensured CRAN compliance.
* All vignettes render correctly as auto-built package vignettes and wiki pages
* Updated vignette dependencies (`DESCRIPTION` Suggests and vignette header `%\VignetteDepends{}`).
* Updated vignette building documentation & tidied many vignette files.
* Added new **recoding vignette**.
* Updated **utilities.qmd** for `bin_age()`.
* Updated **leading COD vignette** for synthetic data.
* Updated **calc**, **chars**, **age\_standardize**, **calculating\_rates\_with\_rads**, and other vignettes to use `rads.data::synthetic_*` instead of APDE-specific functions
* Deleted old, irrelevant vignettes (population, BRFSS, get\_\* functions, obsolete examples).
* Removed `/doc` RMD vignettes entirely.
* Modernized Quarto syntax & removed kableExtra.

## **Deprecations**

* Fully removed deprecated functions for v2.0.0, including:
  * APDE data pull functions (`get_data_death`, etc.)
  * dataset-specific functions (e.g., `as_imputed_brfss`)
  * old CHI functions (moved to `apde.chi.tools`)
  * `generate_yaml()`, `tsql_validate_field_types`, `tsql_chunk_loader`, `tsql_convert_types` (moved to `apde.etl`)
* Removed `get_xwalk()` and `list_ref_xwalk()` (outdated; replaced by `rads.data` crosswalks).

## **Other Changes**

Large structural and standards‑related improvements:

* **Package-wide modernization to APDE R standards**, including:
  * New `R/globals.R` and removal of per‑file imports.
  * Consistent `package::function()` syntax.
  * Added `.gitattributes` for LF line endings (to save my sanity!)
  * Deleted unused `R/rads.R` 
* Numerous **DESCRIPTION** updates:
  * Fixed commas, tidied metadata, added authors, added Suggests.
* Test suite improvements:
  * Fixed tests affected by synthetic data changes.
  * Added extensive tests for completeness in `chars_injury_matrix_count()`, death counting functions, and others.
  * Reorganized test file naming (`test-*.R`).
  * Removed duplicate tests.
* Bug fixes across the package:
  * Encoding fixes
  * Fixes to multi-ttest referent notation
  * Fixed nonASCII issues in `utilities.R`
* Removal of King County/Seattle dependencies throughout code and documentation
* Various documentation updates (examples, comments)
* Updated population and synthetic‑data references throughout
* Addressed a bunch of issues, including #475, #477, #478, #481, #489, #490, #491, #492
* Version bumped to **2.0.0** 
