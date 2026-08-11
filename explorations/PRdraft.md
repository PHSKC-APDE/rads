PR title

Release v2.0.0 — remove deprecated functions, drop kingco filtering, add bin_age()
PR body

## Summary

This is a major release (1.5.5 → 2.0.0) with breaking changes. Full details are in the [release notes](releases page) / `explorations/release_notes_draft.md`; highlights below.

**⚠️ Breaking changes**
- `kingco` removed from `chars_injury_matrix_count()`, `chars_icd_ccs_count()`, `death_injury_matrix_count()`, `death_multicause_count()`, `death_other_count()`, `death_xxx_count()`, `life_table()`, and `life_table_prep()`. It used to default to `TRUE` and silently filter to King County.
- `group_by` renamed to `by` in the same eight functions, plus `age_standardize()`.
- `age_standardize()`'s positional argument order changed.
- All SQL Server/file-server-dependent functions (data pulls, CHI utilities, YAML/SQL tooling) are now fully removed rather than deprecated. See `apde.data`/`apde.chi.tools`/`apde.etl` packages for replacements.
- License changed from GPL-3 to Apache License (>= 2).

**Other changes**
- `calc()`/`calc.imputationList()` default `proportion` changed from `FALSE` to `"autodetect"`. May change CI/RSE output for existing calls on factor/logical/0-1 variables.
- Bug fixes in `age_standardize()` (crude rate calc), `do_recode()` (label lookup), `convert_to_date()` (YYYYMMDD, serial-date bounds), `compare_estimate()`, `life_table_prep()` (complete age table), and `chars_injury_matrix_count()`(complete age table). See release notes for specifics.

**New**
- `bin_age()` exported for standalone age binning.
- `age_standardize()` new `line_level` parameter for un-aggregated input.

**Docs**
- All vignettes migrated to Quarto, rebuilt on synthetic data, new recoding vignette added.
- Roxygen & vignettes fixed numerous typos.

