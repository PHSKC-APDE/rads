# death_validate_data() ----
#' Validate and prepare death data for use with rads death functions
#'
#' @description
#' Validates that a dataset meets the requirements for use with
#' [death_113_count()], [death_130_count()], [death_injury_matrix_count()],
#' [death_other_count()], and [death_multicause_count()]. This function
#' checks for required columns, validates data formats, and applies
#' ICD-10 code cleaning consistent with rads death analysis functions.
#'
#' @param ph.data a data.table or data.frame containing line-level death data,
#' structured with one person per row.
#'
#' The default is `ph.data = NULL`
#'
#' @param icdcol a character vector of length one. The name of the column in
#' `ph.data` that contains the underlying cause of death ICD-10 codes.
#'
#' The default is `icdcol = 'underlying_cod_code'`
#'
#' @param check_multicause a logical vector of length one. When TRUE, the
#' function will also validate the contributing cause of death columns required
#' by [death_multicause_count()]. These are columns following the naming
#' pattern `<contributing_cols>_1`, `<contributing_cols>_2`, etc.
#'
#' The default is `check_multicause = FALSE`
#'
#' @param contributing_cols a character vector of length one. The stem name of
#' the contributing cause of death columns in `ph.data`. Only used when
#' `check_multicause = TRUE`. The function will look for columns named
#' `<contributing_cols>_1`, `<contributing_cols>_2`, etc.
#'
#' The default is `contributing_cols = 'record_axis_code'`
#'
#' @param verbose a logical vector of length one. When TRUE (default), prints
#' informational messages about validation results. When FALSE, only shows
#' warnings and errors.
#'
#' The default is `verbose = TRUE`
#'
#' @details
#' This function performs the following validation checks:
#'
#' **Always checked:**
#' - `ph.data` is a data.frame or data.table
#' - The column specified by `icdcol` exists in `ph.data`
#' - ICD-10 codes in `icdcol` are cleaned using [death_icd10_clean()], consistent
#'   with what all rads death functions do internally
#'
#' **Checked if present:**
#' - `chi_geo_kc`: if this column exists in `ph.data`, it must contain only
#'   `"King County"` or `NA`
#'
#' **Checked when `check_multicause = TRUE`:**
#' - Columns matching `<contributing_cols>_1`, `<contributing_cols>_2`, etc.
#'   must exist in `ph.data`
#' - A warning is issued if fewer than 20 such columns are found, as typically
#'   20 contributing cause columns are expected
#'
#' @return
#' Returns a data.table with the ICD-10 column cleaned via [death_icd10_clean()].
#' Informative messages, warnings, and errors are printed as appropriate.
#'
#' @seealso
#' - [death_113_count()] for NCHS 113 causes of death counts
#' - [death_130_count()] for NCHS 130 causes of infant death counts
#' - [death_injury_matrix_count()] for injury matrix counts
#' - [death_other_count()] for other cause of death counts
#' - [death_multicause_count()] for counts using both underlying and contributing causes
#' - [death_icd10_clean()] for ICD-10 code cleaning details
#'
#' @export
#'
#' @examples
#' # Validate synthetic death data
#' mydata <- rads.data::synthetic_death
#' validated_data <- death_validate_data(ph.data = mydata)
#'
#' # Also validate contributing cause columns for use with death_multicause_count()
#' validated_data2 <- death_validate_data(ph.data = mydata, check_multicause = TRUE)
#'
death_validate_data <- function(ph.data = NULL,
                                icdcol = 'underlying_cod_code',
                                check_multicause = FALSE,
                                contributing_cols = 'record_axis_code',
                                verbose = TRUE) {

  # Validate ph.data ----
  if (missing(ph.data) || !is.data.frame(ph.data)) {
    stop("\n\U0001f47f `ph.data` must be the unquoted name of a data.frame or data.table")
  }
  if (!data.table::is.data.table(ph.data)) {
    data.table::setDT(ph.data)
  }
  ph.data <- data.table::copy(ph.data)

  # Validate verbose ----
  if (!is.logical(verbose) || length(verbose) != 1 || is.na(verbose)) {
    stop("\n\U0001f47f `verbose` must be a logical vector of length 1, i.e., TRUE or FALSE.")
  }

  # Validate icdcol ----
  if (!is.character(icdcol) || length(icdcol) != 1) {
    stop("\n\U0001f47f `icdcol` must be a single character string naming the ICD-10 column in `ph.data`.")
  }
  if (!icdcol %in% names(ph.data)) {
    stop(paste0("\n\U0001f47f `icdcol` ('", icdcol, "') was not found as a column in `ph.data`."))
  }

  # Clean icdcol using death_icd10_clean() ----
  # This mirrors what all rads death functions do internally
  ph.data[, (icdcol) := death_icd10_clean(get(icdcol))]

  # Validate chi_geo_kc (if it exists) ----
  if ('chi_geo_kc' %in% names(ph.data) &&
      length(setdiff(unique(ph.data$chi_geo_kc), c('King County', NA))) > 0) {
    stop('\n\U0001F6D1 `chi_geo_kc` exists and has values other than "King County" and NA.\n',
         "If your analyses are not specific to King County, WA, feel free to delete the chi_geo_kc column.\n",
         "Otherwise, please fix chi_geo_kc and run again.")
  }

  # Validate check_multicause ----
  if (!is.logical(check_multicause) || length(check_multicause) != 1 || is.na(check_multicause)) {
    stop("\n\U0001f47f `check_multicause` must be a logical vector of length 1, i.e., TRUE or FALSE.")
  }

  # Validate contributing cause columns (only when check_multicause = TRUE) ----
  if (isTRUE(check_multicause)) {

    if (!is.character(contributing_cols) || length(contributing_cols) != 1) {
      stop("\n\U0001f47f `contributing_cols` must be a single character string naming the stem of the contributing cause columns in `ph.data`.")
    }

    # Strip trailing underscore if present (mirrors death_multicause_count())
    contributing_cols <- gsub("_$", "", contributing_cols)

    contrib_col_pattern <- paste0("^", contributing_cols, "_[0-9]+$")
    contrib_col_names <- grep(contrib_col_pattern, names(ph.data), value = TRUE)

    if (length(contrib_col_names) == 0) {
      stop(paste0("\n\U0001f47f No columns found matching the pattern '", contributing_cols, "_#'. ",
                  "Expected columns like '", contributing_cols, "_1', '", contributing_cols, "_2', etc. ",
                  "These are required for death_multicause_count()."))
    }

    if (length(contrib_col_names) < 20) {
      warning(paste0("\n\u26A0\ufe0f Only ", length(contrib_col_names), " column(s) were found matching the pattern '",
                     contributing_cols, "_#'.\n",
                     "Typically there are 20 contributing cause columns. You may want to check ph.data."))
    }

    if (verbose) {
      message(paste0("\U00002139 Found ", length(contrib_col_names),
                     " contributing cause column(s) matching '", contributing_cols, "_#'."))
    }
  }

  # Return the modified data.table ----
  if (verbose) {
    message("\U0001f642 Validation passed! Data is ready for use with rads death analysis functions.")
  }
  return(ph.data)
}
