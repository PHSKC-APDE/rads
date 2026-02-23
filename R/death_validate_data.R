# death_validate_data() ----
#' Validate and prepare death data for use with death_icd_ccs_count() &
#' death_injury_matrix_count()
#'
#' @description
#' Validates that a dataset meets the requirements for use with
#' [death_113_count()], [death_130_count()], [death_injury_matrix_count()],
#' [death_other_count()] and [death_multicause_count()]. This function
#' checks for required columns, validates data formats, and performs necessary
#' transformations to ensure compatibility with rads death analysis functions.
#'
#' @param ph.data a data.table or data.frame containing death (Comprehensive
#' Hospital Abstract Reporting System) data. The dataset should be structured
#' with one person per row and include the necessary columns for injury and
#' ICD-CM analysis.
#'
#' The default is `ph.data = NULL`
#'
#' @param icdcol a character vector of length one. The name of the column in
#' `ph.data` that contains the ICD-CM codes.
#'
#' The default is `icdcol = 'diag1'`
#'
#' @param icdcm_version an integer vector of length one, limited to 9 or 10.
#' It specifies which ICD-CM version to validate against.
#'
#' The default is `icdcm_version = 10`
#'
#' @param verbose a logical vector of length one. When TRUE (default), prints
#' messages about validation results. When FALSE, only shows
#' warnings and errors.
#'
#' The default is `verbose = TRUE`
#'
#' @details
#' This function performs the following validation checks:
#'
#' **Required columns:**
#' - `seq_no`: unique patient-level identifier (must be unique integer per row)
#' - `injury_nature_broad`: logical indicator for broad injury definition
#' - `injury_nature_narrow`: logical indicator for narrow injury definition
#' - `injury_intent`: character column with values like "assault", "unintentional", etc.
#' - `injury_mechanism`: character column with values like "fall", "firearm", etc.
#' - ICD column (specified by `icdcol`, default is `diag1`)
#'
#' **Optional columns:**
#' - `chi_geo_kc`: if present, must only contain `"King County"` or `NA`
#'
#' **Data transformations:**
#' - ICDcm codes cleaned and updated to a standardized format
#'
#' @return
#' Returns a data.table with validated and potentially modified death data.
#' The function also prints informative messages about validation results,
#' warnings about missing standard categories, and confirmation when data
#' meets all requirements.
#'
#' @references
#' - CDC Injury Code and Matrices: <https://www.cdc.gov/nchs/injury/injury_matrices.htm>
#'
#' @export
#'
#' @examples
#' # Validate synthetic death data
#' mydata <- rads.data::synthetic_death
#' validated_data <- death_validate_data(ph.data = mydata)
#'
death_validate_data <- function(ph.data = NULL,
                                icdcol = 'diag1',
                                icdcm_version = 10,
                                verbose = TRUE) {

  # Validate ph.data ----
  if (missing(ph.data) || !is.data.frame(ph.data)) {
    stop("\n\U0001f47f `ph.data` must be the unquoted name of a data.frame or data.table")
  }
  if (!data.table::is.data.table(ph.data)) {
    data.table::setDT(ph.data)
  }
  ph.data <- data.table::copy(ph.data)

  # Validate icdcm_version ----
  if(!icdcm_version %in% c(9, 10) | length(icdcm_version) != 1){stop("\n \U0001f47f the `icdcm_version` argument is limited to the integers '9' OR '10'")}

  # Validate verbose ----
  if (!is.logical(verbose) || length(verbose) != 1 || is.na(verbose)){
    stop("\n\U0001f47f `verbose` must be a logical vector of length 1, i.e., TRUE or FALSE.")
  }

  # Check for critical columns ----
  cols <- names(ph.data)
  required_cols <- c('seq_no', 'injury_nature_broad', 'injury_nature_narrow',
                     'injury_intent', 'injury_mechanism', icdcol)

  missing_required <- setdiff(required_cols, cols)
  if (length(missing_required) > 0) {
    stop(paste0("\n\U0001f47f Missing required column(s): ",
                paste(missing_required, collapse = ', '),
                "\n  Required columns are: ",
                paste(required_cols, collapse = ', ')))
  }

  # Validate seq_no uniqueness ----
  if(anyDuplicated(ph.data$seq_no)){
    stop("\U2620\U0001f47f\U2620\nThe 'seq_no' is a unique patient identifier and should not be repeated across rows.")}

  # Validate injury_nature_broad ----
  if(!is.logical(ph.data$injury_nature_broad)){
    stop("\n\U1F6D1 injury_nature_broad must be a logical (TRUE/FALSE) column")
  }

  if(any(is.na(ph.data$injury_nature_broad))){
    stop("\n\U1F6D1 injury_nature_broad contains NA values, which are not allowed")
  }

  # Validate injury_nature_narrow ----
  if(!is.logical(ph.data$injury_nature_narrow)){
    stop("\n\U1F6D1 injury_nature_narrow must be a logical (TRUE/FALSE) column")
  }

  if(any(is.na(ph.data$injury_nature_narrow))){
    stop("\n\U1F6D1 injury_nature_narrow contains NA values, which are not allowed")
  }

  # Validate injury_intent ----
  if(!is.character(ph.data$injury_intent)){
    stop("\n\U1F6D1 'injury_intent' must be character/text type")
  }

  standard_intent <- unique(death_injury_matrix()[intent != 'any']$intent)
  missing_intent <- setdiff(standard_intent, na.omit(unique(ph.data$injury_intent)))
  extra_intent <- setdiff(na.omit(unique(ph.data$injury_intent)), standard_intent)

  if (length(missing_intent) != 0){
    if(verbose){message("\U00002139 The injury_intent column is missing the following standard intent value(s):\n",
                        paste0(missing_intent, collapse = ', '))}
  }

  if (length(extra_intent) != 0){
    if(verbose){message("\U00002139 The injury_intent column has the following non-standard intent value(s):\n",
                        paste0(extra_intent, collapse = ', '))}
  }

  # Validate injury_mechanism ----
  if(!is.character(ph.data$injury_mechanism)){
    stop("\n\U1F6D1 'injury_mechanism' must be character/text type")
  }

  standard_mechanism <- unique(death_injury_matrix()[mechanism != 'any']$mechanism)
  missing_mechanism <- setdiff(setdiff(standard_mechanism, na.omit(unique(ph.data$injury_mechanism))), "motor_vehicle_traffic")
  extra_mechanism <- setdiff(na.omit(unique(ph.data$injury_mechanism)), standard_mechanism)

  if (length(missing_mechanism) != 0){
    if(verbose){message("\U00002139 The injury_mechanism column is missing the following standard mechanism value(s):\n",
                        paste0(missing_mechanism, collapse = ', '))}
  }

  if (length(extra_mechanism) != 0){
    if(verbose){message("\U00002139 The injury_mechanism column has the following non-standard mechanism value(s):\n",
                        paste0(extra_mechanism, collapse = ', '))}
  }

  # Validate chi_geo_kc (if it exists) ----
  if ('chi_geo_kc' %in% names(ph.data) & length(setdiff(unique(ph.data$chi_geo_kc), c('King County', NA))) > 0){
    stop('\n\U1F6D1 chi_geo_kc exists and has values other than "King County" and NA.\n',
         "If you're analyses are not specific to King County, WA, feel free to delete the chi_geo_kc column.\n",
         "Otherwise, please fix chi_geo_kc and run again.")
  }

  # Validate ICD column format ----
  ph.data[, (icdcol) := toupper(get(icdcol))]

  if (length(grep("\\.|-", ph.data[[icdcol]], value = TRUE) > 0)) {
    warning(paste0("Column `", icdcol, "` contains hyphens (-), periods (.), ",
                   "or other non-alphanumeric characters. These will be removed."))
    ph.data[, (icdcol) := gsub("[[:space:].]+", "", gsub("([^A-Za-z0-9 ])+", "", x = get(icdcol)))]
  }

  if (icdcm_version == 10) {
    problem_icds <- ph.data[is.na(get(icdcol)) | !grepl("^[A-Z][0-9]", get(icdcol)), ][[icdcol]]
    if (length(problem_icds) > 0) {
      warning(paste0("Found ", length(problem_icds), " row(s) where `", icdcol,
                     "` does not follow proper ICD-10-CM pattern (should start with a letter followed by a number). ",
                     "These have been set to NA."))
      ph.data[!grepl("^[A-Z][0-9]", get(icdcol)), (icdcol) := NA]
    }
  }

  # Return the modified data.table ----
  if(verbose){message("\U0001f642 Validation passed! Data is ready for death analysis functions.")}
  return(ph.data)
}

