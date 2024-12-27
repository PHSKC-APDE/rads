#' Create a Dictionary for a Data Table or Data Frame
#'
#' @description
#' This function generates a dictionary of the variables in a given data.table
#' or data.frame, detailing the variable types and a sample of unique values.
#' Optionally, it can incorporate descriptions and notes from a reference
#' data.table or data.frame.
#'
#' @usage
#' create_dictionary(ph.data,
#'                   source,
#'                   suppress = NULL,
#'                   varsort = FALSE,
#'                   max_unique_values = 8,
#'                   truncation_threshold = 5,
#'                   ph.ref = NULL)
#'
#' @param ph.data A data.table or data.frame containing the dataset to be
#'   analyzed.
#' @param source A character string indicating the source of the data, e.g.,
#'   `"[my_schema].[my_table]"`.
#' @param suppress A character vector of variable names that will have sample
#'   values suppressed in the dictionary output (e.g., Social Security Numbers).
#'   Default is NULL.
#' @param varsort A logical value (`TRUE` or `FALSE`) indicating whether to sort
#'   the dictionary alphabetically by variable name. Default is FALSE, which
#'   keeps the original column order.
#' @param max_unique_values An integer indicating the maximum number of unique
#'   values to display for each variable. If a variable has more unique values
#'   than this, it will be truncated or summarized. Default is 8.
#' @param truncation_threshold An integer indicating how many values to display
#'   before truncating with an ellipsis, when the number of unique values
#'   exceeds max_unique_values. Only applies to non-numeric variables. Default
#'   is 5.
#' @param ph.ref An optional reference data.table or data.frame with columns:
#'   `source`, `varname`, `desc`, and `notes`. It will be merged on to the new
#'   data dictionary. Default is NULL.
#'
#' @details The `create_dictionary` function generates a dictionary from the
#' provided data.table or data.frame, indicating the variable types and listing
#' unique values for each variable. Different variable types are handled as
#' follows:
#' \itemize{
#'   \item \strong{Character and logical variables}: If the number of unique
#'   values exceeds `max_unique_values`, the function displays the first
#'   `truncation_threshold` values followed by an ellipsis.
#'   \item \strong{Factor variables}: The function displays factor levels and
#'   their corresponding integer codes. These are displayed following the rules
#'   for character values.
#'   \item \strong{Numeric variables (integer, numeric)}: If the number of
#'   unique values exceeds `max_unique_values`, the function displays the
#'   minimum and maximum values.
#'   \item \strong{Date and datetime variables}: Treated similarly to numeric
#'   variables, showing minimum and maximum values if there are too many unique
#'   values.
#'   \item \strong{Other types}: For non-atomic types (e.g., `lists`), the
#'   function suggests checking the original dataset structure.
#' }
#' Users can hide the unique values of sensitive variables (e.g., phone numbers
#' in `ph.data`) using the `suppress` parameter. Additionally, if a reference
#' data.table or data.frame (`ph.ref`) is provided, it will merge descriptions
#' and notes into the output.
#'
#' @return A data.table with the following columns:
#' \describe{
#'   \item{source}{Character: The source of the data.}
#'   \item{varname}{Character: The name of the variable.}
#'   \item{vartype}{Character: The type of the variable (e.g., factor,
#'   character, logical, integer, numeric, date, datetime, other).}
#'   \item{values}{Character: A sample of unique values or a range if the number
#'   of unique values exceeds `max_unique_values`.}
#'   \item{factor_labels}{Character: Labels for factor levels if the variable is
#'   a factor.}
#'   \item{desc}{Character: Description of the variable. This column is only
#'   filled if a `ph.ref` data frame is provided.}
#'   \item{notes}{Character: Additional notes about the variable. This column is
#'   only filled if a `ph.ref` data frame is provided.}
#'   \item{dict_updated}{Date: The date the dictionary was created, i.e., the
#'   date you ran this function.}
#' }
#'
#' @examples
#' library(data.table)
#' dt <- data.table(
#'   xID = paste0(sample(LETTERS, size = 1000, replace = TRUE),
#'                sample(c(12345L:99999L), size = 1000, replace = TRUE)),
#'   xlogical = sample(c(TRUE, FALSE), size = 1000, replace = TRUE),
#'   xchar_long = sample(c(LETTERS), size = 1000, replace = TRUE),
#'   xchar_short = sample(c('a', 'b', 'c', 'd'), size = 1000, replace = TRUE),
#'   xfactor = factor(sample(1L:4L, size = 1000, replace = TRUE),
#'                    levels = 1L:4L,
#'                    labels = c('One', 'Two', 'Three', 'Four')),
#'   xbinary = sample(c(0, 1), size = 1000, replace = TRUE),
#'   xinteger_long = sample(c(0L:5000L), size = 1000, replace = TRUE),
#'   xinteger_short = sample(c(0:4), size = 1000, replace = TRUE),
#'   xnumeric = runif(1000, 0, 100),
#'   xdate_long = as.Date(sample(c(as.Date('1900-01-01'):as.Date('1999-12-31')),
#'                        size = 1000,
#'                        replace = TRUE)),
#'   xdate_short = as.Date(sample(c(as.Date('2000-01-01'):as.Date('2000-01-04')),
#'                         size = 1000,
#'                         replace = TRUE)),
#'   xdatetime_long = as.POSIXct(
#'                     runif(1000,
#'                           min = as.numeric(as.POSIXct('2023-01-01 00:00:00')),
#'                           max = as.numeric(Sys.time())),
#'                           origin = "1970-01-01"),
#'   xother = sample(list(c(1:3), c(2:4), c(3:5), c(4:6)),
#'                   size = 1000,
#'                   replace = TRUE)
#' )
#'
#' dictionary1 <- create_dictionary(ph.data = dt,
#'                                  source = 'test dataset',
#'                                  suppress = c('xID'),
#'                                  varsort = FALSE)
#' print(dictionary1[])
#'
#' ph.ref <- data.table(
#'   source = rep('test dataset', 4),
#'   varname = c('xID', 'xbinary', 'xlogical', 'xchar_long', 'xfactor',
#'              'xinteger_long'),
#'   desc = c('ID', 'Binary variable', 'Logical variable',
#'            'Character variable with long names',
#'            'Factor variable with labels',
#'            'Integer variable with long range'),
#'   notes = c('Sample IDs', 'Generic binary', 'Important', 'Check values',
#'             'Categorical data', 'Range from 0 to 5000')
#' )
#'
#' dictionary2 <- create_dictionary(ph.data = dt,
#'                                  source = 'test dataset',
#'                                  suppress = c('xID'),
#'                                  varsort = FALSE,
#'                                  ph.ref = ph.ref)
#' print(dictionary2[])
#'
#' @import data.table
#' @export
#'
create_dictionary <- function(ph.data,
                              source,
                              suppress = NULL,
                              varsort = FALSE,
                              max_unique_values = 8,
                              truncation_threshold = 5,
                              ph.ref = NULL) {
  # Visible bindings for data.table/check global variables ----
  desc <- notes <- varname <- dict_updated <- NULL
  values <- factor_labels <- desc.desc <- notes.desc <- NULL

  # Check arguments ----
  # Check if input is a data.frame or data.table ----
  if (!is.data.frame(ph.data)) {
    stop("\n\U2620 Input must be a data.frame or data.table")
  }

  # Convert to data.table if it's a data.frame ----
  if (!is.data.table(ph.data)) {
    ph.data <- as.data.table(ph.data)
  }

  # Check ph.data has column names ----
  if (is.null(names(ph.data))) {
    stop("\n\U2620 ph.data must have column names")
  }

  # Check that source is a string of length one ----
  if (!is.character(source) || length(source) != 1) {
    stop("\n\U2620 source must be a character vector of length one")
  }

  # Check that source is not an empty string ----
  if (source == "") {
    stop("\n\U2620 source must not be an empty string")
  }

  if (is.na(source)) {
    stop("\n\U2620 source cannot have the value `NA`")
  }

  # Check suppress is a valid character vector ----
  if (!is.null(suppress) && !is.character(suppress)) {
    stop("\n\U2620 suppress must be a character vector of column names")
  }
  if (!is.null(suppress) && is.character(suppress)) {
    invalid_suppress <- setdiff(suppress, names(ph.data))
    if (length(invalid_suppress) > 0) {
      stop(
        '\U2620 The following are not valid column names in ph.data: ',
        paste0(invalid_suppress, collapse = ', ')
      )
    }
  }

  # Check no duplicate column names in suppress ----
  if (!is.null(suppress) && anyDuplicated(suppress)) {
    stop("\n\U2620 suppress must not contain duplicate column names")
  }

  # Check that varsort is a logical of length one ----
  if (!is.logical(varsort) || length(varsort) != 1) {
    stop(paste0("\n\U2620 varsort must be a logical vector of length one ",
                "(TRUE or FALSE)"))
  }

  # Check that max_unique_values is a positive integer ----
  if (!is.numeric(max_unique_values) ||
      length(max_unique_values) != 1 ||
      max_unique_values <= 0 || max_unique_values %% 1 != 0) {
    stop("\n\U2620 max_unique_values must be a positive integer")
  }

  # Check that truncation_threshold is a positive integer ----
  if (!is.numeric(truncation_threshold) ||
      length(truncation_threshold) != 1 ||
      truncation_threshold <= 0 || truncation_threshold %% 1 != 0) {
    stop("\n\U2620 truncation_threshold must be a positive integer")
  }

  # Check that max_unique_values is >= truncation_threshold ----
  if (max_unique_values < truncation_threshold) {
    stop(paste0("\n\U2620 max_unique_values must be greater than or equal to ",
                "truncation_threshold"))
  }

  # Check ph.ref if provided ----
  if (!is.null(ph.ref)) {
    if (!is.data.table(ph.ref) && !is.data.frame(ph.ref)) {
      stop("\n\U2620 ph.ref must be a data.frame or data.table")
    }

    if (!is.data.table(ph.ref)) {
      ph.ref <- as.data.table(ph.ref)
    }

    required_cols <- c("source", "varname", "desc")
    if (!all(required_cols %in% names(ph.ref))) {
      stop(
        "\n\U2620 ph.ref must contain the following columns: ",
        paste(required_cols, collapse = ", ")
      )
    }

    if (!source %in% unique(ph.ref$source)) {
      stop(
        "\n\U2620 Error in `source` argument or `ph.ref` data:\n",
        "The `source` value must match at least one 'source' value in `ph.ref`.\n",
        "To resolve this, you can:\n",
        "1. Update the `source` argument to match a value in `ph.ref$source`\n",
        "2. Update `ph.ref` to include the specified `source` value\n",
        "3. Set `ph.ref = NULL` if you don't want to use a ph.ref dataset"
      )
    }

    if (!"notes" %in% names(ph.ref)) {
      ph.ref[, notes := NA_character_]
    }
  }

  # Generate results table ----
  # get name of ph.data
  mydtname <- deparse(substitute(ph.data))

  # get column names (and sort if requested)
  mycolnamez <- names(ph.data)
  if (varsort) {
    mycolnamez <- sort(mycolnamez)
  }

  result <- rbindlist(lapply(mycolnamez, function(col) {
    # Check if the column contains non-atomic types (e.g., lists or expressions)
    if (!is.atomic(ph.data[[col]])) {
      vartype <- "other"
      value_list <- paste0("check dataset: `str(", mydtname, "$", col, ")`")
      factor_label <- NA_character_
    } else {
      # Get unique values and sort them
      values <- sort(unique(ph.data[[col]]))

      # Determine the type of the variable ----
      vartype <- if (is.factor(ph.data[[col]])) {
        "factor"
      } else if (is.character(ph.data[[col]])) {
        "character"
      } else if (is.logical(ph.data[[col]])) {
        "logical"
      } else if (inherits(ph.data[[col]], "Date")) {
        "date"
      } else if (inherits(ph.data[[col]], "POSIXct") ||
                 inherits(ph.data[[col]], "POSIXlt")) {
        "datetime"
      } else if (is.numeric(ph.data[[col]]) &&
                 all(ph.data[[col]] %% 1 == 0, na.rm = TRUE)) {
        if (all(values %in% c(0, 1)))
          "binary"
        else
          "integer"
      } else if (is.numeric(ph.data[[col]])) {
        "numeric"
      } else {
        "other"
      }

      # Determine the values to display based on suppression and type ----
      if (!is.null(suppress) && col %in% suppress) {
        value_list <- "_suppressed_"
        factor_label <- NA_character_
      } else {
        if (length(values) <= max_unique_values) {
          if (vartype == "factor") {
            value_list <- as.character(as.integer(values))
            factor_label <- levels(ph.data[[col]])[as.integer(values)]
          } else {
            value_list <- sort(as.character(values))
            factor_label <- NA_character_
          }
        } else {
          if (vartype %in% c('integer', 'numeric', 'date', 'datetime')) {
            # For numeric and date/time values, get min and max ----
            value_list <- paste0('min = ',
                                 min(values, na.rm = TRUE),
                                 ", max = ",
                                 max(values, na.rm = TRUE))
            factor_label <- NA_character_
          } else {
            value_list <- c(sort(as.character(values[1:truncation_threshold])),
                            "...")
            factor_label <- NA_character_
          }
        }
      }
    }

    # Create the data.table ----
    dt <- data.table(
      source = source,
      varname = col,
      vartype = vartype,
      values = value_list,
      factor_labels = factor_label,
      desc = "",
      notes = ""
    )

    # Ensure all blanks and empty strings are true NA
    string_clean(dt)

    # Return object
    return(dt)
  }))

  # Reorder columns in the result ----
  setcolorder(
    result,
    c(
      "source",
      "varname",
      "vartype",
      "values",
      "factor_labels",
      "desc",
      "notes"
    )
  )

  # Merge with ph.ref if provided ----
  if (!is.null(ph.ref)) {
    merged_result <- merge(
      x = result,
      y = ph.ref,
      by = c("source", "varname"),
      all.x = TRUE,
      suffixes = c("", ".desc")
    )

    # Fill desc and notes from ph.ref
    merged_result[, desc := ifelse(is.na(desc.desc), desc, desc.desc)]
    merged_result[, notes := ifelse(is.na(notes.desc), notes, notes.desc)]

    # Drop the .desc columns
    merged_result[, c("desc.desc", "notes.desc") := NULL]

    # Warn if there are rows in ph.data that do not match ph.ref
    unmatched_rows <-
      unique(merged_result[is.na(desc) & is.na(notes)][, list(source, varname)])

    if (nrow(unmatched_rows) > 0) {
      warning(
        paste0("\n\U26A0 The following rows in ph.data do not have a match in ",
               "ph.ref and WILL *NOT* be included in the dictionary: \n"),
        paste0(
          "     source: ",
          unmatched_rows$source,
          ", varname: ",
          unmatched_rows$varname,
          collapse = "\n"
        )
      )
    }

    # Keep only matched rows
    result <- merged_result[!is.na(desc)]
  }

  # Ensure desired varname sorting ----
  result[, varname := factor(varname,
                             levels = intersect(mycolnamez,
                                                unique(result$varname)))]
  setorder(result, varname)
  result[, varname := as.character(varname)]

  # Add creation date
  result[, dict_updated := Sys.Date()]

  # Return results ----
  return(result)
}
