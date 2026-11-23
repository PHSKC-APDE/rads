# complete_counts() ----
#' Add zero-count rows for combinations of values that are missing from a count table
#'
#' @description
#' Count tables usually have no row for combinations with zero events. For example,
#' if nobody aged 21 died, a death count table simply has no row for age 21. When
#' you merge on population and calculate rates, the people aged 21 are silently left
#' out, even though they had zero deaths. The same number of deaths spread over
#' fewer people give a rate that is too high (see [age_standardize()]).
#'
#' `complete_counts()` adds the missing rows, with zero in the `counts` column(s),
#' so that every combination of values in the key columns is present.
#'
#' @param ph.data A data.table or data.frame containing aggregated counts, e.g.,
#' the output of [death_113_count()], `apde.data::population()`, etc.
#' @param counts Character vector of indeterminate length. The name(s) of the
#' column(s) in `ph.data` with counts. New rows get `0` in these columns.
#'
#' The default is `counts = 'deaths'`.
#' @param ... Optional. The complete set of values for one or more key columns,
#' written as `column_name = values`, e.g., `age = 18:64`. Use this for values
#' that might never appear in `ph.data` (e.g., ages with no deaths). Key columns not
#' listed here are completed using the values already in `ph.data`.
#' @param include.na Logical vector of length 1. Should `NA` be treated as a value
#' to complete? When `FALSE`, existing rows with `NA` in a key column are kept as
#' they are, but no new `NA` rows are added. When `TRUE`, `NA` is treated like any
#' other value in key columns that contain it.
#'
#' The default is `include.na = FALSE`.
#'
#' @details
#' **Key columns:** Every column in `ph.data` that is not in `counts` is a key column.
#' The output has one row for every combination of values in the key columns.
#' A message lists the key columns and their number of unique values, so you can
#' spot a column that should have been in `counts` (e.g., `ypll_65`) because it
#' has a suspiciously large number of values.
#'
#' **Rules:**
#'  - `ph.data` must have one row per combination of key columns (i.e., it must already be aggregated).
#'  - Values given in `...` must include every non-missing value of that column in
#'    `ph.data`. Subset `ph.data` first if you want a narrower range.
#'  - Existing rows are never changed. Existing `NA` counts stay `NA`.
#'
#' **Linked columns:** Do not include columns that are just another label for a key
#' column, such as `causeid` alongside `cause.of.death`, or `geo_id_code` alongside
#' `geo_id`. They would be crossed with each other and produce nonsense rows (e.g.,
#' every `causeid` paired with every `cause.of.death`). Drop one of the linked
#' pairs first and merge them back afterward if they are of genuine interest.
#' `complete_counts()` warns when two key columns have a 1:1 relationship, but it
#' can't tell a true link from a coincidence in a small table.
#'
#' @return A data.table with the same columns as `ph.data`, sorted by the key columns.
#'
#' @seealso [age_standardize()], which warns when ages are missing
#' @seealso [death_113_count()], [death_130_count()], [death_injury_matrix_count()],
#' [death_multicause_count()], [death_other_count()], [death_xxx_count()], &
#' [death_rankable_count()], [chars_injury_matrix_count()] and [chars_icd_ccs_count()]
#' which only add zero-count rows for values present in ph.data.
#'
#' @export
#' @name complete_counts
#'
#' @examples
#' library(data.table)
#'
#' # Nobody aged 21 died, so age 21 has no row
#' deaths <- data.table(age = c(20, 20, 22, 22),
#'                      sex = c('Female', 'Male', 'Female', 'Male'),
#'                      deaths = c(3, 1, 2, 4))
#'
#' # Ensure rows for all ages 18-24 (include age 21!) for both sexes
#' complete_counts(deaths, counts = 'deaths', age = 18:24)[]
#'
#' # Linked columns: drop them first, then merge them back
#' causes <- data.table(cause.of.death = c('Cancer', 'Cancer', 'Stroke'),
#'                      causeid = c(1, 1, 2),
#'                      age = c(20, 22, 22),
#'                      deaths = c(1, 2, 3))
#' lookup <- unique(causes[, list(cause.of.death, causeid)])
#' done <- complete_counts(causes[, causeid := NULL], age = 20:22)
#' done <- merge(done, lookup, by = 'cause.of.death')[]
#'
complete_counts <- function(ph.data,
                           counts = 'deaths',
                           ...,
                           include.na = FALSE) {
  ph.data.name <- deparse(substitute(ph.data)) # save name of ph.data as a string for later

  # Validate arguments ----
  ## ph.data ----
  if (!inherits(ph.data, 'data.frame')) {
    stop("\n\U1F6D1 `ph.data` must be a data.frame or a data.table.")
  }
  ph.data <- data.table::setDT(data.table::copy(ph.data))

  ## include.na ----
  if (!is.logical(include.na) || length(include.na) != 1 || is.na(include.na)) {
    stop("\n\U1F6D1 `include.na` must be TRUE or FALSE.")
  }

  ## counts ----
  if (!is.character(counts) || length(counts) == 0) {
    stop("\n\U1F6D1 `counts` must be a character vector of column names, e.g., counts = 'deaths'.")
  }
  missing_counts <- setdiff(counts, names(ph.data))
  if (length(missing_counts) > 0) {
    stop("\n\U1F6D1 The following `counts` column(s) do not exist in ", ph.data.name, ": ",
         paste(missing_counts, collapse = ', '), ".\n",
         "Ensure that your specified column(s) exists in ph.data.")
  }
  not_numeric <- counts[!vapply(counts, function(col) is.numeric(ph.data[[col]]), logical(1))]
  if (length(not_numeric) > 0) {
    stop("\n\U1F6D1 The following `counts` column(s) are not numeric: ", paste(not_numeric, collapse = ', '))
  }

  key_cols <- setdiff(names(ph.data), counts)
  if (length(key_cols) == 0) {
    stop("\n\U1F6D1 ", ph.data.name, " has no key columns (every column in ph.data is listed in `counts`).")
  }

  ## ... (user-defined values) ----
  supplied <- list(...)
  if (length(supplied) > 0) {
    if (is.null(names(supplied)) || any(names(supplied) == '')) {
      stop("\n\U1F6D1 Every argument in `...` must be named with a column name, e.g., age = 18:64.")
    }
    if (anyDuplicated(names(supplied))) {
      stop("\n\U1F6D1 Each column can only be specified once in `...`.")
    }
    in_counts <- intersect(names(supplied), counts)
    if (length(in_counts) > 0) {
      stop("\n\U1F6D1 `...` cannot include `counts` column(s): ", paste(in_counts, collapse = ', '))
    }
    not_cols <- setdiff(names(supplied), names(ph.data))
    if (length(not_cols) > 0) {
      stop("\n\U1F6D1 The following column(s) in `...` do not exist in ", ph.data.name, ": ",
           paste(not_cols, collapse = ', '))
    }

    for (col in names(supplied)) {
      x <- ph.data[[col]]
      vals <- unique(supplied[[col]])
      vals <- vals[!is.na(vals)] # NA is handled by `include.na`

      # make supplied values the same type as the column so they can be merged
      if (is.factor(x)) {
        if (!is.character(vals) && !is.factor(vals)) {
          stop("\n\U1F6D1 `", col, "` is a factor in ", ph.data.name, ", so the values in `...` must be character (or factor).")
        }
        vals <- as.character(vals)
        new_levels <- union(levels(x), vals)
        ph.data[, (col) := factor(x, levels = new_levels)]
        vals <- factor(vals, levels = new_levels)
      } else if (is.numeric(x)) {
        if (!is.numeric(vals)) {
          stop("\n\U1F6D1 `", col, "` is numeric in ", ph.data.name, ", so the values in `...` must be numeric.")
        }
        if (is.integer(x)) {
          if (any(vals %% 1 != 0)) {
            stop("\n\U1F6D1 `", col, "` contains whole numbers in ", ph.data.name, ", so the values in `...` must be whole numbers.")
          }
          vals <- as.integer(vals)
        } else {
          vals <- as.numeric(vals)
        }
      } else if (!identical(class(x), class(vals))) {
        stop("\n\U1F6D1 `", col, "` is class '", paste(class(x), collapse = '/'), "' in ", ph.data.name,
             ", but the values in `...` are class '", paste(class(vals), collapse = '/'), "'. They must match.")
      }

      # supplied values *must* include every value in the data
      extra <- setdiff(unique(x[!is.na(x)]), vals)
      if (length(extra) > 0) {
        stop("\n\U1F6D1 ", ph.data.name, " has values of `", col, "` that are not in the values you supplied: ",
             paste(utils::head(sort(extra), 10), collapse = ', '), if (length(extra) > 10) ', ...' else '', ".\n",
             "complete_counts() does not drop rows. Subset ", ph.data.name, " first if you want a narrower range.")
      }

      supplied[[col]] <- vals # at this point, have complete set of values for the cross-join and of the proper class
    }
  }

  ## one row per combination of key columns ----
  if (anyDuplicated(ph.data, by = key_cols) > 0) {
    stop("\n\U1F6D1 ", ph.data.name, " has more than one row for some combinations of key columns.\n",
         "Key columns are every column not in `counts`: ", paste(key_cols, collapse = ', '), ".\n",
         "Either aggregate ", ph.data.name, " first or add any missing count columns to `counts`.")
  }

  # Warn about linked (1:1) key columns ----
  # e.g., causeid & cause.of.death. Crossing them creates nonsense rows.
  multi_val_cols <- key_cols[vapply(key_cols, function(col) data.table::uniqueN(ph.data[[col]]) > 1, logical(1))]
  if (length(multi_val_cols) >= 2) {
    pairs <- utils::combn(multi_val_cols, 2, simplify = FALSE) # gives a list of every pair
    linked <- Filter(function(p) {
      n1 <- data.table::uniqueN(ph.data[[p[1]]])
      n2 <- data.table::uniqueN(ph.data[[p[2]]])
      n1 == n2 && n1 == data.table::uniqueN(ph.data, by = p)
    }, pairs)

    if (length(linked) > 0) {
      warning(paste0(
        "\n\u26A0\ufe0f These key columns have a 1:1 relationship in ", ph.data.name, ":\n",
        paste0("  - ", vapply(linked, paste, character(1), collapse = ' & '), collapse = "\n"), "\n\n",
        "If one is just another label for the other (e.g., causeid & cause.of.death),\n",
        "the output contains nonsense rows pairing every value of one with every value\n",
        "of the other. Drop one column before running complete_counts() and merge it back afterward.\n",
        "If the relationship is a coincidence (e.g., a small table), you can ignore this warning."),
        immediate. = TRUE, call. = FALSE)
    }
  }

  # Message about existing NA counts ----
  na_counts <- vapply(counts, function(col) sum(is.na(ph.data[[col]])), integer(1))
  if (any(na_counts > 0)) {
    message("\u2139\ufe0f Existing rows with NA counts were left as NA: ",
            paste0(names(na_counts)[na_counts > 0], " (", na_counts[na_counts > 0], " rows)", collapse = ', '), ".")
  }

  # Build every combination of key column values ----
  key_vals <- lapply(key_cols, function(col) {
    vals <- if (col %in% names(supplied)) supplied[[col]] else unique(ph.data[[col]][!is.na(ph.data[[col]])]) # user supplied values overrides what exists in ph.data
    if (include.na && anyNA(ph.data[[col]])) vals <- c(vals, NA)
    vals
  })
  names(key_vals) <- key_cols
  template <- do.call(data.table::CJ, c(key_vals, list(sorted = FALSE)))

  # Merge the data onto the template & fill new rows with zero ----
  orig_names <- names(ph.data)
  ph.data[, .orig_row := TRUE] # flags rows that existed before the merge
  result <- merge(template, ph.data, by = key_cols, all = TRUE)
  result[is.na(.orig_row), (counts) := 0] # new rows (no flag) get zero counts
  result[, .orig_row := NULL]
  ph.data[, .orig_row := NULL]

  data.table::setkey(result, NULL) # a side effect of merge() is to set a key, drop it before I forget
  data.table::setcolorder(result, orig_names)
  data.table::setorderv(result, key_cols, na.last = TRUE)

  # Summarize ----
  key_summary <- data.table::data.table(
    `key column` = key_cols,
    `# unique values` = vapply(key_cols, function(col) data.table::uniqueN(result[[col]]), integer(1)),
    `values from` = ifelse(key_cols %in% names(supplied), 'supplied', 'data'))
  message("\u2139\ufe0f complete_counts() added ", format(nrow(result) - nrow(ph.data), big.mark = ','), " rows (",
          format(nrow(ph.data), big.mark = ','), " -> ", format(nrow(result), big.mark = ','), "). Key columns:\n",
          paste(utils::capture.output(print(key_summary, row.names = FALSE, class = FALSE)), collapse = "\n"))

  return(result)
}
