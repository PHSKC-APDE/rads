#' compute_apde_vars
#'
#' Takes the raw HYS data and year specific HYS crosswalk sheet. Applies value labels to the variables and adds the variable labels to the attributes
#'
#' @param input data.table. HYS data (generally imported raw)
#' @param labels  data.table. Year specific sheet from the DOH provided HYS crosswalk containing the link between variable names and labels
#' @param var_name_col character. Column that contains the variable name (should at least somewhat link with \code{names(input)})
#' @param data_value_col character. Column that links the data value to the value label
#' @param label_col character. Column that provides value (and variable) labels. Variable label is assumed if data_value_col is blank.
#' @param jump_scope logical. Whether or not `input` should be copied before the analysis or whether the modify by reference aspects of data.table should beleveraged.
#'                   Or in other words, should `input` be modified in parent scope? Same applies to `labels`.
#'
#' @return a data.table, unless jump_scope == T, in which case, invisible()
#'
#' @import data.table
#' @importFrom zoo na.locf
#' @export
#'
