#' format_single_year
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
format_single_year = function(input, labels, var_name_col, data_value_col, label_col, jump_scope = F){
  if(!jump_scope){
    input = copy(input)
    labels = copy(labels)
  }

  #validate inputs
  if(!inherits(input, 'data.frame')){
    stop('`input` does not inherit data.frame.')
  }else
  if(!inherits(labels, 'data.frame')){
    stop('`labels` does not inherit data.frame.')
  }

  #confirm that all *_col arguments have been specfied properly
  if(!all(c(var_name_col, data_value_col, label_col) %in% names(labels))){
    mis_cols = setdiff(c(var_name_col, data_value_col, label_col), names(labels))
    stop(paste0('columns: ', paste(mis_cols, collapse = ' | '), ' are missing from input: `labels`'))
  }

  #confirm data.table-ness
  setDT(input)
  setDT(labels)

  #convert labels into something slightly more usable.
  #Roll down var_name until it hits a value that is not NA or ""
  labels[get(var_name_col) == "", (var_name_col) := NA]
  labels[, (var_name_col) := zoo::na.locf(.SD), .SDcols = var_name_col]

  #set labels names
  setnames(labels, c(var_name_col, data_value_col, label_col), c('var_name', 'data_value', 'label'))

  #check to make sure that there is only one label per variable
  if(max(labels[data_value == "" | is.na(data_value), .N, by = 'var_name'][, N])>1){
    stop('Input: `labels` contains instances where a particular variable (specified by var_name_col) is mapped to over 1 variable label (label_col)')
  }

  #reclassify variables into factors with labels
  for(vvv in unique(labels[!is.na(data_value) & data_value != "", var_name])){
    tlab = labels[var_name == vvv & !is.na(data_value) & data_value != "",]
    input[, (vvv) := factor(get(vvv), tlab[,data_value], tlab[, label])]
  }

  var_labs = labels[is.na(data_value) | data_value == "", ]
  for(vvv in unique(var_labs[,var_name])){
    vlab = var_labs[var_name == vvv, label]
    setattr(input, vvv, vlab)
  }


  if(jump_scope){
    return(invisible(input))
  }

  #return
  return(input)
}
