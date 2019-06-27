#'Parse Recode Instructions
#'
#'Imports a data.frame with at a minimum of the following columns: old_var, old_value, new_var, new_value, new_label, start_year, and end_year.
#'
#'old_var: Column in the dataset containing the values to be recoded
#'
#'old_value: value in the column `old_var` to be translated into a new value by recoding. If left blank/NA/""/NULL or otherwise missing, the recode operation
#'will essentially take the form of a rename of `old_var` -> `new_var`
#'
#'new_var: Column where the recoded values will be placed. If `new_var` doesn't exist, than it will be created. Otherwise it will be overwritten.
#'
#'new_value: Column indicating where the new values for the recoding process exist. It is positionally (e.g. row) tied to a particular `old_value`.
#'If blank, it helps identify a simpler rename operation.
#'
#'start_year: Numeric. Year of the data that this recode starts to be relevant (inclusive). If blank, all years before `end_year` are assumed.
#'
#'end_year: Numeric. Year of the data that this recode ends it relevance (inclusive). If blank, it indicates that the recode is valid for
#'all years after `start_year`
#'
#'new_label: character (or convertable to character): The factor label that `old_value` will be recoded to in conjunction with the position of `new_value`
#'
#' @param recode data.frame. In the format described in "Details"
#' @param catch_NAs logical. Should the presence of "NA" be turned into NA_character_
#' @import data.table
#' @export
#'
#' @return list of lists of recode instructions.
#'
parse_recode_instructions = function(recode, catch_NAs = T){

  #check column names
  vvv = c('old_var', 'old_value', 'new_var', 'new_value', 'start_year', 'end_year')
  if(!all(vvv %in% names(recode))){
    stop(paste0(paste(setdiff(vvv, names(recode)), collapse = ', '), 'are missing from `recode`'))
  }

  #setDT for manipulation (and copy just to be safe)
  recode = copy(recode)
  setDT(recode)

  if(catch_NAs){
    recode[old_value == 'NA', old_value := NA]
    recode[new_value == 'NA', new_value := NA]
  }
  #compute the recode instructions
  rec_instruct = recode[, (list(list(create_recode(old_var, new_var, old_value, new_value, new_label, start_year, end_year)))),
                        by = c('old_var', 'new_var', 'start_year', 'end_year')][, V1]

  return(rec_instruct)

}
