#' Create an instruction set to implement a recode
#'
#' @param old_var character. Column in the dataset containing the values to be recoded
#' @param new_var character. Column where the recoded values will be placed. If `new_var` doesn't exist, than it will be created. Otherwise it will be overwritten.
#' @param old_value character (or coercible). value in the column `old_var` to be translated into a new value by recoding. If left blank/NA/""/NULL or otherwise missing, the recode operation
#'                  will essentially take the form of a rename of `old_var` -> `new_var`.
#'                  Except for renaming type situations, nothing of old_var is kept (e.g. all relevant values must have a recode row)
#' @param new_value character (or coercible). Column indicating where the new values for the recoding process exist. It is positionally (e.g. row) tied to a particular `old_value`.
#'                  If blank, it helps identify a simpler rename operation.
#' @param new_label character (or convertable to character). The factor label that `old_value` will be recoded to in conjunction with the position of `new_value`
#' @param start_year Integer (or coercible). Year of the data that this recode starts to be relevant (inclusive). If blank, all years before `end_year` are assumed.
#' @param end_year Integer (or coercible). Year of the data that this recode ends it relevance (inclusive). If blank, it indicates that the recode is valid for
#'                 all years after `start_year`
#'
create_recode = function(old_var, new_var, old_value = NULL, new_value = NULL, new_label = NULL, start_year = NULL, end_year = NULL){

  #make sure unique(old_var) and unique(new_var) are both length 1
  old_var = unique(old_var)
  new_var = unique(new_var)
  start_year = unique(start_year)
  end_year = unique(end_year)

  for(iii in c('old_var', 'new_var', 'start_year', 'end_year')){
    if(length(get(iii)) > 1){
      stop(paste(iii, 'has >1 unique values.'))
    }
  }

  #confirm that both old_value and new_value are either missing or both non-missing
  if(!all(sapply(old_value, check_nan) == sapply(new_value, check_nan))){
    stop('`old_value` and `new_value` must both either be not blank (e.g. "", NA, NULL) or blank')
  }

  #confirm that old value and new value have the same length
  stopifnot(length(new_value) == length(old_value))

  #confirm that new label, if not blank, is the same length as the values
  all_blank = all(sapply(new_label, check_nan))
  if(!all_blank) stopifnot(length(new_label) == length(old_value))


  #Make sure the years play nice
  start_year = ifelse(check_nan(as.integer(start_year)), 2002, as.integer(start_year))
  end_year = ifelse(check_nan(as.integer(end_year)), 3000, as.integer(end_year))

  #make sure end >= start
  if(start_year>end_year){
    stop(paste('End:', end_year, 'Start:', start_year, '| start>end -- this is naughty'))
  }

  ret = list(new_var = unique(new_var),
             old_var = unique(old_var),
             old_value = old_value,
             new_value = new_value,
             new_label = new_label,
             year_bounds = c(start_year, end_year))
  return(ret)
}
