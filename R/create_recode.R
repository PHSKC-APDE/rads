#' Instruction set for recoding
#'
#' Creates/standardizes the parameters required for recoding a variable.
#'
#' @param old_var character. Column in the dataset containing the values to be recoded
#' @param new_var character. Column where the recoded values will be placed. If `new_var` doesn't exist, than it will be created. Otherwise it will be overwritten.
#' @param old character (or coercible). value in the column `old_var` to be translated into a new value by recoding. If left blank/NA/""/NULL or otherwise missing, the recode operation
#'                  will essentially take the form of a rename of `old_var` -> `new_var`.
#'                  Except for renaming type situations, nothing of old_var is kept (e.g. all relevant values must have a recode row)
#' @param new character (or coercible). Column indicating where the new values for the recoding process exist. It is positionally (e.g. row) tied to a particular `old`.
#'                  If blank, it helps identify a simpler rename operation.
#' @param new_label character (or convertable to character). The factor label that `old` will be recoded to in conjunction with the position of `new`
#' @export
#'
#' @return a list of lists (of class recode_instruction).
#'
#' @details This function mostly exists to help translate (via \code{parse_recode_instructions}) data.frame type read-ins of recoding into a standardized form.
#' These results can then be passed to \code{enact_recoding} as part of a bulk recoding process.
#'
#'
create_recode = function(old_var, new_var, old = NULL, new = NULL, new_label = NULL){

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

  #Note: This check can't occur if we want to be able to use recoding to set values to NA
  #confirm that both old and new are either missing or both non-missing
  # if(!all(sapply(old, check_nan) == sapply(new, check_nan))){
  #   stop('`old` and `new` must both either be not blank (e.g. "", NA, NULL) or blank')
  # }

  #confirm that old value and new value have the same length
  stopifnot(length(new) == length(old))
  stopifnot(is.logical(simplify2numeric))

  #convert factor to character
  if(is.factor(old)){
    stop('old must be character or numeric. Not factor.')
  }

  #confirm that new label, if not blank, is the same length as the values
  all_blank = all(sapply(new_label, check_nan))
  if(!all_blank) stopifnot(length(new_label) == length(old))


  #Make sure the years play nice
  start_year = ifelse(check_nan(as.integer(start_year)), 2002, as.integer(start_year))
  end_year = ifelse(check_nan(as.integer(end_year)), 3000, as.integer(end_year))

  ret = structure(list(new_var = unique(new_var),
             old_var = unique(old_var),
             old = old,
             new = new,
             new_label = new_label,
             year_bounds = c(start_year, end_year),
             var_label = var_label), class =  'recode_instruction')

  return(ret)
}
