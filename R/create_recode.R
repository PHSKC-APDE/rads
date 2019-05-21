#' create_recode
#'
#' Takes recode instructions and returns an object to be passed to a later function to implement those recodes.
#'
#' @param old_var character. Existing variable name
#' @param old_values numeric (or characters matching factor labels). Existing values in `old_var` to be remapped.
#' @param new_var character. Name of the new column to be created. Default is to overwrite column specified by `old_var`
#' @param new_values numeric. Numeric representation of the new values. Ordered such that `old_values` -> `new_values`. Exact recycling only.
#' @param new_labels Named list. Expectation of the following conventions (numeric value = character label). Default value of "" indicates no labels.
#' @param new_variable_label character. String describing the newly recoded (and created?) variable
#'
#' @return named list containing the recode instructions
#'
#' @export
#'
create_recode = function(old_var, old_values, new_var = old_var, new_values = old_values, new_labels = "", new_variable_label = ""){

  stopifnot(length(old_values) >= length(new_values))

  if(!(length(old_values) %% length(new_values) == 0)){
    stop('Length of `old_values`` is not clearly divisible by the length of `new_values`')
  }

  if(length(new_values) != length(old_values)){
    new_values = rep(new_values, length(old_values)/length(new_values))
  }


  recode_list = list(new_var, values = new_values, new_labels = new_labels, new_var_lab = new_variable_label)
  names(recode_list)[1] = old_var
  names(recode_list[['values']]) = old_values

  return(recode_list)

}
