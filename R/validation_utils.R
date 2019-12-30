#' Check to see if a vector of variable names do indeed exist in a set of dataset names
#'
#' @param arg_name character. Name of the argument being validated
#' @param dataset_name character. Name of the dataset (or dataset argument) being validated
#' @param var_names character. Vector of variable names
#' @param vars_to_check character. Vector of variables that presumably exist in `var_names` to be confirmed
#'
check_names = function(arg_name, dataset_name, var_names, vars_to_check){
  var_check = vars_to_check %in% var_names

  if(!all(var_check)){
    check_response = paste0('The following variables specified by `', arg_name, '` cannot be found within ', '`', dataset_name, '`:',
                            paste(vars_to_check[!var_check], collapse = ' '))
  }else{
    check_response = ''
  }

  return(check_response)
}

#' Check the depth of a list
#' @param this a list.
#' @param thisdepth numeric. Starting depth.
depth <- function(this,thisdepth=0){
  if(is.list(this) && length(this) == 0){return(0)}
  if(!is.list(this)){
    return(thisdepth)
  }else{
    return(max(unlist(lapply(this,depth,thisdepth=thisdepth+1))))
  }
}
#' Validate list inputs into the tabulate functions
#' @param x list or a vector. The instructions/groupings to be validated. Note: a vector is turned into a list.
#' @param values vector. The values of the column that the instructions in x are relevant for.
#' @param variable_name character. T
#' @param prefix character of length 1. Prefix to be added to the automatic naming process.
#' @param make_names logical. Should names be added to x if they don't exist already.
#'
#' @return A validated list input, optionally with names.
#'
validate_list_input = function(x, values, variable_name, prefix = 'Value', make_names = T){

  if(is.null(x)){
    x = list(unique(values[!is.na(values)]))
  }

  stopifnot(length(prefix) == 1)
  stopifnot(!missing(variable_name) & length(variable_name) == 1)

  #make it a list if its not already
  if(!is.list(x)){
    x = list(x)
  }

  #make sure the instructions are mutually exclusive
  g_ul = unlist(x)
  g_ul_uniq = unique(g_ul)
  if(length(g_ul) != length(g_ul_uniq)) stop(paste0('Grouping instructions passed via ', variable_name, ' are not mutually exclusive'))

  #make
  g_chck = g_ul_uniq %in% values

  if(!all(g_chck)) stop(paste0('Invalid values found in ',  variable_name, ': ', paste(g_ul_uniq[!g_chck], collapse = ', ')))

  #create names if non exist
  if(make_names){
    if(is.null(names(x))){
      names(x) = unlist(lapply(x, function(y) paste0(prefix, ': ', paste(y, collapse = ', '))))
    }
    if(any(is.na(names(x)))){
      names(x)[is.na(names(x))] = unlist(lapply(x[is.na(names(x))], function(y) paste0(prefix, ': ', paste(y, sep = ', '))))
    }
  }

  #return the list
  return(x)
}
#' convert validated list instructions into data frames to be merged on
#'
#' @param x list. The expectation is that the list has previously been formatted/validated via \code{\link{validate_list_input}}
#' @param new_col character. The name of the new column to be created given the reclassifications implied by x
#' @param old_col character. The name of the old column implied from the reclassifications of x
#'
#' @return data.frame. For a given pair of old column and new column, return the recode mapping.
#'
list_to_dt = function(x, new_col, old_col){
  len = length(x)

  res = lapply(1:len, function(i) data.table(new = names(x)[i], old = x[[i]]))
  res = data.table::rbindlist(res)
  setnames(res, c(new_col, old_col))

  setDF(res)
  return(res)
}

#' Apply the recoding on a variable, given an instruction set.
#' @param values vector. Values to be recoded
#' @param instruction list. Validate instruction set from \code{\link{validate_list_input}}
#'
#' @importFrom apdeRecodes create_recode apply_recode
#' @importFrom data.table data.table setnames
#'
#' @return a vector of length of values that have been recoded/altered to meet the specifications of instruction
apply_instructions = function(values, instruction){
  instruction = list_to_dt(instruction, 'a2', 'a1')
  rec = apdeRecodes::create_recode(old_var = 'a1', new_var = 'a2', old_value = as.character(instruction[['a1']]), new_value = instruction[['a2']])
  dat = data.table::data.table(a1 = values)
  res = apdeRecodes::apply_recode(data = dat,recode = rec, jump_scope = F, return_vector = T)
  return(res)

}





