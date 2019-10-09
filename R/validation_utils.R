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
#' borrowed from: https://stackoverflow.com/questions/13432863/determine-level-of-nesting-in-r
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
list_to_dt = function(x, new_col, old_col){
  len = length(x)

  res = lapply(1:len, function(i) data.table(new = names(x)[i], old = x[[i]]))
  res = data.table::rbindlist(res)
  setnames(res, c(new_col, old_col))

  setDF(res)
  return(res)
}

apply_instructions = function(values, old_var, new_var, instruction){
  instruction = list_to_dt(instruction, new_var, old_var)
  rec = apdeRecodes::create_recode(old_var = old_var, new_var = new_var, old_value = as.character(instruction[[old_var]]), new_value = instruction[[new_var]])
  dat = data.table::data.table(old= values)
  data.table::setnames(dat, 'old', old_var)
  res = apdeRecodes::apply_recode(data = dat,recode = rec, jump_scope = F, return_vector = T)
  return(res)

}





