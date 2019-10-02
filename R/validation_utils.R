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
