#' Calculate the Maximum Depth of a Nested List
#'
#' This function examines a list to determine how many layers deep it goes.
#' The depth is defined as the number of nested list layers, where a simple vector
#' has depth 0, a list containing vectors has depth 1, a list containing lists has
#' depth 2, and so on.
#'
#' @param this A list or other R object to analyze. If not a list, the function
#'   returns the starting depth level defined by \code{thisdepth}.
#' @param thisdepth Numeric. The starting depth level for the calculation.
#'   Defaults to 0.
#'
#' @return A numeric value representing the maximum depth of the list structure.
#'
#' @export
#'
#' @examples
#' # Simple vector (depth 0)
#' depth(c(1, 2, 3))
#'
#' # Empty list (depth 0)
#' depth(list())
#'
#' # Simple list with vectors (depth 1)
#' depth(list(a = 1:3, b = letters[1:5]))
#'
#' # Nested list (depth 2)
#' depth(list(a = list(x = 1, y = 2), b = 3))
#'
#' # Nested list (depth 3)
#' nested_list <- list(
#'   level1 = list(
#'     level2 = list(
#'       level3 = c(1, 2, 3)
#'     )
#'   )
#' )
#' depth(nested_list)
#'
#' # Mixed depth list (returns maximum depth)
#' mixed_list <- list(
#'   shallow = c(1, 2),
#'   deep = list(inner = list(deeper = 42))
#' )
#' depth(mixed_list)
#'

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
#' @keywords internal
#' @noRd
#' @return A validated list input, optionally with names.
#'
validate_list_input <- function(x, values, variable_name, prefix = 'Value', make_names = T){

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

  #create names if none exist
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

