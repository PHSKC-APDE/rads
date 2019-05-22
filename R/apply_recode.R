#' Apply a recoding instruction
#'
#' @description
#'
#' Given the output from \code{create_recode}, a dataset, and a few additional instructions, this function implements a recode
#'
#' @param data data.table.
#' @param year numeric or integer. Identifies the year the dataset represents. Helps sort out whether a recode should be applied
#' @param recode list. Output of \code{create_recode} the provides the instructions on how to implement the recode
#' @param jump_scope Logical. Determines whether or not this function takes advantage of data.table's modify by reference semantics
#'                   or returns a single column data.table of nrow(data) to be cbinded on (presumably) later.
#' @details
#'
#' @import data.table
#' @export
#'
#' @return a new column with the recoded values whose class is dependent on the recode instructions.
#' If a recode instruction is passed that is not relevant to the dataset (e.g. year), a column of NAs will be returned.
#'
#'
apply_recode = function(data, year, recode, jump_scope = F){

  #Work with data tables only. Prevents the need to copy the object to avoid data,table::setDT
  stopifnot(inherits(data, 'data.table'))

  #confirm old_var exists within the dataset
  if(!any(names(data) %in% recode$old_var)){
    stop(paste('Column', recode$old_var, 'not found in dataset'))
  }
  #check if the recode instructions are even relevant for the year question
  #if not return all NAs
  if(!data.table::between(year, recode$year_bounds[1], recode$year_bounds[2])){
    return(rep(NA, nrow(data)))
  }


  simple_rename = F
  #check if its a simple renaming
  if(length(recode$new_value) <= 1 & length(recode$old_value) <=1){
    simple_rename = check_nan(recode$new_value) & check_nan(recode$old_value)
    if(simple_rename){
      ret = data[, get(recode$old_var)]
    }
  }
  #otherwise, create the ret object
  if(!simple_rename){
    #check to see if new_var exists in the dataset
    if(any(names(data) %in% recode$new_var)){
      ret = data[, get(recode$new_var)]
    }else{
      ret = rep(NA, nrow(data))
    }
  }

  #if ret is a factor, convert to integer for the time being but save the levels information
  fff = is.factor(ret)
  if(fff){
    factor_label = unique(data.table(levels = ret, int_id = as.numeric(ret)))
    ret = as.numeric(ret)
  }

  #construct recoding
  old = data[, get(recode$old_var)]
  for(i in seq(recode$old_value)){
    ret[which(old %in% recode$old_value[i])] = recode$new_value[i]
  }

  #if there are labels, make a factor
  fls = data.table(levels = recode$new_label, int_id = recode$new_value)
  fls = fls[!is.null(levels),]
  if(fff){
    fls = rbind(fls, factor_label[!int_id %in% fls[, int_id]])
  }
  fls = fls[int_id %in% ret, ]

  if(nrow(fls)>0){
    ret = factor(ret, fls[,int_id], fls[, levels])
  }

  #prepare output
  if(jump_scope){
    data[, (recode$new_var) := ret]
    return(invisible(data))
  }else{
    ret = data.table(a = ret)
    setnames(ret, recode$new_var)
  }
}
