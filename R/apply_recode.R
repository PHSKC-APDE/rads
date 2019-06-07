#' Apply a recoding instruction
#'
#' @description
#'
#' Given the output from \code{create_recode}, a dataset, and a few additional instructions, this function implements a recode.
#' This function translates factors into their underlying numeric representation when recoding. THe levels (that are not reimplemented) are carried through,
#' but recoding with factors will likely break.
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

  print(recode$old_var)

  #Work with data tables only. Prevents the need to copy the object to avoid data,table::setDT
  stopifnot(inherits(data, 'data.table'))

  #confirm old_var exists within the dataset
  if(!any(names(data) %in% recode$old_var)){
    stop(paste('Column', recode$old_var, 'not found in dataset'))
  }

  #check if the recode instructions are even relevant for the year question
  #if not return an error
  if(!data.table::between(year, recode$year_bounds[1], recode$year_bounds[2])){
    stop(paste('Recode instructions not relevant for the given year', year, 'bounds:', recode$year_bounds[1], '-', recode$year_bounds[2]))
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
  bin_me = F
  if(!simple_rename){
    #check to see if new_var exists in the dataset
    if(any(names(data) %in% recode$new_var)){
      ret = data[, get(recode$new_var)]
    }else{
      ret = rep(NA, nrow(data))
    }

    #check if its a likely binning operation
    bin_opt = lapply(recode$old_value, check_bin)

    #if any of those check out, confirm that the the existing variable is numeric. Otherwise throw a warning
    if(any(unlist(bin_opt))){

      if(!(is.numeric(data[, get(recode$old_var)]))){
        warning('Recode implies a recode of the binning type, but old_var is not numeric. Proceeding as a string recode.')
      }else{
        bin_me = T
      }
    }

  }

  #construct recoding
  old = data[, get(recode$old_var)]
  for(i in seq_along(recode$old_value)){

    #binning instructions
    if(bin_me & check_bin(recode$old_value[i])){

      #parse the instructions for recoding
      left = substr(recode$old_value[i],1,1)
      right = substr(recode$old_value[i],nchar(recode$old_value[i]),nchar(recode$old_value[i]))
      inner = substr(recode$old_value[i], 2, nchar(recode$old_value[i]) - 1)

      minmax = strsplit(inner, ',', fixed = T)

      if(length(minmax) != 1){
        stop(paste0('Expecting one comma `,` in the old_value recode instructions. Found:' ,length(minmax)))
      }

      minmax = as.numeric(trimws(minmax[[1]]))

      #check if the coercian created any errors. if so, stop
      if(any(is.na(minmax))){
        stop('Binning operation coerced to NA bounds')
      }

      if(left == '(' & right == ')'){
        ret[which(old > minmax[1] & old< minmax[2])] = recode$new_value[i]
      }else if(left == '[' & right == ')'){
        ret[which(old >= minmax[1] & old< minmax[2])] = recode$new_value[i]
      }else if(left == '(' & right == ']'){
        ret[which(old > minmax[1] & old <= minmax[2])] = recode$new_value[i]
      }else{
        ret[which(old >= minmax[1] & old<= minmax[2])] = recode$new_value[i]
      }

    }else{ #normal recoding
      ret[which(old %in% recode$old_value[i])] = recode$new_value[i]
    }
  }

  #iapply labels
  if(inherits(old, 'haven_labelled')){
    old_labs = labelled::val_labels(old)
    lab_names = names(old_labs)
    old_labs = data.table(value = old_labs)
    old_labs[, label := lab_names]
  }else{
    old_labs = data.table()
  }
  if(!is.null(recode$new_label)){
    new_labs = data.table(value = recode$new_value, label = recode$new_label)
  }else{
    new_labs = data.table()
  }
  labs = unique(rbind(old_labs, new_labs))

  if(nrow(labs)>0){
    v = labs[, value]
    names(v) = labs[, label]
    ret = labelled::labelled(ret, v)
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
