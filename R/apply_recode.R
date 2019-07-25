#' Apply a recoding instruction
#'
#' @description
#'
#' Given the output from \code{create_recode}, a dataset, and a few additional instructions, this function implements a recode.
#' This function translates factors into their underlying numeric representation when recoding. THe levels (that are not reimplemented) are carried through,
#' but recoding with factors will likely break.
#'
#' @param data data.table.
#' @param recode list. Output of \code{create_recode} the provides the instructions on how to implement the recode
#' @param jump_scope Logical. Determines whether or not this function takes advantage of data.table's modify by reference semantics
#'                   or returns a single column data.table of nrow(data) to be cbinded on (presumably) later.
#' @param return_vector Logical. Only matters if jump_scope is FALSE. Determines the return type (vector if T, data.table if F)
#' @details
#'
#' @import data.table
#' @importFrom methods as
#' @export
#'
#' @return a new column with the recoded values whose class is dependent on the recode instructions.
#'
#'
#'
apply_recode = function(data, recode, jump_scope = F, return_vector = F){

  #Work with data tables only. Prevents the need to copy the object to avoid data,table::setDT
  stopifnot(inherits(data, 'data.table'))

  #confirm old_var exists within the dataset
  if(!any(names(data) %in% recode$old_var)){
    stop(paste('Column', recode$old_var, 'not found in dataset'))
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



    #construct recoding
    old = data[, get(recode$old_var)]

    #coerce old to be the class of the specified old values
    #assuming no new NAs
    if(!inherits(old[[1]], class(recode$old_value)) & !bin_me){
      old_class = class(old)

      start_na = sum(is.na(old))
      old = methods::as(old, class(recode$old_value))
      end_na = sum(is.na(old))

      if(end_na > start_na){
        stop(paste('Could not cleanly coerce column --', recode$old_var, '-- to', class(recode$old_value), 'for recoding. Check mismatch between old_var and class(old_value) in recode'))
      }

      #coerce ret as well
      ret = methods::as(ret, class(recode$old_value))

      #strip lingering labels
      if(any(old_class %in% 'factor')){
        attr(ret, 'levels') <- NULL
      }

    }

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
          break
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

    #apply labels if needed
    if(!is.null(recode$new_label)){
      new_labs = unique(data.table(value = recode$new_value, label = as.character(recode$new_label), type = 'new'))
      valclass = class(recode$new_value)
    }else{
      new_labs = data.table()
      valclass = 'numeric'
    }
    if(length(labelled::val_labels(data[1, get(recode$old_var)]))>0 | is.factor(data[1, get(recode$old_var)])){
      ov = recode$old_var
      ifact = is.factor(data[1, get(recode$old_var)])
      #Extract values and labels pair
      if(ifact){
        old_labs = unique(data[, .(value = methods::as(get(ov), valclass),
                                 label = as.character(get(ov)),
                                                type = 'old')])
      }else {
        old_labs = data.table(value = labelled::val_labels(data[1, get(recode$old_var)]),
                              label = names(labelled::val_labels(data[1, get(recode$old_var)])),
                              type = 'old')
      }
    }else{
      old_labs = data.table()
    }
    #identify duplicates in labeling. Use new unless its NA and old is not
    labs = unique(rbind(old_labs, new_labs))

    if(nrow(labs)>0){

      badlabs = labs[type == 'new', .N, by = 'value'][N>1, value]
      if(length(badlabs) > 0 ){
        stop(paste('The following values are mapped to more than new label:', paste0(badlabs, collapse = ' | ')))
      }

      labs = dcast(labs, value ~ type, value.var = 'label')
      labs[, label := new]

      #only attempt to carry over old labels if replacing an existing variable
      if(nrow(old_labs)>0 & (recode$new_var == recode$old_var)) labs[is.na(label), label:= old]

      #if the labels are all NA, replace with value
      if(nrow(labs) == nrow(labs[is.na(label)])){
        labs[,label:= value]
      }

      #keep only labels that are represented in the data
      labs = labs[value %in% unique(ret), ]
      setorder(labs, value)
      #throw an error if the labels are mapping two numbers to the same value
      #if(any())

      #drop value:label pair of NA:NA
      labs = labs[!(is.na(value) & is.na(label)),]

      doublabs = labs[, .N, by = 'label'][N>1,label]
      if(length(doublabs) > 0){
        stop(paste('The following pairs of values:labels indicate duplicate mappings.',
                   'This is likely caused by a partial overwrite of a variable (e.g. old_var == new_var).',
                   paste(labs[label %in% doublabs, paste0(value, ':', label)], collapse = ", ")))
      }

      #if a numeric with labels, transfer to numeric. Otherwise leave it as numeric, character or whatever.
      if(is.numeric(ret)){
        new_labs = labs[,value]
        attr(new_labs, 'levels') = NULL #not sure why its sneaking through but whatever R
        names(new_labs) = labs[, label]
        ret = labelled::labelled(ret, new_labs)
      }
    }
  }
  #prepare output
  if(jump_scope){
    data[, (recode$new_var) := ret]
    return(invisible(data))
  }else{
    if(!return_vector){
      ret = data.table(a = ret)
      setnames(ret, recode$new_var)
    }
    return(ret)
  }
}
