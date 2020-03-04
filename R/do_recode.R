#' Recode a vector
#' @param x some vector to be transformed
#' @param old character (or coercible). value in the x to be translated into a new value by recoding.
#' @param new character (or coercible). Column indicating where the new values for the recoding process exist. It is positionally  tied to a particular `old`.
#'                  If blank, it helps identify a simpler rename operation.
#' @param new_label character (or convertable to character). The factor label that `old` will be recoded to in conjunction with the position of `new`.
#' If NULL or all NA indicates no (new) labelling
#' @param update logical. Governs whether x is modified in place. If `x` is a factor, this will also carry forward any labels unless overwritten by new_label
#' @param verbose logical. Should warnings be displayed/provided?
#'
#' @importFrom methods as
#'
#'
#' @export
do_recode = function(x, old, new, new_label = NULL, update = FALSE, verbose = FALSE){

  #Initial checks
  stopifnot(length(new) == length(old))
  if(is.factor(old)) stop('`old` cannot be of class `factor`')
  if(is.factor(new)) stop('`new` cannot be of class `factor`')

  #check for binning
  if(is.numeric(x) && is.character(old)){
    #check if its a likely binning operation
    bin_opt = vapply(old, check_bin, TRUE)
    bin_me = any(bin_opt)
  }else{
    bin_me = FALSE
  }

  # If class mismatch (except for binning), throw an error
  if(!inherits(old,class(x)) && !is.factor(x) && !bin_me && !(is.numeric(old) && is.numeric(x))){
    stop('Class of `old` and `x` are not compatible')
  }

  if(is.factor(x) && verbose && is.numeric(old)){
    warning('x is a factor and old is numeric/integer. Recoding of factors via numerics is not advised.')
  }

  if(is.factor(x)){
    hold = unique(x)
    hold = data.table::data.table(value =  methods::as(x, class(old)), label = as.character(hold))
    x = methods::as(x, class(old))
  }

  if(update){
    ret = x
  }else{
    ret = rep(methods::as(NA, class(new)), length(x))
  }

  #change the values
  for(i in seq_along(old)){
    if(check_bin(old[i]) && bin_me){
      left = substr(old[i],1,1)
      right = substr(old[i],nchar(old[i]),nchar(old[i]))
      inner = substr(old[i], 2, nchar(old[i]) - 1)

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
        ret[which(x > minmax[1] & x< minmax[2])] = new[i]
      }else if(left == '[' & right == ')'){
        ret[which(x >= minmax[1] & x< minmax[2])] = new[i]
      }else if(left == '(' & right == ']'){
        ret[which(x > minmax[1] & x <= minmax[2])] = new[i]
      }else{
        ret[which(x >= minmax[1] & x<= minmax[2])] = new[i]
      }

    }else{
      ret[x %in% old[i]] <- new[i]
    }
  }

  #if no update, then just apply the new labels
  if(!update){
    #if the new label exists and has at least one non-na value
    if(!is.null(new_label) && !all(is.na(new_label))){
      ret = factor(ret, new[which(!is.na(new_label))], new_label[!is.na(new_label)])
    }
  }else if(!all(is.na(new_label))){
    new_labs = data.table::data.table(value = new, label = new_label)

    if(exists('hold')){
      hold = hold[!value %in% new_labs[,value]]
      if(verbose && any(hold[,label] %in% new_labs[, label])) warning('Possible multi-mapping in creating factor with update == TRUE')
      labs = rbind(new_labs, hold)

    }else{
      labs = new_labs
    }

    labs = labs[value %in% ret]

    stopifnot(nrow(labs)>0)

    ret = factor(ret, labs[,value], labs[, label])
  }

  return(ret)
}
