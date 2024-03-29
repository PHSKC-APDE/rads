#' Apply a set of recode instructions to a dataset
#'
#' @param data data.frame (or something that inherits from a data frame)
#' @param ... objects coercible to a list of `recode_instructions`
#' @param ignore_case logical. should the case of names(data) be ignored?
#' @param copy logical. If false and data is a data.table object, the function omits a copy step and will alter the underlying data.table by reference
#'
#' @importFrom data.table is.data.table ":=" set setnames
#'
#' @export
#'
enact_recodes = function(data, ..., ignore_case = TRUE, copy = TRUE){
  # Global variables used by data.table declared as NULL here to play nice with devtools::check()
    blankblank <- NULL

  stopifnot(inherits(data, 'data.frame'))

  isDT = data.table::is.data.table(data)

  #copy data here so the scope is protected
  if(copy || !isDT) data = as.data.table(data)

  psuedo_blankblank = !('blankblank' %in% names(data))
  if(psuedo_blankblank){
    data[, blankblank := NA] #for tricksy recodes
  }

  #check text case
  if((length(unique(names(data))) != length(unique(tolower(names(data))))) & ignore_case){
    stop('Variable names in data are not unique after setting everything to lower case. Fix or run again with ignore_case = FALSE')
  }

  #create a list of recodes
  dots = list(...)

  #check the dots
  classy = vapply(dots, function(x) inherits(x, 'list') || inherits(x, 'recode_instruction'), TRUE)
  if(any(!classy)){
    stop('At least one item passed through ... is not a list or a recode_instruction object')
  }

  #Unlist 1 level if necessary
  list_idx = vapply(dots, function(x) inherits(x, 'list'), TRUE)
  ri_idx = vapply(dots, function(x) inherits(x, 'recode_instruction'), TRUE)

  #If there is a mixture of lists and recode instructions, make them to a single depth list
  #Otherwise, if they are all lists, take off a level\
  #other otherwise, keep as a list of recode instructions
  if(!all(ri_idx)){
    dots = append(dots[ri_idx], unlist(dots[list_idx & !ri_idx], recursive = F))
  }else if(all(list_idx)){
    dots = unlist(dots, recursive = F)
  }

  classy = vapply(dots, function(x) inherits(x, 'recode_instruction'), TRUE)

  if(!all(classy)){
    stop('At least one item passed through ... cannot be converted into a recode_instruction object')
  }

  if(ignore_case){
    #dots
    for(i in seq(dots)){
      dots[[i]][['old_var']] = tolower(dots[[i]][['old_var']])
      dots[[i]][['new_var']] = tolower(dots[[i]][['new_var']])
    }

    old_names = names(data)[]
    new_names = tolower(names(data))
    setnames(data, new_names)
  }

  for(dot in dots){


  val = tryCatch({
          #The code
          if(isTRUE(all.equal(dot$old,dot$new)) && (length(dot$new_label) <= 1) && (is.na(dot$new_label) || is.null(dot$new_label))){
            val = data[, get(dot$old_var)]
          }else{
            do_recode(data[, get(dot$old_var)], dot$old, dot$new, dot$new_label, update = dot$old_var == dot$new_var, verbose = FALSE)
          }
        },
        #if an error
         error = function(x){
           message(paste(dot$old_var, '->', dot$new_var))
           stop(x)
         },
        #if a warning
         warning = function(x){
           message(paste(paste0(dot$old_var, ' -> ', dot$new_var), '|', x))
        }
      ) #close try catch


    data.table::set(data, NULL, dot$new_var, val)


  }

  #Clean up
  if(ignore_case){
    data.table::setnames(data, new_names, old_names)
  }
  if(psuedo_blankblank){
    data[, blankblank := NULL]
  }

  #return the results
  return(data)

}
