#' suppress data
#' @param suppress logical. Select whether suppression should [T] or should not [F] be applied.
#' @param suppress_range integer vector of length 2. They specify the minimum and maximum range for suppression.
#' 
#' 
#' suppress data
#' @param suppress logical. Select whether suppression should [T] or should not [F] be applied.
#' @param suppress_range integer vector of length 2. They specify the minimum and maximum range for suppression.
#' 
#' 
#' 
suppress = T, 
suppress_range = NULL,

#validate 'suppress'
if(!is.logical(suppress)){
  stop("If specified, the 'suppress' argument must be a logical (i.e., T or F, without quotes)")
}

#validate 'suppress_range'
if(is.null(suppress_range)){suppress_range <- c(0, 10)} # fixed, but eventually dependent upon data source?

if(length(suppress_range) !=2){
  stop("If specified, the 'suppress_range' argument must be a numeric vector of length 2 (e.g., c(0, 10)")
}

if(!is.numeric(suppress_range) |
   suppress_range[1] != round2(suppress_range[1]) | 
   suppress_range[2] != round2(suppress_range[2])){
  stop("If specified, the 'suppress_range' argument must only contain non-negative integers  
             (e.g., c(0, 10)") 
} 