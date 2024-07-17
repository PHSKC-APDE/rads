#' Check to see if a value is NA, NULL, or ""
#'
#'@param x character. Expects length of 1 when not NULL (or NA).
#'@return logical
#'
#'@examples
#' check_nan(NaN)
#' check_nan(NA)
#' check_nan(NULL)
#' check_nan('')
#' check_nan('x')
#' check_nan(10)
#'
#'@export
#'
check_nan = function(x){
  if(any(is.null(x), is.na(x))){
    return(TRUE)

  }else if(length(x) == 0){
    return(TRUE)
  }else{
    stopifnot(length(x) == 1)
    if(x == "") return(TRUE)
  }

  return(FALSE)

}
