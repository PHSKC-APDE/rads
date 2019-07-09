#' Check to see if input implies a binning recode
#'
#'@param x character. Arbitrary character string to be checked for interpretation into binning.
#'@return logical
#'
#'@export
#'
check_bin = function(x){

  stopifnot(length(x) == 1)

  opt1 = substr(x,1,1) %in% c('[','(')
  opt2 = substr(x,nchar(x),nchar(x)) %in% c(')', ']')
  opt3 = grepl(',', x, fixed = T)
  return(opt1 & opt2 & opt3)
}
