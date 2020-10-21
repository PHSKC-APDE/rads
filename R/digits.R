#' Round data according to APDE/CHI standards or custom requests
#'
#' #' @description
#' Default rounding is according to APDE/CHI standards
#'
#' By default, this function expects data that has already been formatted for CHI,
#' i.e., data containing result, lower_bound, upper_bound, se, rse. However, the variables
#' and the number of digits to round can be readily specified.
#'
#'
#' @param digit_data a data.table or data.frame. Must contain the data to be rounded.
#' @param vars_1 character vector of indeterminate length. Specifies the variables to be rounded
#' to the number of digits specified by digits_1. If not specified it defaults to c("result", "lower_bound", "upper_bound", "rse")
#' @param digits_1 integer representing the number of decimal places to round variables specified in vars_1.
#' The default digits_1 = 3, i.e., 0.123456 >> 0.123.
#' @param vars_2 character vector of indeterminate length. Specifies the variables to be rounded
#' to the number of digits specified by digits_2. If not specified it defaults to c("se")
#' @param digits_2 integer representing the number of decimal places to round variables specified in vars_2.
#' The default digits_2 = 4 i.e., 0.123456 >> 0.1234.
#'
#' @return a data.table with appropriate rounding.
#'
#' @export
#'
#' @keywords rounding,
#'
#' @importFrom data.table setDT ":=" .SD data.table
#'
#'
#' @examples
#'
#'  set.seed(98104)
#'  dt <- data.table::data.table(
#'    chi_year = rep(2000:2019),
#'    chi_sex = factor(sample(c("Male", "Female"), 20, rep = TRUE, prob = c(0.5, 0.5))),
#'    result = rnorm(20, .75, 0.025),
#'    se = rnorm(20, 0.0258787654654, 0.00001)
#'  )
#'  head(dt) # before
#'  digits(digit_data = dt,
#'         vars_1 = c("result"),
#'         digits_1 = 2,
#'         vars_2 = c("se"),
#'         digits_2 = 5)
#'   head(dt) # after
#'

digits <- function(digit_data = NULL,
                   vars_1 = c("result", "lower_bound", "upper_bound", "rse"),
                   digits_1 = 3,
                   vars_2 = c("se"),
                   digits_2 = 4){

  ## Global variables used by data.table declared as NULL here to play nice with devtools::check()

  ## VALIDATION ----
      #validate 'digit_data'
          if(is.null(digit_data)){
            stop("You must specify a dataset (i.e., 'digit_data' must be defined)")
          }

          if(is.data.frame(digit_data)){
            data.table::setDT(digit_data)
          } else {
            stop("<digit_data> must be the name of a data.frame or data.table.")}

      #validate vars_1
          if(!is.null(vars_1) & !is.character(vars_1) ){
            stop("<vars_1> should indicate a variable or variables that you wish to round.
                            It must be a character vector (e.g., 'mean' or c('mean', 'lower_bound', 'upper_bound')).")
          }
          if(!is.null(vars_1) & sum(vars_1 %in% names(digit_data)) < length(vars_1) ){
            stop(paste0("<vars_1> contains at least one variable name that is not in the dataset specified by <digit_data>: ",
                        setdiff(names(digit_data), vars_1), ". Please revise <vars_1> and try running the function again"))
          }
          if(sum(vapply(digit_data[, .SD, .SDcols = vars_1], is.numeric, FUN.VALUE=logical(1))) < length(vars_1) ){
            stop("<vars_1> contains at least one variable which is not a numeric.
                 Please revise <vars_1> and try running the function again")
          }

      #validate vars_2
          if(!is.null(vars_2) & !is.character(vars_2) ){
            stop("<vars_2> should indocate a variable or variables that you wish to round.
                                It must be a character vector (e.g., 'mean' or c('mean', 'lower_bound', 'upper_bound')).
                 If you do not wish to round a second set of variables, set vars_2 <- NULL")
          }
          if(!is.null(vars_2) &  sum(vars_2 %in% names(digit_data)) < length(vars_2) ){
            stop("<vars_2> contains at least one variable name that is not in the dataset specified by <digit_data>: ",
                 setdiff(names(digit_data), vars_1), "Please revise <vars_2> and try running the function again")
          }
          if(!is.null(vars_2)){
            if(sum(vapply(digit_data[, .SD, .SDcols = vars_2], is.numeric, FUN.VALUE=logical(1))) < length(vars_2) ){
            stop("<vars_2> contains at least one variable which is not a numeric.
                 Please revise <vars_2> and try running the function again")
            }
          }
          if(!is.null(vars_1) & !is.null(vars_2) & sum(vars_1 %in% vars_2) > 0){
            stop("<vars_1> & <vars_2> have at least one variable in common.
                             Please remove the duplicate and try running the function again.")
          }

      #validate digits_1
          if(!is.null(digits_1) & all.equal(digits_1, as.integer(digits_1))[1] != T){
            stop("<digits_1> indicates the number of places that you would like to round the vars specified in <vars_1>.
                                It must be a single integer (e.g., 2).")
          }

      #validate digits_2
          if(!is.null(digits_2) & all.equal(digits_2, as.integer(digits_2))[1] != T){
            stop("<digits_2> indicates the number of places that you would like to round the vars specified in <vars_2>.
                            It must be a single integer (e.g., 2).")
          }

  ## ROUNDING ----
      #identify defaults if not specified in arguments
          if(is.null(vars_1) & is.null(vars_2)){
            vars_1 <- c("result", "lower_bound", "upper_bound", "rse")
            vars_1 <- vars_1[vars_1 %in% names(digit_data)]
            if(length(vars_1) < 1){
              stop("<vars_1> was not specified and the default variables do not exist in the specified dataset.
                   Please select at least one variable for rounding")
            }
            if("se" %in% names(digit_data) ){
              vars_2 <- c("se")
            }
          }

          if(is.null(digits_1)){
            digits_1 = 3}

          if(is.null(digits_2)){
            digits_2 = 4}

      #rounding numbers <1
        digit_data[, (vars_1) := lapply(.SD, round2, digits_1), .SDcols = vars_1]
        if(!is.null(vars_2)){ digit_data[, (vars_2) := lapply(.SD, round2, digits_2), .SDcols = vars_2] }


  ## RETURN RESULTS ----
    return(digit_data)

} # close digits function
