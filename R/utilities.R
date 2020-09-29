#' Returns the list of datasets currently available for analysis in RADS
#'
#' @return Character vector of available datasets.
#' @export
#' @name list_apde_data
#' @examples
#' \dontrun{
#'  list_apde_data()
#' }
list_apde_data <- function(){

  ret <- c('hys', 'birth', 'bsk')

  return(ret)


}

#' List columns available for analysis for a particular dataset in RADS
#'
#' @param dataset Character vector of length 1. Identifies the dataset to be fetched. Use \code{list_apde_data} for available options
#' @param analytic_only logical. Controls whether columns outside the analytic dataset should be returned.
#'
#'
#' @return Data.frame with two columns. First column is the variable name, while the second identifies whether or not it is in the analytic ready dataset
#' @export
#' @name list_dataset_columns
#' @examples
#' \dontrun{
#'  list_dataset_columns('hys', T)
#' }
list_dataset_columns <- function(dataset, analytic_only = F){
  dat = match.arg(dataset, list_apde_data())

  warning('list_dataset_columns not currently available/implemented')
  return(data.frame(variable_name = '', analytic_ready = 'Sure. Why not?'))
}


#' List ofavailable metrics
#' @return character vector. A vector of the available metrics for calculation.
#' @name metrics
NULL

#' @rdname metrics
#' @export
metrics = function(){
  #c('mean', 'se', 'lower', 'upper', 'numerator', 'denominator', 'total', 'total_se', 'missing', 'rse', 'missing.prop', 'ndistinct')
  c('total', #'total_se', 'total_lower', 'total_upper'
    'mean', 'rse', #'mean_se', 'mean_lower', 'mean_upper',
    'numerator', 'denominator', 'obs', 'median',
    'unique.time', 'ndistinct',
    'missing', 'missing.prop',
    'rate') #, 'rate_per', 'rate_se', 'rate_lower', 'rate_upper')
}



#' List of standard CHI / Tableau Ready columns
#' @export
chi_cols = function(){
  c("data_source", "indicator_key", "tab", "year", "cat1", "cat1_group", "cat1_group_alias", "cat1_varname","cat2",
    "cat2_group", "cat2_group_alias", "cat2_varname", "result", "lower_bound", "upper_bound", "se", "rse",
    "comparison_with_kc", "time_trends", "significance", "caution", "suppression", "numerator", "denominator", "chi",
    "source_date", "run_date")
}


#' List of standard CHI / Tableau Ready metadata columns
#' @export
chi_metadata_cols = function(){
  c("data_source", "indicator_key", "result_type", "valence", "latest_year", "latest_year_result", "latest_year_kc_pop",
    "latest_year_count", "map_type", "unit", "valid_years", "chi", "run_date")
}


#' Improved rounding function
#' @param x values to be rounded
#' @param n number of digits
#' @export
#' @return numeric
round2 = function(x, n = 0) {
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5
  z = trunc(z)
  z = z/10^n
  z*posneg
}



#' Substring selection from the right to complement base R substr
#' @param x character
#' @param x.start digit to start (counting from the right)
#' @param x.stop digit to end  (counting from the right)
#' @export
#' @return character vector
#'
#' @examples
#' \dontrun{
#' substrRight("Good morning!", 2, 8)
#' }
substrRight <- function(x, x.start, x.stop){
  substr(x, nchar(x)-x.stop+1, nchar(x)-x.start+1)
}

#' Compare CHI standard tabular results to the King County average for the same year within a given data set
#' @param orig Character vector of length 1. Identifies the data.table/data.frame to be fetched. Note the table must have the following columns:
#' 'result', 'lower_bound', & 'upper_bound' and all three must be numeric
#' @param new.col.name Character vector of length 1. It is the name of the column containining the comparison results.
#'
#' @importFrom data.table setnames ":=" setDT
#'
#' @export
#' @return data.table comprised of the original data.table and two additional columns ... 'significance' and 'comparison_with_kc' (or alternatively specified name)
chi_compare_kc <- function(orig,
                           linkage.vars = c("indicator_key"),
                           new.col.name = "comparison_with_kc"){

  #Bindings for data.table/check global variables
  cat1_varname <- result <- comp.result <- lower_bound <- comp.upper_bound <- upper_bound <- comp.lower_bound <- significance <- NULL

  #Copy & subset comparator data
  data.table::setDT(copy(orig))

  #Copy & subset comparator data
  comparator.vars <- c(linkage.vars, "year", "result", "lower_bound", "upper_bound")
  comparator <- unique(orig[cat1=="King County" & tab!="crosstabs", ..comparator.vars])
  data.table::setnames(comparator, c("result", "lower_bound", "upper_bound"), c("comp.result", "comp.lower_bound", "comp.upper_bound"))

  #Merge comparator data onto all other data
  orig <- merge(orig, comparator, by=c(linkage.vars, "year"), all.x = TRUE, all.y = TRUE)

  #Compare estimates with comparator
  if(sum(grepl(new.col.name, names(orig))) > 0){orig[, c(new.col.name) := NULL]}
  orig[result == comp.result, c(new.col.name) := "no different"]
  orig[result > comp.result, c(new.col.name) := "higher"]
  orig[result < comp.result, c(new.col.name) := "lower"]

  #According to APDE protocol, we check for overlapping CI rather than SE and Z scores
  if(sum(grepl("significance", names(orig))) > 0){orig[, significance := NULL]}
  orig[, significance := NA_character_]
  orig[(lower_bound > comp.upper_bound) | (upper_bound < comp.lower_bound), significance := "*"]

  #Keep comparison only if statistically significant
  orig[is.na(significance), c(new.col.name) := "no different"]

  #Drop KC level estimates that were just used for the comparisons
  orig[, c("comp.result", "comp.upper_bound", "comp.lower_bound") := NULL]

  return(orig)
}

#' Format a vector of time into a series of human readable chunks
#' @param x numeric
#' @export
#' @return character vector
#'
#' @examples
#' format_time(c(1:5, 10, 12, 24, 25))
#'
format_time <- function(x){

  #get the unique values
  x <- sort(unique(x))

  #find breaks in runs
  breaks = data.table::shift(x, type = 'lead') == (x + 1)
  bps = which(!breaks)

  #seperate
  if(length(bps)>0){
    seper = split(x, cut(x, c(-Inf, x[bps], Inf)))
  }else{
    seper = list(x)
  }

  #format into string
  seper = lapply(seper, function(y){

    if(length(y)>1){
      return(paste(min(y), max(y), sep = '-'))
    }else{
      return(paste(y))
    }

  })

  ret = paste(seper, collapse = ', ')

  return(ret)

}

#' Format a vector of time into a simple human readable chunk
#' @param x numeric
#' @export
#' @return character vector
#'
#' @examples
#' format_time_simple(c(1:5, 10, 12, 24, 25))
#'
format_time_simple <- function(x){

  #get the unique values
    x <- sort(unique(x))

  # format into string
    if(max(x, na.rm = T) == min(x, na.rm = T)){
      ret <- paste0(x)
    } else{
      ret <- paste0(min(x, na.rm = T), "-", max(x, na.rm = T))
    }

  return(ret)

}


#' Clean string columns read from SQL
#' @param dat character vector of length one. Name of data.frame or data.table
#' @param stringsAsFactors logical. Specifies whether to convert strings to factors (TRUE) or not (FALSE)
#' @export
#' @return data.table
  sql_clean <- function(dat = NULL, stringsAsFactors = FALSE){

    # check date.frame
      if(!is.null(dat)){
        if(!is.data.frame(dat)){
          stop("'dat' must be the name of a data.frame or data.table")
        }
        if(is.data.frame(dat) && !data.table::is.data.table(dat)){
          data.table::setDT(dat)
        }
      } else {stop("'dat' (the name of a data.frame or data.table) must be specified")}

    original.order <- names(dat)
    string.columns <- which(vapply(dat,is.character, FUN.VALUE=logical(1) )) # identify string columns
    if(length(string.columns)>0) {
      dat[, (string.columns) := lapply(.SD, trimws, which="both"), .SDcols = string.columns] # trim white space to right or left
      dat[, (string.columns) := lapply(.SD, function(x){gsub("^$|^ $", NA, x)}), .SDcols = string.columns] # replace blanks with NA
      if(stringsAsFactors==TRUE){
        dat <- dat[, (string.columns) := lapply(.SD, factor), .SDcols = string.columns] # convert strings to factors
      }
    }
    # reorder table
    data.table::setcolorder(dat, original.order)
  }

#' Convert the class of a vector to another class is possible without introducing additional NAs
#' @param x vector of indeterminate length and type
#' @param class character vector of length one specifying the preferred new column type (i.e.,
#' 'character', 'numeric', 'integer', or 'factor')
#' @export
#' @return a vector of the same length as x, but of the new class (when possible)
  lossless_convert <- function(x = NULL, class = NULL){
    if(is.null(x)){
      stop("'x', the vector you wish to change, must be specified.")
    }

    if(is.null(class)){
      stop("'class' must be specified by choosing one of the following: 'character', 'integer', 'numeric'")
    }

    if(!class %in% c('character', 'integer', 'numeric') || length(class) != 1 ){
      stop("'class' is limited to *one* of the following: 'character', 'integer', 'numeric'")
    }

    if(sum(is.na(x)) == sum(is.na(suppressWarnings(as(x, class)))) ){
      x <- suppressWarnings(as(x, class))
    }
    return(x)
  }


#' Compare the expected column types in YAML with the actual column types in R
#' @param DF Character vector of length 1. Identifies the data.table/data.frame that you want to assess vis-à-vis the YAML file
#'
#' @param YML Character vector of length 1. It is the name of the YAML object in memory.
#'
#' @param VARS Character vector of length 1. Is is the name of the object in the list contained by YML
#'
#' @importFrom data.table data.table setnames ":=" setDT
#'
#' @export
#' @return A simple printed statement, either identifying incompatible column types or a statement of success

  validate_yaml_data <- function(DF = NULL, YML = NULL, VARS = NULL){

    # Check that DT is a data.frame/data.table
      if(is.data.frame(DF) == FALSE){
        stop("'DF' must be a data.frame or a data.table")
      }else{DF <- data.table::setDT(copy(DF))}

    # identify proper classes from YAML file ----
      class.compare <- data.table(
        name =  c(names(YML[[VARS]])),
        yaml.class = tolower(as.character(YML[[VARS]]))
      )

    # convert names of SQL data types to R classes ----
      class.compare[grepl("varchar", tolower(yaml.class)), yaml.class := "character"]
      class.compare[grepl("int", tolower(yaml.class)), yaml.class := "integer"]
      class.compare[grepl("float", tolower(yaml.class)), yaml.class := "numeric"]

    # identify which VARS should be of which class (assuming YAML is correct) ----
      make.char <- class.compare[yaml.class == "character"]$name
      make.num  <- class.compare[yaml.class == "numeric"]$name
      make.int  <- class.compare[yaml.class == "integer"]$name

    # create function to convert column classes if it can be done without introducing NA's ----
      lossless_convert <- function(x, class){
        if(sum(is.na(x)) == sum(is.na(suppressWarnings(as(x, class)))) ){
          x <- suppressWarnings(as(x, class))
        }
        return(x)
      }

    # use function convert R column types if possible / needed ----
      DF[, (make.char) := lapply(.SD, lossless_convert, class = 'character'), .SDcols = make.char]
      DF[, (make.num) := lapply(.SD, lossless_convert, class = 'numeric'), .SDcols = make.num]
      DF[, (make.int) := lapply(.SD, lossless_convert, class = 'integer'), .SDcols = make.int]

    # check if there are variables that could not be coerced to proper type ----
      class.compare <- merge(data.table::data.table(name = names(sapply(DF, class)), DF.class = tolower(sapply(DF, class))),
                             class.compare, by = "name")

    # allow R to be more strict than YAML with numbers ----
      class.compare[yaml.class=="numeric" & DF.class == "integer", DF.class := "numeric"]

    # Assess whether there were any problems ----
      if(nrow(class.compare[DF.class != yaml.class]) > 0){
        VARS <- class.compare[DF.class != yaml.class]$VARS
        yaml.class <- class.compare[DF.class != yaml.class]$yaml.class
        class.problems <- paste(paste0(VARS, " (", yaml.class, ")"), collapse = ", ")
        stop(glue::glue("The following variables could not be coerced to their proper class (which is specified in parentheses):
                              {class.problems}"))
      }else{success <- print(glue::glue("All column classes in your R dataset are compatible with the YAML reference standard."))}

      return(success)

  }


#' Compare two data.frames with properly formatted CHI data
#' @param OLD Character vector of length 1. Identifies the data.table/data.frame that you want to use as a reference
#'
#' @param NEW Character vector of length 1. Identifies the data.table/data.frame that you are interested in validating
#'
#' @param OLD.year Character vector of length 1. Specifies the exact year that you want to use in the OLD data
#'
#' @param NEW.year Character vector of length 1. Specifies the exact year that you want to use in the NEW data
#'
#' @param META Character vector of length 1. OPTIONAL ... identifies the data.table/data.frame containing the metadata for
#' the NEW data.frame
#'
#' @importFrom data.table data.table setnames ":=" setDT copy
#'
#' @export
#' @return A simple printed statement, either identifying incompatible column types or a statement of success
chi_compare_est <- function(OLD = NULL, NEW = NULL, OLD.year = NULL, NEW.year = NULL, META = NULL){

    # Check if necessary arguments are present
  if(is.null(OLD)){stop("You must provide 'OLD', i.e., the name of the table with the OLD data")}
  if(is.null(NEW)){stop("You must provide 'NEW', i.e., the name of the table with the NEW data")}
  if(is.null(OLD.year)){stop("You must provide 'OLD.year', i.e., the year of interest in the OLD data")}
  if(is.null(NEW.year)){stop("You must provide 'NEW.year', i.e., the year of interest in the NEW data")}

    # Check if objects are data.frames & make into data.table if need be
      if(is.data.frame(OLD) == FALSE){
        stop("'OLD' must be a data.frame or a data.table")
      }else{OLD <- data.table::setDT(copy(OLD))}


      if(is.data.frame(NEW) == FALSE){
        stop("'NEW' must be a data.frame or a data.table")
      }else{NEW <- data.table::setDT(copy(NEW))}

      if(!is.null(META)){
        if(is.data.frame(META) == FALSE){
          stop("'META' must be a data.frame or a data.table")
        }else{META <- data.table::setDT(copy(META))}
      }

    # If metadata provided, add it to the columns to help interret the output
      if(!is.null(META)){
        NEW <- merge(NEW, META[, .(indicator_key, result_type)], by = "indicator_key", all.x = TRUE, all.y = FALSE)
      } else { NEW[, result_type := "Metadata not provided"]}

    # Merge old and new data based on identifiers
      comp <- merge(copy(OLD[year == OLD.year]),
                    copy(NEW[year == NEW.year]),
                    by = c("indicator_key", "tab",
                           "cat1", "cat1_group_alias", "cat1_varname",
                           "cat2", "cat2_group_alias", "cat2_varname"),
                    all = T)

    # calculate percent differences between old (x) and new(y)
      comp[, relative.diff := round2(abs((result.x - result.y) / result.x)*100, 1)]
      comp[, absolute.diff := round2(abs(result.x - result.y)*100, 1)]
      comp <- comp[!is.na(absolute.diff)]  # drop if absolute difference is NA

    # order variables
      comp <- comp[, c("absolute.diff", "relative.diff", "result_type",
                       "indicator_key", "tab",
                       "cat1", "cat1_group_alias", "cat1_varname",
                       "cat2", "cat2_group_alias", "cat2_varname", "year.x", "year.y",
                       "result.x", "result.y", "lower_bound.x", "lower_bound.y",
                       "upper_bound.x", "upper_bound.y",
                       "numerator.x", "numerator.y", "denominator.x", "denominator.y",
                       "se.x", "se.y")]

    # rename suffixes
      setnames(comp, names(comp), gsub("\\.x$", ".OLD", names(comp)))
      setnames(comp, names(comp), gsub("\\.y$", ".NEW", names(comp)))

    # order based on percent difference
      setorder(comp, -absolute.diff)

    # return object
      return(comp)

}

#' Convert from one type to another type
#'
#' @param x factor
#' @param target character. class of the object to transform the factor into. One of integer, numeric, or character.
#'
#'
dumb_convert <- function(x, target = 'character'){

  stopifnot(length(target) == 1)
  if(target == 'character'){
    return(as.character(x))
  }

  if(target == 'numeric'){
    return(as.numeric(as.character(x)))
  }

  if(target == 'integer'){
    return(as.integer(as.character(x)))
  }

  if(target == 'logical') return(as.logical(as.character(x)))

  if(target == 'factor'){
    if(is.factor(x)) return(x)

    return(as.factor(x))
  }

  stop(paste0('Target class of ', target, ' is invalid'))
}
