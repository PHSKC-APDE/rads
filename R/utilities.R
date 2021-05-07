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


#' List of available metrics
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
#' @param linkage.vars Character vector of length 1. It is the name of the column that you will use for merging.
#'
#' @importFrom data.table setnames ":=" setDT
#'
#' @export
#' @return data.table comprised of the original data.table and two additional columns ... 'significance' and 'comparison_with_kc' (or alternatively specified name)
chi_compare_kc <- function(orig,
                           linkage.vars = c("indicator_key"),
                           new.col.name = "comparison_with_kc"){

  #Bindings for data.table/check global variables
  cat1 <- cat1_varname <- result <- comp.result <- lower_bound <- comp.upper_bound <- upper_bound <- comp.lower_bound <- significance <- tab <- ..comparator.vars <- NULL

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
    factor.columns <- which(vapply(dat,is.factor, FUN.VALUE=logical(1) )) # identify factor columns
    if(length(factor.columns)>0) {
      dat[, (factor.columns) := lapply(.SD, as.character), .SDcols = factor.columns] # convert factor to string
    }
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
    ## Global variables used by data.table declared as NULL here to play nice with devtools::check()
      DF.class <- NULL

    # Check that DT is a data.frame/data.table
      if(is.data.frame(DF) == FALSE){
        stop("'DF' must be a data.frame or a data.table")
      }else{DF <- data.table::setDT(copy(DF))}

    # identify proper classes from YAML file ----
      class.compare <- data.table::data.table(
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
        yaml.name <- class.compare[DF.class != yaml.class]$name
        yaml.class <- class.compare[DF.class != yaml.class]$yaml.class
        class.problems <- paste(paste0(yaml.name, " (", yaml.class, ")"), collapse = ", ")
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

    #Bindings for data.table/check global variables
  indicator_key <- result_type <- relative.diff <- result.x <- result.y <- absolute.diff <- cat1 <- tab <- ..comparator.vars <-  NULL

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
        NEW <- merge(NEW, META[, list(indicator_key, result_type)], by = "indicator_key", all.x = TRUE, all.y = FALSE)
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


#' Return vector of all reference populations available in RADS
#'
#' @return Character vector of available reference populations
#' @export
#' @name list_ref_pop
#' @examples
#' \dontrun{
#'  list_ref_pop()
#' }
#' @importFrom data.table fread
list_ref_pop <- function(){
  ref_single_to_99 <- data.table::fread("https://raw.githubusercontent.com/PHSKC-APDE/reference-data/master/population_data/reference_pop_single_age_to_99.csv", showProgress=FALSE)
  ref_single_to_84 <- data.table::fread("https://raw.githubusercontent.com/PHSKC-APDE/reference-data/master/population_data/reference_pop_single_age_to_84.csv", showProgress=FALSE)
  ref_agecat_18 <- data.table::fread("https://raw.githubusercontent.com/PHSKC-APDE/reference-data/master/population_data/reference_pop_18_age_groups.csv", showProgress=FALSE)
  ref_agecat_19 <- data.table::fread("https://raw.githubusercontent.com/PHSKC-APDE/reference-data/master/population_data/reference_pop_19_age_groups.csv", showProgress=FALSE)
  ref_pop_table <- unique(rbind(ref_single_to_99[, .(standard)], ref_single_to_84[, .(standard)], ref_agecat_18[, .(standard)], ref_agecat_19[, .(standard)]))
  return(ref_pop_table$standard)
}

#' Load a reference population as a data.table object in memory
#'
#' @param ref_name Character vector of length 1. Loads a reference population identified by list_ref_pop()
#'
#' @return data.table with complete reference population data
#' @export
#' @name get_ref_pop
#' @examples
#' \dontrun{
#'  get_ref_pop("2000 U.S. Std Population (single ages to 84 - Census P25-1130)")
#' }
#' @importFrom data.table fread
get_ref_pop <- function(ref_name = NULL){
  ref_single_to_99 <- data.table::fread("https://raw.githubusercontent.com/PHSKC-APDE/reference-data/master/population_data/reference_pop_single_age_to_99.csv", showProgress=FALSE)
  ref_single_to_84 <- data.table::fread("https://raw.githubusercontent.com/PHSKC-APDE/reference-data/master/population_data/reference_pop_single_age_to_84.csv", showProgress=FALSE)
  ref_agecat_18 <- data.table::fread("https://raw.githubusercontent.com/PHSKC-APDE/reference-data/master/population_data/reference_pop_18_age_groups.csv", showProgress=FALSE)
  ref_agecat_19 <- data.table::fread("https://raw.githubusercontent.com/PHSKC-APDE/reference-data/master/population_data/reference_pop_19_age_groups.csv", showProgress=FALSE)
  ref_pop_table <- rbind(ref_single_to_99, ref_single_to_84, ref_agecat_18, ref_agecat_19)
  ref_pop_table <- ref_pop_table[standard == ref_name, .(agecat, age_start, age_end, pop)]
  if(nrow(ref_pop_table) == 0){stop(strwrap(paste0("`ref_name` ('", ref_name, "') does not refer to a valid standard reference population.
                                                     Type `list_ref_pop()` to get a list of all valid populations."), prefix = " ", initial = ""))}
  return(ref_pop_table)
}

#' Calculate crude and directly adjusted rates
#'
#' @param count Numeric vector of indeterminate length. The # of events of interest (e.g., deaths, births, etc.)
#' @param pop Numeric vector of indeterminate length. The population denominator for the count.
#' @param stdpop Numeric vector of indeterminate length. The reference standard population corresponding to the population.
#' @param per Integer vector of length 1. A multiplier for all rates and CI, e.g., when per = 1000, the rates are per 1000 people
#' @param conf.level A numeric vector of length 1. The confidence interval used in the calculations.
#'
#' @return a labeled numeric vector of the count, rate, and adjusted rate with the CI
#' @export
#' @name adjust_direct
#' @examples
#' \dontrun{
#'  adjust_direct(count = c(11, 9), pop = c(500, 500), stdpop = c(640, 720), per = 100, conf.level = 0.95)[]
#' }
#'
adjust_direct <- function (count, pop, stdpop, per = 100000, conf.level = 0.95)
{
  # adapted from epitools v0.5-10.1 :: ageadjust.direct & survival 3.2-7 :: cipoisson

  # logic checks ----
  if((length(count)==length(pop) & length(pop)==length(stdpop)) != T){stop("The length of `count`, `pop`, and `stdpop` must be equal.")}
  if( !class(per) %in% c("numeric", "integer")){stop(paste0("The `per` argument ('", per, "') you entered is invalid. It must be a positive integer, e.g., 100000."))}
  if( per%%1 != 0 | (per%%1 == 0 & per <= 0)){stop(paste0("The `per` argument (", per, ") you entered is invalid. It must be a positive integer, e.g., 100000."))}
  if( !class(conf.level) %in% c("numeric")){stop(paste0("`conf.level` (", conf.level, ") should be a two digit decimal between 0.00 & 0.99"))}
  if( (100*conf.level)%% 1 != 0 | !(0<=conf.level & conf.level<=0.99)){stop(paste0("`conf.level` (", conf.level, ") should be a two digit decimal between 0.00 & 0.99"))}

  # basic calculations ----
  rate <- count/pop
  alpha <- 1 - conf.level
  cruderate <- sum(count)/sum(pop)
  stdwt <- stdpop/sum(stdpop)

  # calc exact poisson CI for crude rates ----
  dummycount <- ifelse(sum(count) == 0, 1, sum(count))
  crude.lci <- ifelse(sum(count) == 0, 0, qgamma(alpha/2, dummycount)) / sum(pop)
  crude.uci <- qgamma(1 - alpha/2, sum(count) + 1) / sum(pop)

  # calc exact CI for adjusted rates ----
  dsr <- sum(stdwt * rate)
  dsr.var <- sum((stdwt^2) * (count/pop^2))
  wm <- max(stdwt/pop)
  gamma.lci <- qgamma(alpha/2, shape = (dsr^2)/dsr.var, scale = dsr.var/dsr)
  gamma.uci <- qgamma(1 - alpha/2, shape = ((dsr + wm)^2)/(dsr.var +
                                                             wm^2), scale = (dsr.var + wm^2)/(dsr + wm))
  # prep output ----
  adjusted <- per*c(crude.rate = cruderate, crude.lci = crude.lci, crude.uci = crude.uci, adj.rate = dsr, adj.lci = gamma.lci, adj.uci = gamma.uci)
  adjusted <- c(count = sum(count), adjusted)
}

#' Calculate age standardized rates from a data.table with age, counts, and population columns. (Built on adjust_direct())
#' @param my.dt Name of a data.frame or data.table object. Note, if my.dt already has a standard population (ref.popname = "none"),
#' it must contain a numeric column named 'stdpop'. Otherwise, it must contain a numeric column named 'age'.
#' @param ref.popname Character vector of length 1. Only valid options are those in list_ref_pop() and
#' "none" (when standard population already exists in my.dt)
#' @param collapse Logical vector of length 1. Do you want to collapse my.dt ages to match those in ref.popname?
#' @param my.count Character vector of length 1. Identifies the column with the count data aggregated by the given demographics.
#' @param my.pop Character vector of length 1. Identifies the column with the population corresponding to the given demographics.
#' @param per Integer vector of length 1. A multiplier for all rates and CI, e.g., when per = 1000, the rates are per 1000 people
#' @param conf.level A numeric vector of length 1. The confidence interval used in the calculations.
#' @param group_by Character vector of indeterminate length. By which variable(s) do you want to stratify the rate results, if any?
#'
#' @return a data.table of the count, rate & adjusted rate with CIs, name of the reference population and the 'group_by' variable(s) -- if any
#' @export
#' @name age_standardize
#' @examples
#' \dontrun{
#' temp1 <- data.table(age = c(50:60), count = c(25:35), pop = c(seq(1000, 900, -10)) )
#' age_standardize(my.dt = temp1, ref.popname = "2000 U.S. Std Population (18 age groups - Census P25-1130)", collapse = T, my.count = "count", my.pop = "pop", per = 1000, conf.level = 0.95)[]
#' age_standardize(my.dt = temp1, ref.popname = "2000 U.S. Std Population (18 age groups - Census P25-1130)", collapse = T, my.count = "count", my.pop = "pop", per = 1000, conf.level = 0.95, group_by = "agecat")[]
#'
#' temp2 <- data.table(sex = c(rep("M", 11), rep("F", 11)), age = rep(50:60, 2), count = c(25:35, 26:36), pop = c(seq(1000, 900, -10), seq(1100, 1000, -10)), stdpop = rep(1000, 22))
#' age_standardize(my.dt = temp2, ref.popname = "none", collapse = F, my.count = "count", my.pop = "pop", per = 1000, conf.level = 0.95, group_by = "sex")[]
#' }
#' @importFrom data.table ":=" setDT

age_standardize <- function (my.dt, ref.popname = NULL, collapse = T, my.count = "count", my.pop = "pop", per = 100000, conf.level = 0.95, group_by = NULL)
{
  my.dt.name <- deparse(substitute(my.dt))
  my.dt <- copy(my.dt)
  # Logic checks ----
  # Check that my.dt is a data.frame or data.table ----
  if( inherits(my.dt, "data.frame") == FALSE){stop("my.dt must be a data.frame or a data.table containing both counts and population data.")}
  if( inherits(my.dt, "data.table") == FALSE){setDT(my.dt)}

  # Check arguments needed for adjust_direct ----
  if(! my.count %in% colnames(my.dt)){stop(strwrap(paste0("The column '", my.count, "' does not exist in my.dt.
                                                                my.dt must have a column indicating the count of events (e.g., deaths, births, etc.) and is typically named 'count'.
                                                                If such a column exists with a different name, you need to specify it in the `my.count` argument. I.e., my.count = 'count.varname'."), prefix = " ", initial = ""))}

  if(! my.pop %in% colnames(my.dt)){stop(strwrap(paste0("The column '", my.pop, "' does not exist in my.dt.
                                                                my.dt must have a column for the population denominator correspondnig to the given demographics. It is typically named 'pop'.
                                                                If such a column exists with a different name, you need to specify it in the `my.pop` argument. I.e., my.pop = 'pop.varname'."), prefix = " ", initial = ""))}

  # Ensure the reference population exists ----
  if(is.null(ref.popname)){ref.popname <- "2000 U.S. Std Population (18 age groups - Census P25-1130)"}
  if(! ref.popname %in% c( list_ref_pop(), "none")){
    stop(strwrap(paste0("ref.popname ('", ref.popname, "') is not a valid reference population name.
          The names of standardized reference populations can be viewed by typing `list_ref_pop()`.
          If my.dt is already aggregated/collapsed and has a relevant 'stdpop' column, please set ref.popname = 'none'"), prefix = " ", initial = ""))}

  if(ref.popname == "none" & !"stdpop" %in% colnames(my.dt)){stop("When specifying ref.popname = 'none', my.dt must have a column named 'stdpop' with the reference standard population data.")}

  if(ref.popname == "none" & collapse == T){stop(strwrap("When ref.popname = 'none', collapse should equal F.
                                                                  Selecting ref.popname = 'none' expects that my.dt has already been collapsed/aggregated and has a 'stdpop' column."), prefix = " ", initial = "")}

  # Standardize column names ----
  # purposefully did not use setnames() because it is possible that count | pop already exists and are intentionally using different columns for this function
  my.dt[, "count" := get(my.count)]
  my.dt[, "pop" := get(my.pop)]

  # Collapse my.dt to match standard population bins ----
  if(collapse==T){
    if(! "age" %in% colnames(my.dt)){stop(strwrap("When collapse = T, my.dt must have a column named 'age' where age is an integer.
                                                        This is necessary to generate age bins that align with the selected standard
                                                        reference population. If my.dt already has an 'agecat' column that is formatted
                                                        identically to that in the standard reference population, set collapse = F"), prefix = " ", initial = "")}
    if(is.numeric(my.dt$age) == F){stop("When collapse = T, the 'age' column must be comprised entirely of integers")}
    if(sum(as.numeric(my.dt$age) %% 1) != 0){stop("When collapse = T, the 'age' column must be comprised entirely of integers")}
    if("agecat" %in% colnames(my.dt)){stop(strwrap("When collapse = T, a new column named 'agecat' is created to match that in the standard reference population.
                                                  my.dt already has a column named 'agecat' and it will not be automatically overwritten.
                                                  If you are sure you want to create a new column named 'agecat', delete the existing column in my.dt and run again."),
                                           prefix = " ", initial = "")}
    my.ref.pop <- get_ref_pop(ref.popname)
    for(z in seq(1, nrow(my.ref.pop))){
      my.dt[age %in% my.ref.pop[z, age_start]:my.ref.pop[z, age_end], agecat := my.ref.pop[z, agecat]]
    }
    my.dt <- my.dt[, .(count = sum(count), pop = sum(pop)), by = "agecat"]
  }

  # Merge standard pop onto count data ----
  if(ref.popname != "none"){
    my.dt <- merge(my.dt, get_ref_pop(ref.popname)[, .(agecat, stdpop = pop)], by = "agecat")
  }

  # Calculate crude & adjusted rates with CI ----
  if(!is.null(group_by)){my.rates <- my.dt[, as.list(adjust_direct(count = count, pop = pop, stdpop = stdpop, conf.level = as.numeric(conf.level), per = per)), by = group_by]}
  if( is.null(group_by)){my.rates <- my.dt[, as.list(adjust_direct(count = count, pop = pop, stdpop = stdpop, conf.level = as.numeric(conf.level), per = per))]}

  # Tidy results ----
  rate_estimates <- c("crude.rate", "crude.lci", "crude.uci", "adj.rate", "adj.lci", "adj.uci")
  my.rates[, c(rate_estimates) := lapply(.SD, rads::round2, 2), .SDcols = rate_estimates]
  my.rates[, reference_pop := ref.popname]
  if(ref.popname == "none"){my.rates[, reference_pop := paste0("stdpop column in `", my.dt.name, "`")]}

  return(my.rates)
}
