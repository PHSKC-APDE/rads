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
#'  adjust_direct(count = c(11, 9), pop = c(500, 500), stdpop = c(640, 720),
#'  per = 100, conf.level = 0.95)[]
#' }
#' @importFrom stats qgamma
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
  adjusted <- c(count = sum(count), pop = sum(pop), adjusted)
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
#' age_standardize(my.dt = temp1,
#' ref.popname = "2000 U.S. Std Population (18 age groups - Census P25-1130)", collapse = T,
#' my.count = "count", my.pop = "pop", per = 1000, conf.level = 0.95)[]
#'
#' temp2 <- data.table(sex = c(rep("M", 11), rep("F", 11)), age = rep(50:60, 2),
#' count = c(25:35, 26:36), pop = c(seq(1000, 900, -10), seq(1100, 1000, -10)),
#' stdpop = rep(1000, 22))
#' age_standardize(my.dt = temp2, ref.popname = "none", collapse = F, my.count = "count",
#' my.pop = "pop", per = 1000, conf.level = 0.95, group_by = "sex")[]
#' }
#' @importFrom data.table ":=" setDT

age_standardize <- function (my.dt, ref.popname = NULL, collapse = T, my.count = "count", my.pop = "pop", per = 100000, conf.level = 0.95, group_by = NULL)
{
  #global variables used by data.table declared as NULL here to play nice with devtools::check()
  my.dt.name <- age <- age_start <- age_end <- agecat <- count <- pop <- stdpop <- reference_pop <- NULL

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
    if(!is.null(group_by)){my.dt <- my.dt[, list(count = sum(count), pop = sum(pop)), by = c("agecat", group_by)]}
    if(is.null(group_by)){my.dt <- my.dt[, list(count = sum(count), pop = sum(pop)), by = "agecat"]}
  }

  # Merge standard pop onto count data ----
  if(ref.popname != "none"){
    my.dt <- merge(my.dt, get_ref_pop(ref.popname)[, list(agecat, stdpop = pop)], by = "agecat")
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


#' Proper calculation of age in years
#'
#' @param from Vector of dates or characters ("YYYY-MM-DD") of indeterminate length.  vector of length 1.
#' @param to Vector of dates or characters ("YYYY-MM-DD") of indeterminate length.  vector of length 1.
#'
#' @return Character vector of available datasets.
#' @export
#' @name calc_age
#' @examples
#' \dontrun{
#'  calc_age(from = "2000-02-29", to = "2021-07-01")
#' }
#'
calc_age <- function(from, to) {
  from_lt = as.POSIXlt(from)
  to_lt = as.POSIXlt(to)

  age = to_lt$year - from_lt$year

  ifelse(to_lt$mon < from_lt$mon |
           (to_lt$mon == from_lt$mon & to_lt$mday < from_lt$mday),
         age - 1, age)
}


#' List of standard CHI / Tableau Ready columns
#' @export
chi_cols = function(){
  c("data_source", "indicator_key", "tab", "year", "cat1", "cat1_group", "cat1_group_alias", "cat1_varname","cat2",
    "cat2_group", "cat2_group_alias", "cat2_varname", "result", "lower_bound", "upper_bound", "se", "rse",
    "comparison_with_kc", "time_trends", "significance", "caution", "suppression", "numerator", "denominator", "chi",
    "source_date", "run_date")
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
  indicator_key <- result_type <- relative.diff <- result.x <- result.y <- absolute.diff <- cat1 <- tab <-  NULL

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

  #Deprecation warning
  .Deprecated("comparison")

  #Bindings for data.table/check global variables
  cat1 <- cat1_varname <- result <- comp.result <- lower_bound <- comp.upper_bound <- upper_bound <- comp.lower_bound <- significance <- tab <- comparator_vars <- NULL

  #Copy & subset comparator data
  data.table::setDT(copy(orig))

  #Copy & subset comparator data
  comparator_vars <- c(linkage.vars, "year", "result", "lower_bound", "upper_bound")
  comparator <- unique(orig[cat1=="King County" & tab!="crosstabs", (comparator_vars), with = F])
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


#' List of standard CHI / Tableau Ready metadata columns
#' @export
chi_metadata_cols = function(){
  c("data_source", "indicator_key", "result_type", "valence", "latest_year", "latest_year_result", "latest_year_kc_pop",
    "latest_year_count", "map_type", "unit", "valid_years", "chi", "run_date")
}


#' Compare aggregated results (proportions or means) for one strata to the rest
#' of the strata in the summary table.
#' @param mydt Unquoted name of a data.table or data.frame to be processed. Note
#' the table must have the following columns: 'result'  OR 'mean' OR 'proportion',
#' and corresponding confidence interval columns with 'lower' & 'upper' as part
#' of their names.
#' @param id_vars Character vector of length >= 1. It contains the name(s) of
#' columns which identify the grouping for which you want to use for comparison.
#' For standard rads::calc() output, id_vars should be c("variable", "level") and
#' for standard CHI tableau ready output, it should be c("indicator_key", "year")
#' @param key_where An expression identifying the referent/comparator/key to
#' which other data will be compared. It should be passed unquoted.
#' rows to be filtered / excluded from secondary suppression because
#' the categories are not mutually exclusive (e.g., race3)
#' @param new_col Character vector of length 1. It is the name of the new column
#' that contains the comparison results (i.e., higher, lower, or no difference).
#' It is also the stem for the column noting the significance of the results (
#' e.g., if new_col = "comp", the significance column will be named "comp_sig")
#' @param tidy logical. Determines whether to drop intermediate variables with
#' the estimate, lower bound, and upper bound for the referent.
#'
#' @importFrom data.table setnames ":=" setDT data.table
#'
#' @return data.table comprised of the original data.table and two additional
#' columns ... 'comp' and 'comp_sig' (or alternatively specified names)
#'
#' @export
#'
#' @keywords suppression
#'
#' @examples
#' # create test data
#' set.seed(98104)
#' dt <- data.table::data.table(
#'   chi_year = rep(2008:2018, 2000),
#'   fetal_pres = factor(sample(c("Breech", "Cephalic", "Other", NA),
#'                              22000, rep = TRUE,
#'                              prob = c(0.04, 0.945, 0.01, 0.005))),
#'   bw_grams = round(rnorm(22000, 3343, 576), 0)
#' )
#' dt[fetal_pres=='Other', bw_grams := 0.5*bw_grams]
#' dt = dtsurvey::dtadmin(dt)
#' dt <- calc(dt, what = c("bw_grams"), by = c("fetal_pres"))
#' # run function
#' test <- compare_estimate(mydt = dt,
#'                          id_vars = c("variable", "level"),
#'                          key_where = fetal_pres == "Breech",
#'                          new_col = "comp",
#'                          tidy = FALSE)
#' test[]
#'
compare_estimate <- function (mydt,
                              id_vars = c("variable", "level"),
                              key_where ,
                              new_col = "comp",
                              tidy = T){
  #Bindings for data.table/check global variables
  comparator_vars <- comp_est <- comp_upper <- comp_lower <- NULL

  # validate 'mydt' ----
  if(is.null(mydt)){
    stop("You must specify a dataset (i.e., 'mydt' must be defined)")
  }

  if(!is.data.table(mydt)){
    if(is.data.frame(mydt)){
      data.table::setDT(copy(mydt))
    } else {
      stop(paste0("<{mydt}> must be the name of a data.frame or data.table."))
    }
  }

  # validate 'id_vars' ----
  if(length(setdiff(id_vars, names(mydt))) > 0 ){
    stop("At least one name in 'id_vars' is not found among the column names in 'mydt'")
  }

  # validate 'key_where' ----
  if(!missing(key_where)){
    call = match.call()

    if(is.character(call[['key_where']])){
      where = str2lang(call[['key_where']])
      warning('`key_where` is a string. It was converted so that it would work, but in the future, this might turn into an error.
                  In the future, please pass unquoted commands that will resolve to a logical' )

    } else {where = copy(call[['key_where']])}

    e <- substitute(expr = where) # get parse tree expression `where`
    r <- eval(expr = e, envir = mydt, enclos = parent.frame()) # evaluate

    stopifnot('`where` does not resolve to a logical' = is.logical(r))
    if(nrow(mydt[r,]) <1 ){
      stop(paste0("Your 'key_where' argument filters out all rows of data. Please revise and submit again"))
    }
  }

  # validate 'new_col' ----
  if(is.null(new_col) | new_col == "" | is.na(new_col)){stop("You must enter a 'new_col' for the results of the comparison")}
  if(length(new_col) > 1){stop("'new_col' is limited to one name")}
  if(new_col %in% names(mydt)){stop("'new_col' exists in mydt. Please select a novel column name instead")}
  new_col_sig <- paste0(new_col, "_sig")
  if(new_col_sig %in% names(mydt)){stop(paste0(new_col_sig, " exists in mydt. Please select a new 'new_col' column name instead"))}

  # validate 'tidy' ----
  if(!is.logical(tidy)){
    stop("'tidy' must be specified as a logical (i.e., TRUE, T, FALSE, or F)")
  }

  # split off the comparator data from main data ----
  comparator_est_vars <- grep("^mean$|^result$|^proportion$|lower|upper", names(mydt), value = T)
  comparator_est_vars2 <- gsub("^mean$|^result$|^proportion$", "comp_est", comparator_est_vars)
  comparator_est_vars2 <- replace(comparator_est_vars2, grep("lower", comparator_est_vars2), "comp_lower")
  comparator_est_vars2 <- replace(comparator_est_vars2, grep("upper", comparator_est_vars2), "comp_upper")
  comparator_vars <- c(id_vars, comparator_est_vars)
  r <- eval(expr = e, envir = mydt, enclos = parent.frame())
  comparator <- unique(mydt[r,])
  comparator <- unique(comparator[, (comparator_vars), with = F])
  data.table::setnames(comparator, comparator_est_vars, comparator_est_vars2)

  # merge comparator data onto main data ----
  mydt <- merge(mydt, comparator, by = c(id_vars), all.x = TRUE, all.y = TRUE)

  # compare estimates ----
  name_of_est <- setdiff(comparator_est_vars, grep("upper|lower", comparator_est_vars, value = T))
  name_of_lower <- grep("lower", comparator_est_vars, value = T)
  name_of_upper <- grep("upper", comparator_est_vars, value = T)

  mydt[get(name_of_est) == comp_est, c(new_col) := "no different"]
  mydt[get(name_of_est) > comp_est, c(new_col) := "higher"]
  mydt[get(name_of_est) < comp_est, c(new_col) := "lower"]

  mydt[, c(new_col_sig) := NA_character_]
  mydt[(get(name_of_lower) > comp_upper) | (get(name_of_upper) < comp_lower), c(new_col_sig) := "*"]

  mydt[is.na(get(new_col_sig)), c(new_col) := "no different"] # if not significant force "no different"

  # drop intermediate columns ----
  if(tidy==T){
    mydt[, c("comp_est", "comp_lower", "comp_upper") := NULL]
  }

  # return table ----
  return(mydt)
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


#' Generate a YAML file for SQL loading based on in a data.frame or data.table
#'
#' @description
#' YAML files can be helpful for uploading data to SQL efficiently and correctly. This function should enable
#' the user to create a standard YAML file that be be used to push data from R to SQL.
#'
#' This function expects data in the form of a data.frame or data.table
#'
#'
#' @param mydt the name of a data.table or data.frame for which you want to create a YAML file
#' @param outfile optional character vector of length one. The complete filepath for where the *.yaml file
#' should be saved. If it is not specified, the YAML file will be returned in memory
#' @param datasource A character vector of length one. A human readable description of the datasource to be
#' uploaded to SQL. This could be a filepath to the original data on a shared drive or a simple description.
#' @param schema A character vector of length one. The schema to be used within the specific server and
#' database that will be specified in your odbc connection.
#' @param table A character vector of length one. The table to be used within the specific server and
#' database that will be specified in your odbc connection and within the schema that you specified above.
#'
#'
#' @return a list with the YAML file contents (if outfile not specified) or a message stating where the YAML
#' file has been saved (if outfile was specified)
#'
#' @export
#'
#' @keywords YAML
#'
#' @name generate_yaml
#'
#' @importFrom data.table ':=' data.table copy setDT is.data.table
#' @importFrom yaml read_yaml
#'
#' @examples
#'
#' \dontrun{
#' data(mtcars)
#' # output to object in memory
#'   check <- generate_yaml(mtcars, schema = "SCH", table = "TBL",
#'   datasource = "R standard mtcars")
#' # output to a file
#'   generate_yaml(mtcars, outfile = "C:/temp/test.yaml", schema = "SCH", table = "TBL",
#'   datasource = "R standard mtcars")
#' }
#'
generate_yaml <- function(mydt, outfile = NULL, datasource = NULL, schema = NULL, table = NULL){

  #Bindings for data.table/check global variables
  vartype <- binary <- varname <- i <- varlength <- sql <- '.' <- NULL

  mi.outfile = 0


  ## Error check ----
  if(is.null(mydt))stop("mydt, the name of a data.frame or data.table for which you wish to create a YAML file, must be provided.")

  if(!is.data.table(mydt)){
    if(is.data.frame(mydt)){
      setDT(mydt)
    } else {
      stop(paste0("<mydt> must be the name of a data.frame or data.table."))
    }
  }

  if(!is.null(outfile)){
    if(!grepl("\\.yaml$", outfile)){
      stop(paste0("The value for 'outfile' (the complete filepath for saving the YAML you are creating), \n",
                  "must have the file extension '.yaml"))
    }}

  if(is.null(outfile)){
    message(paste0("You did not submit a value for 'outfile' (the complete filepath for saving the YAML you are creating), \n",
                   "and that's okay! \n \n",
                   "To save the yaml object in memory (as a list), assign a name to the output of this function, e.g., \n",
                   "my_new_yaml <- generate_yaml(...)"))
    mi.outfile = 1
    outfile <- tempfile("blahblah", fileext = ".yaml")
  }

  if(is.null(schema)){
    stop("You must submit a SQL schema for the header of the YAML file")
  }

  if(is.null(schema)){
    stop("You must submit a SQL table name for the header of the YAML file")
  }

  if(is.null(datasource)){
    message(paste0("Warning: You did not enter a datasource for where the underlying data exists on a shared drive. \n",
                   "         The YAML file will be created, but the datasource will not be recorded in the header."))
  }

  ## Set up ----
  # identify column type
  temp.vartype <- data.table(varname = names(sapply(mydt, class)), vartype = sapply(mydt, class))

  # identify if it is a binary
  temp.binary <- data.table(varname = names(sapply(mydt,function(x) { all(na.omit(x) %in% 0:1) })), binary = sapply(mydt,function(x) { all(na.omit(x) %in% 0:1) }))

  # merge binary indicator to the column types
  mydict <- merge(temp.vartype, temp.binary, by = "varname")

  # identify vartype == binary
  mydict[vartype %in% c("numeric", "integer") & binary == TRUE, vartype := "binary"]
  mydict[, binary := NULL]

  # ensure consistent ordering
  mydict[, varname := factor(varname, levels = names(mydt))]
  setorder(mydict, varname)

  # Identify standard TSQL numeric & string types ----
  # Identify all integers << tinyint, smallint, and bigint probably should not be automatically ascribed
  potential.int <- as.character(mydict[vartype %in% c("numeric", "integer")]$varname)
  for(i in potential.int){
    mydict[varname==i & all(mydt[!is.na(get(i)), .SD, .SDcols = i] == floor(mydt[!is.na(get(i)), .SD, .SDcols = i])) == TRUE, vartype := "integer"]
    mydict[varname==i & max(mydt[[i]], na.rm = T) >= 2147483647, vartype := "numeric"]
  }

  # Set varchar (assumed 1 chars ~= 1 byte and will add buffer of 100%)
  potential.varchar <- as.character(mydict[vartype %in% c("character", "factor")]$varname)
  for(i in potential.varchar){
    mydict[varname==i, varlength := 2+ceiling(max(nchar(as.character(mydt[[i]])[!is.na(mydt[[i]])]))*2)]
    mydict[varname==i & is.na(varlength), varlength := 36] # arbitrarily chose 'n'==36 when character vector is 100% NA
  }

  # Ascribe SQL data type names ----
  sqlkey <- data.table(
    vartype = c("logical", "character", "factor", "binary", "integer", "numeric", "Date", "POSIXct,POSIXt"),
    sql = c("BIT", "NVARCHAR", "NVARCHAR", "BIT", "INT", "NUMERIC(38,5)", "DATE", "DATETIME")  # NUMERIC(38,5) ... allows for up to 38 digits of precision, with 5 of those to the right of the decimal
  )

  mydict <- merge(mydict, sqlkey, by = "vartype", all.x = TRUE, all.y = FALSE)

  ## Clean up ----
  mydict[, varname := factor(varname, levels = names(mydt))]
  mydict[sql == "NVARCHAR", sql := paste0(sql, "(", varlength, ")")]
  mydict[, sql := paste0("    ", varname, ": ", sql)]
  setorder(mydict, varname) # sort in same order as the data.table
  mydict <- mydict[, .(sql)]

  if(!is.null(datasource)){
    header <- data.table(
      sql = c(paste0("datasource: ", datasource),
              paste0("schema: ", schema),
              paste0("table: ", table),
              "vars: "))
  } else {
    header <- data.table(
      sql = c(paste0("schema: ", schema),
              paste0("table: ", table),
              "vars: "))
  }


  mydict <- rbind(header, mydict)

  # save yaml file ----
  fwrite(x = mydict,
         file = outfile,
         quote = F,
         col.names=F,
         row.names = F,
         append=F)

  MyYAML <- yaml::read_yaml(outfile)

  if(mi.outfile == 1){return(MyYAML)}else{message(paste0("YAML saved to ", outfile))}

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
#' @importFrom data.table copy
#' @import rads.data
#'
get_ref_pop <- function(ref_name = NULL){
  #global variables used by data.table declared as NULL here to play nice with devtools::check()
  standard <- agecat <- age_start <- age_end <- pop <- ref_pop_name <- NULL

  ref_single_to_99 <- data.table::copy(rads.data::population_reference_pop_single_age_to_99)
  ref_single_to_84 <- data.table::copy(rads.data::population_reference_pop_single_age_to_84)
  ref_agecat_18 <- data.table::copy(rads.data::population_reference_pop_18_age_groups)
  ref_agecat_19 <- data.table::copy(rads.data::population_reference_pop_19_age_groups)
  ref_pop_table <- rbind(ref_single_to_99, ref_single_to_84, ref_agecat_18, ref_agecat_19)
  ref_pop_table <- ref_pop_table[standard == ref_name, list(agecat, age_start, age_end, pop)]
  if(nrow(ref_pop_table) == 0){stop(strwrap(paste0("`ref_name` ('", ref_name, "') does not refer to a valid standard reference population.
                                                     Type `list_ref_pop()` to get a list of all valid populations."), prefix = " ", initial = ""))}
  ref_pop_table[, ref_pop_name := ref_name]
  return(ref_pop_table)
}


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

  ret <- c('hys', 'birth')

  return(ret)


}


#' List columns available for analysis for a particular dataset in RADS
#'
#' @param dataset Character vector of length 1. Identifies the dataset to be fetched. Use \code{list_apde_data} for available options
#' @param year Year of dataset to check.
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
list_dataset_columns <- function(dataset, year = 2021, analytic_only = F){

  # create a negate function of %in% for readability
  '%!in%' = Negate('%in%')
  opts = c('birth', 'hys')

  stopifnot('dataset must be a character vector of length 1' = length(dataset) == 1)
  if(dataset %!in% opts){
    stop(paste0('list_dataset_columns functionality for dataset "', dataset, '" not currently available/implemented. ',
                "Only the following datasets are implemented: ", paste(opts, collapse = ', ')))

  }

  # The below code would ideally be replaced by a single call to a generic interface configured by the user
  if(dataset == "birth") {
    #message("Column names for birth data are taken from all available years.")
    # get list of all colnames from SQL
    con <- odbc::dbConnect(odbc::odbc(),
                           driver = getOption('rads.odbc_version'),
                           server = "KCITSQLPRPDBM50",
                           database = "PH_APDEStore",
                           Encrypt = 'yes',
                           TrustServerCertificate = 'yes',
                           Authentication = 'ActiveDirectoryIntegrated',
                           encoding = 'latin1')
    var.names <- names(DBI::dbGetQuery(con, "SELECT top (0) * FROM [PH_APDEStore].[final].[bir_wa]"))
    ar = rep(TRUE, length(var.names))
  } else if(dataset =="hys") {
    if(!all(year %in% c(seq(2004,2018,2), 2021))) {
      stop(paste0("invalid year(s) indicated for Health Youth Survey data. Please see department documentation for details on currently correct years."))
      #year <- 2021
    }
    dat <- suppressWarnings(get_data_hys(year = year, ar = T))
    var.names.ar <- names(dat)

    if(!analytic_only){
      var.names.stg = names(suppressWarnings(get_data_hys(year = year, ar = FALSE)))
    } else{
      var.names.stg = NULL
    }

    var.names = c(var.names.ar, var.names.stg)
    ar = c(rep(TRUE, length(var.names.ar)), rep(FALSE, length(var.names.stg)))

  }

  Variable_Descriptions = unique(data.frame(var.names = var.names, analytic_ready = ar))

  return(Variable_Descriptions)

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
#' @importFrom data.table copy
#' @import rads.data
#'
list_ref_pop <- function(){
  #global variables used by data.table declared as NULL here to play nice with devtools::check()
  standard <- NULL

  ref_single_to_99 <- data.table::copy(rads.data::population_reference_pop_single_age_to_99)
  ref_single_to_84 <- data.table::copy(rads.data::population_reference_pop_single_age_to_84)
  ref_agecat_18 <- data.table::copy(rads.data::population_reference_pop_18_age_groups)
  ref_agecat_19 <- data.table::copy(rads.data::population_reference_pop_19_age_groups)
  ref_pop_table <- unique(rbind(ref_single_to_99[, list(standard)], ref_single_to_84[, list(standard)], ref_agecat_18[, list(standard)], ref_agecat_19[, list(standard)]))
  setorder(ref_pop_table, standard)
  ref_pop_table <- rbind(ref_pop_table[standard %like% "2000 U.S. Std P"], ref_pop_table[!standard %like% "2000 U.S. Std P"])
  return(ref_pop_table$standard)
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


#' List of available metric for `calc`
#' @return character vector. A vector of the available metrics for `calc`
#' @name metrics
#' @details
#' 1) total: Count of people with the given value. Mostly relevant for surveys
#' (where total is approximately mean * sum(pweights)).
#' Returns total, total_se, total_upper, total_lower.
#' total_se, total_upper, & total_lower are only valid for survey data.
#' Default ci (e.g. upper and lower) is 95 percent.
#'
#' 2) mean: Average response and associated metrics of uncertainty.
#' Returns mean, mean_se, mean_lower, mean_upper.
#' Default ci (e.g. upper and lower) is 95 percent.
#'
#' 3) rse: Relative standard error. 100*se/mean.
#'
#' 4) numerator: Sum of non-NA values for `what``.
#' The numerator is always unweighted.
#'
#' 5) denominator: Number of rows where `what` is not NA.
#' The denominator is always unweighted.
#'
#' 6) obs: Number of unique observations (i.e., rows), agnostic as to whether
#' there is missing data for `what`. The obs is always unweighted.
#'
#' 7) median: The median non NA response. Not populated when `what` is a factor
#' or character. Even for surveys, the median is the unweighted result.
#'
#' 8) unique.time: Number of unique time points (from `time_var`) included in
#' each tabulation (i.e., number of unique time points when the `what` is not missing).
#'
#' 9) missing: Number of rows in a given grouping with an NA value for `what`.
#'    missing + denominator = Number of people in a given group.
#'    When `what` is a factor/character, the missing information is provided for the other.
#'
#' 10) missing.prop: The proportion of the data that has an NA value for `what`.
#'
#' 11) rate: mean * per. Provides rescaled mean estimates (i.e., per 100 or per 100,0000).
#' Returns rate, rate_se, rate_lower, rate_upper.
#' Default ci (e.g. upper and lower) is 95 percent.
#'
#' 12) ndistinct: The unique number of `what` values in the given subset. For factors, it is the unique number of levels in the subset.
#'
#' @rdname metrics
#' @export
metrics = function(){
  c('total',
    'mean', 'rse',
    'numerator','denominator', 'obs', 'median',
    'unique.time',
    'missing', 'missing.prop',
    'rate', 'ndistinct')
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


#' Clean string columns read from SQL
#' @param dat character vector of length one. Name of data.frame or data.table
#' @param stringsAsFactors logical. Specifies whether to convert strings to factors (TRUE) or not (FALSE)
#' @export
#' @importFrom utf8 utf8_encode
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
    dat[, (string.columns) := lapply(.SD, utf8::utf8_encode), .SDcols = string.columns] # ensure encoding is UTF8
    dat[, (string.columns) := lapply(.SD, trimws, which="both"), .SDcols = string.columns] # trim white space to right or left
    dat[, (string.columns) := lapply(.SD, function(x){gsub("^ *|(?<= ) | *$", "", x, perl = TRUE)}), .SDcols = string.columns] # collapse multiple consecutive white spaces into one
    dat[, (string.columns) := lapply(.SD, function(x){gsub("^$|^ $", NA, x)}), .SDcols = string.columns] # replace blanks with NA
    if(stringsAsFactors==TRUE){
      dat <- dat[, (string.columns) := lapply(.SD, factor), .SDcols = string.columns] # convert strings to factors
    }
  }
  # reorder table
  data.table::setcolorder(dat, original.order)
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
validate_yaml_data <- function(DF = NULL, YML = NULL, VARS = "vars"){
  ## Global variables used by data.table declared as NULL here to play nice with devtools::check()
  DF.class <- yamlcols <- yamlnames <- yamlextra <- dfcols <- dfnames <- NULL

  # Check that DT is a data.frame/data.table ----
  if(is.data.frame(DF) == FALSE){
    stop("'DF' must be a data.frame or a data.table")
  }else{DF <- data.table::setDT(copy(DF))}

  # Check that number of vars in YML is same as ncols in DF ----
  yamlcols <- length(YML[[VARS]])
  dfcols <- ncol(DF)
  if(yamlcols != dfcols){
    stop(paste0("The number of vars specified in the YAML (", yamlcols, ") does not match the number of columns in DF (", dfcols, ")"))
  }

  # Check that the column names in YML match those in DF ----
  yamlnames <- sort(names(YML[[VARS]]))
  dfnames <- sort(names(DF))
  yamlextra <- setdiff(yamlnames, dfnames)
  if(length(yamlextra) == 0){yamlextra <- "_____"}
  dfextra <- setdiff(dfnames, yamlnames)
  if(length(dfextra) == 0){dfextra <- "_____"}
  if(setequal(yamlnames, dfnames)==F){
    stop(paste0("The following variables are in the YAML but not in DF: ", yamlextra),
         paste0(". The following variables are in DF but not in the YAML: ", dfextra), ".")
  }

  # notice that this might take a while ----
  message("The validation may take a few minutes if your data & yaml contain dates and or times")

  # identify proper classes from YAML file ----
  class.compare <- data.table::data.table(
    name =  c(names(YML[[VARS]])),
    yaml.class = tolower(as.character(YML[[VARS]]))
  )

  # convert names of SQL data types to R classes ----
  class.compare[grepl("char|text|uniqueidentifier", tolower(yaml.class)), yaml.class := "character"]
  class.compare[tolower(yaml.class) %in% c("tinyint", "smallint", "int"), yaml.class := "integer"]
  class.compare[grepl("bigint|decimal|float|money|numeric|real", tolower(yaml.class)), yaml.class := "numeric"]
  class.compare[grepl("bit", tolower(yaml.class)), yaml.class := "logical"]
  class.compare[grepl("date", tolower(yaml.class)), yaml.class := "date"]
  class.compare[grepl("time", tolower(yaml.class)), yaml.class := "POSIXct"]

  # identify which VARS should be of which class (assuming YAML is correct) ----
  make.char <- class.compare[yaml.class == "character"]$name
  make.num  <- class.compare[yaml.class == "numeric"]$name
  make.int  <- class.compare[yaml.class == "integer"]$name
  make.logical  <- class.compare[yaml.class == "logical"]$name
  make.Date  <- class.compare[yaml.class == "Date"]$name
  make.POSIXct  <- class.compare[yaml.class == "POSIXct"]$name

  # create function to convert column classes if it can be done without introducing NA's ----
  lossless_convert <- function(x, class){
    if(!class %in% c("Date", "POSIXct")){
      if(sum(is.na(x)) == sum(is.na(suppressWarnings(as(x, class)))) ){
        x <- suppressWarnings(as(x, class))
      }
    }
    if(class %in% c("Date")){
      if(sum(is.na(x)) == sum(is.na(suppressWarnings(as.Date(as.character(x))))) ){
        x <- suppressWarnings(as.Date(as.character(x)))
      }
    }
    if(class %in% c("POSIXct")){
      if(sum(is.na(x)) == sum(is.na(suppressWarnings(as.POSIXct(as.character(x))))) ){
        x <- suppressWarnings(as.POSIXct(as.character(x)))
      }
    }
    return(x)
  }

  # use function convert R column types if possible / needed ----
  DF[, (make.char) := lapply(.SD, lossless_convert, class = 'character'), .SDcols = make.char]
  DF[, (make.num) := lapply(.SD, lossless_convert, class = 'numeric'), .SDcols = make.num]
  DF[, (make.int) := lapply(.SD, lossless_convert, class = 'integer'), .SDcols = make.int]
  DF[, (make.logical) := lapply(.SD, lossless_convert, class = 'logical'), .SDcols = make.logical]
  DF[, (make.Date) := lapply(.SD, lossless_convert, class = 'Date'), .SDcols = make.Date]
  DF[, (make.POSIXct) := lapply(.SD, lossless_convert, class = 'POSIXct'), .SDcols = make.POSIXct]

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
  }else{success <- message("All column classes in your R dataset are compatible with the YAML reference standard.")}

  return(success)

}


#' Silence (i.e., suppress or mute) printed messages from functions
#'
#' @description
#' Silence noisy functions
#'
#' @param myf the name of the function that you desire to silence, along with its arguments
#'
#' @return whatever should be returned by the function that is being silenced
#'
#' @export
#'
#' @keywords quiet quietly silence silent
#'
#' @name quiet
#'

#' @examples
#' \dontrun{
#' test <- function(x) {
#'   x = 3^x
#'   cat("silences cat(): ", x, "\n")
#'   print(paste0("silences print():", x))
#'   message("does not silence message(): ", x)
#' }
#' test(4)
#' quiet(test(4))
#' }
quiet <- function(myf) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(myf))
}
