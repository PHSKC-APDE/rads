options("scipen"=999) # turn off scientific notation

# adjust_direct() ----
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
#' \donttest{
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

# age_standardize() ----
#' Calculate age standardized rates from a data.table with age, counts, and population columns. (Built on adjust_direct())
#'
#' @description
#' Calculate age standardized rates from a data.table with age, counts, and population columns.
#'
#' Your dataset must have the following three columns ...
#' \itemize{
#' \item 'age' or 'agecat': 'age' in single years (if collapse = T) or 'agecat' with the same age bins as your selected reference population (if collapse = F)
#' \item an aggregated count for the event (e.g., disease) for which you want to find an age standardized rate
#' \item the population corresponding to the age or agecat in your original data
#' }
#'
#' @param ph.data a data.table or data.frame containing the data to be age-standardized.
#' @param ref.popname Character vector of length 1. Only valid options are those in list_ref_pop() and
#' "none" (when standard population already exists in ph.data)
#' @param collapse Logical vector of length 1. Do you want to collapse ph.data ages to match those in ref.popname?
#' @param my.count Character vector of length 1. Identifies the column with the count data aggregated by the given demographics.
#' @param my.pop Character vector of length 1. Identifies the column with the population corresponding to the given demographics.
#' @param per Integer vector of length 1. A multiplier for all rates and CI, e.g., when per = 1000, the rates are per 1000 people
#' @param conf.level A numeric vector of length 1. The confidence interval used in the calculations.
#' @param group_by Character vector of indeterminate length. By which variable(s) do you want to stratify the rate results, if any?
#'
#' @return a data.table of the count, rate & adjusted rate with CIs, name of the reference population and the 'group_by' variable(s) -- if any
#' @export
#' @name age_standardize
#' @references \url{https://github.com/PHSKC-APDE/rads/wiki/age_standardize}
#' @examples
#' \donttest{
#' library(data.table)
#' temp1 <- data.table(age = c(50:60), count = c(25:35), pop = c(seq(1000, 900, -10)) )
#' age_standardize(ph.data = temp1,
#' ref.popname = "2000 U.S. Std Population (18 age groups - Census P25-1130)",
#' collapse = TRUE,
#' my.count = "count",
#' my.pop = "pop",
#' per = 1000,
#' conf.level = 0.95)[]
#'
#' temp2 <- data.table(sex = c(rep("M", 11), rep("F", 11)), age = rep(50:60, 2),
#' count = c(25:35, 26:36), pop = c(seq(1000, 900, -10), seq(1100, 1000, -10)),
#' stdpop = rep(1000, 22))
#' age_standardize(ph.data = temp2, ref.popname = "none",
#' collapse = FALSE,
#' my.count = "count",
#' my.pop = "pop",
#' per = 1000,
#' conf.level = 0.95,
#' group_by = "sex")[]
#' }
#' @importFrom data.table ":=" setDT

age_standardize <- function (ph.data,
                             ref.popname = NULL,
                             collapse = T,
                             my.count = "count",
                             my.pop = "pop",
                             per = 100000,
                             conf.level = 0.95,
                             group_by = NULL)
{
  # Global variables used by data.table declared as NULL here to play nice with devtools::check() ----
    ph.data.name <- age <- age_start <- age_end <- agecat <- count <- pop <-
      stdpop <- reference_pop <- adj.lci <- adj.uci <- NULL

    ph.data.name <- deparse(substitute(ph.data))
    ph.data <- copy(ph.data)

  # Logic checks ----
    # Check that ph.data is a data.frame or data.table ----
      if( inherits(ph.data, "data.frame") == FALSE){stop("\n\U1F6D1 ph.data must be a data.frame or a data.table containing both counts and population data.")}
      if( inherits(ph.data, "data.table") == FALSE){setDT(ph.data)}

    # Check that ph.data has either 'age' or 'agecat' ----
      age_exists <- "age" %in% names(ph.data)
      agecat_exists <- "agecat" %in% names(ph.data)

      if (age_exists && agecat_exists) {
        stop("\n\U1F6D1 Both 'age' and 'agecat' columns are present, but only one is needed. Please check your data.")
      } else if (!age_exists && !agecat_exists) {
        stop("\n\U1F6D1 Neither 'age' nor 'agecat' columns are present. Please check your data.")
      } else if (age_exists) {
        # Check if 'age' is integer or can be converted to integer without loss
        if (!is.integer(ph.data$age) & is.numeric(ph.data$age)) {
          if (all(ph.data$age == floor(ph.data$age))) {
            ph.data[, age := as.integer(age)]
          } else {
          stop("\n\U1F6D1 The 'age' column is not an integer and cannot be converted to integer without loss of data.")
          }
        }
      } else if (agecat_exists) {
        # Check if 'agecat' is character or factor
        if (!is.character(ph.data$agecat)) {
          if (is.factor(ph.data$agecat)) {
            ph.data[, agecat := as.character(agecat)]
          } else {
            stop("\n\U1F6D1 The 'agecat' column is neither character nor factor.")
          }
        }
      }

    # Check arguments needed for adjust_direct ----
      if(! my.count %in% colnames(ph.data)){
        stop(paste0("\n\U1F6D1 The column '", my.count, "' does not exist in ph.data.\n",
                    "ph.data must have a column indicating the count of events (e.g., deaths, births, etc.) and is typically named 'count'.\n",
                    "If such a column exists with a different name, you need to specify it in the `my.count` argument. e.g., my.count = 'deaths'."))
        }

      if(! my.pop %in% colnames(ph.data)){
        stop(paste0("\n\U1F6D1 The column '", my.pop, "' does not exist in ph.data.\n",
                    "ph.data must have a column for the population denominator correspondnig to the given demographics. It is typically named 'pop'.\n",
                    "If such a column exists with a different name, you need to specify it in the `my.pop` argument. e.g., my.pop = 'wapop'."))
        }

    # Ensure the reference population exists ----
      if(is.null(ref.popname)){ref.popname <- "2000 U.S. Std Population (11 age groups)"}
      if(! ref.popname %in% c( list_ref_pop(), "none")){
        stop(strwrap(paste0("\n\U1F6D1 ref.popname ('", ref.popname, "') is not a valid reference population name.
              The names of standardized reference populations can be viewed by typing `list_ref_pop()`.
              If ph.data is already aggregated/collapsed and has a relevant 'stdpop' column, please set ref.popname = 'none'"), prefix = " ", initial = ""))}

      if(ref.popname == "none" & !"stdpop" %in% colnames(ph.data)){stop("\n\U1F6D1 When specifying ref.popname = 'none', ph.data must have a column named 'stdpop' with the reference standard population data.")}

      if(ref.popname == "none" & collapse == T){stop(strwrap("\n\U1F6D1 When ref.popname = 'none', collapse should equal F.
                                                                      Selecting ref.popname = 'none' expects that ph.data has already been collapsed/aggregated and has a 'stdpop' column."), prefix = " ", initial = "")}

  # Standardize column names ----
    # purposefully did not use setnames() because it is possible that count | pop already exists and are intentionally using different columns for this function
      ph.data[, "count" := get(my.count)]
      ph.data[, "pop" := get(my.pop)]

  # Check ranges for age, count, and population ----
    # Check age ----
      if(!"agecat" %in% names(ph.data)){ # if given agecat, ignore these tests for single years of age
        # check for missing ages ----
          if(nrow(ph.data[is.na(age)]) > 0){
            stop(paste0("\n\U1F6D1 ph.data (", ph.data.name, ") contains at least one row where age is missing.\nCorrect the data and try again."))
          }

        # check for ages > 100 ----
          if(nrow(ph.data[age > 100]) > 0){
            warning(paste0("\n\U00026A0 ph.data (", ph.data.name, ") contains at least one row where age is greater than 100.\n",
                           "Those values have automatically been recoded to 100 because population pulled from\n",
                           "get_population() is top coded to 100 and reference populations are usually top coded at 85."),
                    immediate. = TRUE, call. = FALSE)
            ph.data[age > 100, age := 100]
          }

        # check for negative ages ----
          if(nrow(ph.data[age < 0]) > 0){
            stop(paste0("\n\U1F6D1 ph.data (", ph.data.name, ") contains at least one row where age is negative.\nCorrect the data and try again."))
          }

        # Check for full range of ages (0 to 100) overall and within groups ----
          # simple function to check whether all ages are present (T | F) and which ones are missing (if any)
            check_full_age_range <- function(tempx) {
              actual_ages <- sort(unique(tempx$age))
              missing_ages <- setdiff(0:100, actual_ages)
              return(list(full_range = length(missing_ages) == 0, missing = missing_ages))
            }

          # Simple check when group_by not specified
          if (is.null(group_by)) {
            check_result <- check_full_age_range(ph.data)
            if (!check_result$full_range) {
              warning(paste0("\n\U00026A0 ph.data (", ph.data.name, ") does not have the full range of ages from 0 to 100.\n",
                             "Missing ages: ", paste(check_result$missing, collapse = ", "), "\n",
                             "This may affect the accuracy of your age-adjusted rates.\n",
                             "Consider adding missing ages with zero counts and the appropriate population."),
                      immediate. = TRUE, call. = FALSE)
            }
          } else {
          # Now more complicated case where group_by variables are given
            group_combos <- unique(ph.data[, group_by, with = FALSE])

            # Initialize a list to store results of tests for each table defined by group_combos
            missing_ranges <- vector("list", nrow(group_combos))

            # Check each group combination
            for (i in 1:nrow(group_combos)) {
              group_data <- ph.data
              for (col in group_by) {
                group_data <- group_data[group_data[[col]] == group_combos[[col]][i], ] # repeated filtering for however many group_by vars are specified
              }
              check_result <- check_full_age_range(group_data) # use the function created above
              missing_ranges[[i]] <- list(
                group = as.list(group_combos[i, ]),
                full_range = check_result$full_range, # the T | F indicator from check_full_age_range()
                missing = check_result$missing # the specific missing ages from check_full_age_range()
              )
            }

            # subset the list of test for each group combination to keep when full_range != TRUE (i.e., FALSE, i.e., there are missing numbers)
            groups_missing_ages <- missing_ranges[!sapply(missing_ranges, function(x) x$full_range)]

            if (length(groups_missing_ages) > 0) {
              warning_message <- paste0("\n\U00026A0 Some groups in ph.data (", ph.data.name, ") do not have the full range of ages from 0 to 100:\n")
              for (group in groups_missing_ages) { # identify issues one item of the list (i.e., one group combo) at a time
                group_desc <- paste(names(group$group), group$group, sep = "=", collapse = ", ")
                warning_message <- paste0(warning_message,
                                          "  Group (", group_desc, ") is missing ages: ",
                                          paste(group$missing, collapse = ", "), "\n")
              }
              warning_message <- paste0(warning_message,
                                        "This may affect the accuracy of your age-adjusted rates.\n",
                                        "Consider adding missing ages with zero counts and the appropriate population.")
              warning(warning_message, immediate. = TRUE, call. = FALSE)
            }
          }
      }

    # Check agecat ----
      if("agecat" %in% names(ph.data)){
        if(!identical(sort(unique(ph.data$agecat)),
                      sort(unique(get_ref_pop(ref.popname)[['agecat']])))){
          stop("\n\U1F6D1 STOP and fix your code!\nThe agecat values in ph.data must match those in your reference \npopulation {",ref.popname, "} exactly.")
        }
      }

    # Check count ----
      if(nrow(ph.data[is.na(count)]) > 0){
        warning(paste0("\U00026A0 ph.data (", ph.data.name, ") contains at least one row where my.count is missing.
                    Those values have been replaced with zero."))
        ph.data[is.na(count), count := 0]
      }
      if(nrow(ph.data[count < 0]) > 0){
        stop(paste0("\U0001f47f ph.data (", ph.data.name, ") contains at least one row where my.count is negative.
                        Correct the data and try again."))
      }

    # Check population ----
      if(nrow(ph.data[is.na(pop)]) > 0){
        stop(paste0("\U0001f47f ph.data (", ph.data.name, ") contains at least one row where my.pop is missing.
                     Correct the data and try again."))
      }
      if(nrow(ph.data[pop < 0]) > 0){
        stop(paste0("\U0001f47f ph.data (", ph.data.name, ") contains at least one row where my.pop is negative.
                        Correct the data and try again."))
      }

    # Check count vs population ----
      if(nrow(ph.data[count > pop]) > 0 ){
        warning(paste0("\U00026A0 ph.data (", ph.data.name, ") contains at least one row where the count is greater than the population.
                        This may be correct because OFM populations are just estimates. However, you are encouraged to check the data."))
      }

  # Collapse ph.data to match standard population bins ----
    if(collapse==T){
      if(! "age" %in% colnames(ph.data)){stop(strwrap("\n\U1F6D1 When collapse = T, ph.data must have a column named 'age' where age is an integer.
                                                          This is necessary to generate age bins that align with the selected standard
                                                          reference population. If ph.data already has an 'agecat' column that is formatted
                                                          identically to that in the standard reference population, set collapse = F"), prefix = " ", initial = "")}
      if(is.numeric(ph.data$age) == F){stop("\n\U1F6D1 When collapse = T, the 'age' column must be comprised entirely of integers")}
      if(sum(as.numeric(ph.data$age) %% 1) != 0){stop("\n\U1F6D1 When collapse = T, the 'age' column must be comprised entirely of integers")}
      if("agecat" %in% colnames(ph.data)){stop(strwrap("\n\U1F6D1 When collapse = T, a new column named 'agecat' is created to match that in the standard reference population.
                                                    ph.data already has a column named 'agecat' and it will not be automatically overwritten.
                                                    If you are sure you want to create a new column named 'agecat', delete the existing column in ph.data and run again."),
                                             prefix = " ", initial = "")}
      my.ref.pop <- get_ref_pop(ref.popname)
      for(z in seq(1, nrow(my.ref.pop))){
        ph.data[age %in% my.ref.pop[z, age_start]:my.ref.pop[z, age_end], agecat := my.ref.pop[z, agecat]]
      }
      if(!is.null(group_by)){ph.data <- ph.data[, list(count = sum(count), pop = sum(pop)), by = c("agecat", group_by)]}
      if(is.null(group_by)){ph.data <- ph.data[, list(count = sum(count), pop = sum(pop)), by = "agecat"]}
    }

  # Hack when pop < count in age collapsed data ----
    if(nrow(ph.data[pop < count]) > 0){
      warning(paste0("\U00026A0
      When ph.data (", ph.data.name, ") was collapsed to match the standard
      population, the aggregate `count` was greater than the aggreate `pop` for
      the following age group(s): ",
      sort(paste(unique(ph.data[pop < count]$agecat), collapse = ', ')), ". In these rows, the
      `pop` was ascribed the `count` value. This is necessary to calculate
      the age adjusted rate and only nomimally biases the calculated rates since
      the counts are typically small."))

      ph.data[pop < count, pop := count]
    }

  # Hack when pop == 0 ----
    if(nrow(ph.data[pop ==0]) > 0){
      ph.data[pop == 0, pop := 1]
    }

  # Merge standard pop onto count data ----
    if(ref.popname != "none"){
      ph.data <- merge(ph.data, get_ref_pop(ref.popname)[, list(agecat, stdpop = pop)], by = "agecat")
    }

  # Calculate crude & adjusted rates with CI ----
    if(!is.null(group_by)){my.rates <- ph.data[, as.list(adjust_direct(count = count, pop = pop, stdpop = stdpop, conf.level = as.numeric(conf.level), per = per)), by = group_by]}
    if( is.null(group_by)){my.rates <- ph.data[, as.list(adjust_direct(count = count, pop = pop, stdpop = stdpop, conf.level = as.numeric(conf.level), per = per))]}

  # Tidy results ----
    rate_estimates <- c("crude.rate", "crude.lci", "crude.uci", "adj.rate", "adj.lci", "adj.uci")
    my.rates[, c(rate_estimates) := lapply(.SD, rads::round2, 2), .SDcols = rate_estimates]
    my.rates[, reference_pop := ref.popname]
    my.rates[is.nan(adj.lci) & count == 0, adj.lci := 0]
    if(ref.popname == "none"){my.rates[, reference_pop := paste0("stdpop column in `", ph.data.name, "`")]}

  # Return object ----
    return(my.rates)
}

# as_imputed_brfss() ----
#' Convert modified BRFSS data.table to an imputationList
#'
#' @description
#' Converts a modified BRFSS \code{\link[dtsurvey]{dtsurvey}}/data.table back into
#' a \code{\link[mitools]{imputationList}} by creating multiple imputed datasets
#' for HRA and region assignments. This is necessary after modifying variables in
#' BRFSS data that contains HRA or region variables.
#'
#' @param ph.data A \code{\link[dtsurvey]{dtsurvey}}/data.table containing BRFSS
#' survey data, typically created using \code{\link{as_table_brfss}}
#'
#' @param impute_cols Character vector specifying which geographic imputation
#' columns to retain in the output. Must be some combination of
#' \code{'hra20_id'}, \code{'hra20_name'}, and \code{'chi_geo_region'}. Defaults
#' to \code{impulte_cols = c('hra20_id', 'hra20_name', 'chi_geo_region')}
#'
#' @details
#' When working with BRFSS data that includes Health Reporting Area (HRA) or region
#' variables, modifications to the data must be made on a single data.table.
#' After modifications are complete, this function recreates the proper
#' \code{\link[mitools]{imputationList}} structure required for analysis of
#' HRA and region variables.
#'
#' This function:
#' \itemize{
#'   \item Creates 10 copies of your modified dataset
#'   \item Assigns different HRA IDs to each copy based on ZIP code probabilities
#'   \item Adds corresponding HRA names and region assignments
#'   \item Combines the copies into an imputationList
#' }
#'
#' @return
#' Returns an \code{\link[mitools]{imputationList}} containing 10 imputed datasets,
#' each a survey-weighted \code{\link[dtsurvey]{dtsurvey}}/data.table with
#' different HRA and region assignments based on ZIP code probabilities.
#'
#' @examples
#' \dontrun{
#' # Starting with a modified BRFSS data.table
#' brfss_table[, age_category := fcase(
#'   age %in% 18:66, "working age",
#'   age >= 67, "retirement age",
#'   default = NA_character_
#' )]
#'
#' # Convert back to imputationList for analysis
#' brfss_imputed <- as_imputed_brfss(brfss_table)
#'
#' # Now ready for analysis with rads::calc()
#' }
#'
#' @seealso \code{\link{as_table_brfss}} for converting an imputationList to a
#' single data.table
#'
#' @import data.table
#' @import mitools
#' @export
#'
as_imputed_brfss <- function(ph.data,
                             impute_cols = c('hra20_id', 'hra20_name', 'chi_geo_region')) {
  # Visible bindings for data.table/check global variables
  hra20_id <- hra20_name <- region_name <- chi_geo_region <- NULL

  # Make a deep copy of of ph.data before manipulating anything
  ph.data <- copy(ph.data)

  # Validate input
  if (!(inherits(ph.data, "dtsurvey"))) {
    stop("\n\U1F6D1 'ph.data' must be a dtsurvey object")
  }

  # Ensure required HRA ID columns exist
  hra_cols <- paste0("hra20_id_", 1:10)
  missing_cols <- hra_cols[!hra_cols %in% names(ph.data)]
  if (length(missing_cols) > 0) {
    stop(sprintf("\n\U1F6D1 Required columns for HRA imputation missing: %s",
                 paste(missing_cols, collapse = ", ")))
  }

  # Check if spatial crosswalk data is available
  if (!requireNamespace("rads.data", quietly = TRUE)) {
    stop("\n\U1F6D1 Package 'rads.data' is required but not available")
  }
  tryCatch({
    crosswalk_test <- rads.data::spatial_hra20_to_region20
  }, error = function(e) {
    stop("\n\U1F6D1 Required spatial crosswalk data 'spatial_hra20_to_region20' not found in rads.data package")
  })

  # Drop existing HRA or Region vars
  ph.data[, intersect(c('hra20_id', 'hra20_name', 'chi_geo_region'), names(ph.data)) := NULL]

  # Create list of 10 imputed datasets
  dt <- lapply(1:10, function(i) {
    temp_dt <- copy(ph.data)
    temp_dt[, hra20_id := get(paste0("hra20_id_", i))]
    temp_dt <- merge(
      temp_dt,
      rads.data::spatial_hra20_to_region20[, c("hra20_id", "hra20_name", "region_name")],
      by = "hra20_id",
      all.x = TRUE,
      all.y = FALSE,
      sort = FALSE
    )
    setnames(temp_dt, "region_name", "chi_geo_region")
    temp_dt <- temp_dt[, c(setdiff(c('hra20_id', 'hra20_name', 'chi_geo_region'), impute_cols)) := NULL]
    return(temp_dt)
  })

  # Convert to imputationList
  dt <- mitools::imputationList(dt)

  message('Successfully created an imputationList with 10 imputed datasets.\n',
          'Data is now ready for analysis with rads::calc().')

  return(dt)
}

# as_table_brfss() ----
#' Convert BRFSS imputationList to a single data.table
#'
#' @description
#' Converts a BRFSS \code{\link[mitools]{imputationList}} to a single
#' \code{\link[dtsurvey]{dtsurvey}}/data.table by extracting the first imputed
#' dataset. This is useful when you need to modify variables in BRFSS data that
#' contains HRA or region variables.
#'
#' @param ph.data A \code{\link[mitools]{imputationList}} containing BRFSS survey data
#'
#' @details
#' When working with BRFSS data that includes Health Reporting Area (HRA) or region
#' variables, the data is stored as an \code{\link[mitools]{imputationList}} to
#' account for ZIP codes that cross HRA boundaries. However, if you need to modify
#' variables in your dataset, you must first convert to a single data.table, make
#' your modifications, then convert back to an imputationList using
#' \code{\link{as_imputed_brfss}}.
#'
#' @return
#' Returns a survey-weighted \code{\link[dtsurvey]{dtsurvey}}/data.table object
#' containing the first imputed dataset from the input imputationList.
#'
#' @examples
#' \dontrun{
#' # Get BRFSS data with HRA variables (returns an imputationList)
#' brfss <- get_data_brfss(
#'   cols = c("age", "hra20_id"),
#'   year = 2019:2023
#' )
#'
#' # Convert to single table for modification
#' brfss_table <- as_table_brfss(brfss)
#'
#' # Now you can modify variables
#' brfss_table[, age_category := fcase(
#'   age %in% 18:66, "working age",
#'   age >= 67, "retirement age",
#'   default = NA_character_
#' )]
#' }
#'
#' @seealso \code{\link{as_imputed_brfss}} for converting back to an imputationList
#'
#' @import data.table
#' @import dtsurvey
#' @export
#'
as_table_brfss <- function(ph.data) {
  # Validate input is an imputationList
  if (!inherits(ph.data, "imputationList")) {
    stop("\n\U1F6D1 'ph.data' must be an mitools imputationList")
  }

  # Return first imputed dataset
  dt <- copy(ph.data$imputations[[1]]) # need a deep copy due to data.table assignment by reference

  message('Successfully converted an imputationList to a single dtsurvey/data.table.\n',
          'Remember to use as_imputed_brfss() after making modifications.')

  return(dt)
}



# calc_age() ----
#' Proper calculation of age in years
#'
#' @param from Vector of dates or characters ("YYYY-MM-DD") of indeterminate length.  vector of length 1.
#' @param to Vector of dates or characters ("YYYY-MM-DD") of indeterminate length.  vector of length 1.
#'
#' @return Character vector of available datasets.
#' @export
#' @name calc_age
#' @examples
#' \donttest{
#'  calc_age(from = "2000-02-29", to = "2021-07-01")
#' }
#'
calc_age <- function(from, to) {
  from_lt = as.POSIXlt(from)
  to_lt = as.POSIXlt(to)

  age = to_lt$year - from_lt$year

  age = ifelse(to_lt$mon < from_lt$mon |
                 (to_lt$mon == from_lt$mon & to_lt$mday < from_lt$mday),
               age - 1, age)

  age = as.integer(age)

  return(age)
}

# chi_cols() ----
#
#' Vector of standard CHI / Tableau Ready columns
#'
#' @examples
#' print(chi_cols())
#'
#' @export
chi_cols = function(){
  chi.yaml <- yaml::yaml.load(httr::GET(url = "https://raw.githubusercontent.com/PHSKC-APDE/rads/main/ref/chi_qa.yaml"))
  chi_colnames <- names(chi.yaml$vars)
}

# chi_compare_est() ----
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
#'
chi_compare_est <- function(OLD = NULL, NEW = NULL, OLD.year = NULL, NEW.year = NULL, META = NULL){

  #Bindings for data.table/check global variables
  indicator_key <- result_type <- relative.diff <- result.x <- result.y <- absolute.diff <- cat1 <- tab <-  NULL
  # Check inputs ----
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

  # Process data ----
  # If metadata provided, add it to the columns to help interpret the output
  if(!is.null(META)){
    NEW <- merge(NEW, META[, list(indicator_key, result_type)], by = "indicator_key", all.x = TRUE, all.y = FALSE)
  } else { NEW[, result_type := "Metadata not provided"]}

  # Merge old and new data based on identifiers
  comp <- merge(copy(OLD[year == OLD.year]),
                copy(NEW[year == NEW.year]),
                by = c("indicator_key", "tab",
                       "cat1", "cat1_group", "cat1_varname",
                       "cat2", "cat2_group", "cat2_varname"),
                all = T)

  # calculate percent differences between old (x) and new(y)
  comp[, relative.diff := round2(abs((result.x - result.y) / result.x)*100, 1)]
  comp[result_type != "rate", absolute.diff := round2(abs(result.x - result.y)*100, 1)]
  comp[result_type == "rate", absolute.diff := round2(abs(result.x - result.y), 1)]
  comp <- comp[!is.na(absolute.diff)]  # drop if absolute difference is NA

  # order variables
  comp <- comp[, c("absolute.diff", "relative.diff", "result_type",
                   "indicator_key", "tab",
                   "cat1", "cat1_group", "cat1_varname",
                   "cat2", "cat2_group", "cat2_varname", "year.x", "year.y",
                   "result.x", "result.y", "lower_bound.x", "lower_bound.y",
                   "upper_bound.x", "upper_bound.y",
                   "numerator.x", "numerator.y", "denominator.x", "denominator.y",
                   "se.x", "se.y")]

  # rename suffixes
  setnames(comp, names(comp), gsub("\\.x$", ".OLD", names(comp)))
  setnames(comp, names(comp), gsub("\\.y$", ".NEW", names(comp)))

  # order based on percent difference
  setorder(comp, -absolute.diff)

  # return object ----
  return(comp)

}

# chi_compare_kc() ----
#' Compare CHI standard tabular results to the King County average for the same year within a given data set
#' @description
#' \strong{!!!STOP!!! This function has been deprecated!} Please use
#' \link{compare_estimate} instead.
#' @param orig Character vector of length 1. Identifies the data.table/data.frame to be fetched. Note the table must have the following columns:
#' 'result', 'lower_bound', & 'upper_bound' and all three must be numeric
#' @param new.col.name Character vector of length 1. It is the name of the column containing the comparison results.
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
  .Deprecated("compare_estimate")

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

# chi_metadata_cols() ----
#' Vector of standard CHI / Tableau Ready metadata columns
#'
#' @examples
#' print(chi_metadata_cols())
#'
#' @export
chi_metadata_cols = function(){
  chi.yaml <- yaml::yaml.load(httr::GET(url = "https://raw.githubusercontent.com/PHSKC-APDE/rads/main/ref/chi_qa.yaml"))
  chi_metanames <- names(chi.yaml$metadata)
}

# compare_estimate() ----
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

# convert_to_date() ----
#' Convert Numeric and Character Data to Dates
#'
#' This function attempts to convert specified values, vectors, or table columns
#' into date format when possible. The function handles numeric values by
#' treating them as the number of days since a specified origin date, with the
#' default being "1899-12-30" (Excel's origin date). It also attempts to parse
#' character vectors according to common date formats.
#'
#' @param x A numeric or character vector that needs to be converted to the date
#' format.
#' @param origin A character string specifying the origin date for numeric
#' conversions. It must be in "%Y-%m-%d" format.
#'
#' The default is "1899-12-30".
#'
#' @return Returns a vector of class `Date`. If conversion is not possible for
#' some values, those values will be replaced with `NA`. If none of the original
#' values can be converted, the original vector will be returned.
#'
#' @details The function handles different input types:
#'
#' - If `x` is already a Date object, it is returned unchanged.
#'
#' - Numeric values are treated as the number of days since `origin`.
#'
#' - Character values are parsed using several common American date formats.
#'   If all conversion attempts fail, a warning is issued and the original data
#'   is returned.
#'
#'@importFrom lubridate parse_date_time
#'
#' @examples
#' convert_to_date(c("2024-01-01", "February 13, 1999", "2024/02/01", "03/21/2000", "Not date"))
#' convert_to_date(c(42005, 42006), origin = "1899-12-30")
#' convert_to_date(c('puppies', 'kittens'))
#'
#' @export
convert_to_date <- function(x, origin = "1899-12-30") {
  # validation of 'origin'
  if (! grepl("^\\d{4}-\\d{1,2}-\\d{1,2}$", origin) ||
      inherits(try(as.Date(origin), silent = TRUE), "try-error") ||
      is.na(as.Date(origin))) {
    stop("Origin date must be in '%Y-%m-%d' format, e.g., '1970-01-01'")
  }

  # get name of 'x'
  x_name <- deparse(substitute(x))

  # get copy of original data
  x_orig <- copy(x)

  # if already a date, return it unchanged
  if (inherits(x, 'Date')) {
    return(x)
  }

  # Sometimes we get character data for dates that should be numeric, but aren't
  # x = rads::quiet(rads::lossless_convert(x, 'numeric'))

  # convert numerics first, then strings
  if (is.numeric(x)) {
    return(as.Date(x, origin = origin))
  } else {
    date_out <- as.Date(suppressWarnings(
      lubridate::parse_date_time(x,
                                 orders = c("%Y-%m-%d", "%Y/%m/%d", "%m/%d/%Y",
                                            "%m-%d-%Y", "%B %d, %Y",
                                            "%d %B, %Y", "%Y-%m-%d %H:%M:%S",
                                            "%Y/%m/%d %H:%M:%S"))))
    if (all(is.na(date_out))) {
      warning('\n\U00026A0 `', x_name, '` cannot be converted to a date. Your original data will be returned.')
      return(x_orig)
    } else {return(date_out)}
  }
}


# dumb_convert() ----
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

# format_time() ----
#' Format a vector of time, date, or any numeric values into a series of human readable chunks
#' @param x numeric or Date
#' @param date_format character, format string for dates in output (default: "%Y-%m-%d")
#' @export
#' @return character vector
#'
#' @examples
#' format_time(c(1:5, 10, 12, 24, 25))
#' format_time(as.Date(c("2023-01-01", "2023-01-02", "2023-01-03", "2023-01-05", "2023-01-06")))
#'
format_time <- function(x, date_format = "%Y-%m-%d") {
  # Check if input is Date class
  is_date <- inherits(x, "Date")

  # Get the unique values and sort
  x_sorted <- sort(unique(x))

  # Function to find consecutive sequences
  find_sequences <- function(x) {
    gaps <- diff(x) != 1
    cumsum(c(TRUE, gaps))
  }

  # Split into sequences
  sequences <- split(x_sorted, find_sequences(if(is_date) as.numeric(x_sorted) else x_sorted))

  # Format each sequence
  formatted <- sapply(sequences, function(seq) {
    if (length(seq) > 1) {
      if (is_date) {
        paste(format(min(seq), format = date_format),
              format(max(seq), format = date_format),
              sep = " to ")
      } else {
        paste(min(seq), max(seq), sep = "-")
      }
    } else {
      if (is_date) {
        format(seq, format = date_format)
      } else {
        as.character(seq)
      }
    }
  })

  # Combine all formatted sequences
  paste(formatted, collapse = ", ")
}
# format_time_simple() ----
#' Format a vector of time (or any numeric values) into a single human readable chunk
#' @param x numeric or Date
#' @export
#' @return character vector
#'
#' @examples
#' format_time_simple(c(1:5, 10, 12, 24, 25))
#' format_time_simple(as.Date(c("2023-01-01", "2023-01-02", "2023-01-03", "2023-01-05", "2023-01-06")))
format_time_simple <- function(x){

  # Check if x is of type Date
  is_date <- inherits(x, "Date")

  #get the unique values
  x <- sort(unique(x))

  # format into string
  if (max(x, na.rm = TRUE) == min(x, na.rm = TRUE)) {
    ret <- as.character(x[1])
  } else {
    # Use " to " for dates, "-" for other types
    separator <- if (is_date) " to " else "-"
    ret <- paste0(min(x, na.rm = TRUE), separator, max(x, na.rm = TRUE))
  }

  return(ret)

}

# generate_yaml() ----
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
#' \donttest{
#' data(mtcars)
#' # output to object in memory
#'   check <- generate_yaml(mtcars, schema = "SCH", table = "TBL",
#'   datasource = "R standard mtcars")
#' # output to a file
#'   output_file <- tempfile('output_file', fileext = ".yaml")
#'   generate_yaml(mtcars, outfile = output_file, schema = "SCH", table = "TBL",
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
    message(paste0("\nWarning: You did not enter a datasource for where the underlying data exists on a shared drive. \n",
                   "The YAML file will be created, but the datasource will not be recorded in the header."))
  }

  ## Set up ----
  # identify column type
  temp.vartype <- data.table(varname = names(sapply(mydt, class)), vartype = sapply(mydt, function(x) paste(class(x), collapse = ',')))

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

# get_xwalk ----
#' Load clean geographic crosswalk tables
#' @description
#' This function provides a curated assortment of standardized geographic crosswalks.
#' Though limited in scope, it provides quick and consistent access to many of the
#' standard crosswalks used in APDE. If there is a common crosswalk missing
#' among the options in \code{list_ref_xwalk()}, please let us know by posting a detailed
#' request in a [GitHub issue](https://github.com/PHSKC-APDE/rads/issues/new).
#'
#' If you need less common crosswalks that are not available through this function, please
#' explore the spatial data built into [rads.data](https://github.com/PHSKC-APDE/rads.data),
#' e.g., \code{rads.data::spatial_geocomp_blk10_kps}. These rads.data tables were
#' created by many people over many years so you should expect to invest some time
#' in exploration and data harmonization to prepare your two columns of interest.
#'
#' @param geo1 character vector of length 1 defining one half of the crosswalk
#' desired, e.g., \code{geo1 = 'zip'}
#' @param geo2 character vector of length 1 defining the other  half of the
#' crosswalk desired, e.g., \code{geo1 = 'city'}
#' @details
#' A list of all acceptable geographic pairings can be found by typing
#' \code{list_ref_xwalk()}.
#'
#'Note that the pairings given as arguments to this function are critical but
#' the order is not. In other words, \code{get_xwalk(geo1 = 'zip', geo2 = 'city')}
#' will return the same table as \code{get_xwalk(geo1 = 'city', geo2 = 'zip')}.
#'
#'
#' ## geo definitions
#'
#' * \code{blk1}: 2010 Census Block. 15 digit Census GEOID (e.g., 530330110012006).
#'   * 1-2: State (53 = WA)
#'   * 3-5: County (033 = King County)
#'   * 6-11: Tract (011001)
#'   * 12: Block group (2)
#'   * 12-15: Block (2006)
#' * \code{ccd10}: 2010 Seattle City Council Districts
#' * \code{city}: King County cities
#' * \code{coo10}: 2010 COO places.
#' * \code{hra10}: 2010 Health Reporting Areas
#' * \code{kc}: King County
#' * \code{kccd10}: 2010 King County Council Districts
#' * \code{lgd10}: 2010 WA State legislative districts
#' * \code{puma10}: 2010 Public Use Microdata Areas
#' * \code{region10}: King County regions (North, South, East, & Seattle)
#' * \code{scd10}: 2010 King County school districts
#' * \code{sea10}: Seattle or KC except Seattle
#' * \code{tract10}: 2010 Census Tract. 11 digit Census GEOID.
#' * \code{zip}: Zip codes in King County.
#'   * _Note!_ This is different from the 133 zip
#' codes used with HCA data. To view the latter, please type \code{rads.data::spatial_zip_hca}.
#'
#' ## A note about error propagation!
#' If you're merging the crosswalk table onto line level data, you can use
#' \code{rads::calc}, or \code{data.table}, or whatever package you like
#' for further analysis. However, if you're merging on to pre-aggregated data,
#' to further collapse/aggregate/sum, you'll need to properly account for error
#' propagation. Here is a line of \code{data.table} code as an example:
#' ```
#' DT[, .(estimate = sum(estimate), stderror = sqrt(sum(stderror)^2)), c(group_by_vars)]
#' ```
#'
#' @return a data.table with two columns of geographic identifiers
#' @export
#' @import rads.data
#' @importFrom data.table copy setnames
#' @importFrom utils data
#' @name get_xwalk
#' @examples
#' \donttest{
#'  myxwalk <- get_xwalk(geo1 = 'zip', geo2 = 'city')
#'  myxwalk[]
#' }
get_xwalk <- function(geo1 = NA, geo2 = NA){
  # bindings for data.table/check global variables ----
  ref_get_xwalk <- input <- output <- lgd10 <- scd10 <- region10 <- tract10 <-
    tract10_new <- x <- hra10 <- NULL

  # load xwalk table ----
  data("ref_get_xwalk", envir=environment()) # import ref_get_xwalk from /data as a promise
  geodt <- copy(ref_get_xwalk) # evaluate / import the promise
  geodt <- string_clean(geodt)

  # validate input and output ----
  if(is.null(geo1)){geo1 <- NA}
  if(is.null(geo2)){geo2 <- NA}
  if(!geo1 %in% c(geodt$input, geodt$output)){
    stop("The `geo1` argument is not a valid geography. Please type `list_ref_xwalk` to see all valid values.")
  }
  if(!geo2 %in% c(geodt$input, geodt$output)){
    stop("The `geo1` argument is not a valid geography. Please type `list_ref_xwalk` to see all valid values.")
  }
  geodt.sub <- geodt[input == geo1 & output == geo2]
  if(nrow(geodt.sub) == 0){geodt.sub <- geodt[input == geo2 & output == geo1]}
  if(nrow(geodt.sub) == 0){
    stop("The combination of `geo1` & `geo2` does not exist in the crosswalk reference table. Please type `list_ref_xwalk` to see all valid combinations.")
  }
  if(nrow(geodt.sub) > 1){
    stop("The combination of `geo1` & `geo2` returned more than 1 row in the reference table. Please submit an issue on GitHub.")
  }
  if(nrow(geodt.sub) == 1){
    geodt <- copy(geodt.sub)
  }

  # get crosswalk data ----
  neo <- geodt$object
  xwalkdt <- eval(parse(text = paste0('rads.data::', neo))) #xwalkdt = eval(substitute(rads.data::x, list(x = as.name(neo))))
  string_clean(xwalkdt)
  keepers <- c(geodt$inputvar, geodt$outputvar)
  xwalkdt <- xwalkdt[, (keepers), with = FALSE] # alternative to xwalkdt[, ..keepers]
  setnames(xwalkdt, c(geodt$inputvar, geodt$outputvar), c(geodt$input, geodt$output))

  # clean crosswalk data ----
  xwalkdt <- xwalkdt[!is.na(get(geodt$input)) & !is.na(get(geodt$output))] # drop when either value is missing
  if("lgd10" %in% names(xwalkdt)){xwalkdt[, lgd10 := gsub("Leg Dist ", "", lgd10)]}
  if("scd10" %in% names(xwalkdt)){xwalkdt[, scd10 := gsub(" School District", "", scd10)]}
  if("region10" %in% names(xwalkdt)){xwalkdt[, region10 := gsub("\\b([a-z])", "\\U\\1", tolower(region10), perl = T)]} # ensure first letter capitalized
  if("tract10" %in% names(xwalkdt)){
    xwalkdt[, tract10 := gsub("14000US", "", tract10)]
    xwalkdt[, tract10 := as.numeric(tract10)]
    xwalkdt[, tract10_new := as.character(tract10)]
    xwalkdt[nchar(tract10) == 6, tract10_new := paste0("53033", tract10)]
    xwalkdt[nchar(tract10) < 6, tract10_new := paste0("53033", sprintf("%06i", tract10))]
    xwalkdt[, tract10 := tract10_new]
    xwalkdt[, tract10_new := NULL]
  }

  if('hra10' %in% names(xwalkdt)){
    xwalkdt[hra10 == "Fed Way-Dash Point/Woodmont", hra10 := "Fed Way-Dash Pt"]
    }

  # create informative message ----
  mymessage <- c(paste0("This crosswalk information is pulled from `rads.data::", geodt$object, "`."))
  if(!is.na(geodt$notes)){
    mymessage <- message(c(mymessage, paste0(" Note!! ", geodt$notes)))
  }

  # return object
  message(mymessage)
  return(xwalkdt)
}

# get_ref_pop() ----
#' Load a reference population as a data.table object in memory
#'
#' @param ref_name Character vector of length 1. Loads a reference population identified by list_ref_pop()
#'
#' @return data.table with complete reference population data
#' @export
#' @name get_ref_pop
#' @examples
#' \donttest{
#'  head(get_ref_pop("2000 U.S. Std Population (single ages to 84 - Census P25-1130)"))
#' }
#' @importFrom data.table copy
#' @import rads.data
#'
get_ref_pop <- function(ref_name = NULL){
  #global variables used by data.table declared as NULL here to play nice with devtools::check()
  standard <- agecat <- age_start <- age_end <- pop <- ref_pop_name <- uploaded <- NULL

  ref_single_to_99 <- data.table::copy(rads.data::population_reference_pop_single_age_to_99)
  ref_single_to_84 <- data.table::copy(rads.data::population_reference_pop_single_age_to_84)
  ref_agecat_11 <- data.table::copy(rads.data::population_reference_pop_11_age_groups)
  ref_agecat_18 <- data.table::copy(rads.data::population_reference_pop_18_age_groups)
  ref_agecat_19 <- data.table::copy(rads.data::population_reference_pop_19_age_groups)
  ref_pop_table <- rbind(suppressWarnings(ref_single_to_99[, uploaded := NULL]),
                         suppressWarnings(ref_single_to_84[, uploaded := NULL]),
                         suppressWarnings(ref_agecat_11[, uploaded := NULL]),
                         suppressWarnings(ref_agecat_18[, uploaded := NULL]),
                         suppressWarnings(ref_agecat_19[, uploaded := NULL]))
  ref_pop_table <- ref_pop_table[standard == ref_name, list(agecat, age_start, age_end, pop)]
  if(nrow(ref_pop_table) == 0){stop(strwrap(paste0("`ref_name` ('", ref_name, "') does not refer to a valid standard reference population.
                                                     Type `list_ref_pop()` to get a list of all valid populations."), prefix = " ", initial = ""))}
  ref_pop_table[, ref_pop_name := ref_name]
  return(ref_pop_table)
}

# list_apde_data() ----
#' Returns the list of datasets currently available for analysis in RADS
#'
#' @return Character vector of available datasets.
#' @export
#' @name list_apde_data
#' @examples
#' \donttest{
#'  list_apde_data()
#' }
list_apde_data <- function(){

  ret <- c('birth', 'brfss', 'chars', 'death', 'hys', 'pums')

  return(ret)
}

# list_dataset_columns ----
#' List columns available for analysis in APDE datasets
#'
#' @description
#' Returns the available columns for a specified dataset. This function adapts to
#' different data sources (SQL databases, network files) and handles various
#' dataset-specific requirements like year validation and analytic-ready flags.
#'
#' @param dataset Character vector of length 1. Identifies the dataset to be
#' fetched. Use \code{\link{list_apde_data}} for available options.
#' @param year Year(s) of dataset to check. Only applies to BRFSS, HYS, and PUMS data.
#' For PUMS data, this is limited to a single year (e.g., 2023) or a continuous
#' 5-year period (e.g., 2018:2022). Defaults to \code{year = 2021}.
#' @param mykey Character vector of length 1 OR a database connection. Identifies
#' the \code{keyring::} key that can be used to access the Health & Human Services
#' Analytic Workspace (HHSAW). Defaults to \code{mykey = 'hhsaw'}.
#' @param kingco Logical. Toggle for King County (\code{TRUE}) or WA State
#' (\code{FALSE}) column names. Only applys to BRFSS data. Defaults to
#' \code{kingco = TRUE}.
#' @param analytic_only Logical. Controls whether columns outside the analytic
#' dataset should be returned. Only applies to HYS data. Defaults to
#' \code{analytic_only = FALSE}.
#'
#' @details
#' This function handles multiple data sources with different requirements:
#' \itemize{
#'   \item SQL-based (birth, death, chars): Accessed via HHSAW
#'   \item Network-based (BRFSS, HYS, PUMS): Need appropriate permissions
#' }
#'
#' Network paths required:
#' \itemize{
#'   \item BRFSS (kingco = T): '//dphcifs/APDE-CDIP/BRFSS/prog_all/final_analytic.rds'
#'   \item BRFSS (kingco = F): '//dphcifs/APDE-CDIP/BRFSS/WA/wa_final_analytic.rds'
#'   \item HYS: '//dphcifs/APDE-CDIP/HYS/releases/2021/best/hys_cols.csv'
#'   \item PUMS: '//dphcifs/APDE-CDIP/ACS/PUMS_data/' and subdirectories
#' }
#'
#' @return
#' A \code{data.table} with dataset-specific columns:
#' \itemize{
#'   \item All datasets: 'var.names' (variable names)
#'   \item BRFSS/HYS/PUMS: Additional 'year(s)' column
#'   \item HYS only: 'analytic_ready' flag
#'   \item PUMS only: 'records' indicating household/person level
#' }
#'
#' @export
#' @importFrom data.table data.table setDT
#' @name list_dataset_columns
#'
#' @examples
#' \donttest{
#'  # SQL-based data
#'  list_dataset_columns('birth')
#'  list_dataset_columns('chars', mykey = 'hhsaw')
#'  list_dataset_columns('death', mykey = 'hhsaw')
#'
#'  # Network-based data
#'  list_dataset_columns('hys', year = 2021, analytic_only = TRUE)
#'  list_dataset_columns('brfss', year = 2014:2023)
#'  list_dataset_columns('pums', year = 2018:2022)
#' }
list_dataset_columns <- function(dataset = NULL,
                                 year = 2021,
                                 mykey = 'hhsaw',
                                 kingco = TRUE,
                                 analytic_only = FALSE) {

  # Visible bindings for data.table/check global variables ----
  ar <- colname <- chi_year <- `year(s)` <- NULL

  # Validate inputs ----
  opts <- list_apde_data()
  if(is.null(dataset)){
    stop("\n\U0001f47f The 'dataset' argument cannot be missing. Available options are in `list_apde_data()`.")
  }
  stopifnot('dataset must be a character vector of length 1' = length(dataset) == 1)
  dataset <- tolower(dataset)
  if(!dataset %in% opts){
    stop(paste0('\n\U0001f47f list_dataset_columns functionality for dataset "',
                dataset, '" not currently available/implemented. ',
                "Only the following datasets are implemented: ",
                paste(opts, collapse = ', '), "."))
  }

  # Get configuration for specified dataset ----
  config <- list_dataset_columns_config(dataset)

  # Route to appropriate handler based on dataset type ----
  type_handler <- switch(config$type,
                    "sql" = list_dataset_columns_sql,
                    "brfss" = list_dataset_columns_brfss,
                    "hys" = list_dataset_columns_hys,
                    "pums" = list_dataset_columns_pums,
                    stop("Unknown dataset type"))

  # Process dataset and return results ----
  type_handler(config = config, year = year, mykey = mykey, kingco = kingco, analytic_only = analytic_only)
}

# list_dataset_columns_config ----
#' Configuration settings for APDE datasets
#'
#' @description
#' ___Internal function___ that provides configuration settings for each supported
#' dataset in \code{\link{list_dataset_columns}}. New data sets can be added by
#' including their configuration here.
#'
#' @param dataset Character vector of length 1. Dataset identifier.
#'
#' @return A list containing configuration settings for the specified dataset.
#'
#' @keywords internal
list_dataset_columns_config <- function(dataset) {
  # no need for dataset validation b/c validated in list_dataset_columns()
  configs <- list(
    # SQL-based datasets ----
    birth = list(
      type = "sql",
      query = "SELECT top (0) * FROM [birth].[final_analytic]",
      bonus_vars = NULL
    ),
    death = list(
      type = "sql",
      query = "SELECT top (0) * FROM [death].[final_analytic]",
      bonus_vars = c('wastate', 'age6', 'race3', 'race4', 'bigcities',
                     'hra20_name', 'chi_geo_region')
    ),
    chars = list(
      type = "sql",
      query = "SELECT TOP (0) * FROM [chars].[final_analytic]",
      bonus_vars = c('wastate', 'yage4', 'age6', 'race3', 'race4')
    ),

    # Network-based datasets ----
    brfss = list(
      type = "brfss",
      path = c(KC = "//dphcifs/APDE-CDIP/BRFSS/prog_all/final_analytic.rds",
               WA = "//dphcifs/APDE-CDIP/BRFSS/WA/wa_final_analytic.rds"),
      bonus_vars = c('hra20_id', 'hra20_name', 'chi_geo_region')
    ),
    hys = list(
      type = "hys",
      path = "//dphcifs/APDE-CDIP/HYS/releases/2021/best/hys_cols.csv",
      valid_years = c(seq(2004, 2018, 2), 2021)
    ),
    pums = list(
      type = "pums",
      base_dir = "//dphcifs/APDE-CDIP/ACS/PUMS_data/"
    )
  )

  return(configs[[dataset]])
}

# list_dataset_columns_sql ----
#' Column Type Handler for SQL-based datasets
#'
#' @description
#' ___Internal function___ that processes column names for SQL-based data
#' (birth, death, chars). Adds any 'bonus' variables that will
#' be created by the \code{\link{get_data}} functions. Use by
#' \code{\link{list_dataset_columns}}.
#'
#' @inheritParams list_dataset_columns
#' @param config List of configuration settings for the dataset that are defined
#' by \code{\link{list_dataset_columns_config}}
#'
#' @return data.table with var.names column
#'
#' @keywords internal
list_dataset_columns_sql <- function(config, year, mykey, kingco, analytic_only) {
  # Connect to database and get column names
  con <- validate_hhsaw_key(mykey)
  var.names <- names(DBI::dbGetQuery(con, config$query))

  # Add any bonus variables specific to this dataset
  if(!is.null(config$bonus_vars)) {
    var.names <- tolower(sort(c(var.names, config$bonus_vars)))
  }

  return(data.table(var.names = var.names))
}

# list_dataset_columns_brfss ----
#' Column Type Handler for BRFSS dataset
#'
#' @description
#' ___Internal function___ that processes the column names for the BRFSS data.
#' Used by \code{\link{list_dataset_columns}}.
#'
#' @inheritParams list_dataset_columns_sql
#'
#' @return data.table with var.names and year(s) columns
#'
#' @keywords internal
list_dataset_columns_brfss <- function(config, year, mykey, kingco, analytic_only) {
  # Visible bindings for data.table/check global variables ----
  chi_year <- NULL

  # Validate network path and read data ----
  if(isTRUE(kingco)){config$path <- config$path[['KC']]} else {config$path <- config$path[['WA']]}
  validate_network_path(config$path, is_directory = FALSE)
  dat <- setDT(readRDS(config$path))

  # Check if requested years are available ----
  if(!all(year %in% unique(dat$chi_year))) {
    stop(paste0("Invalid year(s) for BRFSS data. Available years: ",
                format_time(unique(dat$chi_year)), "."))
  }

  # Filter to requested years ----
  dat <- dat[chi_year %in% year]

  # Remove columns that are 100% missing ----
  na_cols <- dat[, which(sapply(.SD, function(x) all(is.na(x)))), .SDcols = names(dat)]
  dat[, (na_cols) := NULL]

  # Get variable names and determine years available for each ----
  var.names <- names(dat)
  var.years <- sapply(var.names, function(var) {
    years_available <- unique(dat[!is.na(get(var)), chi_year])
    format_time(years_available)
  }, simplify = TRUE)

  # Add bonus variables with their years ----
  var.names <- sort(c(var.names, config$bonus_vars))
  var.years <- c(var.years,
                 setNames(rep(format_time(year), length(config$bonus_vars)),
                          config$bonus_vars))

  # return ----
  return(data.table(var.names = var.names, `year(s)` = var.years[var.names]))
}

# list_dataset_columns_hys ----
#' Column Type Handler for HYS dataset
#'
#' @description
#' ___Internal function___ that processes the column names for the HYS data. Used
#' by \code{\link{list_dataset_columns}}.
#'
#' @inheritParams list_dataset_columns_sql
#'
#' @return data.table with var.names, analytic_ready, and year(s) columns
#'
#' @keywords internal
list_dataset_columns_hys <- function(config, year, mykey, kingco, analytic_only) {
  # Visible bindings for data.table/check global variables ----
  ar <- colname <- NULL

  # Validate years against configured valid years
  if(!all(year %in% config$valid_years)) {
    stop("Invalid year(s) for Health Youth Survey data.")
  }

  # Read and process data
  validate_network_path(config$path, is_directory = FALSE)
  dat <- data.table::fread(config$path)
  dat <- dat[year %in% year]

  # Split variables by analytic ready status
  var.names.ar <- dat[ar == TRUE, colname]
  var.names.stg <- if(!analytic_only) dat[ar == FALSE, colname] else NULL
  var.names <- c(var.names.ar, var.names.stg)
  a_r <- c(rep(TRUE, length(var.names.ar)),
           rep(FALSE, length(var.names.stg)))

  return(data.table(var.names = var.names,
                    analytic_ready = a_r,
                    `year(s)` = format_time(year)))
}

# list_dataset_columns_pums ----
#' Column Type Handler for PUMS dataset
#'
#' @description
#' ___Internal function___ that processes column names for the PUMS data. Used
#' by \code{\link{list_dataset_columns}}.
#'
#' @inheritParams list_dataset_columns_sql
#'
#' @return data.table with var.names, records, and year(s) columns
#'
#' @keywords internal
list_dataset_columns_pums <- function(config, year, mykey, kingco, analytic_only) {
  # Visible bindings for data.table/check global variables ----
  varname <- records <- NULL

  # Validate base directory exists
  validate_network_path(config$base_dir, is_directory = TRUE)

  # Get and filter available files
  VNfiles <- list.files(config$base_dir, pattern = 'varnames.rds', recursive = TRUE)
  VNfiles <- grep('experimental', VNfiles, value = TRUE, invert = TRUE)
  VNyears <- gsub('_1_year|_5_year', '', gsub('/.*', '', VNfiles))

  # Determine available year ranges
  maxYear <- max(as.integer(grep("^[0-9]{4}$", VNyears, value = TRUE)))
  minYear <- min(as.integer(grep("^[0-9]{4}$", VNyears, value = TRUE)))
  max5Year <- max(as.integer(substr(grep('_', VNyears, value = TRUE), 1, 4)))
  min5Year <- min(as.integer(substr(grep('_', VNyears, value = TRUE), 1, 4))) - 4

  # Handle year selection and validation
  if(is.null(year)) {
    # Default to most recent year if none specified
    year <- maxYear
    useFile <- grep(paste0(year, '_1_year'), VNfiles, value = TRUE)
  } else {
    # Validate year format
    if(!is.numeric(year) || !all(year == as.integer(year)) || !length(year) %in% c(1, 5)) {
      stop("\n\U1F6D1 `year` must be an integer vector with one value or five continuous values")
    }

    if(length(year) == 1) {
      # Single year validation
      if(year == 2020) {
        stop("\n\U1F6D1 `year` cannot equal 2020 due to COVID-19 pandemic survey disruptions")
      }
      if(year < minYear || year > maxYear) {
        stop("\n\U1F6D1 Single `year` values must be >= ", minYear, " and <= ", maxYear)
      }
      useFile <- grep(paste0(year, '_1_year'), VNfiles, value = TRUE)
    } else {
      # Five year period validation
      if(!all(diff(sort(year)) == 1)) {
        stop("\n\U1F6D1 The `year` values are not continuous.")
      }
      if(min(year) < min5Year || max(year) > max5Year) {
        stop("\n\U1F6D1 Five `year` values must be between ",
             min5Year, ":", min5Year+4, " and ", max5Year-4, ":", max5Year)
      }
      useFile <- grep(paste0(max(year), "_", min(year), "_5_year"),
                      VNfiles, value = TRUE)
    }
  }

  # Read and validate data
  var.names <- readRDS(paste0(config$base_dir, useFile))
  if(!"records" %in% names(var.names)) {
    stop("\n\U1F6D1 PUMS variable names file is missing required 'records' column")
  }

  return(var.names[, list(var.names = varname,
                          records,
                          `year(s)` = format_time(year))])
}

# list_ref_xwalk() ----
#' View table of geographic pairs usable in the get_xwalk() function
#' @description
#' Displays a table of geographic pairings that can be submitted to \code{get_xwalk()}
#' for crosswalk table generation. The numbers in the geographies (e.g.,
#' the \code{10} in \code{hra10}) refer to the vintage, which typically reflects
#' the Census Bureau's decennial updates.
#' @details
#' ## geo definitions
#'
#' * \code{blk1}: 2010 Census Block. 15 digit Census GEOID (e.g., 530330110012006).
#'   * 1-2: State (53 = WA)
#'   * 3-5: County (033 = King County)
#'   * 6-11: Tract (011001)
#'   * 12: Block group (2)
#'   * 12-15: Block (2006)
#' * \code{ccd10}: 2010 Seattle City Council Districts
#' * \code{city}: King County cities
#' * \code{coo10}: 2010 COO places.
#' * \code{hra10}: 2010 Health Reporting Areas
#' * \code{kc}: King County
#' * \code{kccd10}: 2010 King County Council Districts
#' * \code{lgd10}: 2010 WA State legislative districts
#' * \code{puma10}: 2010 Public Use Microdata Areas
#' * \code{region10}: King County regions (North, South, East, & Seattle)
#' * \code{scd10}: 2010 King County school districts
#' * \code{sea}: Seattle or KC except Seattle
#' * \code{tract10}: 2010 Census Tract. 11 digit Census GEOID.
#' * \code{zip}: Zip codes in King County.
#'   * _Note!_ This is different from the 133 zip
#' codes used with HCA data. To view the latter, please type \code{rads.data::spatial_zip_hca}.
#' @return a data.table with two columns (geo1 & geo2), which define the acceptable
#' geographic pairings for get_xwalk
#' @export
#' @import rads.data
#' @importFrom data.table copy
#' @importFrom utils data
#' @name list_ref_xwalk
#' @examples
#' \donttest{
#'  list_ref_xwalk()
#' }
list_ref_xwalk <- function(){
  # bindings for data.table/check global variables ----
  ref_get_xwalk <- input <- output <- '.' <-  NULL
  data("ref_get_xwalk", envir=environment()) # import ref_get_xwalk from /data as a promise
  geodt <- copy(ref_get_xwalk) # evaluate / import the promise
  geodt <- string_clean(geodt)
  geodt <- geodt[, .(geo1 = input, geo2 = output)]
  return(geodt)
}

# list_ref_pop() ----
#' Return vector of all reference populations available in RADS
#'
#' @return Character vector of available reference populations
#' @export
#' @name list_ref_pop
#' @examples
#' \donttest{
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
  ref_agecat_11 <- data.table::copy(rads.data::population_reference_pop_11_age_groups)
  ref_agecat_18 <- data.table::copy(rads.data::population_reference_pop_18_age_groups)
  ref_agecat_19 <- data.table::copy(rads.data::population_reference_pop_19_age_groups)
  ref_pop_table <- unique(rbind(ref_single_to_99[, list(standard)],
                                ref_single_to_84[, list(standard)],
                                ref_agecat_11[, list(standard)],
                                ref_agecat_18[, list(standard)],
                                ref_agecat_19[, list(standard)]))
  setorder(ref_pop_table, standard)
  ref_pop_table <- rbind(ref_pop_table[standard %like% "2000 U.S. Std P"], ref_pop_table[!standard %like% "2000 U.S. Std P"])
  return(ref_pop_table$standard)
}

# lossless_convert() ----
#' Convert the class of a vector to another class -- when possible without
#' introducing additional NAs
#'
#' @description
#' Convert the class of a vector to another class -- when possible without
#' introducing additional NAs. If NAs would be introduced, the original vector
#' will be returned along with a warning so the user knows it has not been
#' converted.
#'
#' @param x vector of indeterminate length and type
#' @param class character vector of length one specifying the preferred new column type (i.e.,
#' 'character', 'numeric', 'integer', or 'factor')
#' @examples
#' \donttest{
#' str(lossless_convert(c('1', '2', '3'), 'integer'))
#' str(lossless_convert(c('one', '2', '3'), 'integer'))
#' str(lossless_convert(c('1', '2', 'three'), 'integer'))
#'
#' str(lossless_convert(c('2020-01-01', '2021-12-31', '2022-02-22'), 'Date'))
#' str(lossless_convert(c('2020-01-01', '2021-12-31', 'z'), 'Date'))
#' str(lossless_convert(c('z', '2020-01-01', '2021-12-31'), 'Date'))
#' }
#'
#' @export
#' @return a vector of the same length as x, but of the new class (when possible)
lossless_convert <- function(x, class) {
  # Validate 'x'
  if (missing(x)) {
    stop("'x', the vector you wish to change, must be specified.")
  }

  # Validate 'class'
  if (missing(class)) {
    stop("'class' must be specified.")
  }

  if (length(class) != 1 || !class %in% c("character", "integer", "numeric", "Date", "POSIXct")) {
    stop("'class' must be one of the following: 'character', 'integer', 'numeric', 'Date', 'POSIXct'")
  }

  # Get the name of x
  x_name <- deparse(substitute(x))
  if (nchar(x_name) > 20) { # If 'x' is a vector rather than a name, use 'x'
    x_name <- "x"
  }

  # Pre-check for Date or POSIXct conversion
  if (class %in% c("Date", "POSIXct")) {
    temp_x <- suppressWarnings(as.character(x))
    if (any(sapply(temp_x, function(x) tryCatch(is.na(as.Date(x)), error = function(e) TRUE)))) {
      message("Conversion of '", x_name, "' to ", class, " would introduce additional NAs. Operation not performed.")
      return(x)
    }
  }

  # Attempt conversion with checks for loss
  original_na_count <- sum(is.na(x))
  converted_x <- tryCatch({
    if (class %in% c("Date", "POSIXct")) {
      # For date types, use already converted temp_x to avoid duplicate conversion
      new_x <- suppressWarnings(if (class == "Date") as.Date(temp_x) else as.POSIXct(temp_x))
    } else {
      # For other types, attempt direct conversion
      new_x <- suppressWarnings(as(x, class))
    }
    if (sum(is.na(new_x)) > original_na_count) {
      message("Conversion of '", x_name, "' to ", class, " would introduce additional NAs. Operation not performed.")
      return(x)
    } else {
      return(new_x)
    }
  }, error = function(e) {
    message("\U0001f47f An unexpected issue occurred during conversion '", x_name, "' to ", class, ": ", e$message)
    return(x)
  })

  return(converted_x)
}

# metrics() ----
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
#' @examples
#' print(metrics())
#' @export
metrics = function(){
  c('total',
    'mean', 'rse',
    'numerator','denominator', 'obs', 'median',
    'unique.time',
    'missing', 'missing.prop',
    'rate', 'ndistinct', 'vcov')
}

# multi_t_test ----
#' Perform t-tests for Multiple Comparisons with Summary Statistics
#'
#' @description
#' This function performs t-tests comparing multiple groups against a reference
#' group using summary statistics. It offers flexibility in the method for
#' calculating degrees of freedom, can estimate sample sizes if they are not
#' provided, and can adjust p-values for multiple comparisons.
#'
#' @details
#' This function conducts t-tests to compare multiple groups against a reference
#' group.
#'
#' The `estimated` degrees of freedom method (Welch's t-test) is generally
#' preferred and is set as the default. However, when sample sizes (`n`) are
#' less than 30, results can be unreliable. When `n` is not specified and
#' `df_method = "estimated"`, the function estimates sample sizes based partly
#' on the distribution of mean values. The quality of these estimates depends on
#' the number of groups (length of the means argument). While the function can
#' estimate sample sizes if not provided, it's always preferable to use actual
#' sample sizes when available to ensure more accurate results.
#'
#' @note This function assumes unequal variances, which is typically more appropriate
#' for comparisons across demographic groups in vital statistics, survey data, and
#' other population-based studies. Equal variances are rarely encountered in such
#' contexts due to inherent differences between subpopulations. If you have the
#' underlying raw data (not just the means and standard errors) and want to
#' perform calculations assuming equal variances or a paired t-test, please
#' refer to \code{\link{t.test}} in the \code{\link{stats}} package.
#'
#' @param means Numeric vector of group means.
#' @param ses Numeric vector of standard errors for each group.
#' @param reference_index Integer indicating the index of the reference group.
#' @param n Optional numeric vector of sample sizes for each group.
#' @param alpha Numeric value for significance level (default is \strong{`0.05`}).
#' @param df_method String specifying the method for calculating degrees of
#' freedom. Options are:
#'    - \strong{`'estimated'`} (Welch–Satterthwaite equation): This method, which
#'    corresponds to Welch's t-test, calculates an approximation of the degrees
#'    of freedom based on the sample variances and sizes. It's particularly
#'    useful when groups have unequal variances and/or unequal sample sizes,
#'    making it generally more reliable than the standard t-test in these
#'    situations. It is a data driven approach and is often preferred due to
#'    balance between Type I Errors (false +) and Type II Errors (false -).
#'    - \strong{`'conservative'`} (df = 2): Uses the minimum possible degrees of
#'    freedom, resulting in the widest confidence intervals (for the difference
#'    in means) and the most conservative (largest) p-values. Reduces Type I
#'    Error (false +) and increases Type II Error (false -).
#'    - \strong{`'moderate'`} (df = k - 1): Uses the number of groups minus 1 as the degrees
#'    of freedom, providing a balance between conservative and liberal approaches.
#'    - \strong{`'liberal'`} (df = Inf): Assumes infinite degrees of freedom, resulting in
#'    the narrowest confidence intervals (for the difference in means) and the
#'    most liberal (smallest) p-values. Increases Type I Error (false +) and
#'    reduces Type II Error (false -).
#'
#' Default is \strong{`'estimated'`}.
#' @param alternative String specifying the alternative hypothesis: \strong{`'two.sided'`}
#' (default), \strong{`'less'`}, or \strong{`'greater'`}. Default is \strong{`'two.sided'`}.
#' @param adjust_method String specifying the method of adjustment for multiple
#' comparisons: \strong{`NULL`}, \strong{`'Holm-Bonferroni'`},
#' \strong{`'Benjamini-Hochberg'`}. Refer to the `holm` and `bh` descriptions
#' in \code{\link{p.adjust}} in the \code{\link{stats}} package for more
#' information. Default is \strong{`NULL`}.
#'
#' @return A data.table containing comparison results with the following columns:
#'   \item{comparison}{String describing the comparison}
#'   \item{diff_means}{Numeric difference in means}
#'   \item{ci_lower}{Numeric lower bound of the confidence interval}
#'   \item{ci_upper}{Numeric upper bound of the confidence interval}
#'   \item{p.value}{Numeric p-value}
#'   \item{significant}{Logical indicating if the result is significant (TRUE if
#'     p-value < alpha, FALSE otherwise)}
#'   \item{t.statistic}{Numeric t-statistic}
#'   \item{df}{Numeric degrees of freedom}
#'   \item{df_method}{String indicating the method used for
#'     calculating degrees of freedom}
#'   \item{adjust_method}{String indicating the method used for multiple
#'     comparisons p.value adjustment (when `adjust_method` is not `NULL`)}
#'
#' @examples
#' # Example 1: Comparing birthweights across different maternal age groups
#' age_groups <- c("18-24", "25-29", "30-34", "35-39", "40+")
#' birthweight_means <- c(3150, 3450, 3400, 3250, 3100)  # in grams
#' birthweight_ses <- c(50, 45, 40, 55, 60)
#' sample_sizes <- c(500, 800, 750, 400, 200)
#' reference_group <- 3  # comparing all groups to the 30-34 age group
#'
#' birthweight_comparison <- multi_t_test(
#'   means = birthweight_means,
#'   ses = birthweight_ses,
#'   reference_index = reference_group,
#'   n = sample_sizes,
#'   df_method = "estimated"
#' )
#'
#' # Add age group labels to the results
#' birthweight_comparison[, Age_Group := age_groups]
#'
#' print(birthweight_comparison)
#'
#' @import data.table
#' @export
multi_t_test <- function(means,
                         ses,
                         reference_index,
                         n = NULL,
                         alpha = 0.05,
                         df_method = "estimated",
                         alternative = "two.sided",
                         adjust_method = NULL) {
  # Bindings for data.table/check global variables ----
  comparison <- p.value <- significant <- NULL

  # Input validation ----
    if (!is.numeric(means) || !is.numeric(ses)) {
      stop("\n\U1F6D1 'means' and 'ses' must be numeric vectors.")
    }

    if (length(means) < 2 || length(ses) < 2) {
      stop("\n\U1F6D1 'means' and 'ses' must have at least two elements.")
    }

    if (any(ses <= 0)) {
      stop("\n\U1F6D1 All values in 'ses' must be positive.")
    }

    if (!is.numeric(reference_index) || length(reference_index) != 1 || reference_index %% 1 != 0) {
      stop("\n\U1F6D1 'reference_index' must be a single integer.")
    }

    if (reference_index < 1 || reference_index > length(means)) {
      stop("\n\U1F6D1 'reference_index' is out of bounds.")
    }

    if (!is.null(n)) {
      if (!is.numeric(n) || any(n <= 0)) {
        stop("\n\U1F6D1 'n' must be a numeric vector of positive values.")
      }
      if (any(n < 30)) {
        warning("\n\U00026A0 Some sample sizes are below 30. ",
                "Results may be unreliable, especially with the 'estimated' df_method. ",
                "Consider using a different df_method if appropriate.")
      }
    }

    if (is.null(alpha)) {
      stop("\n\U1F6D1 'alpha' must be provided as a numeric value between 0 and 1.")
    }

    if (!is.numeric(alpha) || alpha <= 0 || alpha >= 1) {
      stop("\n\U1F6D1 'alpha' must be a numeric value between 0 and 1.")
    }

    if (!df_method %in% c("estimated", "conservative", "moderate", "liberal")) {
      stop("\n\U1F6D1 Invalid df_method. Choose 'estimated', 'conservative', 'moderate', or 'liberal'.")
    }

    if (!alternative %in% c("two.sided", "less", "greater")) {
      stop("\n\U1F6D1 Invalid alternative. Choose 'two.sided', 'less', or 'greater'.")
    }

    if (!is.null(adjust_method) && !adjust_method %in% c("Holm-Bonferroni", "Benjamini-Hochberg")) {
      stop("\n\U1F6D1 Invalid adjust_method. Choose NULL, 'Holm-Bonferroni', or 'Benjamini-Hochberg'.")
    }

  # Check if ses, means, and n (when provided) are of the same length
    if (length(means) != length(ses)) {
      stop("\n\U1F6D1 'means' and 'ses' must have the same length.")
    }

    if (!is.null(n) && length(means) != length(n)) {
      stop("\n\U1F6D1 'n' must have the same length as 'means' and 'ses' when provided.")
    }

  # Number of groups ----
    k <- length(means)

  # Estimate sample sizes if not provided ----
    if (is.null(n) && df_method == "estimated") {
      # Assuming the SEM = SD / sqrt(n), then sqrt(n) = SD / SEM, then n = (SD/SEM)^2
      # and SD ~= (max(means) - min(means)) / 4, because most (~95%) of the data
      # falls within 2 SD of the mean in a normal distribution
      estimated_sd <- (max(means) - min(means)) / 4
      n <- round((estimated_sd / ses)^2)
      warning("\U00026A0 Sample sizes are estimated from standard errors and the range of means.\n",
              "Use with caution. ", "Please provide the sample sizes {`n`} if known.")

      if (k < 10) {
        warning("\n\U00026A0 The number of groups is small (< 10). ",
                "This may affect the reliability of estimated sample sizes.\n",
                "Consider providing actual sample sizes if available.")
      }

      if (any(n < 30)) {
        warning("\n\U00026A0 Some estimated sample sizes are below 30. ",
                "Results may be unreliable, especially with the 'estimated' df_method. ",
                "Consider using actual sample sizes or a different df_method.")
      }

    }

  # Reference mean, SE, and sample size ----
    mean_ref <- means[reference_index]
    se_ref <- ses[reference_index]
    n_ref <- n[reference_index]
    means = means[-reference_index]
    ses = ses[-reference_index]
    n = n[-reference_index]

    diff_means = means - mean_ref
    t_stat = diff_means/sqrt(ses^2 + se_ref^2)

    df <- switch(df_method,
                 "estimated" = (ses^2 + se_ref^2)^2 /
                   ((ses^4 / (n - 1)) + (se_ref^4 / (n_ref - 1))), # Welch–Satterthwaite equation
                 "conservative" = 2,
                 "moderate" = k - 1,
                 "liberal" = Inf
    )

    # Calculate vector of p-values
    p_value <- switch(alternative,
                      "two.sided" = 2 * stats::pt(abs(t_stat), df = df, lower.tail = FALSE), # times 2 bc two tailed
                      "less" = stats::pt(t_stat, df = df, lower.tail = TRUE),
                      "greater" = stats::pt(t_stat, df = df, lower.tail = FALSE)
    )

    # Adjust vector of p-values for multiple comparisons -- if requested
    if (!is.null(adjust_method)) {
      adjusted_p_values <- switch(adjust_method,
                                  "Holm-Bonferroni" = stats::p.adjust(p_value, method = "holm"),
                                  "Benjamini-Hochberg" = stats::p.adjust(p_value, method = "BH")
      )
      p_value <- adjusted_p_values
    }

    # Calculate confidence interval
    ci_margin <- stats::qt(1 - alpha/2, df) * sqrt(ses^2 + se_ref^2)
    ci_lower <- switch(alternative,
                       "two.sided" = diff_means - ci_margin,
                       "less" = -Inf,
                       "greater" = diff_means - ci_margin)
    ci_upper <- switch(alternative,
                       "two.sided" = diff_means + ci_margin,
                       "less" = diff_means + ci_margin,
                       "greater" = Inf)

    #Compile table
    r = data.table::data.table(
      comparison = paste0("Group ", seq_len(length(means)+1)[-reference_index], " vs Reference"),
      diff_means = diff_means,
      ci_lower = ci_lower,
      ci_upper = ci_upper,
      p.value = p_value,
      significant = NA,
      t.statistic = t_stat,
      df = df
    )

    t.results = rbind(r, data.table::data.table(
      comparison = paste0("Group ", reference_index, " - Referent"),
      diff_means = 0,
      ci_lower = NA_real_,
      ci_upper = NA_real_,
      p.value = NA_real_,
      significant = NA,
      t.statistic = NA_real_,
      df = NA_real_
    ))

    data.table::setorder(t.results, comparison)

  # Add significance column & df method----
    t.results[, significant := ifelse(is.na(p.value), NA, p.value < alpha)]
    t.results[, df_method := df_method]

  # Add adjustment method if needed ----
    if (!is.null(adjust_method)){
      t.results[, adjust_method := adjust_method]
    }

  # Return object ----
    return(t.results)
}

# pool_brfss_weights ----
#' Create New BRFSS Survey Weights for Multi-Year Analyses
#'
#' @description
#' Creates a \code{\link[dtsurvey]{dtsurvey}}/data.table object with properly adjusted
#' survey weights for analyzing Behavioral Risk Factor
#' Surveillance System (BRFSS) data across multiple years. The built-in BRFSS
#' survey weight (\code{finalwt1} for King County data and \code{x_llcpwt} for WA
#' State data) is designed for single-year analyses. When
#' analyzing BRFSS data across multiple years, the survey weights must be
#' proportionately down scaled. This function provides three weight adjustment
#' methods.
#'
#' @param ph.data A \code{data.frame}, \code{data.table}, \code{dtsurvey} object
#' or \code{\link[mitools]{imputationList}} containing BRFSS survey data
#' @param years An integer vector specifying which years to include in the weight
#' calculation
#' @param year_var Character string specifying the name of the column containing
#' year values. Defaults to '\code{chi_year}'
#' @param old_wt_var Character string specifying the name of the column
#' containing the single year survey weights. Defaults to '\code{finalwt1}',
#' which is used for King County data.
#' @param new_wt_var Character string specifying the name for the new weight
#' variable to be created
#' @param wt_method Character string specifying the name of the method used
#' to rescale `old_wt_var` to `new_wt_var`. Options include:
#'
#' - '\code{obs}': Rescales weights based on the number of observations per year.
#' This is WA DOH's recommendation
#' - '\code{pop}': Rescales weights by the survey weighted population for each year
#' - '\code{simple}': Rescales weights uniformly by the number of surveys. Use
#' when the survey years have approximately the same sample sizes
#'
#'  Defaults to '\code{obs}'
#' @param strata Character string specifying the name for the strata to be used
#' when survey setting the data. Defaults to '\code{x_ststr}'
#'
#' @details
#' When analyzing multiple years of BRFSS data together, using the original
#' weights would lead to overestimation of population totals, as each year's
#' weights sum to that year's total survey population. This function down scales
#' the weights proportionately within the specified time period(s), ensuring
#' that estimates approximate an average year's population.
#'
#' Note that while \code{\link{get_data_brfss}} automatically creates multi-year
#' weights during the initial data download, you may need to use this function
#' to create new weights when analyzing specific subsets of years. For example,
#' some BRFSS questions are only asked in specific years, requiring custom weights
#' to be calculated for those specific time periods. The original weight
#' will always be saved in order to allow this function to be
#' used repeatedly on the same data set.
#'
#' When aggregating BRFSS data across years, WA DOH recommends including the
#' survey year as an additional stratifying variable. However, the ETL process
#' includes the survey year in '\code{x_ststr}' (
#' \code{d1$x_ststr <- d1$year * 1000000 + d1$x_ststr}) so there is no need to
#' include the survey year on its own.
#'
#' @return
#' For data.frame/data.table input: Returns a survey-weighted
#' \code{\link[dtsurvey]{dtsurvey}}/data.table with
#' an additional column containing the new adjusted weights.
#'
#' For \code{\link[mitools]{imputationList}} input: Returns a new imputationList
#' where each \code{\link[dtsurvey]{dtsurvey}}/data.table is
#' survey-weighted using the new weights.
#'
#' In both cases, the new weight column will be specified by the value of
#' \code{new_wt_var}.
#'
#' @examples
#' \dontrun{
#' # Create weights for analyzing 2017-2022 BRFSS data
#' brfss_data <- pool_brfss_weights(
#'   ph.data = my_brfss_data,
#'   years = 2017:2022,
#'   new_wt_var = "wt_2017_2022"
#' )
#'
#' # Create weights for specific years when a question was asked
#' brfss_data <- pool_brfss_weights(
#'   ph.data = my_brfss_data,
#'   years = c(2017, 2019, 2021),
#'   new_wt_var = "wt_odd_years"
#' )
#' }
#'
#' @import data.table
#' @import dtsurvey
#' @export
#'
pool_brfss_weights <- function(
    ph.data,
    years,
    year_var = 'chi_year',
    old_wt_var = 'finalwt1',
    new_wt_var,
    wt_method = 'obs',
    strata = 'x_ststr') {

  # Visible bindings for data.table/check global variables ----
    wt_adjustment <- miList <- `_id` <- hra20_id <- all_missing <- NULL

  # Set default for return of an imputationList rather than a data.table ----
    miList <- 0

  # Validate arguments ----
    if (missing(ph.data)) {
      stop("\n\U1F6D1 You must specify a dataset (i.e., 'ph.data' argument is required)")
    }

    if (inherits(ph.data, "imputationList")) {
      if (!is.data.frame(ph.data$imputations[[1]])) {
        stop("\n\U1F6D1 If 'ph.data' is an imputation list, it must be a list of data.frames or data.tables")
      }
      ph.data <- as.data.table(ph.data$imputations[[1]])
      miList <- 1
    } else if (is.data.frame(ph.data)) {
      ph.data <- as.data.table(ph.data)
    } else {
      stop("\n\U1F6D1 'ph.data' must be a data.frame, data.table, or mitools imputationList")
    }

    if (!year_var %in% names(ph.data)) {
      stop(sprintf("\n\U1F6D1 Column '%s' not found in dataset", year_var))
    }

    if (!is.integer(ph.data[[year_var]]) &&
        all(ph.data[[year_var]] != as.integer(ph.data[[year_var]])) ) {
      stop(sprintf("\n\U1F6D1 Column '%s' must contain integer values", year_var))
    }

    if (!all(years %in% unique(ph.data[[year_var]]))) {
      missing_years <- years[!years %in% unique(ph.data[[year_var]])]
      stop(sprintf("\n\U1F6D1 The following years are not present in the dataset: %s",
                   format_time(missing_years)))
    }

    if (!old_wt_var %in% names(ph.data)) {
      stop(sprintf("\n\U1F6D1 Weight variable '%s' not found in dataset", old_wt_var))
    }

    if (!is.numeric(ph.data[[old_wt_var]])) {
      stop(sprintf("\n\U1F6D1 Weight variable '%s' must be numeric", old_wt_var))
    }

    if (any(is.na(ph.data[get(year_var) %in% years][[old_wt_var]]))) {
      stop(sprintf("\n\U1F6D1 Missing values found in weight variable '%s' for specified years",
                   old_wt_var))
    }

    if (any(ph.data[get(year_var) %in% years][[old_wt_var]] <= 0)) {
      stop(sprintf("\n\U1F6D1 Weight variable '%s' must contain only positive values", old_wt_var))
    }

    if (new_wt_var %in% names(ph.data)) {
      stop(sprintf("\n\U1F6D1 Column name '%s' already exists in dataset", new_wt_var))
    }

    if (!is.character(wt_method) || length(wt_method) != 1 ||
        is.na(wt_method) || !wt_method %in% c("simple", "obs", "pop")) {
      stop("\n\U1F6D1 'wt_method' must be one of: 'obs', 'pop', or 'simple'")
    }

    if (!all(strata %in% names(ph.data))) {
      stop(sprintf("\n\U1F6D1 Strata variable(s) '%s' not found in dataset",
                   paste(strata[!strata %in% names(ph.data)], collapse = ", ")))
    }

    for (stratum in strata) {
      if (any(is.na(ph.data[get(year_var) %in% years][[stratum]]))) {
        stop(sprintf("\n\U1F6D1 Missing values found in strata variable '%s' for specified years", stratum))
      }
    }

  # Create weight adjustment factors (based on wt_method) ----
    if(wt_method == 'simple'){
      adjustments <- unique(ph.data[get(year_var) %in% years, list(get = get(year_var))])
      adjustments[, wt_adjustment := 1/nrow(adjustments)]
    } else if (wt_method == 'obs'){
      adjustments <- ph.data[get(year_var) %in% years,
                             list(wt_adjustment = .N / nrow(ph.data[get(year_var) %in% years])),
                             list(get(year_var))]
    } else if (wt_method == 'pop'){
      adjustments <- ph.data[get(year_var) %in% years,
                       list(wt_adjustment = sum(get(old_wt_var)) /
                           sum(ph.data[get(year_var) %in% years][[old_wt_var]])),
                       list(get(year_var))]
    }

    setnames(adjustments, 'get', year_var)

    complete_years <- data.table(complete_years = min(ph.data[[year_var]]):max(ph.data[[year_var]]))
    adjustments <- merge(adjustments,
                         complete_years,
                         by.x = c(year_var),
                         by.y = 'complete_years',
                         all = T)
    adjustments[is.na(wt_adjustment), wt_adjustment := 0] # set to zero for years that are not applicable

  # Calculate new weights ----
    ph.data <- merge(ph.data,
                     adjustments,
                     by = c(year_var),
                     all = TRUE)

    ph.data[, c(new_wt_var) := get(old_wt_var) * wt_adjustment]

  # Tidy ----
    # Drop unnecessary columns or those that will be regenterated below
    ph.data[, intersect(c('wt_adjustment', 'hra20_id', 'hra20_name', 'chi_geo_region'), names(ph.data)) := NULL]

    # Remove years where ALL key variables (weights and strata) are 100% missing
    all_missing_years <- ph.data[, list(all_missing = all(
      is.na(get(old_wt_var)) &
        is.na(get(new_wt_var)) &
        Reduce(`&`, lapply(strata, function(strat) is.na(get(strat)))) # annoying but necessary because strata can have length > 1
    )), by = year_var]

    all_missing_years <- all_missing_years[all_missing == TRUE, get(year_var)]

    ph.data <- ph.data[!get(year_var) %in% all_missing_years]

  # Survey set ----
    options(survey.lonely.psu="adjust")

    if('_id' %in% names(ph.data)){
      ph.data[, `_id` := NULL]
    }

    ph.data <- dtsurvey::dtsurvey(DT = ph.data, psu = NULL, weight = new_wt_var, strata = strata)

  # Create imputation list (if needed) ----
    if(miList == 1){
      ph.data <- as_imputed_brfss(ph.data)
    }

  # return ----
    message('Your data was survey set with the following parameters is ready for rads::calc():\n',
            ' - valid years = ', format_time(years), '\n',
            ' - original survey weight = `', old_wt_var, '` \n',
            ' - adjusted survey weight = `', new_wt_var, '` \n',
            ' - strata = `', strata,'`\n')

    return(ph.data)
}


# round2() ----
#' Improved rounding function
#' @param x values to be rounded
#' @param n number of digits
#' @examples
#' # round a decimal
#' round2(12345.6789, 2)
#'
#' # round large numbers
#' round2(12345.6789, -2)
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

# string_clean() ----
#' Clean string & factor columns
#' @param dat name of data.frame or data.table
#' @param stringsAsFactors logical. Specifies whether to convert strings to
#' factors (TRUE) or not (FALSE). Note that columns that were originally factors
#' will always be returned as factors.
#' @description
#' `string_clean` is designed to clean and preprocess strings and factors within a
#' data.frame or data.table after importing from SQL, text files, CSVs, etc. It
#' encodes text to UTF-8, trims and replaces multiple whitespaces, converts blank
#' strings to true NA values, and optionally converts strings factors. The function
#' maintains the original order of columns and leaves numeric and logical columns
#' as they were.
#'
#' @details
#' Depending on the size of the data.frame/data.table, the cleaning
#' process can take a long time.
#'
#' The `string_clean` function modifies objects in place due to the use
#' of data.table's by-reference assignment (e.g., :=). In other words, there is
#' *no need to assign the output*, just
#' type `string_clean(myTable)`.
#' @export
#' @importFrom utf8 utf8_encode
#' @return data.table
#' @examples
#' \donttest{
#' myTable <- data.table::data.table(
#' intcol = as.integer(1, 2, 3),
#' county = c(' King  County ', 'Pierce County', '  Snohomish  county '))
#' myTable[, county_factor := factor(county)]
#' string_clean(myTable, stringsAsFactors = TRUE)
#' print(myTable)
#' }

string_clean <- function(dat = NULL,
                         stringsAsFactors = FALSE) {

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

  # convert factors to strings
    factor.columns <- names(dat)[sapply(dat, is.factor)] # Identify factor columns
    dat[, (factor.columns) := lapply(.SD, as.character), .SDcols = factor.columns]

  string.columns <- which(vapply(dat, is.character, FUN.VALUE = logical(1))) # Identify string columns
  if (length(string.columns) > 0) {
    # Define a custom cleaning function
    clean_string <- function(x) {
      x <- utf8::utf8_encode(x) # Convert encoding to UTF-8
      x <- gsub("[[:space:]]+", " ", x) # Replace common unconventional white spaces to a true white space
      x <- trimws(x, which = "both") # Trim white space to right or left
      x <- gsub("\\s+", " ", x) # Collapse multiple consecutive white spaces into one
      x <- ifelse(nzchar(trimws(x)), x, NA) # Replace blanks (or strings that become blanks after trim) with NA
      return(x)
    }

    # Apply the custom cleaning function to all string columns at once
      dat[, (string.columns) := lapply(.SD, clean_string), .SDcols = string.columns]

    # convert strings to factors
      if (stringsAsFactors == TRUE) {
        dat[, (string.columns) := lapply(.SD, factor), .SDcols = string.columns]
      } else {
        dat[, (factor.columns) := lapply(.SD, factor), .SDcols = factor.columns] # original factors back to factors
      }
  }

  # Reorder table to original column order
  setcolorder(dat, original.order)

  return(dat) # Return cleaned data.table
}

# sql_clean() ----
#' Clean string columns read from SQL
#' @param dat name of data.frame or data.table
#' @param stringsAsFactors logical. Specifies whether to convert strings to factors (TRUE) or not (FALSE)
#' @description
#' \strong{!!!STOP!!! This function has been deprecated!} Please use
#' \link{string_clean} instead.
#' @export
#' @importFrom utf8 utf8_encode
#' @return data.table
sql_clean <- function(dat = NULL, stringsAsFactors = FALSE){
  .Deprecated("string_clean")

  warning("\n\U00026A0 As a courtesy, `sql_clean` remains operational for the time being.\n",
          "Good things don't last forever. \nPlease update your code.",
          immediate. = FALSE)

  string_clean(dat = dat, stringsAsFactors = stringsAsFactors)
}

# std_error() ----
#' Calculate standard error of the mean
#' @param x name of a column in a data.frame/data.table or a vector
#' @export
#' @return numeric
#' @name std_error
#' @source plotrix R package July 11, 2022: \url{https://github.com/plotrix/plotrix/blob/master/R/std_error.R}.
#' @importFrom stats sd
#' @examples
#' \donttest{
#' temp1 <- data.table::data.table(x = c(seq(0, 400, 100), seq(1000, 1800, 200), NA),
#' mygroup = c(rep("A", 5), rep("B", 6))
#' )
#' std_error(c(seq(0, 400, 100), NA)) # expected value for mygroup == A
#' std_error(c(seq(1000, 1800, 200), NA)) # expected value for mygroup == B
#' temp1[, .(sem = std_error(x)), by = 'mygroup'][] # view summary table
#' temp1[, sem := std_error(x), by = 'mygroup'][] # save results in the original
#' }
#'
std_error <- function(x) {
  std_error_simple <- function(x) {
    if (!is.numeric(x)) stop("\n\U1F6D1 Input must be numeric.")
    if (all(is.na(x))) stop("\n\U1F6D1 Input contains only NA values.")
    if (sum(!is.na(x)) < 2) stop("\n\U1F6D1 At least two non-NA values are required to calculate standard error.")

    se <- sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x))) # standard error or mean is sd / sqrt(# samples)

    if (is.nan(se) || is.infinite(se)) {
      warning("\n\U00026A0 Calculation resulted in NaN or Inf. Check your input data.")
    }

    return(se)
  }

  if (is.data.frame(x) || is.matrix(x)) {
    return(apply(x, 2, std_error_simple)) # for data.frames
  } else if (is.list(x)) {
    return(lapply(x, std_error_simple)) # for lists
  } else {
    return(std_error_simple(x)) # for use with vectors
  }
}

# substrRight() ----
#' Substring selection from the right to complement base R substr
#' @param x character
#' @param x.start digit to start (counting from the right)
#' @param x.stop digit to end  (counting from the right)
#' @export
#' @return character vector
#'
#' @examples
#' \donttest{
#' substrRight("Good morning!", 2, 8)
#' }
substrRight <- function(x, x.start, x.stop){
  substr(x, nchar(x)-x.stop+1, nchar(x)-x.start+1)
}

# quiet() ----
#' Silence (i.e., suppress or mute) printed messages from functions
#'
#' @description
#' Silence messages from noisy functions. Optionally silence warning messages
#' too.
#'
#' @param expr the expression that you desire to silence (i.e., the function
#' along with its arguments)
#'
#' @param suppressWarnings a logical (TRUE or FALSE), noting whether you wish
#' to suppress warning messages. The default is `suppressWarnings = FALSE`
#'
#' @return whatever should be returned by the expression that is being silenced
#'
#' @export
#'
#' @keywords quiet quietly silence silent
#'
#' @name quiet
#'
#' @examples
#' \donttest{
#' # Suppresses only messages
#' result <- quiet({
#'   message("This message is silenced")
#'   warning("This warning is shown")
#'   42  # Return a value
#' })
#'
#' # Suppresses both messages and warnings
#' result <- quiet({
#'   message("This message is silenced")
#'   warning("This warning is silenced too")
#'   42  # Return a value
#' }, suppressWarnings = TRUE)
#'
#' }
quiet <- function(expr, suppressWarnings = FALSE) {
  # Evaluate the expression, suppressing messages by default
  if(isFALSE(suppressWarnings)){
    result <- suppressMessages(eval(substitute(expr), envir = parent.frame()))} else {
      result <- suppressWarnings(suppressMessages(eval(substitute(expr), envir = parent.frame())))
    }

  tryCatch({
    result  # Return the evaluated result
  }, error = function(e) {
    stop(e)  # Rethrow errors to interrupt execution
  })
}

# tsql_validate_field_types() ----
#' Validates whether a named vector of TSQL data types is compatible with a
#' data.table
#'
#' \code{tsql_validate_field_types} checks whether a named vector of TSQL data
#' types is compatible with a given data.table that you wish to upload to
#' Microsoft SQL Server. The function does not cover every possible situation!
#' For example, you might want to push your R '`POSIXct`' column to a SQL Server
#' table as an '`nvarchar()`' datatype, but this function will expect you to map
#' it to a more typical data type such as '`datetime`'. Think of this function
#' as a second set of eyes to make sure you didn't do something careless.
#'
#' @param ph.data The name of a single data.table/data.frame to be loaded to SQL
#'   Server.
#' @param field_types A named character vector with the desired TSQL datatypes
#'   for your upload. For example, `c(col1 = 'int', col2 = 'float', col3 =
#'   'date')`. Note that the names in `field_types` must be the same as the
#'   names in `ph.data`. This is often read into memory from a *.yaml file, but
#'   can also be manually created.
#'
#' @name tsql_validate_field_types
#'
#' @details Note that this function may not thoroughly evaluate if the allocated
#' length for character strings, i.e., `nvarchar()` and `varchar()`, is
#' sufficient.
#'
#' Note that this replaces `rads::validate_yaml_data`. To use this function to
#' check the compatibility of field types in a yaml file with your dataset, do
#' the following:
#' 1) load the yaml file, e.g., `yaml <- yaml::read_yaml("X:/code/myyaml.yaml")`
#' 2) unlist the variable descriptions, e.g., `yaml_field_types = unlist(yaml$vars)`
#' 3) use `tsql_validate_field_types`, e.g., `tsql_validate_field_types(ph.data = mydt, field_types = yaml_field_types)`
#'
#' @examples
#' \donttest{
#' # example of a success
#'  library(data.table)
#'  mydt = data.table(col1 = 1:10000L,  # creates integers
#'                    col2 = 1:10000/3) # creates floats
#'  mydt[, col3 := as.Date(Sys.Date()) - col1] # creates dates
#'  mydt[, col4 := as.character(col3)] # create strings
#'  mydt[, col5 := Sys.time()] # create POSIXct
#'
#'  myfieldtypes <- c(col1 = 'int',
#'                    col2 = 'float',
#'                    col3 = 'date',
#'                    col4 = 'nvarchar(255)',
#'                    col5 = 'datetime')
#'
#'  tsql_validate_field_types(ph.data = mydt, field_types = myfieldtypes)
#'
#' }
#'
#' @export
#' @rdname tsql_validate_field_types
#' @import data.table

tsql_validate_field_types <- function(ph.data = NULL,
                                      field_types = NULL) {
  # Visible bindings for data.table/check global variables ----
      Rtypes <- RtypesDT <- TSQLtypesDT <- combotypesDT <- std_type <- NULL
      colname <- size <- value_range <- is_valid <- R_type <- tsql_type <- NULL
      is_compatible <- meets_constraints <- NULL

  # Validate arguments ----
      if (is.null(ph.data)) {
        stop("\n\U1F6D1 You must specify a dataset (i.e., {ph.data} must be defined)")
      }
      if (!is.data.table(ph.data)) {
        if (is.data.frame(ph.data)) {
          setDT(ph.data)
        } else {
          stop("\n\U1F6D1 {ph.data} must be the name of a data.frame or data.table.")
        }
      }

      ph.data = copy(ph.data) # copy ph.data so will not change the underlying file submitted to this function via `set` functions
      setnames(ph.data, tolower(names(ph.data))) # b/c TSQL normally case insensitive re: column names and this function is to validate data types compatability

      if (is.null(field_types) || !(is.character(field_types) && !is.null(names(field_types)) && all(nzchar(names(field_types))))) {
        stop('\n\U1F6D1 {field_types} must specify a named character vector of TSQL data types.')
      }

      if (!identical(sort(tolower(names(ph.data))), sort(tolower(names(field_types))))) {
        stop('\n\U1F6D1 Validation of TSQL data types necessitates exactly one TSQL datatype per column name in {ph.data}.')
      }

  # Define type compatibility and constraints ----
      type_compatibility <- list(
        integer = c("tinyint", "smallint", "int", "bigint", "bit"),
        numeric = c("tinyint", "smallint", "int", "bigint", "decimal", "numeric", "float", "real", "money", "smallmoney"),
        character = c("char", "varchar", "text", "nchar", "nvarchar", "ntext"),
        factor = c("char", "varchar", "text", "nchar", "nvarchar", "ntext"),
        logical = "bit",
        Date = "date",
        POSIXct = c("datetime", "datetime2", "smalldatetime", "datetimeoffset"),
        raw = c("binary", "varbinary", "image")
      )

      type_constraints <- list(
        tinyint = list(min = 0, max = 255),
        smallint = list(min = -32768, max = 32767),
        int = list(min = -2147483648, max = 2147483647),
        bigint = list(min = -9223372036854775808, max = 9223372036854775807)
      )

  # Define helper functions ----
    # Function to extract size from TSQL type ----
        extract_size <- function(type) {
          size <- gsub(".*\\((\\d+)\\).*", "\\1", type)
          if (size == type) NA_integer_ else as.integer(size)
        }

    # Function to check type compatibility ----
        check_compatibility <- function(R_type, tsql_type) {
          compatible_types <- unlist(type_compatibility[R_type])
          tsql_type %in% compatible_types
        }

    # Function to check value constraints ----
        check_constraints <- function(column, tsql_type, size, R_type) {
          if (tsql_type %in% names(type_constraints)) { # check range of vals for variations of INT
            constraints <- type_constraints[[tsql_type]]
            non_na <- !is.na(column)
            all(column[non_na] >= constraints$min & column[non_na] <= constraints$max, na.rm = TRUE) &&
              # Add check for numeric to integer conversion
              if (R_type == "numeric") {
                all(column[non_na] == floor(column[non_na]), na.rm = TRUE)
              } else {
                TRUE # if R_type not numeric, must be integer -- defined in type compatibility
              }
          } else if (tsql_type == "bit" && R_type == "integer") {
            all(column %in% c(0, 1, NA), na.rm = TRUE)
          } else if (tsql_type %in% c("char", "varchar", "nchar", "nvarchar") && !is.na(size)) {
            all(nchar(as.character(column)) <= size, na.rm = TRUE)
          } else {
            TRUE
          }
        }

  # Generate R types data table ----
      RtypesDT <- data.table(
        colname = tolower(names(ph.data)),
        R_type = sapply(ph.data, \(x) class(x)[1]), # keep only first class if there is more than one, e.g., c("POSIXct", "POSIXt")
        key = "colname"
      )
      RtypesDT[R_type %in% c('POSIXt', 'POSIXlt'), R_type := 'POSIXct']

      valid_R_types <- unique(names(type_compatibility))
      if(nrow(RtypesDT[!R_type %in% valid_R_types]) > 0){
        stop(paste0("\n\U1F6D1\U0001f47f The following R classes (column data types) are not recognized: ",
                    paste0(unique(RtypesDT[!R_type %in% valid_R_types]$R_type), collapse = ','),
                    ".\n These data types are not currently supported for TSQL conversion.",
                    " If you think this is a mistake, please submit a GitHub issue."))
      }

  # Generate TSQL types data table ----
      TSQLtypesDT <- data.table(
        colname = tolower(names(field_types)),
        tsql_type = gsub("\\(.*$", "", tolower(field_types)), # drop off (###)
        size = sapply(field_types, extract_size),
        key = "colname"
      )

      valid_tsql_types <- unique(unlist(type_compatibility))
      if(nrow(TSQLtypesDT[!tsql_type %in% valid_tsql_types]) > 0){
        stop(paste0("\n\U1F6D1\U0001f47f The following TSQL field types are not recognized: ",
                    paste0(unique(TSQLtypesDT[!tsql_type %in% valid_tsql_types]$tsql_type), collapse = ','),
                    ".\n If you believe it is valid, please submit a GitHub issue."))
      }

  # Combine R and TSQL type information ----
      combotypesDT <- merge(RtypesDT, TSQLtypesDT, by = "colname")

  # Validate type compatibility and constraints ----
      combotypesDT[, is_compatible := mapply(check_compatibility,
                                             R_type,
                                             tsql_type)]

      combotypesDT[, meets_constraints := mapply(check_constraints,
                                                 ph.data[, .SD, .SDcols = colname],
                                                 tsql_type,
                                                 size,
                                                 R_type)]

      combotypesDT[is.na(meets_constraints), meets_constraints := TRUE]

  # Generate detailed validation results ----
      validation_results <- combotypesDT[, list(
        colname = colname,
        R_type = R_type,
        tsql_type = tsql_type,
        is_valid = is_compatible & meets_constraints,
        issue = fcase(
          !is_compatible, "Incompatible types",
          !meets_constraints & R_type == "numeric" & tsql_type %in% c("tinyint", "smallint", "int", "bigint"),
          "Numeric values cannot be safely converted to integer",
          !meets_constraints, "Fails constraints",
          default = NA_character_
        )
      )]

  # Provide feedback to user ----
      # report if column is all NA
      na_cols <- ph.data[, lapply(.SD, function(x) all(is.na(x))), .SDcols = names(ph.data)]
      na_cols <- names(na_cols)[as.logical(na_cols)]
      if(length(na_cols) > 0){warning('\U00026A0 Validation may be flawed for the following variables because they are 100% missing: ', paste0(na_cols, collapse = ', '))}

      # report back overall validation
      if (all(validation_results$is_valid)) {
        message('\U0001f642 Success! Your desired TSQL data types are suitable for your dataset.')
      } else {
        invalid_columns <- validation_results[is_valid == FALSE]
        stop(paste0(
          '\n\U1F6D1\U0001f47f The following columns in your dataset did not ',
          'align with the proposed TSQL field types:\n',
          paste0(
            "     column: ", invalid_columns$colname,
            ", R Type: ", invalid_columns$R_type,
            ", TSQL Type: ", invalid_columns$tsql_type,
            ", issue: ", invalid_columns$issue,
            collapse = "\n"
          )
        ))
      }

  # Return validation results ----
  return(validation_results)
}


# tsql_chunk_loader() ----
#' Loads large data sets to Microsoft SQL Server (TSQL) in 'chunks'
#'
#' \code{tsql_chunk_loader} divides a data.frame/data.table into smaller tables
#' so it can be easily loaded into SQL. Experience has shown that loading large
#' tables in 'chunks' is less likely to cause errors. It is not needed for small
#' tables which load quickly. For **extremely large** datasets, you will likely
#' want to use another method, perhaps something like
#' [bcp](https://learn.microsoft.com/en-us/sql/tools/bcp-utility?view=sql-server-ver16).
#'
#'
#' @param ph.data The name of a single data.table/data.frame to be loaded to SQL Server
#' @param db_conn The name of the relevant open database connection to SQL Server
#' @param chunk_size The number of rows that you desire to have per upload 'chunk'
#' @param schema_name The name of the schema where you want to write the data
#' @param table_name The name of the table where you want to write the data
#' @param overwrite Do you want to overwrite an existing table? Logical (T|F).
#' Default `overwrite = FALSE`.
#' @param append Do you want to append to an existing table? Logical (T|F).
#' Default `append = TRUE`.
#' @param field_types *Optional!* A named character vector
#' with the desired TSQL datatypes for your upload. For example,
#' `c(col1 = 'int', col2 = 'float', col3 = 'date')`
#' @param validate_field_types Do you want to validate TSQL field types using
#' `rads::tsql_validate_field_types`? Logical (T|F).
#' Default `validate_field_types = TRUE`.
#' @param validate_upload Do you want to validate that all rows have been
#' uploaded? Logical (T|F).
#' Default `validate_upload = TRUE`.
#'
#' @details
#' `overwrite` & `append` are intentionally redundant in order to reduce the risk
#' of accidentally overwriting a table. Note that it is illogical for `overwrite`
#' & `append` to have the same value.
#'
#' The names in `field_types` must be the same as the names in `ph.data`. Also
#' note that `field_types` is only processed when `append = FALSE`. This prevents
#' conflicts with data types in pre-existing SQL tables.
#'
#' `validate_field_types = TRUE` is ignored if the `field_types` argument is not
#' provided.
#'
#' @name tsql_chunk_loader
#'
#' @examples
#' \donttest{
#'  library(data.table)
#'  mydt = data.table(col1 = 1:10000L,  # create integer
#'                    col2 = 1:10000/3) # create float
#'  mydt[, col3 := as.Date(Sys.Date()) - col1] # create date
#'  mydt[, col4 := as.character(col3)] # create string
#'  myfieldtypes <- c(col1 = 'int', col2 = 'float', col3 = 'date', col4 = 'nvarchar(255)')
#'
#'  tsql_chunk_loader(
#'    ph.data = mydt,
#'    db_conn = rads::validate_hhsaw_key(), # connect to Azure 16
#'    chunk_size = 3333,
#'    schema_name = Sys.getenv("USERNAME"),
#'    table_name = 'JustTesting',
#'    overwrite = TRUE,
#'    append = FALSE,
#'    field_types = myfieldtypes,
#'    validate_field_types = TRUE,
#'    validate_upload = TRUE
#'  )
#' }
#'
#' @export
#' @rdname tsql_chunk_loader
#'

tsql_chunk_loader <- function(ph.data = NULL, # R data.frame/data.table
                             db_conn = NULL, # connection name
                             chunk_size = 5000, # of rows of data to load at once
                             schema_name = NULL, # schema name
                             table_name = NULL, # table name
                             overwrite = FALSE, # overwrite?
                             append = TRUE, # append?
                             field_types = NULL,  # want to specify field types?
                             validate_field_types = TRUE, # validate specified field_types
                             validate_upload = TRUE){ # want to validate the upload?
  # Declare local variables used by data.table as NULL here to play nice with devtools::check() ----
    db_conn.name <- queryCount <- finalCount <- uploadedCount <- max.row.num <- NULL
    number.chunks <- starting.row <- ending.row <- NULL

  # Validate arguments ----
    # ph.data
        if(is.null(ph.data)){
          stop("\n\U1F6D1 You must specify a dataset (i.e., {ph.data} must be defined)")
        }
        if(!is.data.table(ph.data)){
          if(is.data.frame(ph.data)){
            setDT(ph.data)
          } else {
            stop(paste0("\n\U1F6D1 {ph.data} must be the name of a data.frame or data.table."))
          }
        }

    # db_conn
        if(is.null(db_conn)){stop('\n\U1F6D1 {db_conn} must be specified.')}
        if(class(db_conn)[1] != 'Microsoft SQL Server'){
          stop('\n\U1F6D1 {db_conn} is not a "Microsoft SQL Server" object.')}
        if(!DBI::dbIsValid(db_conn)){
          stop("\n\U1F6D1 {db_conn} must specify a valid database object. \nIf you are sure that it exists, confirm that it has not been disconnected.")
        }
        db_conn.name <- deparse(substitute(db_conn))

    # chunk_size
        if(chunk_size %% 1 != 0 | !chunk_size %between% c(100, 20000)){
          stop("\n\U1F6D1 {chunk_size} must be an integer between 100 and 20,000.")
        }

    # schema
        if(!is.character(schema_name) | length(schema_name) > 1){
          stop('\n\U1F6D1 {schema_name} must be a quoted name of a single schema, e.g., "ref", "claims", "death", etc.')
        }
        possible.schemas <- DBI::dbGetQuery(db_conn, "SELECT SCHEMA_NAME FROM INFORMATION_SCHEMA.SCHEMATA")[]$SCHEMA_NAME
        if(!schema_name %in% possible.schemas){
          stop(paste0('\n\U1F6D1 The value of {schema_name} (', schema_name, ') is not a valid schema name in {db_conn} (', db_conn.name, ').'))
        }

    # table_name
        if(!is.character(table_name) | length(table_name) > 1){
          stop('\n\U1F6D1 {table_name} must be a quoted name of a single table with your specified schema, e.g., "mytable1", "mytable2", etc.')
        }

    # overwrite
        if(!is.logical(overwrite)){
          stop('\n\U1F6D1 {overwrite} must be specified as a logical (i.e., TRUE, T, FALSE, or F)')
        }

    # append
        if(!is.logical(append)){
          stop('\n\U1F6D1 {append} must be specified as a logical (i.e., TRUE, T, FALSE, or F)')
        }
        if(overwrite == append){
          stop('\n\U1F6D1 {overwrite} & {append} cannot both be set to the same value! \nIf one is TRUE the other must be FALSE.')
        }

    # field_types
        if(append == TRUE){field_types = NULL}
        if(!is.null(field_types) && !identical(sort(names(field_types)), sort(names(ph.data))) ){
          stop('\n\U1F6D1 The names in {field_types} must match the column names in {ph.data}')
        }
        if(!is.null(field_types) && !(is.character(field_types) &&
             !is.null(names(field_types)) &&
             all(nzchar(names(field_types))))){
          stop('\n\U1F6D1 {field_types} is optional, but when provided must specify a named character vector. Please view the help file for details.')
        }

    # validate_upload
        if(!is.logical(validate_upload )){
          stop('\n\U1F6D1 {validate_upload } must be specified as a logical (i.e., TRUE, T, FALSE, or F)')
        }

    # validate_field_types
        if(!is.logical(validate_upload )){
          stop('\n\U1F6D1 {validate_upload } must be specified as a logical (i.e., TRUE, T, FALSE, or F)')
        }
        if(validate_field_types == TRUE & is.null(field_types)){
          validate_field_types = FALSE
        }

  # Validate field types if requested ----
        if(validate_field_types == TRUE){
          tsql_validate_field_types(ph.data = ph.data,
                                    field_types = field_types)
        }

  # Set initial values ----
    max.row.num <- nrow(ph.data)
    number.chunks <-  ceiling(max.row.num/chunk_size) # number of chunks to be uploaded
    starting.row <- 1 # the starting row number for each chunk to be uploaded. Initialize with 1
    ending.row <- chunk_size  # the final row number for each chunk to be uploaded. Initialize with overall chunk size

    if(validate_upload == TRUE){
      if(append == TRUE){
        querycnt <- sprintf("SELECT COUNT(*) as total_rows FROM %s.%s", schema_name, table_name)
        originalCount <- DBI::dbGetQuery(db_conn, querycnt)[]$total_rows
      } else { originalCount <- 0}
    }

  # Drop existing table if requested ----
    if(overwrite == TRUE & append == FALSE){
      DBI::dbGetQuery(conn = db_conn,
                      statement = paste0("IF OBJECT_ID('", schema_name, ".", table_name, "', 'U') IS NOT NULL ",
                                         "DROP TABLE ", schema_name, ".", table_name))
      overwrite = FALSE
      append = TRUE
    }

  # Create loop for appending new data ----
    for(i in 1:number.chunks){
      # counter so we know it is not stuck
        message(paste0(Sys.time(), ": Loading chunk ", format(i, big.mark = ','), " of ", format(number.chunks, big.mark = ','), ": rows ", format(starting.row, big.mark = ','), "-", format(ending.row, big.mark = ',')))

      # load the data chunk into SQL (will try each chunk up to 3 times)
        attempt <- 1 # initializing attempt counter
        while(attempt <= 3){
          tryCatch({
            # try to load to SQL
            if(is.null(field_types)){
              DBI::dbWriteTable(conn = db_conn,
                                name = DBI::Id(schema = schema_name, table = table_name),
                                value = ph.data[starting.row:ending.row,],
                                append = append,
                                row.names = FALSE)
            } else {
              DBI::dbWriteTable(conn = db_conn,
                                name = DBI::Id(schema = schema_name, table = table_name),
                                value = ph.data[starting.row:ending.row,],
                                append = FALSE, # set to false so can use field types
                                row.names = FALSE,
                                field.types = field_types)
              field_types = NULL # Reset field_types after use
            }

            # If operation succeeds, break out of the tryCatch loop
            break
          }, error = function(e){
            # If this was the third attempt, stop the process
            if(attempt == 3){
              stop(paste0('\n\U1F6D1 There have been three failed attempts to load chunk #',
                          format(i, big.mark = ','), '. \nThe script has stopped and you will have to ',
                          'try to correct the problem and run it again.') )
            } else {
              # Print an error message indicating a retry
              message(paste("Attempt", attempt, "failed. Trying again..."))
            }
          })

          # Increment the attempt counter for the next iteration
          attempt <- attempt + 1
        }

      # set the starting and ending rows for the next chunk to be uploaded
        starting.row <- starting.row + chunk_size
        ending.row <- min(starting.row + chunk_size - 1, max.row.num)
    }

  # Print summary of upload ----
    if(validate_upload == TRUE){
      # count rows in SQL
        queryCount <- sprintf("SELECT COUNT(*) as total_rows FROM %s.%s", schema_name, table_name)
        finalCount <- DBI::dbGetQuery(db_conn, queryCount)[]$total_rows

      # how many rows in SQL are new rows
        uploadedCount <- finalCount - originalCount

      # check if all rows of ph.data seem to have been uploaded
        if(uploadedCount == nrow(ph.data)){
          message("\U0001f642 \U0001f389 \U0001f38a \U0001f308 \n Congratulations! All the rows in {ph.data} were successfully uploaded.")
        } else {
          stop(paste0("\n\U1F6D1 \U2620 \U0001f47f \U1F6D1\n",
                      "{ph.data} has ", format(nrow(ph.data), big.mark = ','),
                      " rows, but [", schema_name, '].[', table_name, '] only has ',
                      format(uploadedCount, big.mark = ','), ' new rows.'))
        }
    }

}


# validate_hhsaw_key() ----
#' Validate HHSAW keys and connect (if possible)
#'
#' @description
#' Validates keyring:: `service` name and the corresponding password. If they
#' are valid, it creates a database connection.
#'
#' @param hhsaw_key Character vector of length 1.
#'
#' Identifies the name of the keyring:: `service` that will be used to connect
#' to HHSAW (dbname = hhs_analytics_workspace on server = kcitazrhpasqlprp16).
#'
#' To see the `service` options  you have on your local machine type `keyring::key_list()`
#'
#' Default = 'hhsaw'
#'
#' @return a database connection
#'
#' @import DBI
#' @import keyring
#' @export
#'
#' @examples
#' \donttest{
#'  myConnection <- validate_hhsaw_key(hhsaw_key = 'hhsaw')
#' }

validate_hhsaw_key <- function(hhsaw_key = 'hhsaw'){
  # Key should be a character string that can be used to generate a database connection
  # Also have to allow for the option of interactive authentication
  # TODO: Allow hhsaw_key to be a database connection itself
  is.db = function(x){
    r = try(dbIsValid(hhsaw_key))
    if(inherits(r, 'try-error')){
      r = FALSE
    }
    r
  }

  closeserver = TRUE
  if(is.character(hhsaw_key)){
    server <- grepl('server', tolower(Sys.info()['release']))
    trykey <- try(keyring::key_get(hhsaw_key, keyring::key_list(hhsaw_key)[['username']]), silent = T)
    if (inherits(trykey, "try-error")) stop(paste0("Your hhsaw keyring is not properly configured or you are not connected to the VPN. \n",
                                                   "Please check your VPN connection and or set your keyring and run the function again. \n",
                                                   paste0("e.g., keyring::key_set('hhsaw', username = 'ALastname@kingcounty.gov') \n"),
                                                   "When prompted, be sure to enter the same password that you use to log into to your laptop. \n",
                                                   "If you already have an hhsaw key on your keyring with a different name, you can specify it with the 'mykey = ...' or 'hhsaw_key = ...' argument \n"))
    rm(trykey)

    if(server == FALSE){
      con <- try(con <- DBI::dbConnect(odbc::odbc(),
                                       driver = getOption('rads.odbc_version'),
                                       server = 'kcitazrhpasqlprp16.azds.kingcounty.gov',
                                       database = 'hhs_analytics_workspace',
                                       uid = keyring::key_list(hhsaw_key)[["username"]],
                                       pwd = keyring::key_get(hhsaw_key, keyring::key_list(hhsaw_key)[["username"]]),
                                       Encrypt = 'yes',
                                       TrustServerCertificate = 'yes',
                                       Authentication = 'ActiveDirectoryPassword'), silent = T)
      if (inherits(con, "try-error")) stop(paste("Either your computer is not connected to KC systems (e.g. VPN is not connected), your hhsaw key is not properly configured, and/or your key value is outdated.",
                                                  "To (re)set your hhsaw key use keyring::key_set('", hhsaw_key, "', username = 'ALastname@kingcounty.gov')"),
                                                  "When prompted, be sure to enter the same password that you use to log into to your laptop.")
    }else{
      message(paste0('Please enter the password you use for your laptop into the pop-up window. \n',
                     'Note that the pop-up may be behind your Rstudio session. \n',
                     'You will need to use your two factor authentication app to confirm your KC identity.'))
      con <- DBI::dbConnect(odbc::odbc(),
                            driver = getOption('rads.odbc_version'),
                            server = "kcitazrhpasqlprp16.azds.kingcounty.gov",
                            database = "hhs_analytics_workspace",
                            uid = keyring::key_list(hhsaw_key)[["username"]],
                            Encrypt = "yes",
                            TrustServerCertificate = "yes",
                            Authentication = "ActiveDirectoryInteractive")
    }

    # on.exit(DBI::dbDisconnect(con))

  }else if(is.db(hhsaw_key)){
    closeserver = FALSE
    con = hhsaw_key

  }else{
    stop('`hhsaw_key` is not a reference to database connection or keyring')
  }

  return(con)

}

# validate_network_path ----
#' Validate Network Path Accessibility
#'
#' Check if a network path or file exists and is accessible. Used internally by
#' RADS functions to verify network dependencies before attempting data access.
#'
#' @param path Character string specifying the network path to validate
#' @param is_directory Logical indicating whether path should be a directory (TRUE)
#'        or file (FALSE)
#' @return TRUE if path is accessible, otherwise stops with error
#' @keywords internal
#' @noRd

validate_network_path <- function(path, is_directory = FALSE) {
  if (!base::dir.exists(base::dirname(path))) {
    stop(paste0("\n\U1F6D1 Network path not accessible: ", base::dirname(path),
                "\nPlease verify your network connection and permissions."))
  }

  if (is_directory) {
    if (base::dir.exists(path)) {
      return(TRUE)
    }
    stop(paste0("\n\U1F6D1 Required directory not found: ", path,
                "\nPlease verify your network connection and permissions."))
  } else {
    if (base::file.exists(path)) {
      return(TRUE)
    }
    stop(paste0("\n\U1F6D1 Required file not found: ", path,
                "\nPlease verify your network connection and permissions."))
  }
}

# validate_yaml_data() ----
#' Validate the structure and data types of a data.frame or data.table against
#' specifications defined in a YAML file
#'
#' @description
#' \strong{!!!STOP!!! This function has been deprecated!} Please use
#' \link{tsql_validate_field_types} instead.
#'
#' Validate the structure and data types of a data.frame or data.table against
#' specifications defined in a YAML file. This is most often used to check that
#' your data.frame or data.table is suitable to be pushed to TSQL when a YAML
#' file specifies the field.types to be used by `DBI::dbWriteTable` &
#' `odbc::dbWriteTable`.
#'
#'
#' @param ph.data Name of the data.table/data.frame that you want to assess
#' vis-à-vis the YAML file
#'
#' @param YML Name of the YAML object in memory
#'
#' @param VARS Character vector of length 1. Is is the name of the object in the
#' list contained by YML
#'
#' @importFrom data.table data.table setnames ":=" setDT
#'
#' @examples
#' library(data.table)
#'
#' # create sample data.table
#' mydt <- data.table(myalpha = letters[1:10],
#'                    myint = 1L:10L,
#'                    mynum = seq(.1, 1, .1))
#' mydt[, mydate := as.Date('2000-01-01') + myint]
#' mydt[, myfact := factor(myalpha)]
#'
#' # create sample yaml object
#' myyaml <- list(
#'   myvars = list(
#'     myalpha = "varchar(255)",
#'     myint = "int",
#'     mynum = "float",
#'     mydate = "date",
#'     myfact = "varchar(255)"
#'   )
#' )
#'
#' # use function
#' validate_yaml_data(ph.data = mydt, YML = myyaml, VARS = "myvars")
#'
#' @export
#' @return A simple printed statement, either identifying incompatible column types or a statement of success
validate_yaml_data <- function(ph.data = NULL, YML = NULL, VARS = "vars"){
  ## Global variables used by data.table declared as NULL here to play nice with devtools::check()
  ph.data.class <- orig.ph.dataname <- yamlcols <- yamlnames <- yamlextra <- dfcols <- dfnames <- NULL

  #Deprecation warning
  .Deprecated("tsql_validate_field_types")

  # Get the name of of the data.frame/data.table passed to to the function ----
  orig.ph.dataname <- deparse(substitute(ph.data))

  # Check that DT is a data.frame/data.table ----
  if(is.data.frame(ph.data) == FALSE){
    stop("'ph.data' must be a data.frame or a data.table")
  }else{ph.data <- data.table::setDT(copy(ph.data))}

  # Check that number of vars in YML is same as ncols in ph.data ----
  yamlcols <- length(YML[[VARS]])
  dfcols <- ncol(ph.data)
  if(yamlcols != dfcols){
    stop(paste0("The number of vars specified in the YAML (", yamlcols, ") does not match the number of columns in ph.data (", dfcols, ")"))
  }

  # Check that the column names in YML match those in ph.data ----
  yamlnames <- sort(names(YML[[VARS]]))
  dfnames <- sort(names(ph.data))
  yamlextra <- setdiff(yamlnames, dfnames)
  if(length(yamlextra) == 0){yamlextra <- "_____"}
  dfextra <- setdiff(dfnames, yamlnames)
  if(length(dfextra) == 0){dfextra <- "_____"}
  if(setequal(yamlnames, dfnames)==F){
    stop(paste0("The following variables are in the YAML but not in ph.data: ", yamlextra),
         paste0(". The following variables are in ph.data but not in the YAML: ", dfextra), ".")
  }

  # notice that this might take a while ----
  message("The validation may take a few minutes if your data & yaml contain dates and or times ...")

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
  class.compare[grepl("time", tolower(yaml.class)), yaml.class := "POSIXct"]
  class.compare[grepl("date", tolower(yaml.class)), yaml.class := "date"]

  # identify which VARS should be of which class (assuming YAML is correct) ----
  make.char <- class.compare[yaml.class == "character"]$name
  make.num  <- class.compare[yaml.class == "numeric"]$name
  make.int  <- class.compare[yaml.class == "integer"]$name
  make.logical  <- class.compare[yaml.class == "logical"]$name
  make.POSIXct  <- class.compare[yaml.class == "POSIXct"]$name
  make.Date  <- class.compare[yaml.class == "date"]$name

  # use lossless_convert function to convert R column types if possible / needed ----
  suppressWarnings(ph.data[, (make.char) := lapply(.SD, lossless_convert, class = 'character'), .SDcols = make.char])
  suppressWarnings(ph.data[, (make.num) := lapply(.SD, lossless_convert, class = 'numeric'), .SDcols = make.num])
  suppressWarnings(ph.data[, (make.int) := lapply(.SD, lossless_convert, class = 'integer'), .SDcols = make.int])
  suppressWarnings(ph.data[, (make.logical) := lapply(.SD, lossless_convert, class = 'logical'), .SDcols = make.logical])
  suppressWarnings(ph.data[, (make.Date) := lapply(.SD, lossless_convert, class = 'Date'), .SDcols = make.Date])
  suppressWarnings(ph.data[, (make.POSIXct) := lapply(.SD, lossless_convert, class = 'POSIXct'), .SDcols = make.POSIXct])

  # check if there are variables that could not be coerced to proper type ----
  class.compare <- merge(data.table::data.table(name = names(sapply(ph.data, class)), ph.data.class = tolower(sapply(ph.data, class))),
                         class.compare, by = "name")
  class.compare[ph.data.class == 'c("posixct", "posixt")', ph.data.class := 'POSIXct']

  # allow R to be more strict than YAML with numbers ----
  class.compare[yaml.class=="numeric" & ph.data.class == "integer", ph.data.class := "numeric"]

  # Assess whether there were any problems ----
  if(nrow(class.compare[ph.data.class != yaml.class]) > 0){
    yaml.name <- class.compare[ph.data.class != yaml.class]$name
    yaml.class <- class.compare[ph.data.class != yaml.class]$yaml.class
    class.problems <- paste(paste0(yaml.name, " (", yaml.class, ")"), collapse = ", ")
    stop(glue::glue("\n\U0001f47f The following variables could not be coerced to their proper class (which is specified in parentheses):
                              {class.problems}"))
  }else{message(paste0("\U0001f642 All column classes in `", orig.ph.dataname,"` are compatible with the YAML reference standard."))}

  return(invisible(NULL))

}
