# adjust_direct() ----
#' Calculate crude and directly adjusted rates
#'
#' @param count Numeric vector of indeterminate length. The number of events of interest (e.g., deaths, births, etc.)
#' @param pop Numeric vector of indeterminate length. The population denominator for the count.
#' @param stdpop Numeric vector of indeterminate length. The reference standard population corresponding to the population.
#' @param per Integer vector of length 1. A multiplier for all rates and CI, e.g., when per = 1000, the rates are per 1000 people
#' @param conf.level A numeric vector of length 1. The confidence interval used in the calculations, >0 & <1, typically 0.95
#' @param event_type Character vector of length 1. Either `"unique"` (default) for events
#'   that can only occur once per person (e.g., death, first diagnosis), or `"repeatable"`
#'   for events that can occur multiple times per person (e.g., ER visits, infections).
#'   When `"unique"`, rates are capped at 100%; when "repeatable", rates can exceed 100%.
#'
#' @return A labeled numeric vector containing: count (total events), pop (total population),
#'   crude.rate, crude.lci, crude.uci, adj.rate, adj.lci, adj.uci (rates and CI are scaled by `per`)
#' @export
#' @name adjust_direct
#'
#' @section Data anomalies:
#' When `event_type = "unique"` and `pop < count` (which can occur when small
#' populations are modeled estimates), `adjust_direct()` internally caps the rate
#' at 100% by using the count as the denominator. This ensures logical
#' consistency for events that can only occur once per person.
#'
#' When `pop = 0`, the function sets `pop = 1` to avoid division by zero errors.
#'
#' The original population values are always preserved in the output.
#'
#' @seealso [age_standardize()] for a useful wrapper that calls upon standard reference populations.
#'
#' @examples
#' \donttest{
#' # Unique events (e.g., deaths, first diagnosis)
#' adjust_direct(count = c(11, 9),
#'               pop = c(500, 500),
#'               stdpop = c(500, 900),
#'               per = 100,
#'               conf.level = 0.95,
#'               event_type = "unique")
#'
#' # Repeatable events (e.g., ER visits, infections)
#' adjust_direct(count = c(150, 200),
#'               pop = c(500, 500),
#'               stdpop = c(500, 900),
#'               per = 100,
#'               conf.level = 0.95,
#'               event_type = "repeatable")
#'
#' # Unique events when count > population (caps unique events at 100%)
#' adjust_direct(count = c(25, 30),
#'               pop = c(20, 25),
#'               stdpop = c(500, 900),
#'               per = 100,
#'               conf.level = 0.95,
#'               event_type = "unique")
#'
#' # Repeatable events when count > population (allows > 100%)
#' adjust_direct(count = c(25, 30),
#'               pop = c(20, 25),
#'               stdpop = c(500, 900),
#'               per = 100,
#'               conf.level = 0.95,
#'               event_type = "repeatable")
#'
#' }
#'
adjust_direct <- function(count,
                          pop,
                          stdpop,
                          per = 100000,
                          conf.level = 0.95,
                          event_type = "unique") {
  # adapted from epitools v0.5-10.1 :: ageadjust.direct & survival 3.2-7 :: cipoisson

  # Validate arguments ----
  n_count <- length(count)
  if(n_count != length(pop) || n_count != length(stdpop)) {
    stop("\n\U1F6D1 The length of `count`, `pop`, and `stdpop` must be equal.")
  }

  validation_params <- list(
    count = "counts",
    pop = "populations",
    stdpop = "standard populations"
  )
  for(arg_name in names(validation_params)) {
    arg_value <- get(arg_name)
    context_name <- validation_params[[arg_name]]

    # Check if numeric
    if(!is.numeric(arg_value)) {
      stop(paste0("\n\U1F6D1 The `", arg_name, "` argument must be numeric."))
    }

    # Check for NA values first
    if(any(is.na(arg_value))) {
      na_positions <- which(is.na(arg_value))

      if(length(na_positions) <= 5) {
        positions_text <- paste(na_positions, collapse = ", ")
      } else {
        positions_text <- paste(c(na_positions[1:5], "..."), collapse = ", ")
      }

      stop(paste0("\n\U1F6D1 NA values detected in `", arg_name, "` at position(s): ",
                  positions_text,
                  ".\nAll strata must have valid ", context_name, " for rate calculation."))
    }

    # Check for negative or zero values (no na.rm needed since we confirmed no NAs)
    if(any(arg_value < 0)) {
      stop(paste0("\n\U1F6D1 The `", arg_name, "` argument must not contain negative values."))
    }
  }

  if(sum(stdpop) == 0) {
    stop("\n\U1F6D1 The sum of `stdpop` cannot be zero.")
  }

  if(any(stdpop == 0)) {
    warning("\n\u26A0\ufe0f\u26A0\ufe0f\u26A0\ufe0f At least one stratum of your standard population has a population of 0.\n",
            "In practice this is extremely rare and is likely the result of an error in data preparation.\n",
            "If this was not intentional, correct your data and try again.")
  }

  if(!is.numeric(per) || per <= 0 || per %% 1 != 0) {
    stop("\n\U1F6D1 The `per` argument must be a positive integer, e.g., 100000.")
  }

  if(!is.numeric(conf.level) || conf.level <= 0 || conf.level >= 1) {
    stop("\n\U1F6D1 'conf.level' should be a decimal between 0 and 1")
  }

  if(!event_type %in% c("unique", "repeatable")) {
    stop("\n\U1F6D1 The `event_type` argument must be either 'unique' or 'repeatable'.")
  }

  # Stop when population not usable ----
  if(event_type == "repeatable" && sum(pop) == 0 && any(count > 0)) {
    stop("\n\U1F6D1 Cannot calculate rates when all populations are zero and \n",
         "there are positive counts with event_type = 'repeatable'.\n",
         "This represents undefined division (count/0). Please review your data.")
  }

  # Calculate sums used multiple times ----
  sum_count <- sum(count)
  sum_pop <- sum(pop)
  sum_stdpop <- sum(stdpop)

  # Create pop_calc for calculations to handle when pop < count ----
  if(event_type == "unique") {
    pop_calc <- ifelse(pop < count, count, pop)
  } else {
    pop_calc <- pop
  }

  # Handle zero populations ----
  if(any(pop_calc == 0)) {
    zero_indices <- which(pop_calc == 0)

    for(i in zero_indices) {
      if(count[i] == 0) {
        # When pop = 0 & count = 0, set pop_calc = 1 to get rate = 0/1 = 0
        pop_calc[i] <- 1
      } else {
        # when pop = 0 & count > 0 (only possible when event_type == "repeatable")
        stop("\n\U1F6D1 When event_type = 'repeatable', cannot have pop = 0 with count > 0.\n",
             "Please review your data.")
      }
    }
  }

  # Get sum of pop_calc for crude calculations below
  sum_pop_calc <- sum(pop_calc)

  # Basic calculations ----
  rate <- count/pop_calc
  alpha <- 1 - conf.level
  cruderate <- sum_count/sum_pop_calc
  stdwt <- stdpop/sum_stdpop

  # Calculate exact poisson CI for crude rates ----
  dummycount <- if(sum_count == 0) 1 else sum_count
  crude.lci <- if(sum_count == 0) 0 else stats::qgamma(alpha/2, dummycount)/sum_pop_calc
  crude.uci <- stats::qgamma(1 - alpha/2, sum_count + 1)/sum_pop_calc

  # Calculate exact CI for adjusted rates ----
  dsr <- sum(stdwt * rate)
  dsr.var <- sum((stdwt^2) * (count/pop_calc^2))

  if(dsr == 0 && dsr.var == 0) { # when counts == 0
    gamma.lci <- 0 # Lower should be 0
    # Upper is conservative and weighted -- assume one event in the stratum with maximum stdwt / pop_calc
    wm_local <- max(stdwt / pop_calc)
    gamma.uci <- stats::qgamma(1 - alpha/2, 1) * wm_local
  } else {
    wm <- max(stdwt/pop_calc)

    shape_lower <- (dsr^2)/dsr.var
    scale_lower <- dsr.var/dsr
    gamma.lci <- stats::qgamma(alpha/2, shape = shape_lower, scale = scale_lower)

    shape_upper <- ((dsr + wm)^2)/(dsr.var + wm^2)
    scale_upper <- (dsr.var + wm^2)/(dsr + wm)
    gamma.uci <- stats::qgamma(1 - alpha/2, shape = shape_upper, scale = scale_upper)
  }

  # Prep output ----
  adjusted <- c(count = sum_count,
                pop = sum_pop,
                crude.rate = per * cruderate,
                crude.lci = per * crude.lci,
                crude.uci = per * crude.uci,
                adj.rate = per * dsr,
                adj.lci = per * gamma.lci,
                adj.uci = per * gamma.uci
  )

  # Return output ----
  adjusted
}

# age_standardize() ----
#' Calculate age standardized rates from a data.table with age, counts, and population columns
#'
#' @description
#' Calculate age standardized rates from a data.table with age, counts, and population columns.
#'
#' @param ph.data a data.table or data.frame containing the data to be age-standardized.
#' @param ref.popname Character vector of length 1. Only valid options are those
#' in [list_ref_pop()] and `"none"` (when standard population already exists in ph.data)
#' @param collapse Logical vector of length 1. Do you want to collapse ph.data ages
#' to match those in `ref.popname`?
#' @param my.count Character vector of length 1. Identifies the column with the
#' count data aggregated by the given demographics.
#' @param my.pop Character vector of length 1. Identifies the column with the
#' population corresponding to the given demographics.
#' @param per Integer vector of length 1. A multiplier for all rates and CI, e.g.,
#' when per = 1000, the rates are per 1000 people
#' @param conf.level A numeric vector of length 1. The confidence interval used
#' in the calculations, >0 & <1, typically 0.95
#' @param group_by Character vector of indeterminate length. By which variable(s)
#' do you want to stratify the rate results, if any?
#' @param diagnostic_report Logical vector of length 1. If `group_by` is used and
#' there are groups with missing ages, setting `diagnostic_report = TRUE` returns
#' a diagnostic table instead of normal results. Use this option if a warning about
#' missing age groups appears when running the function normally.
#' @param event_type Character vector of length 1. Either `"unique"` (default) for
#' events that can only occur once per person (e.g., death, first diagnosis), or
#' `"repeatable"` for events that can occur multiple times per person (e.g., ER visits, infections).
#' When `"unique"`, rates are capped at 100%; when "repeatable", rates can exceed 100%.
#'
#' @details
#' `ph.data` must have the following three columns:
#'  - 'age' or 'agecat': 'age' in single years (if collapse = TRUE) or 'agecat'
#'    with the same age bins as your selected reference population (if collapse = FALSE)
#'  - `my.count`: an *aggregated* count for the event (e.g., disease) for which you
#'    want to find an age standardized rate
#'  - `my.pop`: the population corresponding to the age or agecat
#'
#' When analyzing *specific age subsets* (e.g., adolescents, seniors, etc.), include *only* those ages.
#'
#' When analyzing the *whole population*, *include all ages* with zero counts where no
#' events occurred to ensure accurate age-standardized rates.
#'
#' @section Note on Standard Errors:
#' This function calculates confidence intervals using the
#' \href{https://wonder.cdc.gov/controller/pdf/FayFeuerConfidenceIntervals.pdf}{Fay-Feuer method},
#' which does not provide direct standard error (SE) estimates. If you need SE approximations,
#' common methods used by health departments include `SE = adjusted_rate/sqrt(cases)` and
#' `RSE = 1/sqrt(cases)`.
#'
#' @section Data anomalies:
#' When the aggregate event count (`count`) is greater than the corresponding
#' aggregate population (`pop`) AND`event_type = "unique"`, the function issues
#' a warning and internally adjusts the denominator in the [adjust_direct()]
#' function to cap rates at 100%. When `count > pop` AND `event_type = "repeatable"`,
#' no adjustment is made as rates can legitimately exceed 100%.
#'
#' If the population is zero, the function always makes a small adjustment to allow
#' rate calculation. These adjustments ensure stable results and only minimally bias
#' estimates, since such anomalies usually occur in small strata.
#'
#' @return A data.table of the count, rate & adjusted rate with CIs, name of the reference population and the 'group_by' variable(s) -- if any
#'
#' @seealso [adjust_direct()] for calculating crude and directly adjusted rates.
#'
#' @export
#' @name age_standardize
#'
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
                             collapse = TRUE,
                             my.count = "count",
                             my.pop = "pop",
                             per = 100000,
                             conf.level = 0.95,
                             group_by = NULL,
                             diagnostic_report = FALSE,
                             event_type = "unique") {
  # Global variables used by data.table declared as NULL here to play nice with devtools::check() ----
  ph.data.name <- age <- age_start <- age_end <- agecat <- count <- pop <-
    stdpop <- reference_pop <- adj.lci <- adj.uci <- complete <- id <- NULL

  ph.data.name <- deparse(substitute(ph.data))
  ph.data <- copy(ph.data)

  # Logic checks ----
  # Check that ph.data is a data.frame or data.table ----
  if(!inherits(ph.data, "data.frame")){stop("\n\U1F6D1 ph.data must be a data.frame or a data.table containing both counts and population data.")}
  if(!inherits(ph.data, "data.table")){setDT(ph.data)}

  # Check that ph.data has either 'age' or 'agecat' ----
  age_exists <- "age" %in% names(ph.data)

  if (age_exists && (!is.numeric(ph.data$age) || !all(ph.data$age %% 1 == 0))) {
    stop("\n\U1F6D1 The 'age' column must be numeric (whole years) or integer.")
  }

  agecat_exists <- "agecat" %in% names(ph.data)
  if (age_exists && agecat_exists) {
    stop("\n\U1F6D1 Both 'age' and 'agecat' columns are present, but only one is needed.\n",
         " Please check your data and rename or delete one of these variables.")
  } else if (!age_exists && !agecat_exists) {
    stop("\n\U1F6D1 Neither 'age' nor 'agecat' columns are present. Please check your data.")
  } else if (age_exists) {
    if (!is.integer(ph.data$age)) { # already validated that they are whole numbers
      ph.data[, age := as.integer(age)]
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
  if(!my.count %in% colnames(ph.data)){
    stop(paste0("\n\U1F6D1 The column '", my.count, "' does not exist in ph.data.\n",
                "ph.data must have a column indicating the count of events (e.g., deaths, births, etc.) and is typically named 'count'.\n",
                "If such a column exists with a different name, you need to specify it in the `my.count` argument. e.g., my.count = 'deaths'."))
  }

  if(!my.pop %in% colnames(ph.data)){
    stop(paste0("\n\U1F6D1 The column '", my.pop, "' does not exist in ph.data.\n",
                "ph.data must have a column for the population denominator corresponding to the given demographics. It is typically named 'pop'.\n",
                "If such a column exists with a different name, you need to specify it in the `my.pop` argument. e.g., my.pop = 'wapop'."))
  }

  if(!event_type %in% c("unique", "repeatable")) {
    stop("\n\U1F6D1 The `event_type` argument must be either 'unique' or 'repeatable'.")
  }

  if(!is.numeric(per) || per <= 0 || per %% 1 != 0) {
    stop("\n\U1F6D1 The `per` argument must be a positive integer, e.g., 100000.")
  }

  if(!is.numeric(conf.level) || conf.level <= 0 || conf.level >= 1) {
    stop("\n\U1F6D1 'conf.level' should be a decimal between 0 and 1")
  }

  # Check the reference population exists ----
  if(is.null(ref.popname)){ref.popname <- "2000 U.S. Std Population (11 age groups)"}
  if(! ref.popname %in% c( list_ref_pop(), "none")){
    stop(paste0("\n\U1F6D1 ref.popname ('", ref.popname, "') is not a valid reference population name.\n",
                "The names of standardized reference populations can be viewed by typing `list_ref_pop()`.\n",
                "If ph.data is already aggregated/collapsed and has a relevant 'stdpop' column, please set ref.popname = 'none'"))
  }

  if(ref.popname == "none" & !"stdpop" %in% colnames(ph.data)){
    stop("\n\U1F6D1 When specifying ref.popname = 'none', ph.data must have a column named 'stdpop' with the reference standard population data.")}

  if(ref.popname == "none" & collapse == TRUE){
    stop(paste0("\n\U1F6D1 When ref.popname = 'none', collapse should equal FALSE.\n",
                "Selecting ref.popname = 'none' expects that ph.data has already been collapsed/aggregated and has a 'stdpop' column."))
  }

  if (!isTRUE(collapse) && ref.popname != "none" && !"agecat" %in% names(ph.data)) {
    stop("\n\U1F6D1 When collapse = FALSE and ref.popname != 'none', ph.data must contain \n",
         "an 'agecat' column matching the selected reference population.")
  }

  # Standardize column names ----
  # purposefully did not use setnames() because it is possible that count | pop already exists and are intentionally using different columns for this function
  ph.data[, "count" := get(my.count)]
  ph.data[, "pop" := get(my.pop)]

  # Check ranges for age, count, and population ----
  # Check age ----
  if(!"agecat" %in% names(ph.data)){ # if given agecat, ignore these tests for single years of age
    # check for missing ages ----
    if(nrow(ph.data[is.na(age)]) > 0){
      stop(paste0("\n\U1F6D1 ph.data (", ph.data.name, ") contains at least one row where age is missing.\n",
                  "Correct the data and try again."))
    }

    # check for ages > 100 ----
    if(nrow(ph.data[age > 100]) > 0){
      warning(paste0("\n\u26A0\ufe0f ph.data (", ph.data.name, ") contains at least one row where age is greater than 100.\n",
                     "Those values have been recoded to 100 because populations pulled from\n",
                     "get_population() are top coded to 100 and reference populations are usually top coded at 85."),
              immediate. = TRUE, call. = FALSE)
      ph.data[age > 100, age := 100]
    }

    # check for negative ages ----
    if(nrow(ph.data[age < 0]) > 0){
      stop(paste0("\n\U1F6D1 ph.data (", ph.data.name, ") contains at least one row where age is negative.\n",
                  "Correct the data and try again."))
    }

    # Check for full range of ages (0 to 100) overall and within groups ----
    # simple function to check whether all ages are present (T | F) and which ones are missing (if any)
    check_full_age_range <- function(tempx) {
      actual_ages <- sort(unique(tempx$age))
      missing_ages <- setdiff(0:100, actual_ages)
      return(list(full_range = length(missing_ages) == 0, missing = missing_ages))
    }

    # Identify when missing ages
    if (is.null(group_by)) {
      check_result <- check_full_age_range(ph.data)
      if (!check_result$full_range) {
        warning(paste0("\n\u26A0\ufe0f ph.data (", ph.data.name, ") does not include all ages 0-100.\n",
                       "Missing ages: ", format_time(check_result$missing), "\n\n",
                       "This is expected if analyzing a specific age group (e.g., adolescents, working adults, etc.).\n",
                       "However, if analyzing the whole population, missing ages should be added with:\n",
                       "  - count = 0 (if no events occurred)\n",
                       "  - pop = actual population for that age\n\n",
                       "Without complete age data, age-standardized rates may not accurately represent\n",
                       "the total population burden."),
                immediate. = TRUE, call. = FALSE)

      }
    } else {
      age_chk = ph.data[, list(complete = all(0:100 %in% age), missing = list(setdiff(0:100, age))), by = group_by]
      age_chk = age_chk[complete == FALSE][, complete := NULL]
      age_chk[, missing := vapply(missing, function(x) format_time(unlist(x)), character(1))] # better formatting in table of missing

      if(diagnostic_report) {
        message(
          "\U0001F50D Returning diagnostic report showing groups with incomplete age ranges.\n",
          "\u26A0\ufe0f ", format(nrow(age_chk), big.mark = ','),
          " group(s) have missing ages.\n\n",
          "If analyzing specific age subsets, this may be intentional.\n",
          "If analyzing whole populations, add missing ages with count = 0 and appropriate population."
        )
        return(age_chk)
      }

      if (nrow(age_chk) >= 1) {
        if (nrow(age_chk) <= 5) { # detailed table for small number of groups
          table_output <- paste(capture.output(print(age_chk, row.names = FALSE, class = FALSE, print.keys = FALSE)), collapse = "\n")
          groups_display <- paste0(":\n\n", table_output, "\n\n")
          diagnostic_note <- "" # no need to run diagnostic_report
        } else { # just a count for larger number of groups
          groups_display <- paste0(" for ", format(nrow(age_chk), big.mark = ','), " groups.\n\n")
          diagnostic_note <-  "\nRun age_standardize() with `diagnostic_report = TRUE` to see affected groups and ages."
        }

        warning(paste0(
          "\n\u26A0\ufe0f Missing ages detected in ", ph.data.name, groups_display,
          "This is expected if analyzing a specific age group (e.g., adolescents, working adults, etc.).\n",
          "However, if analyzing the whole population, missing ages should be added with:\n",
          "  - count = 0 (if no events occurred)\n",
          "  - pop = actual population for those ages\n\n",
          "Without complete age data, age-standardized rates may not accurately represent\n",
          "the total population burden.", diagnostic_note
        ), immediate. = TRUE, call. = FALSE)
      }
    }
  }

  # Check agecat ----
  if ("agecat" %in% names(ph.data) && ref.popname != "none") {
    if (!identical(sort(unique(ph.data$agecat)),
                   sort(unique(get_ref_pop(ref.popname)[['agecat']])))) {
      stop("\n\U1F6D1 STOP and fix your code!\n",
           "The agecat values in ph.data must match those in your reference \n",
           "population {",ref.popname, "} exactly.")
    }
  }

  # Check count ----
  if(nrow(ph.data[is.na(count)]) > 0){
    warning(paste0("\u26A0\ufe0f ph.data (", ph.data.name, ") contains at least one row where my.count is missing.\n",
                   "Those values have been replaced with zero."))
    ph.data[is.na(count), count := 0]
  }
  if(nrow(ph.data[count < 0]) > 0){
    stop(paste0("\n\U0001f47f ph.data (", ph.data.name, ") contains at least one row where my.count is negative.\n",
                "Correct the data and try again."))
  }

  # Check population ----
  if(nrow(ph.data[is.na(pop)]) > 0){
    stop(paste0("\n\U0001f47f ph.data (", ph.data.name, ") contains at least one row where my.pop is missing.\n",
                "Correct the data and try again."))
  }
  if(nrow(ph.data[pop < 0]) > 0){
    stop(paste0("\n\U0001f47f ph.data (", ph.data.name, ") contains at least one row where my.pop is negative.\n",
                "Correct the data and try again."))
  }

  # Check count vs population ----
  if(nrow(ph.data[count > pop]) > 0 ){
    warning(paste0("\u26A0\ufe0f ph.data (", ph.data.name, ") contains at least one row where the count is greater than the population.\n",
                   "This may be correct because OFM populations are just estimates. However, you are encouraged to check the data."))
  }

  # Check collapse ----
  if(!is.logical(collapse) || length(collapse) != 1) {
    stop("The `collapse` argument must be TRUE or FALSE.")
  }

  # Check diagnostic_report----
  if(!is.logical(diagnostic_report) || length(diagnostic_report) != 1) {
    stop("The `diagnostic_report` argument must be TRUE or FALSE.")
  }


  # Check group_by ----
  if(!is.null(group_by)) {
    if(!is.character(group_by)) {
      stop("The `group_by` argument must be a character vector.")
    }
    missing_cols <- setdiff(group_by, names(ph.data))
    if(length(missing_cols) > 0) {
      stop(paste0("The following `group_by` columns do not exist in ph.data: ",
                  paste(missing_cols, collapse = ", ")))
    }
  }

  # Check that group_by doesn't conflict with required columns
  if(!is.null(group_by)) {
    reserved_cols <- c("age", "agecat", "count", "pop", "stdpop")
    conflicts <- intersect(group_by, reserved_cols)
    if(length(conflicts) > 0) {
      stop(paste0("\n\U1F6D1 The `group_by` argument cannot include reserved column names: ",
                  paste(conflicts, collapse = ", ")))
    }
  }

  # Collapse ph.data to match standard population bins ----
  if(isTRUE(collapse)){
    if(! "age" %in% colnames(ph.data)){
      stop("\n\U1F6D1 When collapse = TRUE, ph.data must have a column named 'age' where age is an integer.\n",
           "This is necessary to generate age bins that align with the selected standard\n",
           "reference population. If ph.data already has an 'agecat' column that is formatted\n",
           "identically to that in the standard reference population, set collapse = FALSE")
    }
    if (!is.numeric(ph.data$age)) {
      stop("\n\U1F6D1 When collapse = TRUE, the 'age' column must be comprised entirely of integers.")
    }
    if (any(ph.data$age %% 1 != 0)) {
      stop("\n\U1F6D1 When collapse = TRUE, the 'age' column must be comprised entirely of integers.")
    }
    if("agecat" %in% colnames(ph.data)){
      stop("\n\U1F6D1 When collapse = TRUE, a new column named 'agecat' is created to match that in the standard reference population.\n",
           "ph.data already has a column named 'agecat' and it will not be automatically overwritten.\n",
           "If you are sure you want to create a new column named 'agecat', delete the existing column in ph.data and run again.")
    }
    my.ref.pop <- get_ref_pop(ref.popname)
    for(z in seq(1, nrow(my.ref.pop))){
      ph.data[age %in% my.ref.pop[z, age_start]:my.ref.pop[z, age_end], agecat := my.ref.pop[z, agecat]]
    }
    if(!is.null(group_by)){ph.data <- ph.data[, list(count = sum(count), pop = sum(pop)), by = c("agecat", group_by)]}
    if(is.null(group_by)){ph.data <- ph.data[, list(count = sum(count), pop = sum(pop)), by = "agecat"]}
  }

  # Warning when pop < count in age collapsed data ----
  if(event_type == "unique" && nrow(ph.data[pop < count]) > 0){
    warning(paste0("\u26A0\ufe0f When ph.data (", ph.data.name, ") was collapsed to match the standard\n",
                   "population, the aggregate `count` was greater than the aggregate `pop` for\n",
                   "the following age group(s): \n",
                   paste(sort(unique(ph.data[pop < count]$agecat)), collapse = ', '), ".\n",
                   "These anomalies are handled internally by adjust_direct(), which uses\n",
                   "adjusted values for rate calculations while preserving the original\n",
                   "population values in the output. This only nominally biases the\n",
                   "calculated rates since the counts are typically small."))
  }

  # Merge standard pop onto count data ----
  if(ref.popname != "none"){
    ph.data <- merge(ph.data, get_ref_pop(ref.popname)[, list(agecat, stdpop = pop)], by = "agecat")
  }

  # Calculate crude & adjusted rates with CI ----
  if(!is.null(group_by)){
    my.rates <- ph.data[, as.list(adjust_direct(count = count, pop = pop, stdpop = stdpop,
                                                conf.level = as.numeric(conf.level), per = per,
                                                event_type = event_type)), by = group_by]
  }
  if(is.null(group_by)){
    my.rates <- ph.data[, as.list(adjust_direct(count = count, pop = pop, stdpop = stdpop,
                                                conf.level = as.numeric(conf.level), per = per,
                                                event_type = event_type))]
  }

  # Tidy results ----
  rate_estimates <- c("crude.rate", "crude.lci", "crude.uci", "adj.rate", "adj.lci", "adj.uci")
  my.rates[, c(rate_estimates) := lapply(.SD, rads::round2, 4), .SDcols = rate_estimates]
  my.rates[, reference_pop := ref.popname]
  my.rates[is.nan(adj.lci) & count == 0, adj.lci := 0]
  if(ref.popname == "none"){my.rates[, reference_pop := paste0("stdpop column in `", ph.data.name, "`")]}

  # Return object ----
  return(my.rates)
}

