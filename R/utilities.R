# calc_age() ----
#' Proper calculation of age in years
#'
#' @param from Vector of dates or characters ("YYYY-MM-DD") of indeterminate length.  vector of length 1.
#' @param to Vector of dates or characters ("YYYY-MM-DD") of indeterminate length.  vector of length 1.
#'
#' @return Integer vector of ages in years.
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
  # validate 'mydt' ----
  if(is.null(mydt)){
    stop("You must specify a dataset (i.e., 'mydt' must be defined)")
  }

  if(!data.table::is.data.table(mydt)){
    if(is.data.frame(mydt)){
      mydt <- data.table::setDT(data.table::copy(mydt))
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

    } else {where = data.table::copy(call[['key_where']])}

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
#' @examples
#' convert_to_date(c("2024-01-01", "February 13, 1999", "2024/02/01",
#'                   "03/21/2000", "05/15/89", "10Sep1998", "10 September 1998",
#'                   "Not date"))
#' convert_to_date(c(42005, 42006), origin = "1899-12-30")
#' convert_to_date(c('puppies', 'kittens'))
#'
#' @export
convert_to_date <- function(x, origin = "1899-12-30") {
  # clean random spaces
  if (is.character(x)) {
    tmp <- data.table::data.table(val = x)
    tmp <- string_clean(tmp)
    x   <- tmp$val
  }

  # validate origin
  if (! grepl("^\\d{4}-\\d{1,2}-\\d{1,2}$", origin) ||
      is.na(as.Date(origin))) {
    stop("Origin date must be in 'YYYY-MM-DD' format, e.g., '1970-01-01'")
  }

  x_name <- deparse(substitute(x))
  x_orig <- data.table::copy(x)

  # early return for Date input
  if (inherits(x, 'Date')) return(x)

  is_yyyymmdd <- grepl("^\\d{8}$", x) & grepl("^(19|20)\\d{2}", x)

  parse_orders <- c(
    "%d%b%Y", "%d-%b-%Y",
    "%d %B, %Y", "%d %B %Y",
    "%Y-%m-%d", "%Y/%m/%d",
    "%m/%d/%Y", "%m-%d-%Y",
    "%B %d, %Y",
    "%Y-%m-%d %H:%M:%S", "%Y/%m/%d %H:%M:%S",
    "%m/%d/%y", "%m-%d-%y",
    "%Y%m%d"
  )

  parse_dates <- function(vec) {
    as.Date(suppressWarnings(
      lubridate::parse_date_time(vec, orders = parse_orders, exact = TRUE)
    ))
  }

  is_serial <- grepl("^\\d+$", x) &
    !is_yyyymmdd &
    !is.na(suppressWarnings(as.numeric(x)))

  date_out <- rep(as.Date(NA), length(x))

  # apply YYYYMMDD conversion first
  if (any(is_yyyymmdd)) {
    date_out[is_yyyymmdd] <- as.Date(x[is_yyyymmdd], format = "%Y%m%d")
  }

  # apply Excel serial conversion

  if (any(is_serial)) {
    max_excel <- 73051   # Jan 1, 2100
    nums <- suppressWarnings(as.numeric(x[is_serial]))
    nums[nums > max_excel] <- NA
    date_out[is_serial] <- as.Date(nums, origin = origin)
  }


  # remaining character dates
  remaining <- !is_serial & !is_yyyymmdd
  if (any(remaining)) {
    date_out[remaining] <- parse_dates(x[remaining])
  }

  # all failed?
  if (all(is.na(date_out))) {
    warning('\u26A0\ufe0f `', x_name, '` cannot be converted to a date.')
  }

  return(date_out)
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

# get_xwalk ----
#' Load clean geographic crosswalk tables
#' @description
#' This function provides a curated assortment of standardized geographic crosswalks.
#' Though limited in scope, it provides quick and consistent access to many of the
#' standard crosswalks used in APDE. If there is a common crosswalk missing
#' among the options in `list_ref_xwalk()`, please let us know by posting a detailed
#' request in a [GitHub issue](https://github.com/PHSKC-APDE/rads/issues/new).
#'
#' If you need less common crosswalks that are not available through this function, please
#' explore the spatial data built into [rads.data](https://github.com/PHSKC-APDE/rads.data),
#' e.g., `rads.data::spatial_geocomp_blk10_kps`. These rads.data tables were
#' created by many people over many years so you should expect to invest some time
#' in exploration and data harmonization to prepare your two columns of interest.
#'
#' @param geo1 character vector of length 1 defining one half of the crosswalk
#' desired, e.g., `geo1 = 'zip'`
#' @param geo2 character vector of length 1 defining the other  half of the
#' crosswalk desired, e.g., `geo1 = 'city'`
#' @details
#' A list of all acceptable geographic pairings can be found by typing
#' `list_ref_xwalk()`.
#'
#'Note that the pairings given as arguments to this function are critical but
#' the order is not. In other words, `get_xwalk(geo1 = 'zip', geo2 = 'city')`
#' will return the same table as `get_xwalk(geo1 = 'city', geo2 = 'zip')`.
#'
#'
#' ## geo definitions
#'
#' * `blk1`: 2010 Census Block. 15 digit Census GEOID (e.g., 530330110012006).
#'   * 1-2: State (53 = WA)
#'   * 3-5: County (033 = King County)
#'   * 6-11: Tract (011001)
#'   * 12: Block group (2)
#'   * 12-15: Block (2006)
#' * `ccd10`: 2010 Seattle City Council Districts
#' * `city`: King County cities
#' * `coo10`: 2010 COO places.
#' * `hra10`: 2010 Health Reporting Areas
#' * `kc`: King County
#' * `kccd10`: 2010 King County Council Districts
#' * `lgd10`: 2010 WA State legislative districts
#' * `puma10`: 2010 Public Use Microdata Areas
#' * `region10`: King County regions (North, South, East, & Seattle)
#' * `scd10`: 2010 King County school districts
#' * `sea10`: Seattle or KC except Seattle
#' * `tract10`: 2010 Census Tract. 11 digit Census GEOID.
#' * `zip`: Zip codes in King County.
#'   * _Note!_ This is different from the 133 zip
#' codes used with HCA data. To view the latter, please type `rads.data::spatial_zip_hca`.
#'
#' ## A note about error propagation!
#' If you're merging the crosswalk table onto line level data, you can use
#' `rads::calc`, or `data.table`, or whatever package you like
#' for further analysis. However, if you're merging on to pre-aggregated data,
#' to further collapse/aggregate/sum, you'll need to properly account for error
#' propagation. Here is a line of `data.table` code as an example:
#' ```
#' DT[, list(estimate = sum(estimate), stderror = sqrt(sum(stderror)^2)), c(group_by_vars)]
#' ```
#'
#' @return a data.table with two columns of geographic identifiers
#' @export
#' @name get_xwalk
#' @examples
#' \donttest{
#'  myxwalk <- get_xwalk(geo1 = 'zip', geo2 = 'city')
#'  myxwalk[]
#' }
get_xwalk <- function(geo1 = NA, geo2 = NA){
  # load xwalk table ----
  utils::data("ref_get_xwalk", envir=environment()) # import ref_get_xwalk from /data as a promise
  geodt <- data.table::copy(ref_get_xwalk) # evaluate / import the promise
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
    geodt <- data.table::copy(geodt.sub)
  }

  # get crosswalk data ----
  neo <- geodt$object
  xwalkdt <- eval(parse(text = paste0('rads.data::', neo))) #xwalkdt = eval(substitute(rads.data::x, list(x = as.name(neo))))
  string_clean(xwalkdt)
  keepers <- c(geodt$inputvar, geodt$outputvar)
  xwalkdt <- xwalkdt[, (keepers), with = FALSE] # alternative to xwalkdt[, ..keepers]
  data.table::setnames(xwalkdt, c(geodt$inputvar, geodt$outputvar), c(geodt$input, geodt$output))

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
#'
get_ref_pop <- function(ref_name = NULL){
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

# list_ref_xwalk() ----
#' View table of geographic pairs usable in the get_xwalk() function
#' @description
#' Displays a table of geographic pairings that can be submitted to `get_xwalk()`
#' for crosswalk table generation. The numbers in the geographies (e.g.,
#' the `10` in `hra10`) refer to the vintage, which typically reflects
#' the Census Bureau's decennial updates.
#' @details
#' ## geo definitions
#'
#' * `blk1`: 2010 Census Block. 15 digit Census GEOID (e.g., 530330110012006).
#'   * 1-2: State (53 = WA)
#'   * 3-5: County (033 = King County)
#'   * 6-11: Tract (011001)
#'   * 12: Block group (2)
#'   * 12-15: Block (2006)
#' * `ccd10`: 2010 Seattle City Council Districts
#' * `city`: King County cities
#' * `coo10`: 2010 COO places.
#' * `hra10`: 2010 Health Reporting Areas
#' * `kc`: King County
#' * `kccd10`: 2010 King County Council Districts
#' * `lgd10`: 2010 WA State legislative districts
#' * `puma10`: 2010 Public Use Microdata Areas
#' * `region10`: King County regions (North, South, East, & Seattle)
#' * `scd10`: 2010 King County school districts
#' * `sea`: Seattle or KC except Seattle
#' * `tract10`: 2010 Census Tract. 11 digit Census GEOID.
#' * `zip`: Zip codes in King County.
#'   * _Note!_ This is different from the 133 zip
#' codes used with HCA data. To view the latter, please type `rads.data::spatial_zip_hca`.
#' @return a data.table with two columns (geo1 & geo2), which define the acceptable
#' geographic pairings for get_xwalk
#' @export
#' @name list_ref_xwalk
#' @examples
#' \donttest{
#'  list_ref_xwalk()
#' }
list_ref_xwalk <- function(){
  utils::data("ref_get_xwalk", envir=environment()) # import ref_get_xwalk from /data as a promise
  geodt <- data.table::copy(ref_get_xwalk) # evaluate / import the promise
  geodt <- string_clean(geodt)
  geodt <- geodt[, list(geo1 = input, geo2 = output)]
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
#'
list_ref_pop <- function(){
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
  data.table::setorder(ref_pop_table, standard)
  ref_pop_table <- rbind(ref_pop_table[grepl("2000 U.S. Std P", standard)],
                         ref_pop_table[!grepl("2000 U.S. Std P", standard)])
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
#' @param class character vector of length one specifying the preferred new column
#' type. Options are limited to 'character', 'Date', 'integer', 'numeric',
#' 'POSIXct', and 'raw'
#' @param column_name optional name of the column being converted (for better error messages)
#'
#' @details
#' For `class = "raw"`, this function enforces a *strictly lossless*
#' conversion:
#' * Input vectors containing any `NA` values are rejected, because
#'   raw vectors cannot represent missing data (`as.raw(NA)` ==
#'   `as.raw(0)`).
#' * All values must be whole numbers in the range 0-255.
#'   Values outside this range or non-integer numerics trigger a
#'   warning and are not converted.
#'
#' @examples
#' \donttest{
#' # Create a bunch of sample vectors
#' alpha <- c('2022-01-01', '2023-01-01', '2024-01-01', '2025-01-01')
#' beta <- c(NA, '2023-01-01', '2024-01-01', '2025-01-01')
#' gamma <- c(NA, 'Not a Date', '2024-01-01', '2025-01-01')
#' delta <- c('Not a Date', '2023-01-01', '2024-01-01', '2025-01-01')
#' epsilon <- c('1', '2', '3', NA)
#' zeta <- c('One', '2', '3', NA)
#' eta <- c('1.1', '2', '3', NA)
#' tau <- c(NA, '2023-01-01 12:30:45', '2024-12-31 23:59:59', '2025-01-01 11:11:11')
#'
#' # Successful Date conversion
#' inherits(lossless_convert(alpha, 'Date'), 'Date')
#'
#' # Failed Date conversion (preserves original)
#' inherits(lossless_convert(gamma, 'Date'), 'character')
#'
#' # Successful integer conversion
#' inherits(lossless_convert(epsilon, 'integer'), 'integer')
#'
#' # Failed integer conversion
#' inherits(lossless_convert(eta, 'integer'), 'character')
#'
#' # Successful POSIXct conversion
#' inherits(lossless_convert(tau, 'POSIXct'), 'POSIXct')
#'
#' # Convert all possible columns in a data.table to numeric
#' library(data.table)
#' mydt <- data.table(alpha, beta, gamma, delta, epsilon, zeta, eta)
#' mydt[, (names(mydt)) := lapply(names(mydt), function(col_name) {
#'   lossless_convert(get(col_name), class = 'numeric', column_name = col_name)
#' })]
#' all.equal(names(mydt)[sapply(mydt, is.numeric)], c('epsilon', 'eta'))
#'
#' # Convert all possible columns in a data.table to Date
#' mydt[, (names(mydt)) := lapply(names(mydt), function(col_name) {
#'   lossless_convert(get(col_name), class = 'Date', column_name = col_name)
#' })]
#' all.equal(names(mydt)[sapply(mydt, function(x) inherits(x, "Date"))], c("alpha", "beta"))
#' }
#'
#' @export
#' @return a vector of the same length as x, but of the new class (when possible)
lossless_convert <- function(x, class, column_name = NULL) {
  # Validation ----
    # Validate 'x'
    if (missing(x)) {
      stop("\n\U1F6D1 'x', the vector you wish to change, must be specified.")
    }

    # Validate 'class'
    if (missing(class)) {
      stop("\n\U1F6D1 'class' must be specified.")
    }

    if (length(class) != 1 || !class %in% c("character", "integer", "numeric", "Date", "POSIXct", "raw")) {
      stop("\n\U1F6D1 'class' must be one of the following: 'character', 'Date', 'integer', 'numeric', 'POSIXct', and 'raw'")
    }

    if (!is.null(column_name) && ((length(column_name) != 1 || !inherits(column_name, 'character')))) {
      stop("\n\U1F6D1 'column_name' must be a character vector of length == 1.")
    }

    if (inherits(x, class)) return(x) # if already the correct class, return the original

  # Set up ----
    # Get the name of x for reporting warnings
    x_name <- if (!is.null(column_name)) {
      column_name
    } else {
      temp <- deparse(substitute(x))
      if (grepl("[\\$\\[\\]\\(\\)\\{\\}]", temp)) { # if it is a not a simple column name, use 'x' as the name
        "x"
      } else {
        temp
      }
    }

    # Get original NA count
    original_na_count <- sum(is.na(x))

    # Create generic warning
    warn_lossy_conversion <- function() {
      message("Conversion of '", x_name, "' to ", class, " would introduce additional NAs. Operation not performed.")
    }

    # Create helper to assess if something is a whole number within a reasonable tolerance
    is.wholenumber <- function(x) {
      abs(x - round(x)) < sqrt(.Machine$double.eps) # a commonly used tolerance ~ 0.00000001490116
    }

  # Simple conversions for empty or 100% NA vectors ----
    # DO NOT INCLUDE 'raw' because as.raw(NA) == as.raw(0), which is not lossless
    if (class != 'raw' & (length(x) == 0 || all(is.na(x)))) {
      return(switch(class,
                    character = as.character(x),
                    numeric   = as.numeric(x),
                    integer   = as.integer(x),
                    Date      = as.Date(x),
                    POSIXct   = as.POSIXct(x)
      ))
    }

  # Attempt less simple class conversions ----
    if (class == "character") {
      new_x <- as.character(x)
      if (sum(is.na(new_x)) > original_na_count) {
        warn_lossy_conversion()
        return(x)
      }
      return(new_x)
    }
    else if (class %in% c("numeric", "integer")) {
      # Convert to numeric first
      numeric_x <- suppressWarnings(as.numeric(x))

      # Check if conversion to numeric introduces NAs
      if (sum(is.na(numeric_x)) > original_na_count) {
        warn_lossy_conversion()
        return(x)
      }

      if (class == "integer") {
        non_na_vals <- numeric_x[!is.na(numeric_x)]

        # check for non whole numbers
        if (any(!is.wholenumber(non_na_vals))) {
          warn_lossy_conversion()
          return(x)
        }

        # check for integers that are too large or too small for R (overflow)
        if (any(non_na_vals > .Machine$integer.max | non_na_vals < -(.Machine$integer.max + 1))) {
          warn_lossy_conversion()
          return(x)
        }

      }

      # If we've reached this point, conversion should be safe
      return(if (class == "numeric") numeric_x else as.integer(numeric_x))
    }
    else if (class %in% c("Date", "POSIXct")) {
      # Get the first non-NA value
      first_non_na <- x[!is.na(x)][1]

      # Try converting the first non-NA value to check for immediate errors
      # Necessary because non-convertible value in first position will cause an error
      # e.g., as.Date(c('Not a Date', '2025-01-01')) # ERROR, but as.Date(c('2025-01-01', 'Not a Date')) # gives an NA
      first_converted <- tryCatch({
        if (class == "Date") {
          as.Date(as.character(first_non_na))
        } else {
          as.POSIXct(as.character(first_non_na))
        }
      }, error = function(e) {
        return(NA)
      }, warning = function(w) {
        return(NA)
      })

      # If first value couldn't be converted, we know the conversion is lossy
      if (is.na(first_converted)) {
        warn_lossy_conversion()
        return(x)
      }

      # If first value worked, convert the whole vector
      new_x <- suppressWarnings({
        if (class == "Date") {
          as.Date(as.character(x))
        } else {
          as.POSIXct(as.character(x))
        }
      })

      # Check if any new NAs were introduced
      if (sum(is.na(new_x)) > original_na_count) {
        warn_lossy_conversion()
        return(x)
      }

      return(new_x)
    }
    else if (class == "raw") {
      # Raw vectors cannot represent NA values - they get converted to 00
      # e.g., identical(as.raw(NA), as.raw(0)) == TRUE
      if (original_na_count > 0) {
        warn_lossy_conversion()
        return(x)
      }

      # First convert to numeric to check for conversion issues
      numeric_x <- suppressWarnings(as.numeric(x))

      # Check if conversion to numeric introduces NAs beyond original
      if (sum(is.na(numeric_x)) > original_na_count) {
        warn_lossy_conversion()
        return(x)
      }

      non_na_vals <- numeric_x[!is.na(numeric_x)]

      # Check if any values are not whole numbers (would be truncated)
      if (any(!is.wholenumber(non_na_vals))) {
        warn_lossy_conversion()
        return(x)
      }

      # Check if any values are outside valid raw range (0-255)
      # as.raw() would silently convert these to 0, which is lossy
      if (any(non_na_vals < 0 | non_na_vals > 255)) {
        warn_lossy_conversion()
        return(x)
      }

      # Safe to convert - as.raw() will preserve NAs and valid values
      return(suppressWarnings(as.raw(as.integer(numeric_x))))
    }
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
#' refer to [stats::t.test()].
#'
#' @param means Numeric vector of group means.
#' @param ses Numeric vector of standard errors for each group.
#' @param reference_index Integer indicating the index of the reference group.
#' @param n Optional numeric vector of sample sizes for each group.
#' @param alpha Numeric value for significance level (default is **`0.05`**).
#' @param df_method String specifying the method for calculating degrees of
#' freedom. Options are:
#'    - **`'estimated'`** (Welch-Satterthwaite equation): This method, which
#'    corresponds to Welch's t-test, calculates an approximation of the degrees
#'    of freedom based on the sample variances and sizes. It's particularly
#'    useful when groups have unequal variances and/or unequal sample sizes,
#'    making it generally more reliable than the standard t-test in these
#'    situations. It is a data driven approach and is often preferred due to
#'    balance between Type I Errors (false +) and Type II Errors (false -).
#'    - **`'conservative'`** (df = 2): Uses the minimum possible degrees of
#'    freedom, resulting in the widest confidence intervals (for the difference
#'    in means) and the most conservative (largest) p-values. Reduces Type I
#'    Error (false +) and increases Type II Error (false -).
#'    - **`'moderate'`** (df = k - 1): Uses the number of groups minus 1 as the degrees
#'    of freedom, providing a balance between conservative and liberal approaches.
#'    - **`'liberal'`** (df = Inf): Assumes infinite degrees of freedom, resulting in
#'    the narrowest confidence intervals (for the difference in means) and the
#'    most liberal (smallest) p-values. Increases Type I Error (false +) and
#'    reduces Type II Error (false -).
#'
#' Default is **`'estimated'`**.
#' @param alternative String specifying the alternative hypothesis: **`'two.sided'`**
#' (default), **`'less'`**, or **`'greater'`**. Default is **`'two.sided'`**.
#' @param adjust_method String specifying the method of adjustment for multiple
#' comparisons: **`NULL`**, **`'Holm-Bonferroni'`**,
#' **`'Benjamini-Hochberg'`**. Refer to the `holm` and `bh` descriptions
#' in [stats::p.adjust()] for more information. Default is **`NULL`**.
#'
#' @return A data.table containing comparison results with the following columns:
#' - `comparison`: String describing the comparison
#' - `diff_means`: Numeric difference in means
#' - `ci_lower`: Numeric lower bound of the confidence interval
#' - `ci_upper`: Numeric upper bound of the confidence interval
#' - `p.value`: Numeric p-value
#' - `significant`: Logical indicating if the result is significant (TRUE if
#'   p-value < alpha, FALSE otherwise)
#' - `t.statistic`: Numeric t-statistic
#' - `df`: Numeric degrees of freedom
#' - `df_method`: String indicating the method used for
#'   calculating degrees of freedom
#' - `adjust_method`: String indicating the method used for multiple
#'   comparisons p.value adjustment (when `adjust_method` is not `NULL`)
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
#' @seealso [`propagate_uncertainty()`] for more robust uncertainty
#'   propagation when comparing two estimates with potentially asymmetric
#'   confidence intervals or non-normal distributions.
#' @export
multi_t_test <- function(means,
                         ses,
                         reference_index,
                         n = NULL,
                         alpha = 0.05,
                         df_method = "estimated",
                         alternative = "two.sided",
                         adjust_method = NULL) {
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
        warning("\n\u26A0\ufe0f Some sample sizes are below 30. ",
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
      warning("\u26A0\ufe0f Sample sizes are estimated from standard errors and the range of means.\n",
              "Use with caution. ", "Please provide the sample sizes {`n`} if known.")

      if (k < 10) {
        warning("\n\u26A0\ufe0f The number of groups is small (< 10). ",
                "This may affect the reliability of estimated sample sizes.\n",
                "Consider providing actual sample sizes if available.")
      }

      if (any(n < 30)) {
        warning("\n\u26A0\ufe0f Some estimated sample sizes are below 30. ",
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
                   ((ses^4 / (n - 1)) + (se_ref^4 / (n_ref - 1))), # Welch-Satterthwaite equation
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
      comparison = paste0("Group ", reference_index, " (Reference)"),
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
#' @param ph.data name of data.frame or data.table
#' @param stringsAsFactors logical. Specifies whether to convert strings to
#' factors (TRUE) or not (FALSE). Note that columns that were originally factors
#' will always be returned as factors. Default `stringsAsFactors = FALSE`.
#' @param  convert_to_utf8 logical. Specifies whether to convert character strings
#' to UTF-8 encoding. UTF-8 ensures consistent handling of international characters
#' and special symbols across different systems and prevents display/processing
#' errors from incompatible character encodings. If you have a few extra minutes
#' to spare, `convert_to_utf8 = TRUE` is recommended. Default
#' `convert_to_utf8 = FALSE`.
#' @description
#' `string_clean` is designed to clean and preprocess strings and factors within a
#' data.frame or data.table after importing from SQL, text files, CSVs, etc. It
#' removes zero-width and invisible characters, normalizes all white spaces,
#' replaces multiple white spaces with a single white space, trims beginning and
#' ending white spaces, converts empty strings to true `NA` and optionally
#' encodes text to UTF-8 and strings as factors. The function maintains the
#' original order of columns and leaves numeric and logical columns as they were.
#'
#' @details
#' Depending on the size of the data.frame/data.table, the cleaning
#' process can take a long time.
#'
#' If you want a more thorough cleaning or if your
#' data have international characters or special symbols, you are encouraged to
#' set `convert_to_utf8 = TRUE`.
#'
#' The `string_clean` function modifies objects in place due to the use
#' of data.table's by-reference assignment (e.g., `:=`). In other words, there is
#' *no need to assign the output*, just
#' type `string_clean(myTable)`.
#'
#' @usage string_clean(ph.data = NULL,
#'              stringsAsFactors = FALSE,
#'              convert_to_utf8 = FALSE)
#' @export
#' @return A modified data.table, invisibly.
#' @examples
#' \donttest{
#' myTable <- data.table::data.table(
#' intcol = as.integer(c(1, 2, 3)),
#' county = c(' King  County ', 'Pierce County', '  Snohomish  county '))
#' myTable[, county_factor := factor(county)]
#' string_clean(myTable, stringsAsFactors = TRUE)
#' print(myTable)
#' }
#'
string_clean <- function (ph.data = NULL,
                          stringsAsFactors = FALSE,
                          convert_to_utf8 = FALSE) {
  # validation
  if (is.null(ph.data) || !is.data.frame(ph.data)) {
    stop("'ph.data' must be the name of a data.frame or data.table")
  }
  if (!data.table::is.data.table(ph.data)) data.table::setDT(ph.data)
  if(!is.logical(stringsAsFactors)){
    stop('\n\U1F6D1 stringsAsFactors must be specified as a logical (i.e., TRUE, T, FALSE, or F)')
  }
  if(!is.logical(convert_to_utf8)){
    stop('\n\U1F6D1 convert_to_utf8 must be specified as a logical (i.e., TRUE, T, FALSE, or F)')
  }

  # Save original column order
  original.order <- names(ph.data)

  # Get column types
  col_types <- vapply(ph.data, function(x) {
    if (is.factor(x)) return("factor")
    if (is.character(x)) return("character")
    return("other")
  }, character(1))
  factor.columns <- names(col_types[col_types == "factor"])
  string.columns <- names(col_types[col_types == "character"])

  # Cleaning helper function
  clean_vec <- function(x) {
    # Basic UTF-8 normalization
    x <- iconv(x, from = "", to = "UTF-8", sub = "byte")

    if (convert_to_utf8) {
      x <- utf8::utf8_encode(x)  # Thorough check, but slow
    } else {
      x <- enc2utf8(x)  # Quick pass
    }

    x <- tryCatch({
      # Step 1: Remove zero-width and invisible characters
      x <- gsub("[\u200B\u200C\u200D\uFEFF]", "", x)

      # Step 2: Normalize all sorts of whitespace to regular space
      x <- gsub("[\u00A0\u2000-\u200A\u2028\u2029\u202F\u205F\u3000[:space:]]+", " ", x)

      # Step 3: Trim leading/trailing space
      trimws(x)
    }, error = function(e) {
      result <- character(length(x))
      for (i in seq_along(x)) {
        result[i] <- tryCatch({
          xi <- gsub("[\u200B\u200C\u200D\uFEFF]", "", x[i])
          xi <- gsub("[\u00A0\u2000-\u200A\u2028\u2029\u202F\u205F\u3000[:space:]]+", " ", xi)
          trimws(xi)
        }, error = function(e) trimws(x[i]))
      }
      result
    })

    # Replace empty strings with NA
    data.table::fifelse(nzchar(x), x, NA_character_)
  }


  # Process string columns if any exist
  if (length(string.columns) > 0) {
    ph.data[, (string.columns) := lapply(.SD, clean_vec), .SDcols = string.columns]
    # Convert to factors if requested
    if (stringsAsFactors) {
      ph.data[, (string.columns) := lapply(.SD, factor), .SDcols = string.columns]
    }
  }

  # Process factor columns if any exist
  if (length(factor.columns) > 0) {
    # Convert factors to character, clean them, then back to factor
    ph.data[, (factor.columns) := lapply(.SD, function(x) {
      # Store factor levels / labels
      lvls <- levels(x)
      lvls_clean <- clean_vec(as.character(lvls))
      # Convert to character, clean it, then back to factor with same levels
      x_char <- as.character(x)
      x_clean <- clean_vec(x_char)
      factor(x_clean, levels = lvls_clean, exclude = NULL)
    }), .SDcols = factor.columns]
  }

  # Reorder columns
  data.table::setcolorder(ph.data, original.order)
  return(invisible(ph.data))
}

# std_error() ----
#' Calculate standard error of the mean
#' @param x name of a column in a data.frame/data.table or a vector
#' @export
#' @return numeric
#' @name std_error
#' @source plotrix R package July 11, 2022: <https://github.com/plotrix/plotrix/blob/master/R/std_error.R>.
#' @examples
#' \donttest{
#' temp1 <- data.table::data.table(x = c(seq(0, 400, 100), seq(1000, 1800, 200), NA),
#' mygroup = c(rep("A", 5), rep("B", 6))
#' )
#' std_error(c(seq(0, 400, 100), NA)) # expected value for mygroup == A
#' std_error(c(seq(1000, 1800, 200), NA)) # expected value for mygroup == B
#' temp1[, list(sem = std_error(x)), by = 'mygroup'][] # view summary table
#' temp1[, sem := std_error(x), by = 'mygroup'][] # save results in the original
#' }
#'
std_error <- function(x) {
  std_error_simple <- function(x) {
    if (!is.numeric(x)) stop("\n\U1F6D1 Input must be numeric.")
    if (all(is.na(x))) stop("\n\U1F6D1 Input contains only NA values.")
    if (sum(!is.na(x)) < 2) stop("\n\U1F6D1 At least two non-NA values are required to calculate standard error.")

    se <- stats::sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x))) # standard error or mean is sd / sqrt(# samples)

    if (is.nan(se) || is.infinite(se)) {
      warning("\n\u26A0\ufe0f Calculation resulted in NaN or Inf. Check your input data.")
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

