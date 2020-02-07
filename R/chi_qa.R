#' QA for CHI/Tableau ready standards using R.
#'
#' @description
#' This functions seeks to ensure that the data being assessed meet the detailed CHI
#' Tableau Ready standards detailed on our network drive:
#' "//phshare01/epe_share/WORK/CHI Visualizations/Tableau Ready Output Format_v2.xlsx"
#'
#' @details
#' This function ensures that the structure of the data matches all CHI Tableau Ready
#' specifications. QA for data quality vis-à-vis previous production data, CHAT, or any other source,
#' must be performed seperately
#'
#'
#' @param chi_data Name of a data.table or data.frame containing the prepared data, typically processed by RADS
#'
#' @return If there are no problems, a printed statement of success. Otherwise, it will stop and provide informative
#' feedback everytime there is an error.
#'
#' @export
#'
#' @keywords CHI, Tableau, Production
#'
#' @importFrom data.table is.data.table ':=' setDT setDF data.table setorder copy setnames setorder dcast setcolorder fread shift
#' @importFrom dplyr '%>%' distinct mutate n_distinct left_join
#' @importFrom glue glue
#' @importFrom utils write.table
#'
#' @examples
#'
#' \dontrun{
#' # create sample data
#'
#' # run function
#' }

# chi_qa function ----

chi_qa <- function(chi_data){

    chi_data <- data.table::setDT(copy(chi_data))

  ## Load reference YAML ----
    chi.yaml <- yaml::yaml.load(httr::GET(url = "https://raw.githubusercontent.com/PHSKC-APDE/chi/master/ref/chi_generic.yaml", httr::authenticate(Sys.getenv("GITHUB_TOKEN"), "")))


  ## Check columns ----
      # Confirm that all column names are unique ----
          if(length(names(chi_data)) != length(unique(names(chi_data)))) {
            stop("You submitted a dataset where at least two columns have the same name.
                 All names in chi_data must be unique.")
          }

      # Confirm all necessary columns exist ----
          missing.var <- setdiff(names(chi.yaml$vars), names(chi_data))
          if(length(missing.var) > 0){
            missing.var <- paste(missing.var, collapse = ", ")
            stop(glue::glue("You are missing the following critical columns(s): {missing.var}"))
          }

      # Confirm that there are no additional variables ----
          extra.var <- setdiff(names(chi_data), names(chi.yaml$vars))
          if(length(extra.var) > 0){
            extra.var <- paste(extra.var, collapse = ", ")
            stop(glue::glue("Your dataset contains the following columns that are not CHI compliant: {extra.var}.
                            Please drop these variables before attempting to QA the data again."))
          }

  ## Confirm that variables are of the proper class ----
      # identify proper classes from YAML file ----
          class.compare <- data.table::data.table(vars = names(chi.yaml$vars), yaml.class = as.character(chi.yaml$vars))

      # convert names of SQL data types to R classes ----
          class.compare[grepl("varchar", tolower(yaml.class)), yaml.class := "character"]
          class.compare[grepl("int", tolower(yaml.class)), yaml.class := "integer"]
          class.compare[grepl("float", tolower(yaml.class)), yaml.class := "numeric"]

      # identify which chi vars should be of which class ----
          make.char <- class.compare[yaml.class == "character"]$vars
          make.num  <- class.compare[yaml.class == "numeric"]$vars
          make.int  <- class.compare[yaml.class == "integer"]$vars

      # create function to convert column classes if it can be done without introducing NA's ----
          lossless_convert <- function(x, class){
            if(sum(is.na(x)) == sum(is.na(suppressWarnings(as(x, class)))) ){
              x <- suppressWarnings(as(x, class))
            }
            return(x)
          }

      # use function convert class ----
          chi_data[, (make.char) := lapply(.SD, lossless_convert, class = 'character'), .SDcols = make.char]
          chi_data[, (make.num) := lapply(.SD, lossless_convert, class = 'numeric'), .SDcols = make.num]
          chi_data[, (make.int) := lapply(.SD, lossless_convert, class = 'integer'), .SDcols = make.int]

      # check if there are variables that could not be coerced to proper type ----
          class.compare <- merge(data.table::data.table(vars = names(sapply(chi_data, class)), chi.class = sapply(chi_data, class)),
                             class.compare, by = "vars")

          if(nrow(class.compare[chi.class != yaml.class]) > 0){
            vars <- class.compare[chi.class != yaml.class]$vars
            yaml.class <- class.compare[chi.class != yaml.class]$yaml.class
            class.problems <- paste(paste0(vars, " (", yaml.class, ")"), collapse = ", ")
            stop(glue::glue("The following variables could not be coerced to their proper class (which is specified in parentheses):
                            {class.problems}"))
          }


  ## Set the columns in standard order ----
      setcolorder(chi_data, names(chi.yaml$vars))

  ## Basic logic checks ----
      # upper_bound should be greater than lower_bound ----
        if(nrow(chi_data[upper_bound < lower_bound, ])){
          stop("There is at least one row where the upper_bound is less than the lower_bound.
               Please fix this error prior to re-running the chi_qa() function.
               You can view the problematic data by typing something like: View(chi_data[upper_bound < lower_bound, ])")
        }

      # result should be less than or equal to the upper bound ----
        if(nrow(chi_data[!(result <= upper_bound)])){
          stop("There is at least one row where the result is not less than or equal to the upper_bound.
               Please fix this error prior to rerunning the chi_qa() function.
               You can view the problematic data by typing something like: View(chi_data[!(result <= upper_bound)])")
        }

      # result should be greater than or equal to the lower_bound ----
        if(nrow(chi_data[!(result >= lower_bound)])){
          stop("There is at least one row where the result is not greater than or equal to the lower_bound.
           Please fix this error prior to rerunning the chi_qa() function.
           You can view the problematic data by typing something like: View(chi_data[!(result >= lower_bound)])")
        }

      # lower_bound should never be less than zero ----
        if(nrow(chi_data[lower_bound < 0])){
          stop("There is at least one row where the lower_bound is less than zero (i.e., it is negative).
           Please fix this error prior to rerunning the chi_qa() function.
          You can view the problematic data by typing something like: View(chi_data[lower_bound < 0])")
        }

      # RSE should always be between 0 and 100 ----
          # confirmed with Abby 2/7/2020 that want RSE * 100
          if(nrow(chi_data[!rse %between% c(0, 100)]) > 0 ){
            stop("There is at least one row where the RSE (relative standard error) is outside the range of (0, 100].
                 Please fix this error prior to rerunning the chi_qa() function.
                 You can view the problematic data by typing something like: View(chi_data[lower_bound < 0])")
          }

      # RSE should be on scale of 0-100 (i.e., the proportion should have been multiplied by 100) ----
          if(nrow(chi_data[!is.na(rse)]) == nrow(chi_data[rse <=1])){
            stop("All RSEs are within the range (0, 1]. CHI Tableau Ready standards necessitate that these proportions
               be mutliplied by 100. I.e., .12345 >> 12.345
               Please fix this error prior to rerunning the chi_qa() function.")
          }

      # Caution flag should be toggled if RSE >= 30% ----
          if(nrow(chi_data[rse>=30 & (caution != "!" | is.na(caution))]) > 0 ){
            stop("There is at least one row where a caution flag ('!') is not used and the RSE is >= 30%.
               Please fix this error prior to rerunning the chi_qa() function.")
          }

  ## Ensure proper rounding ----
      # result should be to three digits ----
          if(sum(chi_data$result != round2(chi_data$result, 3), na.rm = T) != 0) {
            stop("The 'result' column does not appear to be rounded to 3 digits, as specified in the CHI standards")
          }

      # lower_bound should be to three digits ----
          if(sum(chi_data$lower_bound != round2(chi_data$lower_bound, 3), na.rm = T) != 0) {
            stop("The 'lower_bound' column does not appear to be rounded to 3 digits, as specified in the CHI standards")
          }

      # upper_bound should be to three digits ----
          if(sum(chi_data$upper_bound != round2(chi_data$upper_bound, 3), na.rm = T) != 0) {
            stop("The 'upper_bound' column does not appear to be rounded to 3 digits, as specified in the CHI standards")
          }

      # rse should be to three digits ----
          if(sum(chi_data$rse != round2(chi_data$rse, 3), na.rm = T) != 0) {
            stop("The 'rse' column does not appear to be rounded to 3 digits, as specified in the CHI standards")
          }

      # se should be rounded to four digits ----
          if(sum(chi_data$se != round2(chi_data$se, 4), na.rm = T) != 0) {
            stop("The 'se' column does not appear to be rounded to 3 digits, as specified in the CHI standards")
          }

  ## Check that core identification variables are all present ----
      for(var in c("indicator_key", "tab", "year", "cat1", "cat1_group",
                   "cat1_group_alias", "cat1_varname", "source_date", "run_date")){
        if(nrow(chi_data[is.na(get(var))]) > 0 ){
          stop(glue::glue("There is at least one row where '{var}' is missing.
                          Please fill in the missing value before rerunning chi_qa()"))
        }
      }

  ## Check that craosstab identification variables are all present ----
      for(var in c("cat2", "cat2_group", "cat2_group_alias", "cat2_varname")){
        if(nrow(chi_data[tab=="crosstabs" & is.na(get(var))]) > 0 ){
          stop(glue::glue("There is at least one row where tab=='crosstabs' & where '{var}' is missing.
                          Please fill in the missing value before rerunning chi_qa()"))
        }
      }

  ## Check that results are always present if row is not suppressed ----
      for(var in c("result", "lower_bound", "upper_bound", "se", "rse", "numerator", "denominator", "comparison_with_kc")){
        if(nrow(chi_data[suppression != "^" & is.na(get(var))]) > 0 ){
          stop(glue::glue("There is at least one row that is not suppressed & where '{var}' is missing.
                          Please fill in the missing value before rerunning chi_qa()"))
        }
      }

  ## Check that time_trends are always provided when tab=="trends" ----
        if(nrow(chi_data[tab == "trends" & is.na(time_trends)]) > 0 ){
          stop(glue::glue("There is at least one row where tab=='trends' & where '{var}' is missing.
                          Please fill in the missing value before rerunning chi_qa()"))}


  ## Compare with previous year's results (FOR FUTURE???)----
      # in function arguments, have user submit most recent and comparison year(s). Submit as character b/c can be 2013-2017, not just 2017
      # if both are null, skip the comparison
      # it not submitted, merge newer data on old data and identify rows with > 3% absolute difference
      # save this dataset for manual review by the user
  ## Compare with a CSV (FOR FUTURE) ----
      # sometimes want to compare with external data source ---
      # must identify year of interest as above
      # instead of submitting a reference year, the user specifies a reference file
      # reference file will attempt to match on all columns that have the same name, except those with results (i.e., results, lower_bound, se, etc.)
      # actual comparison code should be the same as when comparing to a previous year, so write a small funcion to do this

  ## Print success statement!!!!!!!! ####
    cat(paste("Congratulations!",
          "",
          "Your data has passed all CHI Tableau Ready formatting, style, and logic checks.",
          "",
          "Please note that these tests did NOT include assessing whether your data was",
          "similar to the data extracted in previous years or that found in CHAT.",
          sep = "\n"))

} # close function

