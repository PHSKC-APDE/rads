  combo.dt <- data.table::fread("S:/WORK/surveys/ACS/health insurance/2018/Health_Insurance_Big_Cities_2018_Complete_2020-01-17.csv")
  combo.dt[, c("table", "release_date") := NULL]
  # data.table, httr, yaml, glue

  #changes to old data

  # changes to historic data



chi_qa <- function(chi.data){

    chi.data <- data.table::setDT(chi.data)

  ## Load reference YAML ----
      chi.yaml <- httr::GET("https://raw.githubusercontent.com/PHSKC-APDE/rads/chi_qa/ref/chi_qa.yaml", httr::authenticate(Sys.getenv("GITHUB_TOKEN"), "")) # pull all data from GitHub page
      httr::stop_for_status(chi.yaml) # check to ensure there wasn't an error
      chi.yaml <- httr::content(chi.yaml, type="text", encoding = "ISO-8859-1") # save only the "content" from GitHub data
      temp.yaml <- tempfile() # create a temporary "file" in memory
      writeLines(chi.yaml, con=temp.yaml) # write YAML data to tempfile
      chi.yaml <- yaml::read_yaml(temp.yaml) # read in yaml data


  ## Check structure of CHI data ----
    # Confirm that all column names are unique ----
      # R allows for duplicate column names
      if(length(names(chi.data)) != length(unique(names(chi.data)))) {
        stop("You submitted a dataset where at least two columns have the same name.
             All names in chi.data must be unique.")
      }

    # Confirm all critical variables exist ----
      missing.var <- setdiff(names(chi.yaml$vars), names(chi.data))
      if(length(missing.var) > 0){
        missing.var <- paste(missing.var, collapse = ", ")
        stop(glue::glue("You are missing the following critical variable(s): {missing.var}"))
      }

    # Confirm that there are no additional variables ----
      extra.var <- setdiff(names(chi.data), names(chi.yaml$vars))
      if(length(extra.var) > 0){
        extra.var <- paste(extra.var, collapse = ", ")
        stop(glue::glue("Your dataset contains the following columns that are not CHI compliant: {extra.var}.
                        Please drop these variables before attempting to QA the data again."))
      }

    # Confirm that variables are of the proper class ----
        # identify proper classes from YAML file ----
            class.compare <- data.table::data.table(vars = names(chi.yaml$vars), yaml.class = as.character(chi.yaml$vars))

        # convert names of SQL data types to R classes ----
            class.compare[grepl("VARCHAR", yaml.class), yaml.class := "character"]
            class.compare[grepl("INT", yaml.class), yaml.class := "integer"]
            class.compare[grepl("FLOAT", yaml.class), yaml.class := "numeric"]

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
            chi.data[, (make.char) := lapply(.SD, lossless_convert, class = 'character'), .SDcols = make.char]
            chi.data[, (make.num) := lapply(.SD, lossless_convert, class = 'numeric'), .SDcols = make.num]
            chi.data[, (make.int) := lapply(.SD, lossless_convert, class = 'integer'), .SDcols = make.int]

        # check if there are variables that could not be coerced to proper type ----
            class.compare <- merge(data.table::data.table(vars = names(sapply(chi.data, class)), chi.class = sapply(chi.data, class)),
                               class.compare, by = "vars")

            vars <- class.compare[chi.class != yaml.class]$vars
            yaml.class <- class.compare[chi.class != yaml.class]$yaml.class
            class.problems <- paste(paste0(vars, "(", yaml.class, ")"), collapse = ", ")

            if(nrow(class.compare[chi.class != yaml.class]) > 0){
              stop(glue::glue("The following variables could not be coerced to their proper class (which is specified in parentheses):
                              {class.problems}"))
            }


    # set the columns in standard order ----
      setcolorder(chi.data, names(chi.yaml$vars))

  ## Basic logic checks ----
    # check confidence intervals ----
      # upper_bound should be greater than lower_bound ----
        if(nrow(chi.data[upper_bound < lower_bound, ])){
          stop("There is at least one row where the upper_bound is less than the lower_bound.
               Please fix this error prior to rerunning the chi_qa() function.
               You can view the problematic data by typing something like: View(chi.data[upper_bound < lower_bound, ])")
        }

      # result should be less than or equal to the upper bound ----
        if(nrow(chi.data[!(result <= upper_bound)])){
          stop("There is at least one row where the result is not less than or equal to the upper_bound.
               Please fix this error prior to rerunning the chi_qa() function.
               You can view the problematic data by typing something like: View(chi.data[!(result <= upper_bound)])")
        }

      # result should be greater than or equal to the result ----
        if(nrow(chi.data[!(result >= lower_bound)])){
          stop("There is at least one row where the result is not greater than or equal to the lower_bound.
           Please fix this error prior to rerunning the chi_qa() function.
           You can view the problematic data by typing something like: View(chi.data[!(result >= lower_bound)])")
        }

      # lower_bound should never be less than zero ----
        if(nrow(chi.data[lower_bound < 0])){
          stop("There is at least one row where the lower_bound is less than zero (i.e., it is negative).
           Please fix this error prior to rerunning the chi_qa() function.
          You can view the problematic data by typing something like: View(chi.data[lower_bound < 0])")
        }


    # RSE should always be between 0 and 1 ??? ----
      # shoudl we add this?
  ## Compare with previous year's results ----
      # in function arguments, have user submit most recent and comparison year(s). Submit as character b/c can be 2013-2017, not just 2017
      # if both are null, skip the comparison
      # it not submitted, merge newer data on old data and identify rows with > 3% absolute difference
      # save this dataset for manual review by the user
  ## Compare with a CSV ----
      # sometimes want to compare with external data source ---
      # must identify year of interest as above
      # instead of submitting a reference year, the user specifies a reference file
      # reference file will attempt to match on all columns that have the same name, except those with results (i.e., results, lower_bound, se, etc.)
      # actual comparison code should be the same as when comparing to a previous year, so write a small funcion to do this

} # close function

chi_qa(combo.dt)
