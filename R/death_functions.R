# death_113() ----
#' View the NCHS 113 COD causeids
#'
#' @description
#' Function to view the National Center for Health Statistics (NCHS) 113
#' Selected Causes of Death (COD) causeids.
#'
#' Generates a table with two columns, \code{causeid} & \code{cause.of.death}.
#' Use it to identify the causeids given as an argument in \code{death_113_count}.
#'
#' @details
#' There are actually 114 rows, with causeid 114 being the official
#' \code{CDC version of causeid 95 (Residual)}, i.e., \code{All other diseases
#' (Residual)}. Causeid 95 was intentionally changed to match the definition
#' used by WA DOH.
#'
#' @note
#' This function does not take any arguments
#'
#' @source
#' \code{rads.data::icd_nchs113causes_raw}
#'
#' @references
#' \url{https://www.cdc.gov/nchs/data/dvs/Part9InstructionManual2020-508.pdf} &
#' \url{https://secureaccess.wa.gov/doh/chat/Content/FilesForDownload/CodeSetDefinitions/NCHS113CausesOfDeath.pdf}
#'
#' @seealso
#' [death_113_count()] for using this information to count the NCHS 113 leading causes of death
#'
#' @return
#' A data.table with 114 rows and two columns: causeid (integers between 1 & 114, inclusive) and cause.of.death (human readable description).
#'
#' @export
#'
#' @name death_113
#'
#' @examples
#' # Save and view table as a data.table named 'blah'
#' blah <- death_113()
#' blah[]
#'
#' @import data.table rads.data
#'
death_113 <- function(){
  # Global variables used by data.table declared as NULL here to play nice with devtools::check() ----
  deaths113_causeid_list <- causeid <- cause.of.death <-  NULL

  deaths113_causeid_list <- copy(rads.data::icd_nchs113causes_raw)
  deaths113_causeid_list <- deaths113_causeid_list[, .SD, .SDcols = c("causeid", "cause.of.death")] # from rads.data
  return(deaths113_causeid_list)
}

# death_113_count() ----
#' Summarize NCHS 113 causes of deaths
#'
#' @description
#' Generate death counts for the National Center for Health
#' Statistics (NCHS) 113 Selected Causes of Death (COD). Needs line-level death
#' data with a properly formatted ICD10 column.
#'
#' In addition to the causes of death you specify with \code{causeids} or
#' \code{cause}, it will automatically return the total deaths as well as
#' COVID-19 deaths (since they do not have their own NCHS category).
#'
#' See \code{rads::death_113()} for a complete list of available causesid
#' and cause values.
#'
#' @details
#' There are actually 114 rows, with causeid 114 being the official
#' \code{CDC version of causeid 95 (Residual)}, i.e., \code{All other diseases
#' (Residual)}. Causeid 95 was intentionally changed to match the definition
#' used by WA DOH. You can get results for any or all of the 113(+1) causes of
#' death using the \code{causeids} or \code{cause} arguments.
#'
#' @param ph.data a data.table or data.frame. Must contain death data structured
#' with one person per row and with at least one column of ICD10 death codes.
#'
#' @param causeids an integer vector of length >=1 & <= 114, with a minimum value
#' of 1 and a maximum value of 114.
#'
#' The default is \code{1:113}, i.e., the standard panel of WA DOH / NCHS 113
#' causes of death.
#'
#' @param  cause an OPTIONAL character vector specifying the complete or partial
#' keyword for the cause of death of interest. It is not case sensitive and you
#' can specify it in two ways: 1) \code{cause = c('viral', 'cough')} or 2)
#' \code{cause = c("viral|cough")}. If you specify any keyword(s),
#' the function will ignore the \code{causeids} argument.
#'
#' The default is \code{NULL}, i.e., the function will rely on the \code{causeids}
#' argument to identify the causes of death.
#'
#' @param icdcol a character vector of length one that specifies the name of the
#' column in ph.data that contains the ICD10 death codes of interest.
#'
#' The default is \code{underlying_cod_code}, which is found in the properly
#' formatted death data obtained using the \code{get_data_death()} function.
#'
#' @param kingco a logical vector of length one. It specifies whether you want to
#' limit the analysis to King County.
#'
#' **NOTE** this only works with data imported with the \code{get_data_death()}
#' function because it needs the variable \code{chi_geo_kc}.
#'
#' The default is kingco = TRUE.
#'
#' @param group_by a character vector of indeterminate length. This is used to
#' specify all the variables by which you want to group (a.k.a. stratify) the
#' results. For example, if you specified \code{group_by = c('chi_sex',
#' 'chi_race_6')}, the results would be stratified by each combination of sex
#' and race.
#'
#' The default is \code{group_by = NULL}
#'
#' @param ypll_age an optional numeric vector of length 1. When specified, it
#' should be the age (an integer) used for **Years of Potential Life Lost** (YPLL)
#' calculations. Valid values are between 1 & 99 (inclusive), though 65 and 85 are the most
#' common. For example, \code{ypll_age = 65} would sum the total number of years
#' that could have been lived had everyone in the data lived to at least 65.
#' Note that this function returns the total number of YPLL. Additional
#' processing is necessary to calculate rates per 100,000.
#'
#' The default is \code{ypll_age = NULL}, which will skip YPLL calculations.
#'
#' @param death_age_col an optional character vector of length one that specifies
#' the name of the column in ph.data with the decedents' age at death
#' in years. It is only needed if \code{ypll_age} is
#' specified AND if ph.data lacks a column named \code{chi_age}.
#'
#' The default is \code{death_age_col = NULL}.
#'
#' @references
#' \url{https://www.cdc.gov/nchs/data/dvs/Part9InstructionManual2020-508.pdf} &
#' \url{https://secureaccess.wa.gov/doh/chat/Content/FilesForDownload/CodeSetDefinitions/NCHS113CausesOfDeath.pdf}
#'
#' @seealso
#' - [death_113()] for viewing teh CDC NCHS 113 leading causes of death
#' - [get_data_death()] for importing properly formatted death data
#' - [death_icd10_clean()] for preparing ICD-10 codes for use with all rads death functions
#' - [age_standardize()] for calculating age standardized rates
#' - [death_130_count()] for generating CDC NCHS 130 causes of infant death counts
#' - [death_injury_matrix_count()] for generating injury matrix counts
#' - [death_multicause_count()] for generating counts of deaths defined by BOTH underlying & contributing causes
#' - [death_other_count()] for generating counts of causes NOT included in the NCHS
#'
#' @return
#' Generates a table with three columns, \code{causeid},  \code{cause.of.death},
#' and \code{deaths}. If \code{ypll_age} is specified, a \code{ypll_##} column
#' will also be added to the table. Columns in the \code{group_by}
#' argument will also be returned.
#'
#' By default, it will return all 113 causes of death. You can specify which
#' causes of death you want to assess using the \code{causeids} or \code{cause}
#' arguments.
#'
#' @note
#' Calls upon \code{rads::death_xxx_count}.
#'
#' @export
#'
#' @name death_113_count
#'
#' @examples
#' # example 1: death count only
#' set.seed(98104)
#' deathdata <- data.table::data.table(
#'   cod.icd10 = c(rep("A85.2", round(runif(1, 30, 100000), 0)),
#'                 rep("B51", round(runif(1, 30, 100000), 0)),
#'                 rep("U071", round(runif(1, 30, 100000), 0)),
#'                 rep("E44", round(runif(1, 30, 100000), 0)),
#'                 rep("E62", round(runif(1, 30, 100000), 0)),
#'                 rep("G00", round(runif(1, 30, 100000), 0)),
#'                 rep("J10", round(runif(1, 30, 100000), 0)),
#'                 rep("J15", round(runif(1, 30, 100000), 0)),
#'                 rep("V874", round(runif(1, 30, 100000), 0)))
#' )
#' eg1 <- death_113_count(ph.data = deathdata,
#'                        causeids = seq(1, 113, 1),
#'                        cause = NULL,
#'                        icdcol = "cod.icd10",
#'                        kingco = FALSE,
#'                        ypll_age = NULL,
#'                        death_age_col = NULL)
#' head(eg1)
#'
#' # example 2: with YPLL calculation
#' deathdata2 <- data.table::copy(deathdata)
#' set.seed(98104)
#' deathdata2[, ageofdeath := rads::round2(rnorm(1, mean = 70, sd = 5 ), 0),
#'            1:nrow(deathdata2)] # synthetic age of death
#' eg2 <- death_113_count(ph.data = deathdata2,
#'                        causeids = seq(1, 113, 1),
#'                        cause = NULL,
#'                        icdcol = "cod.icd10",
#'                        kingco = FALSE,
#'                        ypll_age = 65,
#'                        death_age_col = "ageofdeath")
#' head(eg2)
#'
#' @import data.table rads.data
#'
death_113_count <- function(ph.data,
                            causeids = seq(1, 113, 1),
                            cause = NULL,
                            icdcol = "underlying_cod_code",
                            kingco = TRUE,
                            group_by = NULL,
                            ypll_age = NULL,
                            death_age_col = NULL){

  # use the generalized function death_xxx_count()
  nchs113_countz <- death_xxx_count(ph.data = ph.data,
                                    causeids = causeids,
                                    cause = cause,
                                    icdcol = icdcol,
                                    kingco = kingco,
                                    group_by = group_by,
                                    ypll_age = ypll_age,
                                    death_age_col = death_age_col,
                                    nchsnum = 113)
  return(nchs113_countz)
}

# death_130() ----
#' View the NCHS 130 Infant COD causeids
#'
#' @description
#' Function to view the National Center for Health Statistics (NCHS) 130
#' Selected Causes of Infant Death (COD) causeids.
#'
#' Generates a table with two columns, \code{causeid} & \code{cause.of.death}.
#' Use it to identify the causeids given as an argument in \code{death_130_count}.
#'
#' @details
#' To see the corresponding leading cause groups and 'levels' (i.e., the
#' hierarchy), check out \code{?rads.data::icd_nchs130causes_raw} &
#' \code{?rads.data::icd_nchs130causes}.
#'
#' @note
#' This function does not take any arguments
#'
#' @source
#' \code{rads.data::icd_nchs130causes_raw}
#'
#' @references
#' \url{https://www.cdc.gov/nchs/data/dvs/Part9InstructionManual2020-508.pdf}, Table C.
#' \url{https://secureaccess.wa.gov/doh/chat/Content/FilesForDownload/CodeSetDefinitions/NCHS130CausesInfantDeath_Codes.pdf}
#'
#' @seealso
#' [death_130_count()] for using this information to count the NCHS 130 causes of infant deaths
#'
#' @return
#' A data.table with 130 rows and two columns: causeid (integers between 1 & 130, inclusive) and cause.of.death (human readable description).
#'
#' @export
#'
#' @name death_130
#'
#' @examples
#' # Save and view table as a data.table named 'blah'
#' blah <- death_130()
#' blah[]
#'
#' @import data.table rads.data
#'
death_130<- function(){
  # Global variables used by data.table declared as NULL here to play nice with devtools::check() ----
  deaths130_causeid_list <- causeid <- cause.of.death <-  NULL

  deaths130_causeid_list <- copy(rads.data::icd_nchs130causes_raw)
  deaths130_causeid_list <- deaths130_causeid_list[, .SD, .SDcols = c("causeid", "cause.of.death")] # from rads.data
  return(deaths130_causeid_list)
}

# death_130_count() ----
#' Summarize NCHS 130 Select Causes of Infant Death
#'
#' @description
#' Generate death counts for the National Center for Health
#' Statistics (NCHS) 130 Selected Causes of Infant Death. Needs line-level death
#' data with a properly formatted ICD10 column.
#'
#' In addition to the causes of death you specify with \code{causeids} or
#' \code{cause}, it will automatically return the total deaths as well as
#' COVID-19 deaths (since they do not have their own NCHS category).
#'
#' See \code{rads::death_130()} for a complete list of available causesid
#' and cause values.
#'
#' @details
#' These 130 causes of death are for decedants ages < 1 year old.
#'
#' @param ph.data a data.table or data.frame. Must contain death data structured
#' with one person per row and with at least one column of ICD10 death codes.
#'
#' @param causeids an integer vector of length >=1 & <= 130, with a minimum value
#' of 1 and a maximum value of 130.
#'
#' The default is \code{1:130}, i.e., the compolte standard panel of WA DOH /
#' NCHS 130 causes of death.
#'
#' @param  cause an OPTIONAL character vector specifying the complete or partial
#' keyword for the cause of death of interest. It is not case sensitive and you
#' can specify it in two ways: 1) \code{cause = c('viral', 'cough')} or 2)
#' \code{cause = c("viral|cough")}. If you specify any keyword(s),
#' the function will ignore the \code{causeids} argument.
#'
#' The default is \code{NULL}, i.e., the function will rely on the \code{causeids}
#' argument to identify the causes of death.
#'
#' @param icdcol a character vector of length one that specifies the name of the
#' column in ph.data that contains the ICD10 death codes of interest.
#'
#' The default is \code{underlying_cod_code}, which is found in the properly
#' formatted death data obtained using the \code{get_data_death()} function.
#'
#' @param kingco a logical vector of length one. It specifies whether you want to
#' limit the analysis to King County.
#'
#' **NOTE** this only works with data imported with the \code{get_data_death()}
#' function because it needs the variable \code{chi_geo_kc}.
#'
#' The default is kingco = TRUE.
#'
#' @param group_by a character vector of indeterminate length. This is used to
#' specify all the variables by which you want to group (a.k.a. stratify) the
#' results. For example, if you specified \code{group_by = c('sex',
#' 'race')}, the results would be stratified by each combination of sex
#' and race.
#'
#' The default is \code{group_by = NULL}
#'
#' @param ypll_age an optional numeric vector of length 1. When specified, it
#' should be the age (an integer) used for **Years of Potential Life Lost** (YPLL)
#' calculations. Valid values are between 1 & 99 (inclusive), though 65 and 85 are the most
#' common. For example, \code{ypll_age = 65} would sum the total number of years
#' that could have been lived had everyone in the data lived to at least 65.
#' Note that this function returns the total number of YPLL. Additional
#' processing is necessary to calculate rates per 100,000.
#'
#' The default is \code{ypll_age = NULL}, which will skip YPLL calculations.
#'
#' @param death_age_col an optional character vector of length one that specifies
#' the name of the column in ph.data with the decedents' age at death
#' in years. It is only needed if \code{ypll_age} is
#' specified AND if ph.data lacks a column named \code{chi_age}.
#'
#' The default is \code{death_age_col = NULL}.
#'
#' @references
#' \url{https://www.cdc.gov/nchs/data/dvs/Part9InstructionManual2020-508.pdf} &
#' \url{https://secureaccess.wa.gov/doh/chat/Content/FilesForDownload/CodeSetDefinitions/NCHS130CausesInfantDeath_Codes.pdf}
#'
#' @seealso
#' - [death_130()] for viewing the CDC NCHS 130 causes of infant death
#' - [get_data_death()] for importing properly formatted death data
#' - [death_icd10_clean()] for preparing ICD-10 codes for use with all rads death functions
#' - [age_standardize()] for calculating age standardized rates
#' - [death_113_count()] for generating CDC NCHS 113 leading causes of death counts
#' - [death_injury_matrix_count()] for generating injury matrix counts
#' - [death_multicause_count()] for generating counts of deaths defined by BOTH underlying & contributing causes
#' - [death_other_count()] for generating counts of causes NOT included in the NCHS
#'
#' @return
#' Generates a table with three columns, \code{causeid},  \code{cause.of.death},
#' and \code{deaths}. If \code{ypll_age} is specified, a \code{ypll_##} column
#' will also be added to the table. Columns identifies by the \code{group_by}
#' argument will also be returned.
#'
#' By default, it will return all 130 causes of death. You can specify which
#' causes of death you want to assess using the \code{causeids} or \code{cause}
#' arguments.
#'
#' @note
#' Calls upon \code{rads::death_xxx_count}.
#'
#' @export
#'
#' @name death_130_count
#'
#' @examples
#' # example 1: death count only
#' set.seed(98104)
#' deathdata <- data.table::data.table(
#'   cod.icd10 = c(rep("P36.3", round(runif(1, 30, 100000), 0)),
#'                 rep("V022", round(runif(1, 30, 100000), 0)),
#'                 rep("P021", round(runif(1, 30, 100000), 0)),
#'                 rep("P202", round(runif(1, 30, 100000), 0)),
#'                 rep("I26", round(runif(1, 30, 100000), 0)),
#'                 rep("R951", round(runif(1, 30, 100000), 0)),
#'                 rep("P080", round(runif(1, 30, 100000), 0)),
#'                 rep("A09", round(runif(1, 30, 100000), 0)),
#'                 rep("P702", round(runif(1, 30, 100000), 0)))
#' )
#' eg1 <- death_130_count(ph.data = deathdata,
#'                        causeids = seq(1, 130, 1),
#'                        cause = NULL,
#'                        icdcol = "cod.icd10",
#'                        kingco = FALSE,
#'                        ypll_age = NULL,
#'                        death_age_col = NULL)
#' head(eg1)
#'
#' @import data.table rads.data
#'
death_130_count <- function(ph.data,
                            causeids = seq(1, 130, 1),
                            cause = NULL,
                            icdcol = "underlying_cod_code",
                            kingco = TRUE,
                            group_by = NULL,
                            ypll_age = NULL,
                            death_age_col = NULL){

  # use generalized function death_xxx_count()
  nchs130_countz <- death_xxx_count(ph.data = ph.data,
                                    causeids = causeids,
                                    cause = cause,
                                    icdcol = icdcol,
                                    kingco = kingco,
                                    group_by =  group_by,
                                    ypll_age = ypll_age,
                                    death_age_col = death_age_col,
                                    nchsnum = 130)
  return(nchs130_countz)
}

# death_icd10_clean() ----
#' Clean and Standardize ICD-10 Death Codes
#'
#' @description
#' This function prepares ICD-10 codes for processing with the death functions
#' in the `rads` package.
#'
#' @details
#' This function prepares ICD-10 codes for processing with the death functions
#' in the `rads` package. It performs cleaning and tidying operations, such as
#' converting codes to uppercase, removing non-alphanumeric characters (e.g.,
#' hyphens and periods), validating the ICD-10 code format, trimming codes to a
#' maximum of 4 characters, and padding shorter codes with zeros when needed.
#' It is used internally by `rads` death functions but can also be used as a
#' standalone function.
#'
#' @param icdcol A character vector containing ICD-10 codes.
#'
#' @return A cleaned and standardized character vector of ICD-10 codes.
#'
#' @examples
#' \donttest{
#' icd_codes <- c("A85.2", "B99-1", "J20.9", "INVALID", "C34")
#' cleaned_icd_codes <- death_icd10_clean(icd_codes)
#' print(cleaned_icd_codes)
#' }
#' @export
#'
death_icd10_clean <- function(icdcol){

  # Validate
  if(missing(icdcol)){stop("\n\U0001f47f `icdcol` cannot be missing in death_icd10_clean()")}

  # Set icd10 to upper case as per standards
  icdcol <- toupper(icdcol)

  # Check for hyphens and periods which are sometimes present
  if(length(grep("\\.|-", icdcol, value = T) > 0 )){
    warning(paste0("\n\u26A0\ufe0f There is at least one row where `icdcol` contains a hyphen (-), period (.), " ,
                   "\nspace or some other non alpha-numeric character. These characters have been deleted, ",
                   "\ne.g., A85.2 will become A852. This is necessary because rads death functions expect",
                   "\npure alpha numeric ICD codes."
    ))

    icdcol <- gsub("[[:space:].]+", "", gsub("([^A-Za-z0-9 ])+", "", x = icdcol))
  }

  # Check for codes that don't follow the ICD pattern and replace with NA
  problem.icds <- grep("^[A-Z].*[0-9]$", icdcol, value = TRUE, invert = TRUE)
  problem.icds <- problem.icds[!is.na(problem.icds)]
  if (length(problem.icds) > 0) {
    warning(paste0("\n\u26A0\ufe0f  There is/are ", length(problem.icds), " value(s) in `icdcol` that do not follow the proper ",
                   "\nICD pattern. All ICDs that do not begin with a letter and end with",
                   "\na numeric have been replaced with NA."))
    icdcol[!grepl("^[A-Z].*[0-9]$", icdcol)] <- NA
  }

  # Trim codes to a maximum of 4 characters and pad shorter codes
  icdcol <- substr(icdcol, 1, 4)

  two_chars_idx <- which(nchar(icdcol) == 2 & !is.na(icdcol))
  icdcol[two_chars_idx] <- paste0(icdcol[two_chars_idx], "00")

  three_chars_idx <- which(nchar(icdcol) == 3 & !is.na(icdcol))
  icdcol[three_chars_idx] <- paste0(icdcol[three_chars_idx], "0")

  # Return ICD10 codes
  return(icdcol)
}

# death_injury_matrix() ----
#' View available combinations of injury death mechanisms and intents
#'
#' @description
#' Function to view the ICD10 Death Injury Matrix (ICE: International
#' Collaborative Effort on Injury Statistics) combinations of mechanism and
#' intent available in rads.
#'
#' Generates a table with two columns, \code{mechanism} & \code{intent}.
#' Use it to identify the combinations of \code{mechanism} & \code{intent} that
#' you want to use in \code{death_injury_matrix_count}.
#'
#' @details
#' This function provides the terms used by the death/mortality function and may
#' not be the same as those used with hospitalization functions.
#'
#' @note
#' This function does not take any arguments
#'
#' @source
#' \code{rads.data::icd10_death_injury_matrix}
#'
#' @references
#' \url{https://secureaccess.wa.gov/doh/chat/Content/FilesForDownload/CodeSetDefinitions/CHATInjury(ICE)codes.pdf}
#'
#' \url{https://www.cdc.gov/nchs/data/ice/icd10_transcode.pdf}
#'
#' @seealso
#' [death_injury_matrix_count()] for using this information to count injury related deaths
#' by mechanism and intent
#'
#' @return
#' A data.table with 88 rows and two columns: \code{mechanism} & \code{intent}.
#'
#' @export
#'
#' @name death_injury_matrix
#'
#' @examples
#' # Save and view table as a data.table named 'blah'
#' blah <- death_injury_matrix()
#' print(blah)
#'
#' @import data.table rads.data
#'
death_injury_matrix<- function(){
  # Global variables used by data.table declared as NULL here to play nice with devtools::check() ----
  death_injury_matrix_list <- mechanism <- intent <-  NULL

  death_injury_matrix_list <- unique(data.table::copy(rads.data::icd10_death_injury_matrix)[, list(mechanism, intent)])

  return(death_injury_matrix_list)
}

# death_injury_matrix_count() ----
#' Generate a death injury matrix from line-level death data
#'
#' @description
#' Generate death counts for a death injury matrix
#' specifying the intent and mechanism of injury. Needs line-level death data
#' with a properly formatted ICD10 column.
#'
#' @param ph.data a data.table or data.frame. Must contain death data structured
#' with one person per row and with at least one column of ICD10 death codes.
#'
#' @param intent a character vector of length 1 to 5. It specifies the
#' intent of death that you want returned ("Unintentional", "Suicide", "Homicide",
#' "Undetermined", or "Legal intervention/war"). "none" will ignore the intent
#' and only return the mechanism of death.
#'
#' **NOTE**
#' You do not have to type the entire keyword for the intent, a
#' partial string match is sufficient and is case insensitive. E.g.,
#' \code{intent = c("cide")} would return both "Suicide" and "Homicide" and
#' \code{intent = c("un")} would return both "Unintentional" and "Undetermined".
#'
#' The default is \code{'*'}, which selects all possible intents.
#'
#' @param mechanism a character vector of length 1 to 28. It specifies the
#' mechanism of death that you want returned (E.g., "Cut/pierce", "Drowning",
#' "Fall", "Firearm", etc.). "none" will ignore the mechanism
#' and only return the intent of death.
#'
#' To see the complete list of mechanisms, type
#' \code{unique(rads.data::icd10_death_injury_matrix$mechanism)} in your
#' R console.
#'
#' **NOTE**
#' You do not have to type the entire keyword for the mechanism, a
#' partial string match is sufficient and is case insensitive. E.g.,
#' \code{mechanism = c("cycl")} would return both "Pedal cyclist" and
#' "Motorcyclist".
#'
#' The default is \code{'*'}, which selects all possible mechanisms
#'
#' @param icdcol a character vector of length one that specifies the name of the
#' column in ph.data that contains the ICD10 death codes of interest.
#'
#' The default is \code{underlying_cod_code}, which is found in the properly
#' formatted death data obtained using the \code{get_data_death()} function.
#'
#' @param kingco a logical vector of length one. It specifies whether you want to
#' limit the analysis to King County. Note that this only works with data
#' imported from the \code{get_data_death()} function.
#'
#' The default is \code{kingco = TRUE}.
#'
#' @param group_by a character vector of indeterminate length. This is used to
#' specify all the variables by which you want to group (a.k.a. stratify) the
#' results. For example, if you specified \code{group_by = c('chi_sex',
#' 'chi_race_6')}, the results would be stratified by each combination of sex
#' and race.
#'
#' The default is \code{group_by = NULL}
#'
#' @param ypll_age an optional numeric vector of length 1. When specified, it
#' should be the age (an integer) used for **Years of Potential Life Lost** (YPLL)
#' calculations. Valid values are between 1 & 99 (inclusive), though 65 and 85 are the most
#' common. For example, \code{ypll_age = 65} would sum the total number of years
#' that could have been lived had everyone in the data lived to at least 65.
#' Note that this function returns the total number of YPLL. Additional
#' processing is necessary to calculate rates per 100,000.
#'
#' The default is \code{ypll_age = NULL}, which will skip YPLL calculations.
#'
#' @param death_age_col an optional character vector of length one that specifies
#' the name of the column in ph.data with the decedents' age at death
#' in years. It is only needed if \code{ypll_age} is
#' specified AND if ph.data lacks a column named \code{chi_age}.
#'
#' The default is \code{death_age_col = NULL}.
#'
#' @details
#' The matrix coding is based on the ICE (International Collaborative Effort on
#' Injury Statistics) standard.
#'
#' @seealso
#' - [death_injury_matrix()] for viewing available injury death mechanisms and intents
#' - [get_data_death()] for importing properly formatted death data
#' - [death_icd10_clean()] for preparing ICD-10 codes for use with all rads death functions
#' - [age_standardize()] for calculating age standardized rates
#' - [death_113_count()] for generating CDC NCHS 113 leading causes of death counts
#' - [death_130_count()] for generating CDC NCHS 130 causes of infant death counts
#' - [death_multicause_count()] for generating counts of deaths defined by BOTH underlying & contributing causes
#' - [death_other_count()] for generating counts of causes NOT included in the NCHS
#'
#' @return
#' The function returns a data.table with a minimum of three columns:
#' \code{mechanism}, \code{intent}, & \code{deaths}. \code{ypll_##} and
#' \code{group_by} columns will also be returned if specified in the arguments.
#'
#' @note
#' The function default is to return the matrix of all intents and mechanisms
#' of death. You can choose to only return the intent or only return the
#' mechanism. If you set both to "none", you will receive a summary of all
#' injury deaths without regard to the intent.
#'
#' Also note that terrorism codes (U01.#, U02.#, & U03.#) are not included
#' because they are not included in the coding used by WA DOH. If they are
#' needed, they can be obtained from the CDC link below.
#'
#' @source
#' \code{rads.data::icd10_death_injury_matrix}
#'
#' @references
#' WA DOH CHAT: \url{https://secureaccess.wa.gov/doh/chat/Content/FilesForDownload/CodeSetDefinitions/CHATInjury(ICE)codes.pdf}
#'
#' CDC: \url{https://www.cdc.gov/nchs/data/ice/icd10_transcode.pdf}
#'
#'
#' @export
#'
#' @name death_injury_matrix_count
#'
#' @examples
#' # create synthetic line level data
#' set.seed(98104)
#' injurydata <- data.table::data.table(
#'   cod.icd10 = c(
#'     # Cut/pierce, Homicide
#'     rep("X99", round(runif(1, 30, 10000), 0)),
#'     # Drowning, Unintentional
#'     rep("W65", round(runif(1, 30, 10000), 0)),
#'     # Fall, Suicide
#'     rep("X80", round(runif(1, 30, 10000), 0)),
#'     # Fire/flame, Undetermined
#'     rep("Y26", round(runif(1, 30, 10000), 0)),
#'     # Firearm, Legal intervention/war
#'     rep("Y350", round(runif(1, 30, 10000), 0)),
#'     # Poisoning, Unintentional
#'     rep("X40", round(runif(1, 30, 10000), 0)),
#'     # Overexertion, Unintentional
#'     rep("X50", round(runif(1, 30, 10000), 0)),
#'     # Other land transport, Homicide
#'     rep("Y03", round(runif(1, 30, 10000), 0)),
#'     # Pedal cyclist, other, Unintentional
#'     rep("V10", round(runif(1, 30, 10000), 0)))
#' )
#'
#' injurydata[, year := sample(2015:2020, nrow(injurydata), replace = TRUE)]
#'
#' # example 1: every available combination of mechanism and intent
#' eg1 <- death_injury_matrix_count(ph.data = injurydata,
#'                             intent = "*",
#'                             mechanism = "*",
#'                             icdcol = "cod.icd10",
#'                             kingco = FALSE,
#'                             ypll_age = NULL,
#'                             death_age_col = NULL)
#' head(eg1) # note the data are stratified by year because year was in ph.data
#'
#' # example 2: falls designated as homicides and or suicides
#' eg2 <- death_injury_matrix_count(ph.data = injurydata,
#'                             intent = "icide",
#'                             mechanism = "fall",
#'                             icdcol = "cod.icd10",
#'                             kingco = FALSE,
#'                             ypll_age = NULL,
#'                             death_age_col = NULL)
#' head(eg2)
#'
#' # example 3: summary of all injury deaths regardless of intent and mechanism
#' eg3 <- death_injury_matrix_count(ph.data = injurydata,
#'                             intent = "none",
#'                             mechanism = "none",
#'                             icdcol = "cod.icd10",
#'                             kingco = FALSE,
#'                             ypll_age = NULL,
#'                             death_age_col = NULL)
#' eg3[]
#'
#' # example 4: any intent and mechanism with YPLL_65 given death_age_col
#' injurydata4 <- data.table::copy(injurydata)
#' set.seed(98104)
#' injurydata4[, ageofdeath := rads::round2(rnorm(1, mean = 70, sd = 5 ), 0),
#' 1:nrow(injurydata4)] # synthetic age of death
#' eg4 <- death_injury_matrix_count(ph.data = injurydata4,
#'                             intent = "none",
#'                             mechanism = "none",
#'                             icdcol = "cod.icd10",
#'                             kingco = FALSE,
#'                             ypll_age = 65,
#'                             death_age_col = "ageofdeath")
#' eg4[]
#'
#' # example 5: all suicides, regardless of mechanism, stratified by age
#'
#' injurydata5 <- data.table::copy(injurydata4)
#'
#' eg5 <- death_injury_matrix_count(ph.data = injurydata5,
#'                             intent = "suicide",
#'                             mechanism = "none",
#'                             icdcol = "cod.icd10",
#'                             kingco = FALSE,
#'                             group_by = 'ageofdeath',
#'                             ypll_age = NULL,
#'                             death_age_col = NULL)
#' eg5[]
#' @import data.table rads.data
#'
death_injury_matrix_count <- function(ph.data,
                                intent = "*",
                                mechanism = "*",
                                icdcol = "underlying_cod_code",
                                kingco = TRUE,
                                group_by = NULL,
                                ypll_age = NULL,
                                death_age_col = NULL){
  # Global variables used by data.table declared as NULL here to play nice with devtools::check() ----
  x_intent <- x_mechanism <- x_reftable <- x_combo <- orig.coding <- orig.order <- underlying_cod_code <- NULL
  chi_geo_kc <- '.' <- deaths <- icd10 <- icd10_tempy <- NULL
  calculated.age <- x_ypll <- date_of_death <- date_of_birth <-  NULL

  # Check arguments ----
    # ph.data ----
      if (missing(ph.data) || !is.data.frame(ph.data)) {
        stop("\n\U0001f47f `ph.data` must be the unquoted name of a data.frame or data.table")
      }

      # Create a copy of ph.data and ensure it's a data.table
      ph.data <- data.table::setDT(data.table::copy(ph.data))

    # intent ----
      if(isFALSE(is.character(intent)) || length(intent) > 5){
        stop("\n\U0001f47f `intent` must specify a character vector with a lenghth <= 5.\nTo select all options, use intent = '*'.")
      }
      myorig.intent <- copy(intent)

    # mechanism ----
      if(isFALSE(is.character(mechanism)) || length(mechanism) > 28){
        stop("\n\U0001f47f `mechanism` must specify a character vector with a lenghth <= 28.\nTo select all options, use mechanism = '*'.")
      }

    # icdcol ----
      if(isFALSE(icdcol %in% colnames(ph.data))){
        stop("\n\U0001f47f `icdcol` must be the name of column that exists in `ph.data`.")
      }

      ph.data[, icd10_tempy := death_icd10_clean(get(icdcol))]

    # kingco ----
      if (!is.logical(kingco)) {
        stop("\n\U0001f47f `kingco` must be a logical value, i.e., TRUE or FALSE.")
      }

      if (kingco && !"chi_geo_kc" %in% names(ph.data)) {
        stop(
          "\n\U0001f47f `ph.data` does not have the column `chi_geo_kc`, which is required for King County data."
        )
      }

      if (kingco) {
        ph.data <- ph.data[chi_geo_kc == 'King County']
      }

    # group_by ----
      if (!is.null(group_by)) {
        group_col_error <- setdiff(group_by, names(ph.data))
        if (length(group_col_error) > 0) {
          stop(
            paste0(
              "\U0001f6d1 The following `group_by` values are not column names in `ph.data`: ",
              paste0(group_col_error, collapse = ', '),
              "."
            )
          )
        }
      }

    # ypll_age ----
      if (!is.null(ypll_age)) {
        if (!is.numeric(ypll_age) ||
            !all(ypll_age == floor(ypll_age)) ||
            length(ypll_age) != 1 || ypll_age < 1 | ypll_age > 99) {
          stop("\n\U0001f47f `ypll_age` must be an integer between 1 and 99.")
        }

        ypll_col_name <- paste0("ypll_", ypll_age)
      }

    # death_age_col ----
      if (!is.null(death_age_col)) {
        if (!(death_age_col %in% names(ph.data))) {
          stop("\n\U0001f47f `death_age_col` must be the name of column that exists in `ph.data`.")
        }
        col_data <- ph.data[[death_age_col]]
        if (!is.numeric(col_data) || !all(is.na(col_data) | col_data == floor(col_data), na.rm = TRUE)) {
          stop("\n\U0001f47f If `death_age_col` is specified, it must be a column of integers in `ph.data`.")
        }
        if (is.null(ypll_age)) {
          stop("\n\U0001f47f `death_age_col` should not be specified when `ypll_age` is NULL.")
        }
      }

      if (is.null(death_age_col) & !is.null(ypll_age)) {
        if ("chi_age" %in% names(ph.data)) {
          death_age_col <- 'chi_age'
          message("\U0001F4E3 \nYou requested the calculation of YPLL by specifying `ypll_age` and did not provide `death_age_col`.",
                  "\nThe function found and used a column named `chi_age` for the YPLL calculation. If this was not",
                  "\nyour intention, please specify the correct column with the decendant's age with the",
                  "\n`death_age_col` argument.")
        } else {
          stop(paste0("\n\U0001f47f You requested the calculation of YPLL by specifying `ypll_age` and did not provide `death_age_col`.",
                      "\nThe function attempted to use a column named `chi_age`, but it was not found.",
                      "\nTo calculate YPLL, please set death_age_col to the name of the column with the age at death."))
        }
      }

  # Identify intent of interest ----
    unique_intents <- unique(rads.data::icd10_death_injury_matrix$intent)

    intent = tolower(intent)

    if("*" %in% intent){x_intent = unique_intents}

    if("none" %in% intent){x_intent = unique_intents} # "Any intent" will be sum of all of intents

    if(length(intersect(c("*", "none"), intent)) == 0){
      x_intent <- unique(unlist(lapply(intent, function(i) grep(i, unique_intents, value = TRUE, ignore.case = TRUE))))
    }

    if(length(x_intent) == 0){
      stop(paste0("\n\U0001f47f Your `intent` value (", intent, ") has filtered out all of the death injury intents.",
                  "\nPlease enter 'none', '*', or a new partial keyword term and try again."))
      }

  # Identify mechanism of interest ----
    unique_mechanisms <- unique(rads.data::icd10_death_injury_matrix$mechanism)

    mechanism = tolower(mechanism)

    if("*" %in% mechanism){x_mechanism = unique_mechanisms}

    if("none" %in% mechanism){x_mechanism = "All injury"}

    if(length(intersect(c("*", "none"), mechanism)) == 0){
      x_mechanism <- unique(unlist(lapply(mechanism, function(i) grep(i, unique_mechanisms, value = TRUE, ignore.case = TRUE))))
    }

    if(length(x_mechanism) == 0){
      stop(paste0("\n\U0001f47f Your `mechanism` value (", mechanism, ") has filtered out all of the death injury mechanisms.",
      "\nPlease enter 'none', '*', or a new partial keyword term and try again."))
      }

  # Count deaths for each intent_x_mechanism of interest ----
    # prep injury matrix reference table ----
      # get reference table from rads.data
        x_reftable <- unique(rads.data::icd10_death_injury_matrix)[, .(mechanism, intent, icd10)]

      # subset for intent
        if(length(x_intent) == 1 && x_intent == "Any intent"){
          x_reftable[, intent := x_intent]
        } else {
          x_reftable <- x_reftable[intent %in% x_intent]
        }

      # subset for mechanism
        x_reftable <- x_reftable[mechanism %in% x_mechanism]

    # merge reference table onto death data ----
      x_combo <- ph.data[x_reftable, # data.table join faster than merge for large datasets
                         on = .(icd10_tempy = icd10),
                         allow.cartesian = TRUE, # needed because there can be duplicate icd10 in x_reftable
                         nomatch = 0]

      x_combo[, c(icdcol, 'icd10_tempy') := NULL]
      if("none" %in% intent){x_combo[, intent := "Any intent"]}

    # calculate YPLL count ----
      if(is.null(ypll_age)){
        x_combo <- x_combo[, .(deaths = .N), by = c("mechanism", "intent", group_by)]
      } else {
        # create table with ypll summary
          x_ypll <- copy(x_combo)
          x_ypll[get(death_age_col) < ypll_age, ypll_col_name := ypll_age - get(death_age_col)]
          x_ypll[, c(death_age_col) := NULL]
          x_ypll <- x_ypll[, .(temp_ypll = sum(ypll_col_name, na.rm = TRUE)), # use temporary name because data.table doesn't accept quoted value after .(
                           by = c("mechanism", "intent", group_by)]
          setnames(x_ypll, "temp_ypll", ypll_col_name)

        # calculate death count
          x_combo <- x_combo[, .(deaths = .N), by = c("mechanism", "intent", group_by)]

        # merge ypll onto death summaries
          x_combo <- merge(x_combo, x_ypll, all = T)
      }

  # Tidy ----
    # Rename & aggregate by mechanism and intent when needed ----
      if("none" %in% myorig.intent & "none" %in% mechanism){
        if(is.null(ypll_age)){
          x_combo[, intent := 'Any intent']
          x_combo <- x_combo[, list(deaths = sum(deaths)), by = setdiff(names(x_combo), c("deaths"))]
        } else {
          x_combo[, intent := 'Any intent']
          x_combo <- x_combo[, list(deaths = sum(deaths), temp_ypll = sum(get(ypll_col_name))),
                             by = setdiff(names(x_combo), c("deaths", ypll_col_name))]
          setnames(x_combo, "temp_ypll", paste0("ypll_", ypll_age))
        }
      }

      if(mechanism == 'none'){x_combo[, mechanism := 'Any mechanism']}

    # Create rows for zero values (otherwise rows would simply be missing) ----
      # Select columns to use for combination
        cols_to_use <- setdiff(names(x_combo), c('deaths', grep('^ypll_', names(x_combo), value = TRUE)))

      # Create list of unique values for each selected column
        unique_col_vals <- lapply(cols_to_use, function(col) unique(x_combo[[col]]))

      # Create names for the list
        names(unique_col_vals) <- c(cols_to_use)

      # If myorig.intent is '*', add intent column
        if (exists("myorig.intent") && myorig.intent == '*') {
          unique_col_vals$intent <- unique(death_injury_matrix()$intent)
        }

      # Use CJ to create all combinations
        template_xyz <- do.call(data.table::CJ, unique_col_vals)

      # Merge actual values onto template_xyz
        x_combo <- merge(template_xyz,
                         x_combo,
                         all.x = TRUE, # to ensure every combination of categorical vars are available
                         all.y = TRUE) # could also be FALSE because, by definition, all values of y are in X

      # Fill deaths with zeros
        x_combo[is.na(deaths), deaths := 0]

      # Fill ypll_## with zeros if needed
        if(!is.null(ypll_age)){
          ypll_name = grep('^ypll_[0-9]', names(x_combo), value = T)
          x_combo[is.na(get(ypll_name)), paste0(ypll_name) := 0]
        }

    # Sort columns and rows ----
      setcolorder(x_combo, c("mechanism", "intent", "deaths", grep('^ypll_', names(x_combo), value = TRUE)))
      sort_cols <- c("mechanism", "intent", setdiff(names(x_combo), c("deaths", "mechanism", "intent", grep('^ypll_', names(x_combo), value = TRUE))))
      setorderv(x_combo, sort_cols)

  # Return data ----
    return(x_combo)
}

# death_multicause() ----
#' View available multicause death definitions for `death_multicause_count()`
#'
#' @description
#' Function to view the multicause death definitions available in RADS. These are
#' causes of death that require BOTH specific underlying causes AND contributing
#' causes to be present.
#'
#' Generates a data.table showing the available cause names and their definitions.
#'
#' @details
#' Multicause deaths are those requiring specific combinations of underlying and
#' contributing causes. For example, opioid deaths require poisoning as the
#' underlying cause AND opioid codes in the contributing causes.
#'
#' @note
#' This function does not take any arguments
#'
#' @source
#' [rads.data::icd10_multicause()]
#'
#' @seealso
#' [death_multicause_count()] for using this information to count multicause deaths
#'
#' @return
#' A data.table with unique cause names and a description of what each includes.
#'
#' @export
#'
#' @name death_multicause
#'
#' @examples
#' # View available multicause definitions
#' available_causes <- death_multicause()
#' print(available_causes)
#'
#' @import data.table rads.data
#'
death_multicause <- function(){
  # Global variables used by data.table declared as NULL to play nice with devtools::check() ----
  cause_name <- underlying_contributing <- icd10 <- n_underlying <- n_contributing <- NULL

  # Get the reference table
  multicause_ref <- data.table::copy(rads.data::icd10_multicause)

  # Summarize by cause_name
  summary_table <- multicause_ref[, list(
    n_underlying = sum(underlying_contributing == "underlying"),
    n_contributing = sum(underlying_contributing == "contributing")
  ), by = cause_name]

  # Add description
  summary_table[, description := paste0(
    "Requires any of ", n_underlying, " underlying cause codes AND any of ",
    n_contributing, " contributing cause codes"
  )]

  # Keep only the columns we want to show
  summary_table <- summary_table[, list(cause_name, description)]

  return(summary_table)
}

# death_multicause_count() ----
#' Summarize multicause deaths requiring **BOTH** underlying and contributing causes
#'
#' @description
#' Generate death counts for causes of death that require BOTH specific underlying
#' causes AND contributing causes. Needs line-level death data with properly
#' formatted ICD10 columns for both underlying and contributing causes of death.
#'
#' See [death_multicause()] for a complete list of available multicause
#' definitions.
#'
#' @details
#' This function identifies deaths where specific combinations of underlying and
#' contributing causes must both be present. For example, opioid-related deaths
#' require poisoning as the underlying cause AND opioid codes in the contributing
#' causes.
#'
#' The function will automatically include "All causes" in addition to your
#' specified multicause definition.
#'
#' @param ph.data a data.table or data.frame. Must contain death data structured
#' with one person per row, with at least one column of ICD10 underlying cause
#' codes and columns for contributing cause codes.
#'
#' @param cause_name a character vector of length one that specifies the multicause
#' death definition from the reference table. For example, `"Opioid"`. To see
#' available options, use `rads::death_multicause()`.
#'
#' The default is `NULL`, which means you must specify `cause_name` *OR* the
#' `underlying_codes` and `contributing_codes` directly.
#'
#' @param underlying_codes an optional character vector of ICD10 codes that must
#' appear in the underlying cause of death. Only used if `cause_name` is NULL.
#' For example, `c("X40", "X41", "X42")`.
#'
#' The default is `NULL`.
#'
#' @param contributing_codes an optional character vector of ICD10 codes that must
#' appear in the contributing causes of death. Only used if `cause_name` is NULL.
#' For example, `c("T400", "T401")`.
#'
#' The default is `NULL`.
#'
#' @param icdcol a character vector of length one that specifies the name of the
#' column in ph.data that contains the underlying ICD10 death codes.
#'
#' The default is `"underlying_cod_code"`, which is found in properly formatted
#' death data obtained using the `get_data_death()` function.
#'
#' @param contributing_cols a character vector of length one that specifies the
#' stem of the column names containing contributing cause codes. The function
#' expects columns named with this stem followed by numbers 1-20.
#'
#' The default is `"record_axis_code"`, expecting columns like `record_axis_code_1`
#' through `record_axis_code_20`.
#'
#' @param contributing_logic a character vector of length one. Specifies whether
#' ANY or ALL of the `contributing_codes` must be present.
#'
#' The default is `"ANY"`.
#'
#' @param kingco a logical vector of length one. It specifies whether you want to
#' limit the analysis to King County.
#'
#' **NOTE** this only works with data imported with the `get_data_death()`
#' function because it needs the variable `chi_geo_kc`.
#'
#' The default is `kingco = TRUE`.
#'
#' @param group_by a character vector of indeterminate length. This is used to
#' specify all the variables by which you want to group (a.k.a. stratify) the
#' results. For example, if you specified `group_by = c('chi_sex', 'chi_race_6')`,
#' the results would be stratified by each combination of sex and race.
#'
#' The default is `group_by = NULL`
#'
#' @param ypll_age an optional numeric vector of length 1. When specified, it
#' should be the age (an integer) used for **Years of Potential Life Lost** (YPLL)
#' calculations. Valid values are between 1 & 99 (inclusive), though 65 and 85
#' are the most common. For example, `ypll_age = 65` would sum the total number
#' of years that could have been lived had everyone in the data lived to at least 65.
#' Note that this function returns the total number of YPLL. Additional
#' processing is necessary to calculate rates per 100,000.
#'
#' The default is `ypll_age = NULL`, which will skip YPLL calculations.
#'
#' @param death_age_col an optional character vector of length one that specifies
#' the name of the column in ph.data with the decedents' age at death
#' in years. It is only needed if `ypll_age` is specified AND if ph.data lacks
#' a column named `chi_age`.
#'
#' The default is `death_age_col = NULL`.
#'
#' @seealso
#' - [death_multicause()] for viewing available multicause definitions
#' - [get_data_death()] for importing properly formatted death data
#' - [death_icd10_clean()] for preparing ICD-10 codes for use with all rads death functions
#' - [age_standardize()] for calculating age standardized rates
#' - [death_113_count()] for generating CDC NCHS 113 leading causes of death counts
#' - [death_130_count()] for generating CDC NCHS 130 causes of infant death counts
#' - [death_injury_matrix_count()] for generating injury matrix counts
#' - [death_other_count()] for generating counts of causes NOT included in the NCHS
#' 113 Causes of death or injury matrix
#'
#' @keywords mortality multicause
#'
#' @return
#' Generates a table with three columns: `cause.of.death`, `deaths`, and
#' if specified, `ypll_##`. Columns in the `group_by` argument will also be
#' returned.
#'
#' @export
#'
#' @name death_multicause_count
#'
#' @examples
#' \dontrun{
#' # Example using reference table definition
#' opioid_deaths <- death_multicause_count(
#'   ph.data = death_data,
#'   cause_name = "Opioid",
#'   icdcol = "underlying_cod_code",
#'   contributing_cols = "record_axis_code",
#'   kingco = TRUE
#' )
#'
#' # Example using custom codes
#' custom_deaths <- death_multicause_count(
#'   ph.data = death_data,
#'   underlying_codes = c("X40", "X41", "X42"),
#'   contributing_codes = c("T400", "T401"),
#'   contributing_logic = "ANY",
#'   icdcol = "underlying_cod_code",
#'   contributing_cols = "record_axis_code",
#'   kingco = FALSE
#' )
#' }
#'
death_multicause_count <- function(ph.data,
                                   cause_name = NULL,
                                   underlying_codes = NULL,
                                   contributing_codes = NULL,
                                   icdcol = "underlying_cod_code",
                                   contributing_cols = "record_axis_code",
                                   contributing_logic = "ANY",
                                   kingco = TRUE,
                                   group_by = NULL,
                                   ypll_age = NULL,
                                   death_age_col = NULL) {
  # Check arguments ----
    # ph.data ----
      if (missing(ph.data) || !is.data.frame(ph.data)) {
        stop("\n\U0001f47f `ph.data` must be the unquoted name of a data.frame or data.table")
      }

      # Create a copy of ph.data and ensure it's a data.table
      ph.data <- data.table::setDT(data.table::copy(ph.data))

    # cause_name vs underlying/contributing codes ----
      if (!is.null(cause_name) && (!is.null(underlying_codes) || !is.null(contributing_codes))) {
        warning("\n\u26A0\ufe0f You specified both `cause_name` and custom codes. \n",
                "The custom codes have been ignored.\n",
                "If you want to use custom codes, set `cause_name = NULL`.\n",
                "Otherwise set `underlying_codes = NULL` and `contributing_codes = NULL`.")
        underlying_codes <- NULL
        contributing_codes <- NULL
      }

      if (is.null(cause_name) && (is.null(underlying_codes) || is.null(contributing_codes))) {
        stop("\n\U0001f47f You must specify either `cause_name` OR both `underlying_codes` ",
             "and `contributing_codes`.")
      }

    # Get codes from reference table if cause_name specified ----
      if (!is.null(cause_name)) {
        if (!is.character(cause_name) || length(cause_name) != 1) {
          stop("\n\U0001f47f `cause_name` must be a single character value.\n",
               "See `death_multicause()` for available options.")
        }

        # Make lower case
        cause_name <- tolower(cause_name)

        # Load reference table
        ref_table <- data.table::copy(rads.data::icd10_multicause)

        # Check if cause_name exists
        if (!cause_name %in% tolower(unique(ref_table$cause_name))) {
          stop("\n\U0001f47f '", cause_name, "' is not a valid cause_name. ",
               "Use `rads::death_multicause()` to see available options.")
        }

        # Extract codes
        underlying_codes <- ref_table[tolower(cause_name) == param_cause_name &
                                        underlying_contributing == "underlying",
                                      icd10,
                                      env = list(param_cause_name = I(cause_name))] # I() means use AsIs, it isn't a col name

        contributing_codes <- ref_table[tolower(cause_name) == param_cause_name &
                                          underlying_contributing == "contributing",
                                        icd10,
                                        env = list(param_cause_name = I(cause_name))]

        # Set cause label
        cause_label <- unique(ref_table[tolower(cause_name) == param_cause_name,
                                        cause_name,
                                        env = list(param_cause_name = I(cause_name))])[1]
      } else {
        # Using custom codes - create a generic label
        cause_label <- "Custom multicause"
      }

    # Clean and validate ICD codes ----
      # Clean all ICD codes (underlying and contributing)
      underlying_codes <- death_icd10_clean(underlying_codes)
      contributing_codes <- death_icd10_clean(contributing_codes)

      # Remove any NA codes
      underlying_codes <- underlying_codes[!is.na(underlying_codes)]
      contributing_codes <- contributing_codes[!is.na(contributing_codes)]

      # Check if any codes remain after cleaning
      if (length(underlying_codes) == 0) {
        stop("\n\U0001f47f All underlying_codes became NA after cleaning with `death_icd10_clean()`.")
      }

      if (length(contributing_codes) == 0) {
        stop("\n\U0001f47f All contributing_codes became NA after cleaning with `death_icd10_clean()`.")
      }

    # contributing_logic ----
      if (!contributing_logic %in% c("ANY", "ALL")) {
        stop("\n\U0001f47f `contributing_logic` must be either 'ANY' or 'ALL'.")
      }

    # icdcol ----
    if (!icdcol %in% names(ph.data)) {
      stop("\n\U0001f47f `icdcol` must be the name of a column that exists in `ph.data`.")
    }

    # Clean underlying cause codes
    ph.data[, icd10_tempy := death_icd10_clean(get(icdcol))]

    # contributing_cols ----
      # Clean up the stem (remove trailing underscore if exists)
      contributing_cols <- gsub("_$", "", contributing_cols)

      # Find all matching columns
      contrib_col_pattern <- paste0("^", contributing_cols, "_[0-9]+$")
      contrib_col_names <- grep(contrib_col_pattern, names(ph.data), value = TRUE)

      if (length(contrib_col_names) == 0) {
        stop("\n\U0001f47f No columns found matching pattern '", contributing_cols, "_#'. ",
             "Expected columns like '", contributing_cols, "_1', '", contributing_cols, "_2', etc.")
      }

      if (length(contrib_col_names) < 20) {
        warning("\n\u26A0\ufe0f Less than 20 columns were found matching the pattern '", contributing_cols, "_#'.\n",
                "Typically there are 20 columns for the contributing causes of death. You may want to check ph.data.")
      }

      # Clean contributing causes columns in ph.data
      ph.data[, (contrib_col_names) := lapply(.SD, death_icd10_clean), .SDcols = contrib_col_names]

    # kingco ----
      if (!is.logical(kingco)) {
        stop("\n\U0001f47f `kingco` must be a logical value, i.e., TRUE or FALSE.")
      }

      if (kingco && !"chi_geo_kc" %in% names(ph.data)) {
        stop("\n\U0001f47f `ph.data` does not have the column `chi_geo_kc`, ",
             "which is required for King County data.")
      }

      if (kingco) {
        ph.data <- ph.data[chi_geo_kc == 'King County']
      }

    # group_by ----
      if (!is.null(group_by)) {
        group_col_error <- setdiff(group_by, names(ph.data))
        if (length(group_col_error) > 0) {
          stop("\n\U0001f6d1 The following `group_by` values are not column names in `ph.data`: ",
               paste0(group_col_error, collapse = ', '), ".")
        }
      }

    # ypll_age ----
      if (!is.null(ypll_age)) {
        if (!is.numeric(ypll_age) ||
            !all(ypll_age == floor(ypll_age)) ||
            length(ypll_age) != 1 || ypll_age < 1 | ypll_age > 99) {
          stop("\n\U0001f47f `ypll_age` must be an integer between 1 and 99.")
        }

        ypll_col_name <- paste0("ypll_", ypll_age)
      }

    # death_age_col ----
      if (!is.null(death_age_col)) {
        if (!(death_age_col %in% names(ph.data))) {
          stop("\n\U0001f47f `death_age_col` must be the name of column that exists in `ph.data`.")
        }

        col_data <- unique(ph.data[[death_age_col]])
        if (!is.numeric(col_data) || !all(is.na(col_data) | col_data == floor(col_data), na.rm = TRUE)) {
          stop("\n\U0001f47f If `death_age_col` is specified, it must be a column of integers in `ph.data`.")
        }

        if (is.null(ypll_age)) {
          stop("\n\U0001f47f `death_age_col` should not be specified when `ypll_age` is NULL.")
        }
      }

      if (is.null(death_age_col) & !is.null(ypll_age)) {
        if ("chi_age" %in% names(ph.data)) {
          death_age_col <- 'chi_age'
          message("\U0001F4E3 \nYou requested the calculation of YPLL by specifying `ypll_age` and did not provide `death_age_col`.",
                  "\nThe function found and used a column named `chi_age` for the YPLL calculation. If this was not",
                  "\nyour intention, please specify the correct column with the decedent's age with the",
                  "\n`death_age_col` argument.")
        } else {
          stop(paste0("\n\U0001f47f You requested the calculation of YPLL by specifying `ypll_age` and did not provide `death_age_col`.",
                      "\nThe function attempted to use a column named `chi_age`, but it was not found.",
                      "\nTo calculate YPLL, please set death_age_col to the name of the column with the age at death."))
        }
      }

  # Identify deaths meeting criteria ----
    # Check underlying cause
    ph.data[, meets_criteria := icd10_tempy %in% underlying_codes]

    # Check contributing causes (made my brain hurt, but it works!)
    if (contributing_logic == "ANY") {
      # ANY logic - at least one contributing code must be present
      ph.data[meets_criteria == TRUE,
              meets_criteria := Reduce(`|`, lapply(.SD, function(x) x %in% contributing_codes)),
              .SDcols = contrib_col_names]
    }

    if (contributing_logic == "ALL") {
      # For each contributing code, check if it appears in any contributing columns
      # If it does, it continues to `meet_criteria`.
      # If it doesn't, it is removed from further comparisons
      for(code in contributing_codes) {
        code_present <- ph.data[meets_criteria == TRUE,
                                Reduce(`|`, lapply(.SD, function(x) x %in% code)),
                                .SDcols = contrib_col_names]
        ph.data[meets_criteria == TRUE, meets_criteria := meets_criteria & code_present]
      }
    }

  # Calculate YPLL if needed ----
    if (!is.null(ypll_age)) {
      ph.data[, (ypll_col_name) := fifelse(get(death_age_col) < ypll_age,
                                           ypll_age - get(death_age_col),
                                           0)]
    }

  # Create summary of death and ypll counts ----
    if (is.null(ypll_age)) {
      # All causes
      all_causes <- ph.data[, list(cause.of.death = "All causes",
                                deaths = .N), by = group_by]

      # Specific multicause
      specific_cause <- ph.data[meets_criteria == TRUE,
                                list(cause.of.death = cause_label,
                                  deaths = .N), by = group_by]
    } else {
      # All causes with YPLL
      all_causes <- ph.data[, list(cause.of.death = "All causes",
                                deaths = .N,
                                temp_ypll = sum(get(ypll_col_name), na.rm = TRUE)),
                            by = group_by]

      # Specific multicause with YPLL
      specific_cause <- ph.data[meets_criteria == TRUE,
                                list(cause.of.death = cause_label,
                                  deaths = .N,
                                  temp_ypll = sum(get(ypll_col_name), na.rm = TRUE)),
                                by = group_by]

      # Rename YPLL column
      setnames(all_causes, "temp_ypll", ypll_col_name)
      setnames(specific_cause, "temp_ypll", ypll_col_name)
    }

    # Combine results
    result <- rbind(all_causes, specific_cause)

  # Create rows for zero values ----
    # Select columns to use for combination
    cols_to_use <- setdiff(names(result), c('deaths', grep('^ypll_', names(result), value = TRUE)))

    # Create list of unique values for each selected column
    unique_col_vals <- lapply(cols_to_use, function(col) unique(result[[col]]))
    names(unique_col_vals) <- cols_to_use

    # Use CJ to create all combinations
    template_xyz <- do.call(data.table::CJ, unique_col_vals)

    # Merge actual values onto template
    result <- merge(template_xyz,
                    result,
                    all.x = TRUE,
                    all.y = TRUE)

    # Fill deaths with zeros
    result[is.na(deaths), deaths := 0]

    # Fill ypll with zeros if needed
    if (!is.null(ypll_age)) {
      ypll_name <- grep('^ypll_[0-9]', names(result), value = TRUE)
      result[is.na(get(ypll_name)), (ypll_name) := 0]
    }

  # Sort columns and rows ----
    if (!is.null(ypll_age)) {
      setcolorder(result, c("cause.of.death", "deaths", ypll_name))
      sort_cols <- c("cause.of.death", setdiff(names(result),
                                               c("deaths", "cause.of.death", ypll_name)))
    } else {
      setcolorder(result, c("cause.of.death", "deaths"))
      sort_cols <- c("cause.of.death", setdiff(names(result),
                                               c("deaths", "cause.of.death")))
    }
    setorderv(result, sort_cols)

  # Return result ----
    return(result)
}


# death_other() ----
#' View "Other" Causes of Death available in RADS
#'
#' @description
#' Function to view "Other" Causes of Death that are availbe in RADS (via
#' \href{https://github.com/PHSKC-APDE/rads.data}{rads.data}). These are causes
#' of death that are NOT included in the NCHS 113 Causes of death
#' (see \code{?death_113_count}) or the CDC death injury matrix (see
#' \code{?death_injury_matrix_count}).
#'
#' Generates a character vector with the names of all available causes of death.
#'
#' @details
#' This function simply returns the unique values of the \code{cause.of.death}
#' column from \code{rads.data::icd_other_causes_of_death}.
#'
#' @note
#' This function does not take any arguments
#'
#' @source
#' \code{rads.data::icd_other_causes_of_death}
#'
#' @references
#' \code{?rads.data::icd_other_causes_of_death}
#'
#' @seealso
#' [death_other_count()] for using this information to count 'Other' causes of death
#'
#' @return
#' A character vector with the name of each available cause of death.
#'
#' @export
#'
#' @name death_other
#'
#' @examples
#' #' # Save and view table as a data.table named 'blah'
#' blah <- death_other()
#' print(blah)
#'
#' @import data.table rads.data
#'
death_other<- function(){
  # Global variables used by data.table declared as NULL here to play nice with devtools::check() ----
  death_other_list <- cause.of.death <-  NULL

  death_other_list <- data.table::copy(rads.data::icd_other_causes_of_death)
  death_other_list <- unique(death_other_list$cause.of.death) # from rads.data
  return(death_other_list)
}

# death_other_count() ----
#' Summarize "Other" causes of deaths
#'
#' @description
#' Generate death counts for causes of death that are **NOT** included in the NCHS
#' 113 Causes of death (see \code{?death_113_count}) or the CDC death injury
#' matrix (see \code{?death_injury_matrix_count}).
#'
#' Needs line-level death data with a properly formatted ICD10 column.
#'
#' Use the \code{cause} argument to specify the cause(s) for which you desire to
#' obtain death counts.
#'
#' @details
#' None
#'
#' @param ph.data a data.table or data.frame. Must contain death data structured
#' with one person per row and with at least one column of ICD10 death codes.
#'
#' @param  cause a character vector specifying the complete or partial
#' keyword for the cause of death of interest. It is not case sensitive and you
#' can specify it in two ways: 1) \code{cause = c('induce', 'overdose')} or 2)
#' \code{cause = c("induce|overdose")}.
#'
#' The default is \code{cause = NULL}.
#'
#' @param icdcol a character vector of length one that specifies the name of the
#' column in ph.data that contains the ICD10 death codes of interest.
#'
#' The default is \code{underlying_cod_code}, which is found in the properly
#' formatted death data obtained using the \code{get_data_death()} function.
#'
#' @param kingco a logical vector of length one. It specifies whether you want to
#' limit the analysis to King County.
#'
#' **NOTE**
#' this only works with data imported with the \code{get_data_death()} function.
#'
#' The default is kingco = TRUE.
#'
#' @param group_by a character vector of indeterminate length. This is used to
#' specify all the variables by which you want to group (a.k.a. stratify) the
#' results. For example, if you specified \code{group_by = c('chi_sex',
#' 'chi_race_6')}, the results would be stratified by each combination of sex
#' and race.
#'
#' The default is \code{group_by = NULL}
#'
#' @param ypll_age an optional numeric vector of length 1. When specified, it
#' should be the age (an integer) used for **Years of Potential Life Lost** (YPLL)
#' calculations. Valid values are between 1 & 99 (inclusive), though 65 and 85 are the most
#' common. For example, \code{ypll_age = 65} would sum the total number of years
#' that could have been lived had everyone in the data lived to at least 65.
#' Note that this function returns the total number of YPLL. Additional
#' processing is necessary to calculate rates per 100,000.
#'
#' The default is \code{ypll_age = NULL}, which will skip YPLL calculations.
#'
#' @param death_age_col an optional character vector of length one that specifies
#' the name of the column in ph.data with the decedents' age at death
#' in years. It is only needed if \code{ypll_age} is
#' specified AND if ph.data lacks columns named \code{date_of_birth} and
#' \code{date_of_death} that are of class \code{"Date"}. If the latter two
#' columns exist, the code calculates the age at death for you.
#'
#' The default is \code{death_age_col = NULL}.'
#'
#' @seealso
#' - [death_other()] for viewing available 'Other' cause of death definitions
#' - [get_data_death()] for importing properly formatted death data
#' - [death_icd10_clean()] for preparing ICD-10 codes for use with all rads death functions
#' - [age_standardize()] for calculating age standardized rates
#' - [death_113_count()] for generating CDC NCHS 113 leading causes of death counts
#' - [death_130_count()] for generating CDC NCHS 130 causes of infant death counts
#' - [death_injury_matrix_count()] for generating injury matrix counts
#' - [death_multicause_count()] for generating counts of deaths defined by BOTH underlying & contributing causes
#'
#' @return
#' Generates a table with two columns: \code{cause.of.death} and \code{deaths}.
#' If \code{ypll_age} is specified, a \code{ypll_##} column will also be added to the
#' table. Columns in the \code{group_by} argument will also be returned.
#'
#'
#' @export
#'
#' @name death_other_count
#'
#' @examples
#' # example 1: death count only
#' set.seed(98104)
#' deathdata <- data.table::data.table(
#'   cod.icd10 = c(rep("D52.1", round(runif(1, 30, 100000), 0)),
#'                 rep("E66.1", round(runif(1, 30, 100000), 0)),
#'                 rep("K85.3", round(runif(1, 30, 100000), 0)),
#'                 rep("X85", round(runif(1, 30, 100000), 0)),
#'                 rep("R78.4", round(runif(1, 30, 100000), 0)),
#'                 rep("Y13.2", round(runif(1, 30, 100000), 0)),
#'                 rep("X42.3", round(runif(1, 30, 100000), 0)),
#'                 rep("X60.7", round(runif(1, 30, 100000), 0)),
#'                 rep("J70.3", round(runif(1, 30, 100000), 0)))
#' )
#' eg1 <- death_other_count(ph.data = deathdata,
#'                        cause = "dose|induce",
#'                        icdcol = "cod.icd10",
#'                        kingco = FALSE,
#'                        ypll_age = NULL,
#'                        death_age_col = NULL)
#' head(eg1)
#'
#' # example 2: with YPLL calculation
#' deathdata2 <- data.table::copy(deathdata)
#' set.seed(98104)
#' deathdata2[, ageofdeath := rads::round2(rnorm(1, mean = 70, sd = 5 ), 0),
#'            1:nrow(deathdata2)] # synthetic age of death
#' eg2 <- death_other_count(ph.data = deathdata2,
#'                        cause = "dose|induce",
#'                        icdcol = "cod.icd10",
#'                        kingco = FALSE,
#'                        ypll_age = 65,
#'                        death_age_col = "ageofdeath")
#' head(eg2)
#'
#' @import data.table rads.data
#'
death_other_count <- function(ph.data,
                               cause,
                               icdcol = "underlying_cod_code",
                               kingco = TRUE,
                               group_by = NULL,
                               ypll_age = NULL,
                               death_age_col = NULL){
  # Global variables used by data.table declared as NULL here to play nice with devtools::check() ----
  problem.icds <- long113 <-  cause.of.death <- deaths <- '.' <- NULL
  x_reftable <- x_combo <- x_covid <- x_cause <- x_all <- x_ypll <- NULL
  chi_geo_kc <- underlying_cod_code <- icd10_tempy <- ypll_col_name <- NULL
  date_of_death <- date_of_birth <- calculated.age <- orig.coding <- icd10 <- NULL

  # Check arguments ----
    # ph.data ----
      if (missing(ph.data) || !is.data.frame(ph.data)) {
        stop("\n\U0001f47f `ph.data` must be the unquoted name of a data.frame or data.table")
      }

      # Create a copy of ph.data and ensure it's a data.table
      ph.data <- data.table::setDT(data.table::copy(ph.data))

    # cause ----
      if(missing(cause)){
        stop("\n\U0001f47f `cause` cannot be missing. Please specify the `cause = XXX` argument and submit again")
      }
      if(isFALSE(is.character(cause))){
        stop("\n\U0001f47f `cause` must be a character vector with whole or partial keywords for the cause of death of interest.")
      }

    # icdcol ----
      if (!icdcol %in% names(ph.data)) {
        stop("\n\U0001f47f `icdcol` must be the name of a column that exists in `ph.data`.")
      }

      ph.data[, icd10_tempy := death_icd10_clean(get(icdcol))]

    # check that kingco is a logical ----
      if (!is.logical(kingco)) {
        stop("\n\U0001f47f `kingco` must be a logical value, i.e., TRUE or FALSE.")
      }

      if (kingco && !"chi_geo_kc" %in% names(ph.data)) {
        stop(
          "\n\U0001f47f `ph.data` does not have the column `chi_geo_kc`, which is required for King County data."
        )
      }

      if (kingco) {
        ph.data <- ph.data[chi_geo_kc == 'King County']
      }

    # group_by ----
      if (!is.null(group_by)) {
        group_col_error <- setdiff(group_by, names(ph.data))
        if (length(group_col_error) > 0) {
          stop(
            paste0(
              "\U0001f6d1 The following `group_by` values are not column names in `ph.data`: ",
              paste0(group_col_error, collapse = ', '),
              "."
            )
          )
        }
      }

    # ypll_age ----
      if (!is.null(ypll_age)) {
        if (!is.numeric(ypll_age) ||
            !all(ypll_age == floor(ypll_age)) ||
            length(ypll_age) != 1 || ypll_age < 1 | ypll_age > 99) {
          stop("\n\U0001f47f `ypll_age` must be an integer between 1 and 99.")
        }

        ypll_col_name <- paste0("ypll_", ypll_age)
      }

    # death_age_col ----
      if (!is.null(death_age_col)) {
        if (!(death_age_col %in% names(ph.data))) {
          stop("\n\U0001f47f `death_age_col` must be the name of column that exists in `ph.data`.")
        }
        col_data <- ph.data[[death_age_col]]
        if (!is.numeric(col_data) || !all(is.na(col_data) | col_data == floor(col_data), na.rm = TRUE)) {
          stop("\n\U0001f47f If `death_age_col` is specified, it must be a column of integers in `ph.data`.")
        }
        if (is.null(ypll_age)) {
          stop("\n\U0001f47f `death_age_col` should not be specified when `ypll_age` is NULL.")
        }
      }

      if (is.null(death_age_col) & !is.null(ypll_age)) {
        if ("chi_age" %in% names(ph.data)) {
          death_age_col <- 'chi_age'
          message("\U0001F4E3 \nYou requested the calculation of YPLL by specifying `ypll_age` and did not provide `death_age_col`.",
                  "\nThe function found and used a column named `chi_age` for the YPLL calculation. If this was not",
                  "\nyour intention, please specify the correct column with the decendant's age with the",
                  "\n`death_age_col` argument.")
        } else {
          stop(paste0("\n\U0001f47f You requested the calculation of YPLL by specifying `ypll_age` and did not provide `death_age_col`.",
                      "\nThe function attempted to use a column named `chi_age`, but it was not found.",
                      "\nTo calculate YPLL, please set death_age_col to the name of the column with the age at death."))
        }
      }

  # Identify cause(s) of interest ----
    if (!is.null(cause)) {
      other_death_causes <- unique(rads.data::icd_other_causes_of_death$cause.of.death)

      # Create a pattern by joining all cause values with "|"
      pattern <- paste(cause, collapse = "|")

      # Use grep with the combined pattern
      x_cause <- unique(grep(pattern, other_death_causes, value = TRUE, ignore.case = TRUE))
    }

    if(!(is.null(cause)) & length(x_cause) == 0){
      stop("\n\U0001f47f Your `cause` value (", cause, ") has filtered out all of the available causes of death.",
           "\nPlease enter a new keyword or keywords and try again.",
           "\nTo view all available cause of death, type the following in your console: death_other()")
      }

  # Count deaths for each cause of death ----
    # prep cause of death reference table ----
      # get reference table from rads.data
      x_reftable <- rads.data::icd_other_causes_of_death[, .(cause.of.death, icd10)]

      # subset for cause of death
      if(!(is.null(cause))){
        x_reftable <- x_reftable[cause.of.death %in% x_cause] # limit to named causes of death from arguments
      }

    # calculate YPLL line level if needed ----
    if(!(is.null(ypll_age))){
      ph.data[, c(ypll_col_name) := fifelse(get(death_age_col) < ypll_age,
                                            ypll_age - get(death_age_col),
                                            0)]
      ph.data[, c(death_age_col) := NULL]
    }

    # merge reference table onto death data ----
    # can't do a simple merge because some causes are sub-categories of others,
    # e.g., drug-overdose is a subset of drug-induced
      unique_cod <- unique(x_reftable$cause.of.death)

      x_combo <- rbindlist(
        lapply(unique_cod, function(each.cod) {
          ph.data[x_reftable[cause.of.death == each.cod], # data.table join faster alternative to merge
                  on = .(icd10_tempy = icd10),
                  nomatch = 0]
        })
      )

      x_combo[, (icdcol) := NULL]

    # calculate death count ----
    if(is.null(ypll_age)){
      x_all <- ph.data[, list(cause.of.death = "All causes", deaths = .N),
                             by = group_by]

      x_combo <- x_combo[, list(deaths = .N), by = c("cause.of.death", group_by)]

      x_combo <- rbind(x_all, x_combo)
    } else {
      # create summary table of YPLL ----
      # all deaths
      x_all <- ph.data[, list(cause.of.death = "All causes",
                                 deaths = .N,
                                 temp_ypll = sum(get(ypll_col_name), na.rm = TRUE)),
                             by = group_by]

      # NCHS causes of death
      x_combo <- x_combo[, list(deaths = .N,
                                   temp_ypll = sum(get(ypll_col_name), na.rm = TRUE)),
                               by = c('cause.of.death', group_by)]

      # combine all_deaths + NCHS_113
      x_combo <- rbind(x_all, x_combo)
      rm(list = c("x_all"))
      setnames(x_combo, "temp_ypll", ypll_col_name)
    }

  # Tidy ----
    # Create rows for zero values (otherwise rows would simply be missing) ----
      # Select columns to use for combination
        cols_to_use <- setdiff(names(x_combo), c('deaths', grep('^ypll_', names(x_combo), value = T)))

      # Create list of unique values for each selected column
        unique_col_vals <- lapply(cols_to_use, function(col) unique(x_combo[[col]]))

      # Create names for the list
        names(unique_col_vals) <- cols_to_use

      # Use CJ to create all combinations
        template_xyz <- do.call(data.table::CJ, unique_col_vals)

      # merge actual values onto template_xyz
        x_combo <- merge(template_xyz,
                         x_combo,
                         all.x = TRUE, # to ensure every combination of categorical vars are available
                         all.y = TRUE) # could also be FALSE because, by definition, all values of y are in X

      # Fill deaths with zeros
        x_combo[is.na(deaths), deaths := 0]

      # Fill ypll_## with zeros if needed
        if(!is.null(ypll_age)){
          ypll_name = grep('^ypll_[0-9]', names(x_combo), value = T)
          x_combo[is.na(get(ypll_name)), paste0(ypll_name) := 0]
        }

    # Sort columns and rows ----
      if(!is.null(ypll_age)){
        setcolorder(x_combo, c("cause.of.death", "deaths", ypll_name))
        setorderv(x_combo, c('cause.of.death', setdiff(names(x_combo), c("deaths", 'cause.of.death', ypll_name)) ))
      } else{
        setorderv(x_combo, c('cause.of.death', setdiff(names(x_combo), c("deaths", 'cause.of.death')) ))
        setcolorder(x_combo, c("cause.of.death", "deaths"))
      }

  # Return data ----
  return(x_combo)
}

# death_xxx_count() ----
#' Summarize NCHS causes of deaths
#'
#' @description
#' Generalized function that is called upon by \code{death_113_count()} and
#' \code{death_130_count()}. Generates death counts for the National Center for
#' Health Statistics (NCHS) Selected Causes of Death (COD). Needs line-level
#' death data with a properly formatted ICD10 column.
#'
#' In addition to the causes of death you specify with \code{causeids} or
#' \code{cause}, it will automatically return the total deaths as well as
#' COVID-19 deaths (since they do not have their own NCHS category).
#'
#'
#' @details
#' See \code{rads::death_113()} & \code{rads::death_130()} for a complete list
#' of available causesid and cause values.
#'
#' @param ph.data a data.table or data.frame. Must contain death data structured
#' with one person per row and with at least one column of ICD10 death codes.
#'
#' @param causeids an integer vector, with a minimum value of 1 and a maximum
#' value of dependent upon the NCHS reference table.
#'
#' @param  cause an OPTIONAL character vector specifying the complete or partial
#' keyword for the cause of death of interest. It is not case sensitive and you
#' can specify it in two ways: 1) \code{cause = c('viral', 'cough')} or 2)
#' \code{cause = c("viral|cough")}. If you specify any keyword(s),
#' the function will ignore the \code{causeids} argument.
#'
#' The default is \code{NULL}, i.e., the function will rely on the \code{causeids}
#' argument to identify the causes of death.
#'
#' @param icdcol a character vector of length one that specifies the name of the
#' column in ph.data that contains the ICD10 death codes of interest.
#'
#' The default is \code{underlying_cod_code}, which is found in the properly
#' formatted death data obtained using the \code{get_data_death()} function.
#'
#' @param kingco a logical vector of length one. It specifies whether you want to
#' limit the analysis to King County.
#'
#' **NOTE** this only works with data imported with the \code{get_data_death()}
#' function because it needs the variable \code{chi_geo_kc}.
#'
#' The default is kingco = TRUE.
#'
#' @param group_by a character vector of indeterminate length. This is used to
#' specify all the variables by which you want to group (a.k.a. stratify) the
#' results. For example, if you specified \code{group_by = c('chi_sex',
#' 'chi_race_6')}, the results would be stratified by each combination of sex
#' and race.
#'
#' The default is \code{group_by = NULL}
#'
#' @param ypll_age an optional numeric vector of length 1. When specified, it
#' should be the age (an integer) used for **Years of Potential Life Lost** (YPLL)
#' calculations. Valid values are between 1 & 99 (inclusive), though 65 and 85 are the most
#' common. For example, \code{ypll_age = 65} would sum the total number of years
#' that could have been lived had everyone in the data lived to at least 65.
#' Note that this function returns the total number of YPLL. Additional
#' processing is necessary to calculate rates per 100,000.
#'
#' The default is \code{ypll_age = NULL}, which will skip YPLL calculations.
#'
#' @param death_age_col an optional character vector of length one that specifies
#' the name of the column in ph.data with the decedents' age at death
#' in years. It is only needed if \code{ypll_age} is
#' specified AND if ph.data lacks a column named \code{chi_age}.
#'
#' The default is \code{death_age_col = NULL}.
#'
#' @param nchsnum specifies whether the function should reference NCHS 113 Selected
#' COD (\code{rads::death_113()}) or NCHS 130 Selected Causes of Infant Death
#' (\code{rads::death_130()}).
#'
#' @references
#' \url{https://www.cdc.gov/nchs/data/dvs/Part9InstructionManual2020-508.pdf} &
#' \url{https://secureaccess.wa.gov/doh/chat/Content/FilesForDownload/CodeSetDefinitions/NCHS113CausesOfDeath.pdf}
#' \url{https://secureaccess.wa.gov/doh/chat/Content/FilesForDownload/TechnicalNotes.pdf}
#'
#' @return
#' Generates a table with three columns, \code{causeid},  \code{cause.of.death},
#' and \code{deaths}. If \code{ypll_age} is specified, a \code{ypll_##} column
#' will also be added to the table. Columns in the \code{group_by}
#' argument will also be returned.
#'
#' By default, it will return all relevant causes of death. You can specify which
#' causes of death you want to assess using the \code{causeids} or \code{cause}
#' arguments.
#'
#' @examples
#' # example 1: death count only
#' set.seed(98104)
#' deathdata <- data.table::data.table(
#'   cod.icd10 = c(rep("A85.2", round(runif(1, 30, 100000), 0)),
#'                 rep("B51", round(runif(1, 30, 100000), 0)),
#'                 rep("U071", round(runif(1, 30, 100000), 0)),
#'                 rep("E44", round(runif(1, 30, 100000), 0)),
#'                 rep("E62", round(runif(1, 30, 100000), 0)),
#'                 rep("G00", round(runif(1, 30, 100000), 0)),
#'                 rep("J10", round(runif(1, 30, 100000), 0)),
#'                 rep("J15", round(runif(1, 30, 100000), 0)),
#'                 rep("V874", round(runif(1, 30, 100000), 0)))
#' )
#' eg1 <- death_xxx_count(ph.data = deathdata,
#'                        causeids = seq(1, 113, 1),
#'                        cause = NULL,
#'                        icdcol = "cod.icd10",
#'                        kingco = FALSE,
#'                        ypll_age = NULL,
#'                        death_age_col = NULL,
#'                        nchsnum = 113)
#' head(eg1)
#'
#' @export
#'
#' @name death_xxx_count
#'
#' @import data.table rads.data
#'
death_xxx_count <- function(ph.data,
                            causeids = NULL,
                            cause = NULL,
                            icdcol = "underlying_cod_code",
                            kingco = TRUE,
                            group_by = NULL,
                            ypll_age = NULL,
                            death_age_col = NULL,
                            nchsnum = NULL) {
  # Global variables used by data.table declared as NULL here to play nice with devtools::check() ----
    problem.icds  <-  causeid <- cause.of.death <- deaths <- '.' <- NULL
    x_reftable <- x_combo <- x_covid <- x_cause <- x_all <- x_ypll <- NULL
    chi_geo_kc <- underlying_cod_code <- icd10 <- x.causeid <- icd10_tempy <- NULL
    date_of_death <- date_of_birth <- calculated.age <- orig.coding <- ypll_col_name <- NULL

  # Check arguments ----
    # ph.data ----
    if (missing(ph.data) || !is.data.frame(ph.data)) {
      stop("\n\U0001f47f `ph.data` must be the unquoted name of a data.frame or data.table")
    }

    # Create a copy of ph.data and ensure it's a data.table
    ph.data <- data.table::setDT(data.table::copy(ph.data))

    # causeids ----
      if (is.null(causeids) & is.null(cause)) {
        stop("\U0001f6d1 You cannot have both `causeids` and `cause` == NULL.")
      }

      if (!is.null(causeids)) {
        if (any(is.na(causeids))) {
          stop("\n\U0001f47f `causeids` must not contain NA values.")
        }
        if (!is.numeric(causeids) || !all(causeids == floor(causeids))) {
          stop("\n\U0001f47f `causeids` must be a vector of integers.")
        }

        if (nchsnum == 113 &&
            (min(causeids) < 1 | max(causeids) > 114)) {
          stop("\n\U0001f47f `causeids` are limited to integers [1, 114].")
        }

        if (nchsnum == 130 &&
            (min(causeids) < 1 | max(causeids) > 130)) {
          stop("\n\U0001f47f `causeids` are limited to integers [1, 130].")
        }

        causeids <- sort(unique(causeids))
      }

    # cause ----
      if (!is.null(cause) && !is.character(cause)) {
        stop("\n\U0001f47f `cause` must either be NULL or a character vector.")
      }

      if (!is.null(cause) & !is.null(causeids)){
        message('\u26A0\ufe0f You specified both a cause and causeid argument. The causes will replace the causeids.')
        causeids <- NULL
        }

    # icdcol ----
      if (!icdcol %in% names(ph.data)) {
        stop("\n\U0001f47f `icdcol` must be the name of a column that exists in `ph.data`.")
      }

      ph.data[, icd10_tempy := death_icd10_clean(get(icdcol))]

    # kingco ----
      if (!is.logical(kingco)) {
        stop("\n\U0001f47f `kingco` must be a logical value, i.e., TRUE or FALSE.")
      }

      if (kingco && !"chi_geo_kc" %in% names(ph.data)) {
        stop(
          "\n\U0001f47f `ph.data` does not have the column `chi_geo_kc`, which is required for King County data."
        )
      }

      if (kingco) {
        ph.data <- ph.data[chi_geo_kc == 'King County']
      }

    # group_by ----
      if (!is.null(group_by)) {
        group_col_error <- setdiff(group_by, names(ph.data))
        if (length(group_col_error) > 0) {
          stop(
            paste0(
              "\U0001f6d1 The following `group_by` values are not column names in `ph.data`: ",
              paste0(group_col_error, collapse = ', '),
              "."
            )
          )
        }
      }

    # ypll_age ----
      if (!is.null(ypll_age)) {
        if (!is.numeric(ypll_age) ||
            !all(ypll_age == floor(ypll_age)) ||
            length(ypll_age) != 1 || ypll_age < 1 | ypll_age > 99) {
          stop("\n\U0001f47f `ypll_age` must be an integer between 1 and 99.")
        }

        ypll_col_name <- paste0("ypll_", ypll_age)
      }

    # death_age_col ----
      if (!is.null(death_age_col)) {
        if (!(death_age_col %in% names(ph.data))) {
          stop("\n\U0001f47f `death_age_col` must be the name of column that exists in `ph.data`.")
        }
        col_data <- ph.data[[death_age_col]]
        if (!is.numeric(col_data) || !all(is.na(col_data) | col_data == floor(col_data), na.rm = TRUE)) {
          stop("\n\U0001f47f If `death_age_col` is specified, it must be a column of integers in `ph.data`.")
        }
        if (is.null(ypll_age)) {
          stop("\n\U0001f47f `death_age_col` should not be specified when `ypll_age` is NULL.")
        }
      }

      if (is.null(death_age_col) & !is.null(ypll_age)) {
        if ("chi_age" %in% names(ph.data)) {
          death_age_col <- 'chi_age'
          message("\U0001F4E3 \nYou requested the calculation of YPLL by specifying `ypll_age` and did not provide `death_age_col`.",
                  "\nThe function found and used a column named `chi_age` for the YPLL calculation. If this was not",
                  "\nyour intention, please specify the correct column with the decendant's age with the",
                  "\n`death_age_col` argument.")
        } else {
          stop(paste0("\n\U0001f47f You requested the calculation of YPLL by specifying `ypll_age` and did not provide `death_age_col`.",
                      "\nThe function attempted to use a column named `chi_age`, but it was not found.",
                      "\nTo calculate YPLL, please set death_age_col to the name of the column with the age at death."))
        }
      }


  # Import reference table once ----
    x_reftable <- if (nchsnum == 113) {
      unique(rads.data::icd_nchs113causes[, .(cause.of.death, causeid, icd10)])
    } else {
      unique(rads.data::icd_nchs130causes[, .(cause.of.death, causeid, icd10)])
    }

  # Identify cause(s) of interest ----
    if (!is.null(cause)) {
      cause <- tolower(cause)
      x_cause <- unique(unlist(lapply(cause, function(i)
        grep(
          i,
          unique(x_reftable$cause.of.death),
          value = TRUE,
          ignore.case = TRUE
        ))))

      if (length(x_cause) == 0) {
        stop(
          paste0(
            "\n\U0001f47f Your `cause` value(s) filtered out all available causes of death. Please enter new keywords and try again."
          )
        )
      }

      x_reftable <- x_reftable[cause.of.death %in% x_cause] # only keep selected causes in reference table
    }

    if (!is.null(causeids)) {
      x_reftable <- x_reftable[causeid %in% causeids]
    }

    # bring causeid 17 into alignment with WA DOH
    x_reftable <- x_reftable[!(causeid == 17 & icd10 == 'U071')]

  # Calculate YPLL line level if needed ----
    if (!is.null(ypll_age) && !is.null(death_age_col)) {
      ph.data[, (ypll_col_name) := fifelse(get(death_age_col) < ypll_age,
                                           ypll_age - get(death_age_col),
                                           0)]
      ph.data[, (death_age_col) := NULL]
    }

  # Merge reference table onto death data ----
    setkey(ph.data, icd10_tempy)
    setkey(x_reftable, icd10)
    x_combo <- x_reftable[ph.data, on = .(icd10 = icd10_tempy), allow.cartesian = TRUE] # data.table join faster alternative to merge
    x_combo[, icd10 := tolower(icd10)]
    x_combo[icd10 == "u071" |
              grepl("^u071", icd10), cause.of.death := "COVID-19 (U07.1)"]
    x_combo <- x_combo[!is.na(cause.of.death)]

  # calculate death counts & YPLL summaries ----
    if (is.null(ypll_age)) {
      x_all <- ph.data[, .(causeid = NA_character_,
                           cause.of.death = "All causes",
                           deaths = .N), by = group_by]
      x_combo <- x_combo[, .(deaths = .N), by = c("causeid", "cause.of.death", group_by)]
      x_combo <- rbind(x_all, x_combo)
    } else {
      x_all <- ph.data[, .(
        causeid = NA_character_,
        cause.of.death = "All causes",
        deaths = .N,
        temp_ypll = sum(get(ypll_col_name), na.rm = TRUE)
      ), by = group_by]
      x_combo <- x_combo[, .(deaths = .N,
                             temp_ypll = sum(get(ypll_col_name), na.rm = TRUE)), by = c("causeid", "cause.of.death", group_by)]
      x_combo <- rbind(x_all, x_combo)
      setnames(x_combo, "temp_ypll", ypll_col_name)
    }

  # Tidy ----
    # Drop orig.coding if exists ----
      if ("orig.coding" %in% names(x_combo)) {
        x_combo[, orig.coding := NULL]
      }

    # Create rows for zero values ----
      # Select columns to use for combination
      cols_to_use <- setdiff(names(x_combo),
                             c('deaths', 'causeid', grep('^ypll_', names(x_combo), value = TRUE)))

      # Create list of unique values for each selected column
      unique_col_vals <- lapply(cols_to_use, function(col) unique(x_combo[[col]]))

      # Create names for the list
      names(unique_col_vals) <- cols_to_use

      # Use CJ to create all combinations
      template_xyz <- do.call(data.table::CJ, unique_col_vals)

      # merge actual values onto template_xyz
      x_combo <- merge(template_xyz,
                       x_combo,
                       all.x = TRUE, # to ensure every combination of categorical vars are available
                       all.y = TRUE) # could also be FALSE because, by definition, all values of y are in X

      # Fill deaths with zeros
      x_combo[is.na(deaths), deaths := 0]

      # Fill ypll_## with zeros if needed
      if (!is.null(ypll_age)) {
        ypll_name <- grep('^ypll_[0-9]', names(x_combo), value = TRUE)
        x_combo[is.na(get(ypll_name)), paste0(ypll_name) := 0]
      }

      # Fill causeid if needed
      x_reftable <- unique(x_reftable[, .(cause.of.death, causeid)])
      x_combo[is.na(causeid), causeid := x_reftable[x_combo[is.na(causeid)], on = .(cause.of.death), causeid]]

    # Sort columns and rows ----
      if (!is.null(ypll_age)) {
        setcolorder(x_combo,
                    c("cause.of.death", "causeid", "deaths", ypll_name))
        setorderv(x_combo, c('cause.of.death', setdiff(
          names(x_combo),
          c("deaths", 'cause.of.death', "causeid", ypll_name)
        )))
      } else {
        setcolorder(x_combo, c("cause.of.death", "causeid", "deaths"))
        setorderv(x_combo, c('cause.of.death', setdiff(
          names(x_combo), c("deaths", 'cause.of.death', "causeid")
        )))
      }

  # Message about COVID-19 if selected causeid 17 ----
    if (nchsnum == 113 && 17 %in% x_combo$causeid) {
      message(
        "\u26A0\ufe0f You selected causeid == 17 (Other and unspecified infectious and parasitic diseases...).
            COVID-19 (U07.1) has been EXCLUDED from this cause, following the example of WA DOH. Note
            however that, as of October 2020, CDC INCLUDES COVID-19 (U07.1) in causeid == 17. In
            other words, APDE followed WA DOH's decision since we provide a separate row for COVID-19."
      )
    }

  # Return data ----
    return(x_combo)
}

# life_table() ----
#' Generate a standard life table
#'
#' @description
#' Generates a standard life table given a data.frame or data.table with basic
#' essential attributes.
#'
#' @references
#' Chiang, Chin Long & World Health Organization. (1979).
#' Life table and mortality analysis / Chin Long Chiang.
#' World Health Organization. https://apps.who.int/iris/handle/10665/62916
#'
#' Silcocks PB, Jenner DA, Reza R. Life expectancy as a summary of mortality in
#' a population: Statistical considerations and suitability for use by health
#' authorities. J Epidemiol Community Health 55(1):38–43. 2001
#'
#' @param ph.data a data.table or data.frame. Must contain aggregated deaths and
#' corresponding populations, as well as the age interval and the average
#' fraction of years lived in the interval by those who die in the interval. It
#' is *highly recommended*, though not necessary, that you use
#' \code{\link{life_table_prep}} to prepare `ph.data`.
#'
#' The default value is \code{ph.data = NULL}.
#' @param myages a character vector of length one identifying a column
#' specifying the beginning and end of each age interval separated by a hyphen.
#' Note, the start of each interval should be the end of the previous
#' interval, e.g., '5-10', '10-15', '15-20', etc. Think of this as short-hand
#' for [5, 10), [10, 15), [15, 20), etc.. The final interval should be open ended with
#' the starting value followed by a '+' (e.g., '85+', '90+', etc.). The maximum
#' age cannot exceed 100 in order to align with WA State population estimates.
#' Leave the value blank (i.e., NA) for the total deaths
#' at an unknown age. These deaths will be distributed proportionately over the
#' other age groups.
#'
#' The default value is \code{myages = "ages"}.
#' @param mydeaths a character vector of length one identifying a numeric column
#' with the total deaths for the given age interval in the given year(s).
#'
#' The default value is \code{mydeaths = "deaths"}.
#' @param mypops a character vector of length one identifying a numeric column
#' with the total population in the age intervals corresponding to mydeaths.
#' This is technically the mid-year population. In practice we usually
#' use [OFM](https://ofm.wa.gov/) population estimates available from
#' \code{\link{get_population}}.
#'
#' The default value is \code{mypops = "pop"}.
#' @param myprops a character vector of length one identifying a numeric column
#' with the average proportion of the interval lived by those who died in the
#' interval. For example, if those who died in '80-85' lived an average of 1000
#' days past their 80th birthday, myprops would be 0.54 (1000/(365.25*5)).
#'
#' The default value is \code{myprops = "fraction"}.
#' @param ci a numeric value representing the confidence level, which must be
#' greater than 0 and less than 1.
#'
#' The default value is \code{ci = 0.95}.
#' @param group_by a character vector used to
#' specify all the variables by which you want to group (a.k.a. stratify) the
#' results. For example, if you specified \code{group_by = c('chi_sex',
#' 'chi_race_6')}, the results would be stratified by each combination of sex
#' and race.
#'
#' The default is \code{group_by = NULL}
#'
#' @return a data.table with the pre-existing columns plus the
#' standard life table columns
#' @details
#' The function returns the following life table columns:
#'
#' - \emph{\bold{mx}}: age interval specific death rate
#'
#' - \emph{\bold{qx}}: probability of dying in the age interval
#'
#' - \emph{\bold{lx}}: # of (theoretical) persons alive at the start of the age interval
#'
#' - \emph{\bold{dx}}: # of deaths during the age interval
#'
#' - \emph{\bold{ax}}: average fraction of the interval lived by those who died in the interval
#'
#' - \emph{\bold{Lx}}: total person years lived in the age interval
#'
#' - \emph{\bold{Tx}}: total person years lived beyond the start of the age interval
#'
#' - ex: expectation of life (a.k.a., life expectancy) at the start of the age
#' interval. ***The value of ex for those under one year of age is typically
#' referred to as 'Life Expectancy at Birth'.***
#'
#' @export
#' @name life_table
#' @examples
#' \donttest{
#' # 1970 CA abridged death data
#' dt <- data.table::data.table(
#'   ages = c("0-1", "1-5", "5-10", "10-15", "15-20", "20-25", "25-30", "30-35",
#'            "35-40", "40-45", "45-50", "50-55", "55-60", "60-65", "65-70",
#'            "70-75", "75-80", "80-85", "85+"),
#'   deaths = c(6234, 1049, 723, 735, 2054, 2702, 2071, 1964, 2588, 4114, 6722,
#'              8948, 11942, 14309, 17088, 19149, 21325, 20129, 22483),
#'   pop = c(340483, 1302198, 1918117, 1963681, 1817379, 1740966, 1457614,
#'           1219389, 1149999, 1208550, 1245903, 1083852, 933244, 770770,
#'           620805, 484431, 342097, 210953, 142691),
#'   fraction = c(0.09, 0.41, 0.44, 0.54, 0.59, 0.49, 0.51, 0.52, 0.53, 0.54, 0.53,
#'                0.53, 0.52, 0.52, 0.51, 0.52, 0.51, 0.50, NA))
#'
#' # Create arbitrary small variations for 'demographic' groups
#' # first create an empty table
#' mygroups <- data.table::CJ(shape = c('circle', 'square'), color = c('blue', 'orange'))
#' dt_groups <- merge(data.table::copy(dt)[,constant := 1],
#'                    mygroups[, constant := 1],
#'                    by = 'constant',
#'                    allow.cartesian = TRUE)[, constant := NULL]
#' # now modify the values
#' set.seed(98104)
#' dt_groups[, deaths := round(deaths * sample(seq(.75, 1.25, .01), .N, replace = TRUE))]
#' dt_groups[, pop := round(pop * sample(seq(.75, 1.25, .01), .N, replace = TRUE))]
#'
#'
#' nogroups <- life_table(ph.data = dt,
#'                      myages = 'ages',
#'                      mydeaths = 'deaths',
#'                      mypops = 'pop',
#'                      myprops = 'fraction',
#'                      group_by = NULL,
#'                      ci = 0.95)
#' head(nogroups)
#'
#' yesgroups <- life_table(ph.data = dt_groups,
#'                       myages = 'ages',
#'                       mydeaths = 'deaths',
#'                       mypops = 'pop',
#'                       myprops = 'fraction',
#'                       group_by = c('shape', 'color'),
#'                       ci = 0.95)
#' head(yesgroups)
#' }
#'
#' @import data.table
#' @importFrom stats qnorm
#'

life_table <- function(ph.data,
                       myages = "ages",
                       mydeaths = "deaths",
                       mypops = "pop",
                       myprops = "fraction",
                       group_by = NULL,
                       ci = 0.95){

  # Global variables used by data.table declared as NULL here to play nice with devtools::check() ----
  istart <- iend <- irank <- ilength <- mx <- qx <- lx <- dx <- Lx <- Tx <- ex <- NULL
  ax <- mx_upper <- mx_lower <- mx_se <- qnorm <- qx_variance <- px_variance <- NULL
  ex_temp <- ex_temp_cumsum <- ex_variance <- ex_se <- ex_lower <- ex_upper <- NULL
  ordered_cols <- newdeaths <- original_order <- NULL
  predicted_mx <- deaths_original <- deaths <- pop <- NULL

  # Get name of the data.frame/data.table ----
  ph.dataname <- deparse(substitute(ph.data))

  # Check arguments ----
    # ph.data ----
      ph.data.name <- deparse(substitute(ph.data))

      if(missing(ph.data)){stop("\n\U0001f47f `ph.data`, the name of a data.frame or data.table with line level death data, must be specified.")}

      if(!is.data.frame(ph.data)){
        stop("\n\U0001f47f `ph.data` must be the unquoted name of a data.frame or data.table")
      }

      if(is.data.frame(ph.data) && !data.table::is.data.table(ph.data)){
        data.table::setDT(ph.data)
      }

      ph.data <- data.table::setDT(data.table::copy(ph.data)) # to prevent changing of original by reference

    # group_by ----
      if(!is.null(group_by)){
        group_col_error <- setdiff(group_by, names(ph.data))
        if(length(group_col_error) > 0){stop(paste0("\U0001f6d1\n The following `group_by` values are not column names in `ph.data`: ", paste0(group_col_error, collapse = ', '), "."))}
      }

    # myages ----
      if(myages == "myages"){
        stop(paste0("\n\U0001f47f If the argument myages == 'myages', R will get confused and angry. Please rename the column 'myages' and run again."))
      }

      if(!myages %in% names(ph.data)){
        stop(paste0("\n\U0001f47f 'myages' (", myages, ") is not the name of a column in 'ph.data'."))}

      if(nrow(ph.data[!is.na(get(myages))]) != nrow(ph.data[!is.na(get(myages)) & get(myages) %like% "[0-9]-[0-9]|[0-9]\\+"])){
        stop(paste0("\n\U0001f47f The values in 'myages' (i.e., ", myages, ") must be in the form #-# or #+, e.g., '10-15' or '85+'"))}

      # check that myages is unique per combination of values in group_by
        if (is.null(group_by)) {
          if(nrow(ph.data) != length(unique(ph.data[[myages]]))){
            stop(paste0("\n\U0001f47f The values in 'myages' (i.e., ", myages, ") must be unique"))}
        } else {
          # Create a temporary data.table to check for duplicates
          temp <- ph.data[, list(count = .N), by = c(myages, group_by)]
          # Check if any group has more than one entry (which would mean a duplicate)
          if(any(temp$count > 1)){
            stop(paste0("\n\U0001f47f The values in 'myages' (i.e., ", myages, ") must be unique for each combination of the values in the `group_by` variables"))}
        }

      if(sum(grepl("[0-9]\\+", unique(ph.data[[myages]]))) != 1){
        stop(paste0("\n\U0001f47f The final age in 'myages' (i.e., ", myages, ") must be in the form #+, e.g., '85+' or '90+'. \nAll other values must be in the form #-#, e.g., 20-25"))}

    # mypops ----
      if(mypops == "mypops"){
        stop(paste0("\n\U0001f47f If the argument mypops == 'mypops', R will get confused and angry. Please rename the column 'mypops' and run again."))
      }
      if(!mypops %in% names(ph.data)){
        stop(paste0("\n\U0001f47f 'mypops' (", mypops, ") is not the name of a column in 'ph.data'."))}
      if(!is.numeric(ph.data[[mypops]])){
        stop(paste0("\n\U0001f47f 'mypops' (i.e., ", mypops, ") must be of class == numeric"))}

    # mydeaths ----
      if(mydeaths == "mydeaths"){
        stop(paste0("\n\U0001f47f If the argument mydeaths == 'mydeaths', R will get confused and angry. Please rename the column 'mydeaths' and run again."))
      }
      if(!mydeaths %in% names(ph.data)){
        stop(paste0("\n\U0001f47f 'mydeaths' (", mydeaths, ") is not the name of a column in 'ph.data'."))}
      if(!is.numeric(ph.data[[mydeaths]])){
        stop(paste0("\n\U0001f47f 'mydeaths' (i.e., ", mydeaths, ") must be of class == numeric"))}
      if(nrow(ph.data[is.na(get(myages)) & !is.na(get(mydeaths))]) > 1 ){
        stop(paste0("\n\U0001f47f 'ph.data' (i.e., ", ph.dataname, ") can only have 1 row with deaths where the myages is NA."))}

    # myprops ----
      if(myprops == "myprops"){
        stop(paste0("\n\U0001f47f If the argument myprops == 'myprops', R will get confused and angry. Please rename the column 'myprops' and run again."))
      }
      if(!myprops %in% names(ph.data)){
        stop(paste0("\n\U0001f47f 'myprops' (", myprops, ") is not the name of a column in 'ph.data'."))}
      if(!is.numeric(ph.data[[myprops]])){
        stop(paste0("\n\U0001f47f 'myprops' (i.e.,", myprops, ") must be of class == numeric"))}
      if(nrow(ph.data[!get(myprops) %between% 0:1]) > 0){
        stop(paste0("\n\U0001f47f 'myprops' (i.e., ", ax, ") should be a proportion (i.e., it must be between 0 & 1)"))}

    # ci ----
      if( !class(ci) %in% c("numeric")){
        stop(paste0("\n\U0001f47f `ci` (", ci, ") should be a two digit decimal between 0.01 & 0.99"))}
      if(!(ci >= 0.01 & ci <= 0.99)){
        stop(paste0("\n\U0001f47f `ci` (", ci, ") should be a two digit decimal between 0.00 & 0.99"))}

  # Copy ph.data to prevent changing original by reference ----
    ph.data <- data.table::setDT(data.table::copy(ph.data))

  # Get name of pre-existing variables ----
    orig_cols <- data.table::copy(names(ph.data))

  # Split myages to create intervals ----
    ph.data[,c("istart", "iend") := tstrsplit(gsub("\\+", "", get(myages)), "-")]
    ph.data[, c("istart", "iend") := lapply(.SD, as.integer), .SDcols = c("istart", "iend")]
    if(is.null(group_by)){
      ph.data[, irank := rank(istart)]
        setorder(ph.data, istart) # critical that table is sorted from youngest to oldest
    }else{
        ph.data[, irank := rank(istart), group_by]
        setorderv(ph.data, c(group_by, 'istart'))
      }
    ph.data[, ilength := iend - istart]
    ph.data[is.na(iend), ilength := 100-istart] # adjustment for final interval

  # Distribute deaths with unknown age proportionately among deaths with known ages ----
    # Create a simple function for a table of unique demographics
      distribute_deaths <- function(ph.data.sub, myages, mydeaths) {
        if(nrow(ph.data.sub[is.na(get(myages)) & !is.na(get(mydeaths))]) > 0){
          deaths.unk.age <- sum(ph.data.sub[is.na(get(myages)), get(mydeaths)], na.rm = TRUE)  # count num of deaths with unknown age
          ph.data.sub <- ph.data.sub[!is.na(get(myages))] # delete rows from summary table with unknown age
          # Distribute unknown death among rows with ages
          ph.data.sub[, newdeaths := get(mydeaths) + (deaths.unk.age * get(mydeaths) / sum(ph.data.sub[[mydeaths]])), by = list(get(myages))]
          ph.data.sub[, (mydeaths) := NULL] # drop original death count b/c to be replaced by newdeaths
          setnames(ph.data.sub, 'newdeaths', mydeaths)
        }
        return(ph.data.sub)
      }

    # Use the distribute_deaths function
      if(is.null(group_by)){
        ph.data <- distribute_deaths(ph.data.sub = ph.data, myages = myages, mydeaths = mydeaths)
      } else {
        ph.split <- split(ph.data, by = group_by) # create a list of tables with unique combo of group_by values
        ph.data <- rbindlist(lapply(ph.split,
                               FUN = function(x) distribute_deaths(ph.data.sub = x, myages = myages, mydeaths = mydeaths)), use.names = T)
      }

  # Check that beginning of each interval == end of previous interval ----
    if (is.null(group_by)) {
      invalid_rows <- ph.data[, list(rownumber = .I[shift(iend, n = 1L, type = "lag") != istart
                                                 & !is.na(shift(iend, n = 1L, type = "lag"))])]
      } else {
        invalid_rows <- ph.data[, list(rownumber = .I[shift(iend, n = 1L, type = "lag") != istart
                                               & !is.na(shift(iend, n = 1L, type = "lag"))]),
                            by = c(group_by)]
      }

    if(nrow(invalid_rows) > 0){
      stop(paste0("\n\U0001f47f The values in 'myages' (i.e., ", myages, ") are misspecified.",
      "\nThe start of each interval must be the end of the previous interval"))
    }

  # Calculate metrics for life table ----
  # ax ... the proportion (i.e., fraction) of person-years lived in the interval by those who died in the interval ----
    # Note that CDC approximates with 0.5 for 1 year intervals, but I have real data so will use that instead when possible.
    ph.data[get(myprops) == 0, paste0(myprops) := 0.5] # when zero deaths in age bin, approximate fraction half the time period
    if(is.null(group_by)){
      ph.data[irank == max(irank), paste0(myprops) := NA]} else { # fraction for oldest age bin is set to NA following WA DOH example for Adams County
        ph.data[irank == max(irank), paste0(myprops) := NA, group_by]
      }

  # mx ... calculate the age specific death rate ----
  # mx = #_deaths_in_age_group / #_person_years_lived_in_age_group
    # ph.data[, mx := deaths / ((ilength*(pop-deaths)) + (ilength*ax*deaths))] # Chiang ch 2, formula 1.2
    ph.data[, mx := get(mydeaths)/get(mypops)] # Chiang 2.4 ... "age specific death rate can be estimated from ..."
    ph.data[mx > 1, mx := 1] # due to small numbers, it is possible for #deaths>#pop, especially for single old age groups. Probability > 100% illogical.

    # Use predicted mx for highest age group if necessary
      if(nrow(ph.data[grepl('[0-9]+\\+', get(myages)) & mx == 0]) > 0){

        mxPredicted <- life_table_predict_mx(ph.data, group_by, myages)

        warning(paste0(
          "\n\u26A0\ufe0f",
          "\nWarning: Zero deaths detected in one or more oldest age groups (e.g., `", myages, "==", unique(ph.data[istart == max(istart)][[myages]]), "`).",
          "\nThis may indicate an error in data preparation or reflect small population sizes.\n",
          "\nThe function has provided modeled `mx` values, affecting `Tx` and `ex` calculations,",
          "\nincluding life expectancy at birth. `ex_se`, `ex_lower`, and `ex_upper` for the",
          "\noldest age group are based soley on modeled deaths and should be interpreted cautiously.",
          "\n\nPlease double-check your data preparation and consider whether life table calculations ",
          "\nare appropriate for your population size."))

        if(is.null(group_by)){
          ph.data <- merge(ph.data,
                           mxPredicted,
                           by = myages,
                           all = T)
        } else {
          ph.data <- merge(ph.data,
                           mxPredicted,
                           by = c(group_by, myages),
                           all = T)
        }

        ph.data[!is.na(predicted_mx) & mx == 0, mx := predicted_mx]
        ph.data[, predicted_mx := NULL]
      }

    ph.data[, mx_upper := qgamma((ci+(1-ci)/2), get(mydeaths) + 1) / get(mypops)] # exact Poisson upper CI
    ph.data[, mx_se := (mx_upper - mx) / qnorm((ci+(1-ci)/2))] # reverse_engineer poisson standard error
    ph.data[, mx_upper := NULL]

  # qx ... probability of dying in the interval ----
    ph.data[, qx := ilength*mx / (1 + ((1-get(myprops))*ilength*mx))] # Chiang formula 1.4 & 2.3
    ph.data[qx > 1, qx := 1]

    if(is.null(group_by)){
      ph.data[irank == max(irank), qx := 1] # probability of death for those in final age group is always 100%
      } else { ph.data[irank == max(irank), qx := 1, group_by]}

  # lx ... # alive at the start of the age interval ----
    # create mini-function to calculate lx
      create_lx <- function(ph.data.sub){
        ph.data.sub[1, lx := 100000] # start with hypothetical pop of 100K
          for(ii in seq(2, nrow(ph.data.sub), 1)){
            ph.data.sub[ii, lx := ph.data.sub[ii-1]$lx * (1-ph.data.sub[ii-1]$qx) ]
          }
        return(ph.data.sub)
      }
    # use create_lx()
      if(is.null(group_by)){
        ph.data <- create_lx(ph.data.sub = ph.data)
      } else {
        ph.split <- split(ph.data, by = group_by) # create a list of tables with unique combo of group_by values
        ph.data <- rbindlist(lapply(ph.split,
                                    FUN = function(x) create_lx(ph.data.sub = x)), use.names = T)
      }

  # dx ... # deaths in age interval ----
    ph.data[, dx := qx * lx] # same as ph.data[, dx := lx - shift(lx, n = 1L, type = "lead")]

  # Lx ... calculate person-years lived in age interval ----
    # ph.data[!grepl("^0", age.range), Lx := lx - (0.5*dx)] # approximation, approximation doesn't apply to first year
    ph.data[, Lx := ilength*(lx - dx) + (ilength*get(myprops)*dx)] # Chiang formula 2.3 & 2.7
    if(is.null(group_by)){
      ph.data[irank == max(irank), Lx := dx / mx] # Chiang formula 3.10, for final interval which is open ended
    }else{ph.data[irank == max(irank), Lx := dx / mx, group_by]}

  # Tx ... calculate total number of person-years lived over start of age interval ----
    # this is a sum of all Lx for the same age range or older
    # table is sorted from youngest to oldest, so reverse Lx before applying cumulative sum
    # then need to reverse the cumulative sum
    if(is.null(group_by)){
      ph.data[, Tx := rev(cumsum(rev(Lx)))]
      ph.data[irank == max(irank), Tx := Lx] # Chiang formula 3.12, for final interval which is open ended
    } else {
      ph.data[, Tx := rev(cumsum(rev(Lx))), group_by]
      ph.data[irank == max(irank), Tx := Lx, group_by] # Chiang formula 3.12, for final interval which is open ended
    }

  # ex ... expectation of life (aka life expectancy) at start of age interval ----
    ph.data[, ex := Tx / lx]
    if(is.null(group_by)){
      ph.data[irank == max(irank), ex := 1/mx] # Chiang formula 3.12, for final interval which is open ended
    } else {
      ph.data[irank == max(irank), ex := 1/mx, group_by]
    }

  # Calculate uncertainty for life expectancy ----
  ph.data[, qx_variance := ((qx^2)*(1-qx)) / get(mydeaths)] # Chiang 2.2 variance of qx
    # when have zero deaths, would have 0/0 (undefined) as variance, so ascribe the median of the
    # observed variances (except zero for 85+)
    # first create small function
      fill_variance <- function(ph.data.sub, group_by = NULL){
        # Get the group identification if group_by is provided
        group_text <- ""
        if(!is.null(group_by) && length(group_by) > 0) {
          # Create a vector of "column = value" pairs
          group_pairs <- sapply(group_by, function(col) {
            val <- unique(ph.data.sub[[col]])
            if(length(val) == 1) {  # Most common case - one value per split group
              paste0(col, " = ", val)
            } else {
              # Handle unlikely case of multiple values in what should be a unique group
              paste0(col, " = [", paste(val, collapse = ", "), "]")
            }
          })

          # Join the pairs with commas
          group_text <- paste0(" for ", paste(group_pairs, collapse = ", "))
        }

        obs.variances <- sort(ph.data.sub[!is.nan(qx_variance) & qx_variance != 0]$qx_variance)
        if(length(obs.variances) < 0.5*nrow(ph.data.sub)){
          warning(paste0(
            "\u26A0\ufe0f Small population issue", group_text, ".\n",
            "Found ", nrow(ph.data.sub[is.nan(qx_variance)]), " age groups where the variance of the probability of dying in the interval was NaN.\n",
            "This exceeds 50% of age groups and is likely due to zero deaths in these age groups.\n",
            "The median variance will be used as an approximation.\n",
            "Life expectancy calculations may be unreliable.\n"
          ))
        }
        ph.data.sub[is.nan(qx_variance), qx_variance := median(obs.variances)]
        return(ph.data.sub)
      }

    # Then apply fill.variance() function
      if(is.null(group_by)){
        ph.data <- fill_variance(ph.data.sub = ph.data, group_by)
      } else {
        ph.split <- split(ph.data, by = group_by) # create a list of tables with unique combo of group_by values
        ph.data <- rbindlist(lapply(ph.split,
                                    FUN = function(x) fill_variance(ph.data.sub = x, group_by)), use.names = T)
      }

  ph.data[, px_variance := qx_variance] # Chiang 3.6, variance prob(survival) == variance of prob(death)

  if(is.null(group_by)){
      ph.data[, ex_temp := (lx^2) * ((((1-get(myprops))*ilength) + shift(ex, 1L, type = "lead"))^2) * px_variance] # Chiang page 137
  } else {
    ph.split <- split(ph.data, by = group_by) # create a list of tables with unique combo of group_by values
    ph.data <- rbindlist(lapply(ph.split,
                                FUN = function(x){
                                  x[, ex_temp := (lx^2) * ((((1-get(myprops))*ilength) + shift(ex, 1L, type = "lead"))^2) * px_variance]
                                  return(x)} ), use.names = T)}

  # reverse cumulative sum, so flip, get cumsum, then flip back
    # get original order
      ph.data[, original_order := .I]

    # sort by rank (and group_by if needed)
      setorderv(setorder(ph.data, -irank), group_by)

    # generate cumulative sum
      ph.data[!is.na(ex_temp), ex_temp_cumsum := cumsum(ex_temp),
              by = if (is.null(group_by)) NULL else mget(group_by)]

    # restore order
      setorder(ph.data, original_order)
      ph.data[, original_order := NULL]

  # divide ex_temp_cumsum by lx^2 to get sample variance
    ph.data[, ex_variance := ex_temp_cumsum / lx^2]

  # Variance for oldest age interval (Silcocks' formula) ----
  # variance for oldest age interval cannot be calculated using the Chiang method
  # and is assumed to be zero because qx for the oldest interval == 1.00.
  # CDC follows Silcocks' approximation, not Chiang's assumption, which is what we will use here.
  # The next commented out formula appears in multiple CDC publications, but it seems incorrect
  # because (a) I see nothing like this in Silcock's paper, and (b) the variance is enormous
  # to the point of uselessness. On March 9, 2022 I received confirmation from the
  # authors that the printed formula was incorrect.
  # ph.data[irank == max(irank), ex_variance := ((lx^2)/(mx^4)) * mx_se^2]
  # The replacement formula below was derived from careful study of Silcocks' original paper

  # When there are no deaths on record for the oldest age group, the Silcocks' method
  # cannot estimate the variance. For the sake of generating an uncertainty estimate, we
  # will use an estimate of deaths had they experienced the mx predicted by
  # rads:::life_table_predict_mx(). Afterward, we will restore the original death
  # records and will give a warning to the end user about interpreting the confidence
  # interval

  # Generate estimated deaths when needed for max age group
  ph.data[, deaths_original := get(mydeaths)]
  ph.data[irank == max(irank) & get(mydeaths) == 0, c(mydeaths) := as.integer(round2(pop * mx))]

  # Silcocks' formula
  ph.data[irank == max(irank),
          ex_variance := (0.5*ph.data[irank == max(irank)-1]$Lx) *
            (4 / get(mydeaths)*(mx^2))]

  if(is.null(group_by)){
    ph.data[irank == max(irank), ex_variance := (0.5*ph.data[irank == max(irank)-1]$Lx) * (4 / get(mydeaths)*(mx^2))]
  } else {
    ph.split <- split(ph.data, by = group_by) # create a list of tables with unique combo of group_by values
    ph.data <- rbindlist(lapply(ph.split,
                                FUN = function(x){
                                  x[irank == max(irank), ex_variance := (0.5*x[irank == max(irank)-1]$Lx) * (4 / get(mydeaths)*(mx^2))]
                                  return(x)} ), use.names = T)}

  # Use variance to calculate confidence intervals
  ph.data[, ex_se := sqrt(ex_variance)]
  zscore = qnorm(1 - (1-ci)/2) # since two sided, need to split the alpha for upper and lower tails
  ph.data[, ex_lower := ex - ex_se * zscore]
  ph.data[, ex_upper := ex + ex_se * zscore]

  # Restore original death data
  ph.data[, c(mydeaths) := deaths_original][, deaths_original := NULL]

  # Tidy final output ----
  # order and subset columns
  if("ax" %in% names(ph.data)){
    ordered_cols <- c(myages, mypops, mydeaths, "mx", "qx", "lx", "dx", "ax", "Lx", "Tx", "ex", "ex_lower", "ex_upper", "ex_se")
  } else{
    ph.data[, ax := get(myprops)]
    ordered_cols <- c(myages, mypops, mydeaths, "mx", "qx", "lx", "dx", "ax", "Lx", "Tx", "ex", "ex_lower", "ex_upper", "ex_se")
  }
  ordered_cols <- c(setdiff(orig_cols, ordered_cols), ordered_cols)
  ph.data <- ph.data[, ordered_cols, with = FALSE]

  # rounding
  ph.data[, c("lx", "dx", "Lx", "Tx", mydeaths) := lapply(.SD, rads::round2, 0), .SDcols = c("lx", "dx", "Lx", "Tx", mydeaths)]
  ph.data[, c("qx", "mx") := lapply(.SD, rads::round2, 5), .SDcols = c("qx", "mx")]
  ph.data[, c("ex_se") := lapply(.SD, rads::round2, 5), .SDcols = c("ex_se")]
  ph.data[, c("ax", "ex", "ex_lower", "ex_upper") := lapply(.SD, rads::round2, 2), .SDcols = c("ax", "ex", "ex_lower", "ex_upper")]

  # Return object from function ----
  return(ph.data)
}

# life_table_predict_mx() ----
#' Predict Mortality Rate for Highest Age Group
#'
#' @description
#' This function predicts the `mx` value (age-specific mortality rate) for the
#' highest age group (e.g., '85+') where there is insufficient data for
#' calculating it directly. It is intended for internal use by `rads`'
#' \code{\link{life_table}} only.
#'
#' @details
#' The function uses a simplification of the Gompertz–Makeham law of mortality.
#' In the log10-linear model `log10(mx) = a + b * istart`:
#' - `a` (intercept) represents the Makeham age-independent mortality term
#' - `b` represents the Gompertz age-dependent term
#'
#' The use of a base 10 log model and the value of the `empirical_adjustment_factor`
#' were determined empirically by modeling age and gender specific USA 2018
#' mortality data, and 2009-2018 WA and King County mortality data. These model
#' parameters best fit these mortality data.
#'
#' Age specific mortality rates and are known to increase exponentially
#' after age 30 and the `empirical_adjustment_factor` is based on predicting
#' `mx` for those >= 85 years old. It is recommended that you do not use use
#' this function when the max age is is less than '80+'.
#'
#' @param ph.data A data.table containing mortality data
#'
#' The default is \code{ph.data = ph.data}, where `ph.data` is passed
#' from `rads::life_table()`
#'
#' @param group_by Variables used for stratification
#'
#' The default is \code{group_by = group_by}, where `group_by` is passed
#' from `rads::life_table()`
#'
#' @param myages A vector of length one containing the name of the column with
#' the age categories in their proper format (e.g., `c('65-74', 75-84', '85+')`).
#'
#'
#' The default is \code{myages = myages}, where `myages` is passed from
#' `rads::life_table()`
#'
#' @param empirical_adjustment_factor Adjustment factor for predicted mx
#'
#' The default is \code{empirical_adjustment_factor = 1.8}
#'
#' @return A data.table with predicted mx values for the highest age group
#'
#' @examples
#' \donttest{
#' library(data.table)
#'
#' # create data set ----
#' deaths <- data.table(
#'            ages = c("35-44", "45-54", "55-64", "65-74", "75-84", "85+"),
#'            istart = c(35, 45, 55, 65, 75, 85),
#'            gender = rep("Both", 6),
#'            mx = c(0.001947, 0.003959, 0.008867, 0.017833, 0.043861, 0)
#' )
#'
#' # generate predictions ----
#' output <- rads:::life_table_predict_mx(
#'            ph.data = deaths,
#'            group_by = 'gender',
#'            myages = 'ages'
#' )
#'
#' print(output)
#' }
#' @keywords internal
#'
#' @import data.table
#' @importFrom stats lm coef
#'
#' @note
#' This is an internal function and should not be called directly by users.
#' It is exposed for transparency and documentation purposes only.
#'
life_table_predict_mx <- function(ph.data = ph.data,
                                  group_by = group_by,
                                  myages = myages,
                                  empirical_adjustment_factor = 1.8) {
  # Global variables used by data.table declared as NULL here to play nice with devtools::check()
  istart <- mx <- any_zero_mx <- mygroup <- NULL

  # Filter for ages 30 and above, excluding the highest age group
  if (is.null(group_by)) {
    ph.data_2mod <- copy(ph.data)[istart >= 30 & istart < max(istart)]
  } else {
    ph.data_2mod <- copy(ph.data)
    ph.data_2mod[, mygroup := .GRP, by = c(group_by)]
    ph.data_2mod <- ph.data_2mod[mygroup %in% ph.data_2mod[grepl('[0-9]+\\+', get(myages)) & mx == 0]$mygroup]
    ph.data_2mod[, mygroup := NULL]
    ph.data_2mod <- ph.data_2mod[istart >= 30 & istart < max(istart)]
  }

  # Identify the maximum istart value
  max_istart <- max(ph.data$istart)

  # Function to perform log10-linear extrapolation for a single group
  extrapolate_group <- function(group) {
    # Fit log10-linear model: log10(mx) = a + b * istart
    # This is an empirically informed simplification of Gompertz–Makeham law of mortality
    model <- stats::lm(log10(mx) ~ istart, data = group)

    # Extract coefficients
    a <- coef(model)[1] # Makeham age-independent component
    b <- coef(model)[2] # Gompertz age-dependent component

    # Predict mx for the maximum istart value with adjustment
    predicted_mx <- unname(empirical_adjustment_factor * 10^(a + b * max_istart))
    return(predicted_mx)
  }

  # Function to check if any group has mx == 0
  check_for_zero_mx <- function(tempdt, group_by) {
    # Check for groups where any mx is zero
    zero_mx_groups <- tempdt[, list(any_zero_mx = any(mx == 0)), by = group_by]

    # Filter for groups where any mx is zero
    problematic_groups <- zero_mx_groups[any_zero_mx == TRUE]

    # Return the problematic groups
    return(problematic_groups)
  }

  if (is.null(group_by)) {
    # If no strata variables, perform extrapolation on entire dataset
    result <- data.table(ages = unique(ph.data[istart == max(istart)][[myages]]),
                         predicted_mx = extrapolate_group(ph.data_2mod))
  } else {
    # Check for groups with mx == 0
    problematic_groups <- check_for_zero_mx(ph.data_2mod, group_by)[, any_zero_mx := NULL]

    if (nrow(problematic_groups) > 0) {
      # Generate a message listing the problematic groups
      problematic_groups <- paste(capture.output(print(problematic_groups, row.names = FALSE, class = FALSE, print.keys = FALSE)), collapse = "\n")

      problematic_groups_message <- paste0(
        "\n\U1F6D1 Your oldest age bin for at least one of your groups had zero deaths. This results in ",
        "\nan age specific death rate (mx) of zero for the oldest age group, which causes problems ",
        "\nin life table calculations. To get around this problem, we tried to extrapolate mx for ",
        "\nthe oldest age bin based on the mx for age bins above 30 years old. However, the ",
        "\nfollowing group(s) also have mx == 0 for at least one other age bin, which prevents ",
        "\n us from modeling mx for the oldest age bin:\n",
        problematic_groups,
        "\nThis almost certainly means that your population is too small for life table estimation.",
        "\nPlease drop these groups from the analysis or ensure the data is correct."
      )

      # Stop the function and inform the user
      stop(paste(problematic_groups_message))
    }

    # Perform extrapolation within each stratum
    result <- ph.data_2mod[, list(ages = unique(ph.data[istart == max(istart)][[myages]]),
                                  predicted_mx = extrapolate_group(.SD)),
                           by = group_by]
  }

  return(result)
}

# life_table_prep () ----
#' Prepare death data for use with life_table()
#'
#' @description
#' Processes individual-level death data to create a standardized data table.
#' This table is collapsed/aggregated by age bin and optionally, by
#' demographics, for use with \code{\link{life_table}}.
#'
#' @param ph.data a data.table or data.frame. Must contain individual-level
#' death data with the date of birth, date of death, and any demographics which
#' you want to use to aggregate the resulting table.
#'
#' The default value is \code{ph.data = NULL}.
#'
#' @param cuts integer vector of any length greater than 1 (typically of
#' length ~ 20). It specifies the cut-points for the age groupings to be created
#' in the data. Each number represents the beginning of an interval and the final
#' cut extends through to the maximum age observed. For example, when cuts =
#' c(0, 5, 10, 20), the data will be grouped into ages [0,5), [5,10), [10,20), and [20,
#' infinity).
#'
#' The default is \code{cuts= c(0, 1, 5, 10, 15, 18, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65,
#' 70, 75, 80, 85)}, which creates the standard age groupings used by WA DOH.
#'
#' @param dobvar character vector of length one identifying a column with
#' the decedent's date of birth. The referenced column must be of class
#' 'date' or class 'character' in the format "YYYY-MM-DD" or "YYYY/MM/DD."
#'
#' The default is \code{dobvar = "date_of_birth"}, which is the dob variable
#' available via `rads::get_data_death()`.
#'
#' @param dodvar character vector of length one identifying a column with
#' the decedent's date of death. The referenced column must be of class
#' 'date' or class 'character' in the format "YYYY-MM-DD" or "YYYY/MM/DD."
#'
#' The default is \code{dodvar = "date_of_death"}, which is the dod variable
#' available via `rads::get_data_death()`.
#'
#' @param group_by a character vector of indeterminate length. This is used to
#' specify all the variables by which you want to group (a.k.a. stratify) the
#' results. For example, if you specified \code{group_by = c('chi_sex',
#' 'chi_race_6')}, the results would be grouped by each combination of sex
#' and race. If you leave it blank (i.e., `group_by = NULL`), it will only
#' provide the death counts and death fractions by age group
#' described by `cuts`.
#'
#' The default is \code{group_by = NULL}
#'
#' @return a data.table with deaths aggregated by any demographics specified in
#' the `group_by` argument, as well as `ages` (age group), `deaths` (deaths per
#' demographic group and age group), and `fraction` (the mean fraction of the
#' age interval lived by those who died in that interval).
#'
#' @details
#' Note that population data (from \code{\link{get_population}}) must be merged
#' onto the returned data.table before running it through
#' \code{\link{life_table}}.
#'
#' @export
#' @name life_table_prep
#' @examples
#' \donttest{
#'  # create data set ----
#' set.seed(98104)
#'
#' deaths <- data.table::data.table(
#'   date_of_death = as.Date("2020-01-01") + sample(0:365, 10000, replace = TRUE),
#'   race_eth = rep_len(c("AIAN", "Asian", "Black", "Hispanic", "NHPI", "White"),
#'   length.out = 10000),
#'   gender = sample(c('Male', 'Female'), 10000, replace = TRUE),
#'   year = 2020
#' )
#' # Calculate a date of birth based on a maximum age of 120 years (~43800 days)
#' deaths[, date_of_birth := date_of_death - sample(1:43800, 10000, replace = TRUE)]
#'
#'  # process with life_table_prep ----
#'  output1 <- life_table_prep(ph.data = deaths)
#'  head(output1)
#'
#'  # process with life_table_prep using group_by argument----
#'  output2 <- life_table_prep(ph.data = deaths, group_by = c('gender', 'race_eth'))
#'  head(output2)
#' }
#'
#' @import data.table
#' @importFrom lubridate years add_with_rollback
#'
life_table_prep <- function(ph.data,
                            cuts = c(0, 1, 5, 10, 15, 18, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85),
                            dobvar = "date_of_birth",
                            dodvar = "date_of_death",
                            group_by = NULL){
  # Global variables used by data.table declared as NULL here to play nice with devtools::check() ----
  orig_cols <- dob <- dod <- dob_na <- dob_na <- death_age <- tempz <- NULL
  end <- start <- interval <- age.lab <- ages <- length.interval <- NULL
  interval.start <- interval.end <- fraction <- ph.datasum <- '.' <- deaths <- NULL

  # Check arguments ----
    # ph.data ----
      ph.data.name <- deparse(substitute(ph.data))

      if(missing(ph.data)){stop("\n\U0001f47f `ph.data`, the name of a data.frame or data.table with line level death data, must be specified.")}

      if(!is.data.frame(ph.data)){
        stop("\n\U0001f47f `ph.data` must be the unquoted name of a data.frame or data.table")
      }

      if(is.data.frame(ph.data) && !data.table::is.data.table(ph.data)){
        data.table::setDT(ph.data)
      }

      ph.data <- data.table::setDT(data.table::copy(ph.data)) # to prevent changing of original by reference

      if(length(intersect(c("start", "end", "dob", "dod"), names(ph.data))) > 1){
        warning("\n\u26A0\ufe0f ph.data has a column named 'start', 'end', 'dob', or 'dod' which was overwritten by this function. \nTo preserve your columns, rename them, and run this function again.")}

    # cuts ----
      if(isTRUE(any(is.na(cuts))) || is.null(cuts) || !is.numeric(cuts)){stop("\n\U0001f47f 'cuts' must be specified as a numeric vector and cannot contain any NA's.")}
      if(length(cuts) <= 1){stop("\n\U0001f47f 'cuts' should be a numeric vector of length >1 and typically of length ~20.")}
      if(min(cuts) < 0){stop("\n\U0001f47f The minimum age in 'cuts' should be zero.")}
      if(max(cuts) > 100){warning("\n\u26A0\ufe0f You're maximum age in 'cuts' is greater than 100. \nYou do not have to change this, but know that ages are top coded at 100.")}

    # dobvar ----
      if(dobvar == "dobvar"){
        stop(paste0("\n\U0001f47f If the argument dobvar == 'dobvar', R will get confused and angry. Please rename the column 'dobvar' and run again."))
      }
      if(!dobvar %in% names(ph.data)){stop("\n\U0001f47f 'dobvar' must specify the name of a date of birth column that exists in ph.data.")}

    # dodvar ----
      if(dodvar == "dodvar"){
        stop(paste0("\n\U0001f47f If the argument dodvar == 'dodvar', R will get confused and angry. Please rename the column 'dodvar' and run again."))
      }
      if(!dodvar %in% names(ph.data)){stop("\n\U0001f47f 'dodvar' must specify the name of a date of death column that exists in ph.data.")}

      if(nrow(ph.data[get(dodvar) < get(dobvar)]) > 0){
        warning(paste0("\n\u26A0\ufe0f There are ", nrow(ph.data[dodvar < dobvar]), " rows where 'dodvar' is less than 'dobvar'. \nThese date pairs had their values set to NA and will only contribute indirectly to the life_table calculations."))
        ph.data[get(dodvar) < get(dobvar), paste0(dodvar) := NA]
        ph.data[get(dodvar) < get(dobvar), paste0(dobvar) := NA]
      }

    # group_by ----
      if(!is.null(group_by)){
        group_col_error <- setdiff(group_by, names(ph.data))
        if(length(group_col_error) > 0){stop(paste0("\U0001f6d1\nThe following `group_by` values are not column names in `ph.data`: ", paste0(group_col_error, collapse = ', '), "."))}
      }

  # Copy ph.data to prevent changing original by reference ----
    orig_cols <- data.table::copy(names(ph.data))

  # Standardize dob and dod ----
    ph.data[!is.na(get(dobvar)), dob := as.Date(get(dobvar))]
    ph.data[!is.na(get(dodvar)), dod := as.Date(get(dodvar))]

    # confirm that dob and dod are legitimate
    dob_na <- nrow(ph.data[is.na(dob)])/nrow(ph.data)
    dod_na <- nrow(ph.data[is.na(dod)])/nrow(ph.data)
    if(dob_na > 0.01){warning("\n\u26A0\ufe0f More than 1% of the date of birth values are missing. \nPlease check that your variable is of class Date or is of class character in the form 'YYYY-MM-DD' or 'YYYY/MM/DD'. \nDeaths with unknown dob will be distributed proportionately among deaths with known dates of birth.")}
    if(dod_na > 0.01){warning("\n\u26A0\ufe0f More than 1% of the date of death values are missing. \nPlease check that your variable is of class Date or is of class character in the form 'YYYY-MM-DD' or 'YYYY/MM/DD'.")}

  # Properly calculate age at death ----
    ph.data[, death_age := rads::calc_age(dob, dod)]

  # Mark the age bins and time intervals specified by 'cuts' ----
    ph.data[,  ages := cut(death_age, cuts, right = F)]
    ph.data[, ages := gsub("\\[|\\]|\\)", "", ages)]
    ph.data[, ages := gsub("\\,", "-", ages)]
    ph.data[death_age >= max(cuts), ages := paste0(max(cuts), "+")]

  # Calculate proportion of interval lived within the interval in which the person died ----
    ph.data[,  c("start", "end") := tstrsplit(gsub("\\+", "", ages), "-")]
    ph.data[, c("start", "end") := lapply(.SD, as.integer), .SDcols = c("start", "end")]
    ph.data[start == max(as.numeric(ph.data$start), na.rm = T), end := 100] # set max age == 100 because pop also tops out at 100

    ph.data[, interval.start := lubridate::add_with_rollback(dob, lubridate::years(as.integer(start)))] # intervals starts at birthday + # of years for start of interval
    ph.data[, interval.end := lubridate::add_with_rollback(dob, lubridate::years(1 + as.integer(end))) - 1] # end of interval is 1 day before birthday that would start next interval

    ph.data[, fraction := as.integer(dod - interval.start) / as.integer(interval.end - interval.start)]
    ph.data[fraction > 1, fraction := 1] # for oldest age group, can actually live > 100, but capped at 100 because of pop data, so force max prop to 1

  # Collapse/aggregate  ----
  if(!is.null(group_by)){
    collapse_cols <- c(group_by, "ages", "fraction")
  } else {collapse_cols <- c("ages", "fraction")}
  ph.data <- ph.data[, .SD, .SDcols = collapse_cols]
  ph.datasum <- ph.data[, list(deaths = .N, fraction = mean(fraction, na.rm = T)), by = setdiff(collapse_cols, "fraction")]
  data.table::setorderv(ph.datasum, c("ages", setdiff(collapse_cols, "ages")))

  # Ensure every possible combo of group_by and ages is present ----
  possibleAges <- cut(0:100, cuts, right = F)
  possibleAges <- gsub("\\[|\\]|\\)", "", possibleAges)
  possibleAges <- unique(gsub("\\,", "-", possibleAges))
  possibleAges[is.na(possibleAges)] <- paste0(max(cuts), "+")

  if(!is.null(group_by)){
    possibleGroupBy <- lapply(group_by,
                             function(col){
                               unique(ph.data[!is.na(ph.data[[col]]), col, with = FALSE][[1]])})
    possibleGroupBy <- do.call(CJ, possibleGroupBy)
    setnames(possibleGroupBy, group_by)
    template <- possibleGroupBy[rep(1:.N, each = length(possibleAges))]
    template[, ages := rep(possibleAges, times = .N/length(possibleAges))]

    ph.datasum <- merge(template,
                        ph.datasum,
                        by = c('ages', group_by),
                        all = T)
  } else {
    template <- data.table(ages = possibleAges)
    ph.datasum <- merge(template,
                        ph.datasum,
                        by = c('ages'),
                        all = T)
  }

  ph.datasum[is.na(deaths), deaths := 0]
  ph.datasum[is.na(fraction), fraction := 0] # later will be changed to 0.5 when there are zero deaths in an age bin, but keeping it simpler for user of this function


  # return the object from function ----
  #print("Note!! These aggregated deaths need to be merged with population data with the same demographics before running through the life_table() function.")
  return(ph.datasum)

}
