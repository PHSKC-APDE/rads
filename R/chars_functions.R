# chars_injury_matrix() ----
#' View available combinations of CHARS injury mechanisms and intents
#'
#' @description
#' Function to view all combinations of the Comprehensive
#' Hospital Abstract Reporting System (CHARS) Injury Matrix mechanisms and
#' intents available in rads.
#'
#' Generates a table with two columns, `mechanism` & `intent`.
#' Use it to identify the combinations of `mechanism` & `intent` that
#' you want to use in [chars_injury_matrix_count()].
#'
#' @details
#' This function provides the terms used by the hospitalization function
#' [chars_injury_matrix_count()] and may not be the same as those used with
#' [death_injury_matrix_count()].
#'
#' @note
#' This function does not take any arguments.
#'
#' This function will return all available categories, some of which are specific
#' to ICD9-cm (2012-2015) and some of which are specific to ICD10-cm (2016+).
#'
#' @source
#' Reference table [rads.data::icdcm_injury_matrix] created by
#' [rads.internal::make_chars_injury_matrix()].
#'
#' @return
#' A data.table with two columns: `mechanism` & `intent`. The number
#' of rows are determined by the available combinations in the reference table.
#'
#' @references
#' - WA DOH Community Health Assessment Tool > User Guides > Code Set Definitions >
#'   Hospitalization Injury Matrix ICD10CM
#' - CDC Injury Code and Matrices: <https://www.cdc.gov/nchs/injury/injury_matrices.htm>
#'
#' @export
#'
#' @name chars_injury_matrix
#'
#' @examples
#' myDT <- chars_injury_matrix()
#' print(myDT)
#'
chars_injury_matrix <- function(){
  # Get unique combinations from rads.data reference table
  imTable <- unique(rads.data::icdcm_injury_matrix[, list(mechanism, intent)])

  # Sort by mechanism and intent for consistency
  data.table::setorder(imTable, mechanism, intent)

  return(imTable)
}

# chars_injury_matrix_count() ----
#' Generate injury matrix counts from line-level Comprehensive Hospital
#' Abstract Reporting System (CHARS) data
#'
#' @description
#' Generate hospitalization counts for an injury matrix
#' specifying the intent and mechanism of injury. Needs line-level CHARS data
#' with columns beginning with `mechanism_` and `intent_`. Covers both ICD9-cm
#' (2012-2015) and ICD10-cm (2016+).
#'
#' @param ph.data a data.table or data.frame. Must contain CHARS data structured
#' with one person per row and predetermined `mechanism_` and `intent_` columns.
#' In other words, this ph.data should come from use of [apde.data::chars()].
#'
#' The default is `ph.data = NULL`
#'
#' @param intent a character vector. It specifies the intent of the injury
#' related hospitalization that you want returned (e.g., "assault",
#' "intentional", "unintentional", etc.). "none" will ignore the intent and only
#' return the mechanism of the injury leading to hospitalization.
#'
#' **NOTE**
#' You do not have to type the entire keyword for the intent, a
#' partial string match is sufficient and is case insensitive. E.g.,
#' `intent = c("intent")` would return both "intentional" and "unintentional" and
#' `intent = c("un")` would return both "undetermined" and "unintentional".
#'
#' The default is `intent = '*'`, which selects all possible intents
#'
#' @param mechanism a character vector. It specifies the mechanism of the injury
#' related hospitalization that you want returned (e.g., "cut_pierce",
#' "drowning", "fall", "firearm", etc.). "none" will ignore the mechanism and
#' only return the intent of the injury leading to hospitalization.
#'
#' To see the complete list of mechanisms, type
#' `unique(chars_injury_matrix()$mechanism)` in your R console.
#'
#' **NOTE**
#' You do not have to type the entire keyword for the mechanism, a
#' partial string match is sufficient and is case insensitive. E.g.,
#' `mechanism = c("fire")` would return both "firearm" and
#' "fire_burn".
#'
#' The default is `mechanism = '*'`, which selects all possible mechanisms
#'
#' @param group_by a character vector of indeterminate length. This is used to
#' specify all the variables by which you want to group (a.k.a. stratify) the
#' results. For example, if you specified `group_by = c('chi_sex',
#' 'race3')``, the results would be stratified by each combination of sex
#' and race.
#'
#' The default is `group_by = NULL`
#'
#' @param def a character vector of length one, limited to 'narrow' or 'broad'.
#' It specifies whether you want to use the CDC's recommended 'narrow' approach,
#' which requires that the **principal diagnosis** of an injury
#' hospitalization be a nature-of-injury ICD10-CM code. Or, alternatively, the
#' 'broad' definition that searches all available diagnosis fields on the
#' hospital discharge record (there can be a maximum of 54 diagnosis fields in
#' CHARS data).
#'
#' **NOTE**
#' ph.data must contain the columns named `injury_nature_narrow` &
#' `injury_nature_broad`.
#'
#' The default is `def = 'narrow'`
#'
#' @param primary_ecode a logical vector of length one. It specifies whether you
#' want to limit the analysis to using just the primary ecode (i.e., the
#' `injury_ecode` variable), rather than all available ecodes.
#'
#' As of RADS 1.1.7.7, the only valid argument is TRUE (T). Those wanting to perform
#' an analysis with other ecodes would need to perform a custom analysis in SQL using
#' the `chars.stage_diag` & `chars.stage_ecode` tables.
#'
#' The default is `primary_ecode = TRUE`
#'
#' @param kingco a logical vector of length one. It specifies whether you want to
#' limit the analysis to King County. Note that this only works when you have
#' the column `chi_geo_kc` imported from the [apde.data::chars()] function.
#'
#' The default is `kingco = TRUE`
#'
#' @details
#' Since the injury analysis uses many columns, we suggest that you obtain
#' ph.data with `apde.data::chars(cols = NA)`, rather than trying to specify the
#' columns of interest.
#'
#' This function will only count injuries where the type of injury (from
#' ICD10-CM codes) AND the corresponding external cause information (from an
#' ecode) is present. To examine injuries that do not have an external cause,
#' please examine the analytic data directly.
#'
#' See Jeremy Whitehurst's documentation for the CHARS ETL to understand exactly
#' which codes are mapped to which intents and mechanisms.
#'
#' @return
#' The function returns a data.table with a minimum of three columns:
#' `mechanism`, `intent`, & `hospitalizations`. Any
#' `group_by` variables would also have their own columns.
#'
#' The function default is to return the matrix of all intents and mechanisms
#' of injury related hospitalizations. You can choose to only return the intent
#' or only return the mechanism. If you set both to "none", you will receive a
#' summary of all injury hospitalizations without regard to the intent or mechanism.
#'
#' @references
#' - WA DOH Community Health Assessment Tool > User Guides > Code Set Definitions >
#'   Hospitalization Injury Matrix ICD10CM
#' - CDC Injury Code and Matrices: <https://www.cdc.gov/nchs/injury/injury_matrices.htm>
#'
#' @export
#'
#' @name chars_injury_matrix_count
#'
#' @examples
#' # example: 2022 King County hospitalizations due to intentional injury, by sex
#' \donttest{
#' blah = apde.data::chars(year = 2022, kingco = TRUE)
#' myresult <- chars_injury_matrix_count(ph.data = blah,
#'                                       intent = '^intentional',
#'                                       mechanism = 'none',
#'                                       group_by = c('chi_sex'))
#' print(myresult)
#' }
#'
chars_injury_matrix_count<- function(ph.data = NULL,
                                     intent = "*",
                                     mechanism = "*",
                                     group_by = NULL,
                                     def = 'narrow',
                                     primary_ecode = TRUE,
                                     kingco = TRUE){
  # Check arguments ----
    # ph.data ----
      if (missing(ph.data) || !is.data.frame(ph.data)) {
        stop("\n\U0001f47f `ph.data` must be the unquoted name of a data.frame or data.table")
      }
      if (!data.table::is.data.table(ph.data)) {
        data.table::setDT(ph.data)
      }
      ph.data <- data.table::copy(ph.data) # to prevent changing of original by reference

    # seq_no (unique identifier) ----
        if(!'seq_no' %in% names(ph.data)){
          stop("\U2620\U0001f47f\U2620\nph.data must contain the 'seq_no' column, which is the unique identifier.")}
        if('seq_no' %in% names(ph.data) & length(unique(ph.data$seq_no)) != nrow(ph.data)){
          stop("\U2620\U0001f47f\U2620\nThe 'seq_no' is a unique patient identifier and should not be repeated across rows.")}

    # intent ----
        if("none" %in% intent & length(intent) != 1){
          stop("\n\U0001f47f The intent 'none' cannot be specified with any other intents.")
        }
        if("*" %in% intent & length(intent) != 1){
          stop("\n\U0001f47f The intent '*' cannot be specified with any other intents.")
        }

    # mechanism ----
        if("none" %in% mechanism & length(mechanism) != 1){
          stop("\n\U0001f47f The mechanism 'none' cannot be specified with any other mechanisms.")
        }
        if("*" %in% mechanism & length(mechanism) != 1){
          stop("\n\U0001f47f The mechanism '*' cannot be specified with any other mechanisms.")
        }

    # group_by ----
        if(!is.null(group_by)){
          if(!inherits(group_by, 'character')){
            stop("\n\U0001f47f `group_by` must either be NULL or specify a character vector of column names.")}
          if(length(setdiff(group_by, names(ph.data))) > 0){
            stop(paste0("\n\U0001f47f `group_by` contains the following column names which do not exist in ph.data: ", paste(setdiff(group_by, names(ph.data)), collapse = ', ') ))}
        }

    # def ----
        if(!def %in% c('narrow', 'broad') | length(def) != 1){
          stop("\n\U0001f47f `def` can only have ONE of two values, 'narrow' or 'broad'.")
        }
        if(!"injury_nature_narrow" %in% names(ph.data) | !is.logical(ph.data$injury_nature_narrow)){
          stop("\n\U0001f47f `injury_nature_narrow` must exist exist in ph.data and must be of type logical (TRUE|FALSE).")
        }
        if(!"injury_nature_broad" %in% names(ph.data) | !is.logical(ph.data$injury_nature_broad)){
          stop("\n\U0001f47f `injury_nature_broad` must exist exist in ph.data and must be of type logical (TRUE|FALSE).")
        }

    # primary_ecode ----
        if(!isTRUE(primary_ecode) && !isFALSE(primary_ecode) ){stop("\n\U0001f47f `primary_ecode` must be a logical vector of length 1, i.e,. TRUE or FALSE.")}
        if(isFALSE(primary_ecode)){stop(paste0("\n\U1F6D1 \U2620 \U0001f47f\n",
                                        " You set 'primary_ecode = F'. This is no longer a valid option. If you want to use other ecodes\n",
                                        " you will have to perform a custom analysis using [chars].[stage_diag] & [chars].[stage_ecode]."))}

    # kingco ----
        if(!isTRUE(kingco) && !isFALSE(kingco) ){stop("\n\U0001f47f `kingco` must be a logical vector of length 1, i.e,. TRUE or FALSE.")}
        if (isTRUE(kingco) & (!"chi_geo_kc" %in% names(ph.data))){
          stop("\n\U0001f47f You specified kingco=TRUE, but `ph.data` does not have the following columns that identify King County data:
                   chi_geo_kc")
        }
        if (isTRUE(kingco)){ph.data <- ph.data[chi_geo_kc == "King County"]}

  # Apply narrow or broad definition ----
      if(def == 'narrow'){ph.data <- ph.data[injury_nature_narrow == T & !is.na(injury_intent)]}
      if(def == 'broad'){ph.data <- ph.data[injury_nature_broad == T & !is.na(injury_intent)]}

  # Get complete list of all possible mechanisms and intents ----
      possible.intents <- as.character(unique(chars_injury_matrix()$intent))
      possible.mechanisms <- as.character(unique(chars_injury_matrix()$mechanism))

  # Identify intent of interest ----
      intent = tolower(intent)

      if("none" %in% intent){ # none means 'any intent', i.e., 'ignore' the intent
        x_intent = "any"
      }

      if("*" %in% intent){x_intent = possible.intents}

      if(length(intersect(c("*", "none"), intent)) == 0){
        x_intent = c()
        for(i in intent){
          x_intent <- unique(c(x_intent, grep(i, possible.intents, value = TRUE, ignore.case = TRUE)))
        }
      }

      if(length(x_intent) < length(intent) & !("*" %in% intent) & !("none" %in% intent)){
        matched <- sapply(intent, function(i) any(grepl(i, possible.intents, ignore.case = TRUE)))
        unmatched <- intent[!matched]
        warning(paste0("\n\u26A0\ufe0f The following intent value(s) did not match any valid intents and were ignored: ",
                       paste0(unmatched, collapse = ', ')))
      }

      if(length(x_intent) == 0){stop(paste0(
        "\n\U0001f47f \nYour `intent` value (", intent, ") has filtered out all of the hospitalization injury intents.\nPlease enter 'none', '*', or a new partial keyword term and try again."))}

  # Identify mechanism of interest ----
      mechanism = tolower(mechanism)

      if("none" %in% mechanism){ # none means 'any mechanism', i.e., 'ignore' the mechanism
        x_mechanism = "any"
      }

      if("*" %in% mechanism){x_mechanism = possible.mechanisms}

      if(length(intersect(c("*", "none"), mechanism)) == 0){
        x_mechanism = c()
        for(i in mechanism){
          x_mechanism <- unique(c(x_mechanism, grep(i, possible.mechanisms, value = TRUE, ignore.case = TRUE)))
        }
      }

      if(length(x_mechanism) < length(mechanism) & !("*" %in% mechanism) & !("none" %in% mechanism)){
        matched <- sapply(mechanism, function(i) any(grepl(i, possible.mechanisms, ignore.case = TRUE)))
        unmatched <- mechanism[!matched]
        warning(paste0("\n\u26A0\ufe0f The following mechanism value(s) did not match any valid mechanisms and were ignored: ",
                       paste0(unmatched, collapse = ', ')))
      }

      if(length(x_mechanism) == 0){stop(paste0(
        "\n\U0001f47f \nYour `mechanism` value (", mechanism, ") has filtered out all of the hospitalization injury mechanisms.\nPlease enter 'none', '*', or a new partial keyword term and try again.\n",
        "Entering `rads::chars_injury_matrix()` into the console will provide you with a table of valid options."))}

  # Create motor_vehicle_traffic column when needed ----
      if('motor_vehicle_traffic' %in% x_mechanism){
        mvt_cols <- paste0('mechanism_', grep('mvt_', possible.mechanisms, value = TRUE))
        ph.data[, mechanism_motor_vehicle_traffic := Reduce(function(x, y) pmax(x, y, na.rm = TRUE),
                                                            .SD,
                                                            init = NA_real_),
                                                      .SDcols = mvt_cols]
      }

  # Count hospitalizations for each intent_x_mechanism of interest ----
        # create matrix of all mechanisms and intents of interest ----
        x_grid <- data.table::CJ(mechanism = x_mechanism, intent = x_intent)

        # count number of hospitalizations (i.e., rows) when def == 'narrow' ----
        x_combo <- rbindlist(lapply(1:nrow(x_grid), function(ii) {
          temp.ph.data <- copy(ph.data)

        # Identify whether the combination of mech & intent in x_grid has any hospitalizations in person level data ----
          # could theoretically use injury_mechanism & injury_intent, but would need extra coding to address when either has value 'any'
            temp.ph.data[, bingo := as.integer(get(paste0("mechanism_", x_grid[ii]$mechanism)) >= 1 &
                                                 get(paste0("intent_", x_grid[ii]$intent)) >= 1)]

        # Aggregate (sum) the number of hospitalizations for the mech / intent combination from x_grid ----
            if(!is.null(group_by)){
              temp.ph.data <- temp.ph.data[, list(mechanism = as.character(x_grid[ii]$mechanism),
                                                  intent = as.character(x_grid[ii]$intent),
                                                  hospitalizations = sum(bingo)),
                                           by = group_by]}
            if(is.null(group_by)){
              temp.ph.data <- temp.ph.data[, list(mechanism = as.character(x_grid[ii]$mechanism),
                                                  intent = as.character(x_grid[ii]$intent),
                                                  hospitalizations = sum(bingo))]}

          # create grid of all possible combinations of group_by vars ----
            gridvars <- setdiff(names(temp.ph.data), 'hospitalizations')
            complete.grid <- do.call(CJ, lapply(gridvars, function(x) unique(temp.ph.data[[x]])))
            setnames(complete.grid, gridvars)

          # merge temp.ph.data onto complete.grid ----
            temp.ph.data <- merge(complete.grid, temp.ph.data, all = T)
            temp.ph.data[is.na(hospitalizations), hospitalizations := 0]

          return(temp.ph.data)
        }), fill=TRUE)

  # Tidy ----
    # Additional collapse/aggregate if mechanism == 'none' ----
      if("none" %in% mechanism){
        x_combo[, mechanism := "Any mechanism"]
        x_combo <- x_combo[, list(hospitalizations = sum(hospitalizations)), by = setdiff(names(x_combo), "hospitalizations")]
      }

      x_combo[mechanism == 'any', mechanism := "Any mechanism"]

    # Additional collapse/aggregate if intent == 'none' ----
      if("none" %in% intent){
        x_combo[, intent := "Any intent"]
        x_combo <- x_combo[, list(hospitalizations = sum(hospitalizations)), by = setdiff(names(x_combo), "hospitalizations")]
      }

      x_combo[intent == 'any', intent := "Any intent"]

    # Sort columns and rows ----
        setcolorder(x_combo, c("mechanism", "intent", "hospitalizations"))
        setorderv(x_combo, c("mechanism", "intent", setdiff(names(x_combo), c("hospitalizations", "mechanism", "intent")) ))

  # Return data ----
    return(x_combo)
}

# chars_icd_ccs() ----
#' View available CHARS ICD-9-CM OR ICD-10-CM (diagnosis) codes, descriptions,
#' and summary 'broad' and 'detailed' classifications that can be used with
#' [chars_icd_ccs_count()]
#'
#' @description
#' A function to view the complete list of ICD-9-CM OR ICD-10-CM codes and
#' descriptions as well as corresponding 'broad' and 'detailed' classifications.
#' The 'broad' and 'detailed' classifications broadly follow AHRQ's HCUP Clinical
#' Classifications Software Refined (CCSR) standards for ICD-10-CM. ICD-9-CM
#' codes were then mapped to the same 'broad' and 'detailed' categories to maximize
#' comparability across time. The 'superlevel' and 'midlevel' categorizations
#' were developed by APDE, based in large part on the 'broad' and 'detailed'
#' classifications.
#'
#' Output is provided in the form of a table. Use this table to inform your
#' arguments in the [chars_icd_ccs_count()] function.
#'
#' @param ref_type a character vector of length one specifying the hospital diagnosis
#' descriptions that are of interest to you. Acceptable options include: `'all'`,
#' `'icdcm'`, `'superlevel'`, `'broad'`, `'midlevel'`, & `'detailed'`.
#'
#' The default is `ref_type = 'all'`.
#'
#' @param icdcm_version an integer vector of length one specifying the ICD CM
#' version that you want to reference. Acceptable options include: `9`
#' & `10`.
#'
#' The default is `icdcm_version = 10`.
#'
#' @note
#' If you do not specify any arguments, the function will return a table with
#' all ICD-10-CM codes as well ICD-10-CM, superlevel, broad, midlevel, and
#' detailed descriptions.
#'
#' @source
#' Reference tables [rads.data::icd9cm_ccs] and [rads.data::icd10cm_ccs]
#' created by [rads.internal::make_chars_icd_ccs()].
#'
#' @references
#' <https://hcup-us.ahrq.gov/toolssoftware/ccsr/ccs_refined.jsp>
#'
#' @return
#' A data.table. The number of rows and columns are dependent upon the arguments
#' submitted to the function.
#'
#' @export
#'
#' @name chars_icd_ccs
#'
#' @examples
#' myCCS <- chars_icd_ccs(ref_type = 'all')
#' head(myCCS)
#'
chars_icd_ccs <- function(ref_type = 'all',
                          icdcm_version = 10){
  # check arguments ----
  if(length(ref_type) != 1){
    stop("\n\U0001f47f the `ref_type` must have a single value. Valid options are 'all', 'icdcm', 'superlevel', 'broad', 'midlevel', & 'detailed'")}
  if(!ref_type %in% c('all', 'icdcm', 'superlevel', 'broad', 'midlevel', 'detailed')){
    stop(paste0("\n\U0001f47f '", ref_type, "' is not a valid option for the `ref_type` argument. \nValid options are 'all', 'icdcm', 'superlevel', 'broad', 'midlevel', & 'detailed'."))}

  if(!icdcm_version %in% c(9, 10) | length(icdcm_version) != 1){
    stop("\n\U0001f47f the `icdcm_version` argument is limited to the integers '9' OR '10'")}

  # get data from rads.data ----
  if(icdcm_version == 9){
    chars_list <- data.table::copy(rads.data::icd9cm_ccs)
  } else {
    chars_list <- data.table::copy(rads.data::icd10cm_ccs)
  }

  # Select columns based on ref_type ----
  if(ref_type == 'all'){
    chars_list <- chars_list[, list(icdcm_code, icdcm, superlevel, broad, midlevel, detailed, icdcm_version)]
  }
  if(ref_type == 'icdcm'){
    chars_list <- chars_list[, list(icdcm_code, icdcm, icdcm_version)]
  }
  if(ref_type == 'superlevel'){
    chars_list <- chars_list[, list(superlevel, icdcm_version)]
  }
  if(ref_type == 'broad'){
    chars_list <- chars_list[, list(broad, icdcm_version)]
  }
  if(ref_type == 'midlevel'){
    chars_list <- chars_list[, list(midlevel, icdcm_version)]
  }
  if(ref_type == 'detailed'){
    chars_list <- chars_list[, list(detailed, icdcm_version)]
  }

  return(unique(chars_list))
}

# chars_icd_ccs_count() ----
#' Count (non-injury) Comprehensive Hospital Abstract Reporting System
#' (CHARS) hospitalizations
#'
#' @description
#' Generate hospitalization counts from WA State Comprehensive Hospital
#' Abstract Reporting System (CHARS) data using partial strings from the ICD-10-CM
#' or ICD-9-CM descriptions or AHRQ HCUP's CCSR based 'broad' and 'detailed'
#' classifications. Needs line-level CHARS data with a properly formatted
#' ICD-CM column (e.g., the data available from \code{get_data_chars()}).
#'
#' See \code{chars_icd_ccs()} for a complete list of available ICD-10-CM,
#' ICD-9-CM, and superlevel, broad, midlevel, and narrow classifications.
#'
#' \strong{¡¡¡REMEMBER!!!} ICD-10-CM started in 2016! Be sure to use the correct
#' **\code{icdcm_version}**.
#'
#'
#' @details
#' This function needs the user to enter a search string in one or more of the
#' following arguments in order to search the CHARS data for the corresponding
#' ICD CM codes: \code{icdcm}, \code{superlevel}, \code{broad}, \code{midlevel},
#' or \code{detailed}.
#' Partial search terms are acceptable and they are case-insensitive. For
#' example, if you set \code{broad = 'ex'} with \code{icdcm_version = 10}, the
#' function would return counts for "Diseases of the eye and adnEXa" as well as
#' "EXternal causes of morbidity". It also understands simple regex syntax,
#' including **\code{^}**, **\code{$}**, and **\code{|}**.
#'
#' **Note:** If you submit values for more than one of \code{icdcm},
#' \code{superlevel}, \code{broad}, \code{midlevel}, or \code{detailed} they must
#' be nested. For example, \code{broad = 'neoplasms', detailed = 'sarcoma'} will
#' give results because sarcomas are type of cancers. However,
#' \code{broad = 'neoplasms', detailed = 'intestinal infection'} will return an
#' error because your resulting table will have zero rows.
#'
#' @param ph.data a data.table or data.frame. Must contain CHARS data structured
#' with one person per row and with at least one column of ICD CM codes.
#'
#' **NOTE!** ph.data must have a column named `seq_no`, which is a unique patient
#' level identifier.
#'
#' The default is \code{ph.data = NULL}
#'
#' @param icdcm_version an integer vector of length one specifying the ICD CM
#' version that you want to reference. Acceptable options include: \code{9}
#' & \code{10}.
#'
#' The default is \code{icdcm_version = 10}.
#'
#' @param CMtable An optional data.table containing the reference table of ICD codes
#' and their classifications. This should come from \code{\link{chars_icd_ccs}}
#' and have the following columns: \code{icdcm_code}, \code{icdcm}, \code{superlevel},
#' \code{broad}, \code{midlevel}, \code{detailed}, and \code{icdcm_version}. If
#' provided, the function will use this table instead of making a new call to
#' \code{\link{chars_icd_ccs}}, which can significantly improve performance when
#' making multiple calls to this function.
#'
#' The default is \code{CMtable = NULL}, which means the function will fetch the reference
#' table from the database using \code{\link{chars_icd_ccs}}.
#'
#' @param icdcm a character vector of length 1. An ICD CM description OR code.
#' It is case agnostic and works with partial strings. For example, both
#' 'rotavira' & 'A080' would provide the results for 'Rotaviral enteritis' for
#' ICD-10-CM. You can also combine multiple search terms. For example,
#' 'rotavira|choler' would count all Rotaviral enteritis AND cholera
#' hospitalizations. View available options with
#' \code{chars_icd_ccs(ref_type = 'icdcm', icdcm_version = 10)}.
#'
#' The default is \code{icdcm = NULL}
#'
#' @param superlevel a character vector of length 1. Case agnostic and works
#' with partial strings. View available options with
#' \code{chars_icd_ccs(ref_type = 'superlevel', icdcm_version = 10)}.
#'
#' The default is \code{superlevel = NULL}
#'
#' @param broad a character vector of length 1. Case agnostic and works with
#' partial strings. View available options with
#' \code{chars_icd_ccs(ref_type = 'broad', icdcm_version = 10)}.
#'
#' The default is \code{broad = NULL}
#'
#' @param midlevel a character vector of length 1. Case agnostic and works with
#' partial strings. View available options with
#' \code{chars_icd_ccs(ref_type = 'midlevel', icdcm_version = 10)}.
#'
#' The default is \code{midlevel = NULL}
#'
#' @param detailed a character vector of length 1. Case agnostic and works with
#' partial strings. View available options with
#' \code{chars_icd_ccs(ref_type = 'detailed', icdcm_version = 10)}.
#'
#' The default is \code{detailed = NULL}
#'
#' @param icdcol a character vector of length one that specifies the name of the
#' column in ph.data that contains the ICD10-cm codes of interest.
#'
#' The default is \code{icdcol = 'diag1'}, which refers to the principal
#' diagnosis code provided by \code{get_data_chars()}).
#'
#' @param group_by a character vector of indeterminate length. This is used to
#' specify all the variables by which you want to group (a.k.a. stratify) the
#' results. For example, if you specified \code{group_by = c('chi_sex',
#' 'chi_race_6')}, the results would be stratified by each combination of sex
#' and race.
#'
#' The default is \code{group_by = NULL}
#'
#' @param kingco a logical vector of length one. It specifies whether you want to
#' limit the analysis to King County.
#'
#' **NOTE** this only works with data imported
#' with the \code{get_data_chars()} function because it needs the variable
#' \code{chi_geo_kc}.
#'
#' The default is \code{kingco = TRUE}.
#'
#' @param mykey Character vector of length 1. Identifies the keyring:: service
#' that can be used to access the Health & Human Services Analytic Workspace
#' (HHSAW).
#'
#' The default is \code{mykey = 'hhsaw'}
#'
#' @return
#' Generates a table with columns for each of the search terms you entered (e.g.,
#' \code{icdcm}, \code{broad}, and/or \code{detailed}) as well as
#' any \code{group_by} variables and a column named \code{hospitalizations} that
#' contains the relevant counts.
#'
#' @export
#'
#' @name chars_icd_ccs_count
#'
#' @examples
#' # example #1: 2019 King County hospitalizations for chemotherapy, by sex
#' \donttest{
#' blah = get_data_chars(year = 2019, kingco = TRUE)
#' myresult <- chars_icd_ccs_count(ph.data = blah,
#'                                 detailed = 'headache',
#'                                 group_by = c('chi_sex'))
#' print(myresult)
#' }
#'
#' # example #2: 2022 King County hospitalizations for asthma using
#' # an external reference table
#' \donttest{
#' myrefTable <- chars_icd_ccs()
#'
#' mydata = get_data_chars(year = 2022, kingco = TRUE)
#' myresult <- chars_icd_ccs_count(ph.data = mydata,
#'                                 CMtable = myrefTable,
#'                                 detailed = 'asthma',
#'                                 group_by = c('chi_sex'))
#' print(myresult)
#' }
#'
#' @import data.table rads.data
#'
chars_icd_ccs_count <- function(ph.data = NULL,
                                icdcm_version = 10,
                                CMtable = NULL,
                                icdcm = NULL,
                                superlevel = NULL,
                                broad = NULL,
                                midlevel = NULL,
                                detailed = NULL,
                                icdcol = 'diag1',
                                group_by = NULL,
                                kingco = T,
                                mykey = 'hhsaw'){

  # Global variables used by data.table declared as NULL here to play nice with devtools::check() ----
    CMtable_cols <- CMtable.expanded <- filter.count <- problem.icds <- superlevel_desc <- broad_desc <-
      midlevel_desc <- detailed_desc <- chi_geo_kc <- hospitalizations <- icdcm_code <- KeepMe <-
      icdcm_desc <- icdcm_code <- query.group <- diag1 <- intent_ignore <-
      chars_injury_matrix_count <- mechanism_ignore <- dummy <- NULL

  # Check arguments & filter reference table of all ICD CM (CMtable) ----
    # ph.data ----
        if (missing(ph.data) || !is.data.frame(ph.data)) {
          stop("\n\U0001f47f `ph.data` must be the unquoted name of a data.frame or data.table")
        }
        if (!data.table::is.data.table(ph.data)) {
          data.table::setDT(ph.data)
        }
        ph.data <- data.table::copy(ph.data) # to prevent changing of original by reference

    # icdcm_version ----
        if(!icdcm_version %in% c(9, 10) | length(icdcm_version) != 1){stop("\n \U0001f47f the `icdcm_version` argument is limited to the integers '9' OR '10'")}

    # CMtable ----
        if (!is.null(CMtable)){
          if (!is.data.frame(CMtable)) {
            stop("\n\U0001f47f `CMtable` must be the unquoted name of a data.frame or data.table, typically from rads::chars_icd_ccs()")
          }
          if (!data.table::is.data.table(CMtable)) {
            data.table::setDT(CMtable)
          }

          CMtable_cols <- c("icdcm_code", "icdcm", "superlevel", "broad", "midlevel", "detailed", "icdcm_version")
          if (!identical(sort(CMtable_cols), sort(names(CMtable)))){
            stop(paste0("\n\U0001f47f CMtable must have the following columns as specified in rads::chars_icd_ccs:",
                        "\n ", paste(CMtable_cols, collapse = ', ')))
          }
        }

    # seq_no (unique identifier) ----
        if(!'seq_no' %in% names(ph.data)){
          stop("\U2620\U0001f47f\U2620\nph.data must contain the 'seq_no' column, which is the unique identifier.")}
        if('seq_no' %in% names(ph.data) & length(unique(ph.data$seq_no)) != nrow(ph.data)){
          stop("\U2620\U0001f47f\U2620\nThe 'seq_no' is a unique patient identifier and should not be repeated across rows.")}

    # icdcm + superlevel + broad + midlevel + detailed ----
        if(is.null(icdcm) & is.null(superlevel) & is.null(broad) & is.null(midlevel) & is.null(detailed)){
          stop("\n\U0001f47f `icdcm`, `superlevel`, `broad`, `midlevel`, and `detailed` are all NULL. This doesn't make sense! Specify a value for at least one of these arguments.")
        }

        if(is.null(CMtable)){
          CMtable <- chars_icd_ccs(icdcm_version = icdcm_version) # reference table of all potential search terms for this function
        }
        CMtable <- CMtable[, list(icdcm_code, icdcm_desc = icdcm, superlevel_desc = superlevel, broad_desc = broad, midlevel_desc = midlevel, detailed_desc = detailed)]

        filter.count <- sum(!is.null(icdcm), !is.null(superlevel), !is.null(broad), !is.null(midlevel), !is.null(detailed))

    # icdcm ----
        if(!is.null(icdcm)){
          if(length(icdcm) != 1){
            stop("\n\U0001f47f When specified, `icdcm` must be a character vector of length one.")
            }
          CMtable <- CMtable[grepl(icdcm, icdcm_desc, ignore.case = T) | grepl(icdcm, icdcm_code, ignore.case = T)]
          if(nrow(CMtable) < 1){
            stop(paste0("\n\U0001f47f Setting the argument <icdcm='", icdcm, "'> filtered out all possible ICD CM codes in the reference table. Please change your argument(s)."))
          }
        }

    # superlevel ----
        if(!is.null(superlevel)){
          if(length(superlevel) != 1){
            stop("\n\U0001f47f When specified, `superlevel` must be a character vector of length one.")
          }
          CMtable <- CMtable[grepl(superlevel, superlevel_desc, ignore.case = T)]
          if(nrow(CMtable) < 1){
            if(filter.count == 1){
              stop(paste0("\n\U0001f47f Setting the argument <superlevel='", superlevel, "'> filtered out all possible ICD CM codes in the reference table. Please change your argument(s)."))
            }
            if(filter.count > 1){
              stop(paste0("\n\U0001f47f Setting the argument <superlevel='", superlevel, "'>, either alone or in combinaton with other arguments, filtered out all possible ICD CM codes in the reference table. Please change your argument(s)."))
            }
          }
        }

    # broad ----
        if(!is.null(broad)){
          if(length(broad) != 1){
            stop("\n\U0001f47f When specified, `broad` must be a character vector of length one.")
          }
          CMtable <- CMtable[grepl(broad, broad_desc, ignore.case = T)]
          if(nrow(CMtable) < 1){
            if(filter.count == 1){
              stop(paste0("\n\U0001f47f Setting the argument <broad='", broad, "'> filtered out all possible ICD CM codes in the reference table. Please change your argument(s)."))
            }
            if(filter.count > 1){
              stop(paste0("\n\U0001f47f Setting the argument <broad='", broad, "'>, either alone or in combinaton with other arguments, filtered out all possible ICD CM codes in the reference table. Please change your argument(s)."))
            }
          }
        }

    # midlevel ----
        if(!is.null(midlevel)){
          if(length(midlevel) != 1){
            stop("\n\U0001f47f When specified, `midlevel` must be a character vector of length one.")
          }
          CMtable <- CMtable[grepl(midlevel, midlevel_desc, ignore.case = T)]
          if(nrow(CMtable) < 1){
            if(filter.count == 1){
              stop(paste0("\n\U0001f47f Setting the argument <midlevel='", midlevel, "'> filtered out all possible ICD CM codes in the reference table. Please change your argument(s)."))
            }
            if(filter.count > 1){
              stop(paste0("\n\U0001f47f Setting the argument <midlevel='", midlevel, "'>, either alone or in combinaton with other arguments, filtered out all possible ICD CM codes in the reference table. Please change your argument(s)."))
            }
          }
        }

    # detailed ----
        if(!is.null(detailed)){
          if(length(detailed) != 1){
            stop("\n\U0001f47f When specified, `detailed` must be a character vector of length one.")
          }
          CMtable <- CMtable[grepl(detailed, detailed_desc, ignore.case = T)]
          if(nrow(CMtable) < 1){
            if(filter.count == 1){
              stop(paste0("\n\U0001f47f Setting the argument <detailed='", detailed, "'> filtered out all possible ICD CM codes in the reference table. Please change your argument(s)."))
            }
            if(filter.count > 1){
              stop(paste0("\n\U0001f47f Setting the argument <detailed='", detailed, "'>, either alone or in combinaton with other arguments, filtered out all possible ICD CM codes in the reference table. Please change your argument(s)."))
            }
          }
        }

    # icdcol ----
        if(is.null(icdcol)){stop("\n\U0001f47f The `icdcol` argument cannot be NULL. If you are unsure of what enter, try using `icdcol = 'diag1`, which is the default")}
        if(isFALSE(icdcol %in% colnames(ph.data))){
          stop(paste0("\n\U0001f47f You specified icdcol='", icdcol, "', but '", icdcol, "' does not exist in `ph.data`."))
        }

        ph.data[, (icdcol) := toupper(get(icdcol))]

        if(length(grep("\\.|-", ph.data[[icdcol]], value = T) >0 )){
          warning(paste0("\u26A0\ufe0f There is at least one row where `icdcol` (",
          icdcol, ") contains a hyphen (-), period (.), space or some other ",
          "non alpha-numeric character. These characters will be deleted, e.g., ",
          "A85.2 will become A852. This is necessary because causeids in ",
          "rads::chars_icd_ccs contains no hyphens or periods."
          ))
          ph.data[, paste0(icdcol) := gsub("[[:space:].]+", "", gsub("([^A-Za-z0-9 ])+", "", x = get(icdcol)))]
        }

        if(icdcm_version == 10){
          if(nrow(ph.data[is.na(get(icdcol)) | !grepl("^[A-Z][0-9]", get(icdcol))]) > 0){
            problem.icds <- ph.data[is.na(get(icdcol)) | !grepl("^[A-Z][0-9]", get(icdcol)), ][[icdcol]]
            warning(paste0("\u26A0\ufe0f There is/are ", length(problem.icds), " row(s) where `icdcol` (",
            icdcol, ") does not follow the proper ICD-10-CM pattern. All ICD-10-CMs that do not begin with a ",
            "single capital letter followed by a number have been replaced with NA."))
            ph.data[!grepl("^[A-Z][0-9]", get(icdcol)) , paste0(icdcol) := NA]
          }
        }

    # group_by ----
        if(!is.null(group_by)){
          if(!inherits(group_by, 'character')){
            stop("\n\U0001f47f `group_by` must either be NULL or specify a character vector of column names.")}
          if(length(setdiff(group_by, names(ph.data))) > 0){
            stop(paste0("\n\U0001f47f `group_by` contains the following column names which do not exist in ph.data: ", paste(setdiff(group_by, names(ph.data)), collapse = ', ') ))}
          # convert group_by values to character b/c factors cause an error
          for(gb_var in group_by){
            ph.data[, paste0(gb_var) := as.character(get(gb_var))]
          }
        }

    # kingco ----
        if (isTRUE(kingco) & (!"chi_geo_kc" %in% names(ph.data))){
          stop("\U0001f47f You specified kingco=TRUE, but `ph.data` does not have the column `chi_geo_kc` that identifies King County")
        }
        if (isTRUE(kingco)){ph.data <- ph.data[chi_geo_kc == 'King County']}

    # mykey ----
        if(length(mykey) != 1 | !is.character(mykey)){
          stop("\n \U0001f47f the `mykey` argument must be a string of length == 1, \nwhich is the name of your keyring:: service providing the \npassword for connecting to HHSAW, it is typically 'hhsaw'.")
        }
        if(!mykey %in% keyring::key_list()[]$service){
          stop("\n \U0001f47f the `mykey` value passed to this function ('", mykey, "') is not in your `keyring::key_list`.\nPlease create it using `keyring::key_set` and try again.")
        }

  # Drop unnecessary columns from reference table (CMtable) ----
    KeepMe <- c("icdcm_code")
    for(grr in c('icdcm', 'superlevel', 'broad', 'midlevel', 'detailed')){
     if(!is.null(get(grr))){
       KeepMe <- c(KeepMe, paste0(grr, "_desc"))
     }
    }

    CMtable <- CMtable[, KeepMe, with = FALSE]

  # Get counts of hospitalizations for each group of search terms in CMtable (search term reference table) ----
    # flatten CMtable for regex search ----
      CMtable = CMtable[, list(icdcm_code = list(icdcm_code)), by = setdiff(names(CMtable), c("icdcm_code"))]

    # Create a 'query.group' for each combination of levels/icdcm ----
      CMtable[, query.group := .GRP, by = setdiff(names(CMtable), "icdcm_code")]

    # generate counts for each query.group ----
      HospCounts <- rbindlist(lapply(unique(CMtable$query.group), function(QG) {
        tempHospCounts <- if (is.null(group_by)) {
          ph.data[diag1 %in% unlist(CMtable[query.group == QG]$icdcm_code), list(hospitalizations = .N)]
        } else {
          ph.data[diag1 %in% unlist(CMtable[query.group == QG]$icdcm_code), list(hospitalizations = .N), by = group_by]
        }
        tempHospCounts[, query.group := QG]
        # tempHospCounts <- CMtable[tempHospCounts, on = "query.group"] # native data.table merge syntax
        tempHospCounts <- merge(CMtable, tempHospCounts, by = 'query.group', all = FALSE)
        tempHospCounts[, icdcm_code := NULL]
        return(tempHospCounts)
      }), fill = TRUE)

  # Expand reference table for each combination of group_by variables ----
      CMtable[, icdcm_code := NULL]
      if (is.null(group_by)) {
        CMtable.expanded <- CMtable
      } else {
        # Create a list of unique values for each group_by variable
        unique_vals_list <- lapply(group_by, function(col) unique(ph.data[[col]]))
        names(unique_vals_list) <- group_by

        # Create the Cartesian product of unique values using CJ
        template.xyz <- do.call(CJ, unique_vals_list)

        # Expand CMtable for each combination of group_by variables
        CMtable.expanded <- merge(CMtable[, dummy := 1],
                                  template.xyz[, dummy := 1],
                                  by = 'dummy',
                                  allow.cartesian = TRUE)[, dummy := NULL]
      }

  # Merge counts onto the expanded table to get table with all possible combination, even when counts == 0 ----
    HospCounts <- merge(CMtable.expanded, HospCounts,
                        by = intersect(names(HospCounts), names(CMtable.expanded)),
                        all = TRUE)
    HospCounts[is.na(hospitalizations), hospitalizations := 0]
    setorderv(HospCounts, c('query.group', group_by))
    HospCounts[, c("query.group") := NULL]

  # Return data ----
  return(HospCounts)
}


# The end ----
