# chars_injury_matrix() ----
#' View available combinations of CHARS injury mechanisms and intents
#'
#' @description
#' Function to view all combinations of the Comprehensive
#' Hospital Abstract Reporting System (CHARS) Injury Matrix mechanisms and
#' intents available in rads.
#'
#' Generates a table with two columns, \code{mechanism} & \code{intent}.
#' Use it to identify the combinations of \code{mechanism} & \code{intent} that
#' you want to use in \code{chars_injury_matrix_count}.
#'
#' @details
#' This function provides the terms used by the hospitalization function
#' \code{chars_injury_matrix_count} and may not be the same as those used with
#' \code{death_injury_matrix_count}.
#'
#' @note
#' This function does not take any arguments.
#'
#' This function will return all available categories, some of which are specific
#' to ICD9-cm (2012-2015) and some of which are specific to ICD10-cm (2016+).
#'
#' @source
#' Derived from columns beginning with `mechanism_` and `intent_` in
#' Azure Server 16 (chars.final_analytic) that were created during the CHARS
#' ETL process.
#'
#' @return
#' A data.table with two columns: \code{mechanism} & \code{intent}. The number
#' of rows are determined dynamically by scanning the data available in SQL.
#'
#' @references
#' WA DOH CHAT: \url{https://secureaccess.wa.gov/doh/chat/Content/FilesForDownload/CodeSetDefinitions/Hospitalization%20Injury%20Matrix%20ICD10CM.xlsx}
#'
#' @export
#'
#' @name chars_injury_matrix
#'
#' @examples
#' # Save and view table as a data.table named 'blah'
#' blah <- chars_injury_matrix()
#' print(blah)
#'
#' @import data.table rads.data
#'
chars_injury_matrix<- function(){
  # Global variables used by data.table declared as NULL here to play nice with devtools::check() ----
  chars_injury_matrix_list <- mechanism <- intent <-  NULL

  # get column names from SQL table
  con <- validate_hhsaw_key('hhsaw')
  chars.names <- names(DBI::dbGetQuery(con, "SELECT TOP (0) * FROM [chars].[final_analytic]"))

  # create a matrix of every possible mechanism and intent
  chars_injury_matrix_list = unique(data.table::setDT(expand.grid(
    mechanism = sort(c("motor_vehicle_traffic", setdiff(gsub("^mechanism_", "", grep("^mech", chars.names, value = T)), ""))), # motor vehicle is not in data, but will combine mvt variables
    intent = setdiff(gsub("^intent_", "", grep("^intent", chars.names, value = T)), '') #
  )))

  return(chars_injury_matrix_list)
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
#' In other words, this ph.data should come from use of \code{get_data_chars()}.
#'
#' The default is \code{ph.data = NULL}
#'
#' @param intent a character vector. It specifies the
#' intent of the injury related hospitalization that you want returned (e.g.,
#' "assault", "intentional", "unintentional", etc.). "none"
#' will ignore the intent and only return the mechanism of the injury leading
#' to hospitalization.
#'
#' **NOTE**
#' You do not have to type the entire keyword for the intent, a
#' partial string match is sufficient and is case insensitive. E.g.,
#' \code{intent = c("intent")} would return both "intentional" and "unintentional" and
#' \code{intent = c("un")} would return both "undetermined" and "unintentional".
#'
#' The default is \code{intent = '*'}, which selects all possible intents
#'
#' @param mechanism a character vector. It specifies the
#' mechanism of the injury related hospitalization that you want returned
#' (e.g., "cut_pierce", "drowning", "fall", "firearm", etc.). "none" will ignore
#' the mechanism and only return the intent of the injury leading to hospitalization.
#'
#' To see the complete list of mechanisms, type
#' \code{unique(chars_injury_matrix()$mechanism)} in your
#' R console.
#'
#' **NOTE**
#' You do not have to type the entire keyword for the mechanism, a
#' partial string match is sufficient and is case insensitive. E.g.,
#' \code{mechanism = c("fire")} would return both "firearm" and
#' "fire_burn".
#'
#' The default is \code{mechanism = '*'}, which selects all possible mechanisms
#'
#' @param group_by a character vector of indeterminate length. This is used to
#' specify all the variables by which you want to group (a.k.a. stratify) the
#' results. For example, if you specified \code{group_by = c('chi_sex',
#' 'chi_race_6')}, the results would be stratified by each combination of sex
#' and race.
#'
#' The default is \code{group_by = NULL}
#'
#' @param def a character vector of length one, limited to 'narrow' or 'broad'.
#' It specifies whether you want to use the CDC's recommended 'narrow' approach,
#' which requires that the \strong{principal diagnosis} of an injury
#' hospitalization be a nature-of-injury ICD10-CM code. Or, alternatively, the
#' 'broad' definition that searches all available diagnosis fields on the
#' hospital discharge record (there can be a maximum of 54 diagnosis fields in
#' CHARS data).
#'
#' **NOTE**
#' ph.data must contain the columns named \code{injury_nature_narrow} &
#' \code{injury_nature_broad}.
#'
#' The default is \code{def = 'narrow'}
#'
#' @param primary_ecode a logical vector of length one. It specifies whether you
#' want to limit the analysis to using just the primary ecode (i.e., the
#' \code{injury_ecode} variable), rather than all available ecodes.
#'
#' As of RADS 1.1.7.7, the only valid argument is TRUE (T). Those wanting to perform
#' an analysis with other ecodes would need to perform a custom analysis in SQL using
#' the `chars.stage_diag` & `chars.stage_ecode` tables.
#'
#' The default is \code{primary_ecode = TRUE}
#'
#' @param kingco a logical vector of length one. It specifies whether you want to
#' limit the analysis to King County. Note that this only works when you have
#' the column `chi_geo_kc` imported from the \code{get_data_chars()} function.
#'
#' The default is \code{kingco = TRUE}
#'
#' @details
#' Since the injury analysis uses many columns, we suggest that you obtain
#' ph.data with get_data_chars(cols = NA), rather than trying to specify the
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
#' \code{mechanism}, \code{intent}, & \code{hospitalizations}. Any
#' \code{group_by} variables would also have their own columns.
#'
#' The function default is to return the matrix of all intents and mechanisms
#' of injury related hospitalizations. You can choose to only return the intent
#' or only return the mechanism. If you set both to "none", you will receive a
#' summary of all injury hospitalizations without regard to the intent or mechanism.
#'
#' @references
#' WA DOH CHAT: \url{https://secureaccess.wa.gov/doh/chat/Content/FilesForDownload/CodeSetDefinitions/Hospitalization%20Injury%20Matrix%20ICD10CM.xlsx}
#'
#' @export
#'
#' @name chars_injury_matrix_count
#'
#' @examples
#' # example: 2019 King County hospitalizations due to intentional injury, by sex
#' \dontrun{
#' blah = get_data_chars(year = 2019, kingco = TRUE)
#' myresult <- chars_injury_matrix_count(ph.data = blah,
#'                                       intent = '^intentional',
#'                                       mechanism = 'none',
#'                                       group_by = c('chi_sex'))
#' print(myresult)
#' }
#' @import data.table rads.data
#'
chars_injury_matrix_count<- function(ph.data = NULL,
                                intent = "*",
                                mechanism = "*",
                                group_by = NULL,
                                def = 'narrow',
                                primary_ecode = T,
                                kingco = T){
  # Global variables used by data.table declared as NULL here to play nice with devtools::check() ----
    chi_geo_kc <- icd10 <- bingo <- hospitalizations <- icd10cm <- icd10cm_desc <- NULL
    var.names <- injury_nature_narrow <- injury_ecode <- injury_nature_broad <- NULL
    injury_intent <- injury_mechanism <- NULL
    chi_geo_kc <- NULL
    yage4 <- age <- age6 <- geo_type <- geo_id <- pov200grp <- race4 <- race3 <- NULL
    chi_race_aic_hisp <- race3_hispanic <- NULL
    mechanism_motor_vehicle_traffic <- NULL

  # Check arguments ----
    # ph.data ----
        ph.data.name <- deparse(substitute(ph.data))
        if(!is.null(ph.data)){
          if(!is.data.frame(ph.data)){
            stop("\n\U0001f47f 'ph.data' must be the unquoted name of a data.frame or data.table")
          }
          if(is.data.frame(ph.data) && !data.table::is.data.table(ph.data)){
            data.table::setDT(ph.data)
          }
        } else {stop("\n\U0001f47f 'ph.data', the name of a data.frame or data.table with line level CHARS data, must be specified")}

        ph.data <- data.table::setDT(data.table::copy(ph.data)) # to prevent changing of original by reference

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

        if(nrow(setDT(list_dataset_columns('chars'))[grepl('^intent_', var.names)]) >
           length(grep('^intent_', names(ph.data), value = T))){
          mi_col_intent <- setdiff(setDT(list_dataset_columns('chars'))[grepl('^intent_', var.names)]$var.names, grep('^intent_', names(ph.data), value = T))
          warning(paste0("\n\U00026A0 ph.data is missing the following `intent_**` columns: ", paste0(mi_col_intent, collapse = ', '), ". This may impact the completeness of your results."))
        }

    # mechanism ----
        if("none" %in% mechanism & length(mechanism) != 1){
          stop("\n\U0001f47f The mechanism 'none' cannot be specified with any other mechanisms.")
        }
        if("*" %in% mechanism & length(mechanism) != 1){
          stop("\n\U0001f47f The mechanism '*' cannot be specified with any other mechanisms.")
        }

        if(nrow(setDT(list_dataset_columns('chars'))[grepl('^mechanism_', var.names)]) >
           length(grep('^mechanism_', names(ph.data), value = T))){
          mi_col_mechanism <- setdiff(setDT(list_dataset_columns('chars'))[grepl('^mechanism_', var.names)]$var.names, grep('^mechanism_', names(ph.data), value = T))
          warning(paste0("\n\U00026A0 ph.data is missing the following `mechanism_**` columns: ", paste0(mi_col_mechanism, collapse = ', '), ". This may impact the completeness of your results."))
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
        if(!is.logical(primary_ecode)){stop("\n\U0001f47f `primary_ecode` must be a logical vector of length 1, i.e,. TRUE or FALSE.")}
        if(isFALSE(primary_ecode)){stop(paste0("\n\U1F6D1 \U2620 \U0001f47f\n",
                                        " You set 'primary_ecode = F'. This is no longer a valid option. If you want to use other ecodes\n",
                                        " you will have to perform a custom analysis using [chars].[stage_diag] & [chars].[stage_ecode]."))}

    # kingco ----
        if(!is.logical(kingco)){stop("\n\U0001f47f `kingco` must be a logical vector of length 1, i.e,. TRUE or FALSE.")}
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
      if(length(x_mechanism) == 0){stop(paste0(
        "\n\U0001f47f \nYour `mechanism` value (", mechanism, ") has filtered out all of the hospitalization injury mechanisms.\nPlease enter 'none', '*', or a new partial keyword term and try again.\n",
        "Entering `rads::chars_injury_matrix()` into the console will provide you with a table of valid options."))}

  # Create motor_vehicle_traffic column when needed ----
      if('motor_vehicle_traffic' %in% x_mechanism){
        # Matt Dowle's suggestion ... https://stackoverflow.com/questions/7885147/efficient-row-wise-operations-on-a-data-table
        ph.data[, mechanism_motor_vehicle_traffic := do.call(pmax, c(.SD, na.rm = T)),
                .SDcols = paste0('mechanism_', grep('mvt_', possible.mechanisms, value = T))]
      }

  # Count hospitalizations for each intent_x_mechanism of interest ----
      x_combo <- data.table() # table to hold results of all combinations of mech & intent specified by the arguments
        # create matrix of all mechanisms and intents of interest ----
        x_grid <- data.table::setDT(expand.grid(mechanism = x_mechanism, intent = x_intent))

        # count number of hospitalizations (i.e., rows) when def == 'narrow' ----
        for(ii in 1:nrow(x_grid)){
          temp.ph.data <- copy(ph.data)

        # Identify whether the combination of mech & intent in x_grid has any hospitalizations in person level data ----
            # could theoretically use injury_mechanism & injury_intent, but would need extra coding to address when either has value 'any'
            temp.ph.data[, bingo := 0] # By default, assume the row does not have the given combination of mech and intent
            temp.ph.data[get(paste0("mechanism_", as.character(x_grid[ii]$mechanism))) >= 1 &
                           get(paste0("intent_", as.character(x_grid[ii]$intent))) >= 1, bingo := 1]

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
            for(mygridvar in gridvars){
              assign(paste0("xtemp_", mygridvar), unique(temp.ph.data[[mygridvar]]))
            }
            complete.grid <- data.table(setDT(expand.grid(mget(paste0("xtemp_", gridvars)))))
            setnames(complete.grid, gsub("^xtemp_", "", names(complete.grid)))

          # merge temp.ph.data onto complete.grid ----
            temp.ph.data <- merge(complete.grid, temp.ph.data, all = T)
            temp.ph.data[is.na(hospitalizations), hospitalizations := 0]

          # append onto x_combo ----
            x_combo <- rbind(x_combo, temp.ph.data)
        }

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
#'  and summary 'broad' and 'detailed' classifications that can be used with
#' \code{chars_icd_ccs_count}
#'
#' @description
#' A function to view the complete list of ICD-9-CM OR ICD-10-CM codes and
#' descriptions as well as corresponding 'broad' and 'detailed' classifications.
#' The 'broad' and 'detailed' classifications broadly follow AHRQ's HCUP Clinical
#' Classifications Software Refined (CCSR) standards for ICD-10-CM. ICD-9-CM
#' codes were then mapped to the same 'broad' and 'detailed' categories to maximize
#' comparability across time.
#'
#' Output is provided in the form of a table. Use this table to inform your
#' arguments in the  \code{chars_icd_ccs_count} function.
#'
#' @note
#' If you do not specify any arguments, the function will return a table with
#' all ICD-10-CM codes as well ICD-10-CM, broad, and detailed descriptions.
#'
#' @source
#' \code{kcitazrhpasqlprp16.azds.kingcounty.gov >> hhs_analytics_workspace >>
#' ref.icdcm_codes}
#'
#' @references
#' \url{https://hcup-us.ahrq.gov/toolssoftware/ccsr/ccs_refined.jsp}
#'
#' @param ref_type a character vector of length one specifying the hospital diagnosis
#' descriptions that are of interest to you. Acceptable options include: \code{'all'},
#' \code{'icdcm'}, \code{'broad'}, & \code{'detailed'}.
#'
#' The default is \code{ref_type = 'all'}.
#'
#' @param mykey Character vector of length 1. Identifies
#' the keyring:: service that can be used to access the Health & Human Services
#' Analytic Workspace (HHSAW).
#'
#' The default is \code{mykey = 'hhsaw'}
#'
#' @param icdcm_version an integer vector of length one specifying the ICD CM
#' version that you want to reference. Acceptable options include: \code{9}
#' & \code{10}.
#'
#' The default is \code{icdcm_version = 10}.
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
#' # Save and view table as a data.table named 'blah'
#' blah <- chars_icd_ccs(ref_type = 'all')
#' head(blah)
#'
#' @import data.table rads.data
#'
chars_icd_ccs <- function(ref_type = 'all',
                          mykey = "hhsaw",
                          icdcm_version = 10){

  # Global variables used by data.table declared as NULL here to play nice with devtools::check() ----
  chars_list <- icdcm <- icdcm_code <- broad <- detailed  <- NULL

  # check arguments ----
  if(length(ref_type) != 1){
    stop("\n \U0001f47f the `ref_type` must have a single value. Valid options are 'all', 'icdcm', 'broad', & 'detailed'")}
  if(!ref_type %in% c('all', 'icdcm', 'broad', 'detailed')){
    stop(paste0("\n \U0001f47f'", ref_type, "' is not a valid option for the `ref_type` argument. \nValid options are 'all', 'icdcm', 'broad', & 'detailed'."))}

  if(length(mykey) != 1 | !is.character(mykey)){
    stop("\n \U0001f47f the `mykey` argument must be a string of length == 1, \nwhich is the name of your keyring:: service providing the \npassword for connecting to HHSAW, it is typically 'hhsaw'.")}
  if(!mykey %in% keyring::key_list()[]$service){
    stop("\n \U0001f47f the `mykey` value passed to this function ('", mykey, "') is not in your `keyring::key_list`.\nPlease create it using `keyring::key_set` and try again.")
  }
  if(!icdcm_version %in% c(9, 10) | length(icdcm_version) != 1){stop("\n \U0001f47f the `icdcm_version` argument is limited to the integers '9' OR '10'")}

  # get data ----
  con <- validate_hhsaw_key(hhsaw_key = mykey)
  chars_list <- setDT(DBI::dbGetQuery(conn = con,
                                   paste0("SELECT icdcm_version,
                                   icdcm_code = icdcm,
                                   icdcm = icdcm_description,
                                   broad = ccs_broad_desc,
                                   detailed = ccs_detail_desc
                                   FROM [ref].[icdcm_codes] WHERE icdcm_version = ", icdcm_version )))

  if(ref_type == 'all'){chars_list <- chars_list[, list(icdcm_code, icdcm, broad, detailed, icdcm_version)]}
  if(ref_type == 'icdcm'){chars_list <- chars_list[, list(icdcm_code, icdcm, icdcm_version)]}
  if(ref_type == 'broad'){chars_list <- chars_list[, list(broad, icdcm_version)]}
  if(ref_type == 'detailed'){chars_list <- chars_list[, list(detailed, icdcm_version)]}

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
#' ICD-9-CM, and broad and narrow classifications.
#'
#' \strong{¡¡¡REMEMBER!!!} ICD-10-CM started in 2016! Be sure to use the correct
#' **\code{icdcm_version}**.
#'
#'
#' @details
#' This function needs the user to enter a search string in one or more of the
#' following arguments in order to search the CHARS data for the corresponding
#' ICD CM codes: \code{icdcm}, \code{broad}, or \code{detailed}.
#' Partial search terms are acceptable and they are case-insensitive. For
#' example, if you set \code{broad = 'ex'} with \code{icdcm_version = 10}, the
#' function would return counts for "Diseases of the eye and adnEXa" as well as
#' "EXternal causes of morbidity". It also understands simple regex syntax,
#' including **\code{^}**, **\code{$}**, and **\code{|}**.
#'
#' **Note:** If you submit values for more than one of \code{icdcm},
#' \code{broad}, or \code{detailed} they must be nested. For example,
#' \code{broad = 'neoplasms', detailed = 'sarcoma'} will give results because
#' sarcomas are type of cancers. However, \code{broad = 'neoplasms',
#' detailed = 'intestinal infection'} will return an error because your resulting
#' table will have zero rows.
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
#' @param broad a character vector of length 1. View available options with
#' \code{chars_icd_ccs(ref_type = 'broad', icdcm_version = 10)}.
#'
#' The default is \code{broad = NULL}
#'
#' @param detailed a character vector of length 1. View available options with
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
#' # example: 2019 King County hospitalizations for chemotherapy, by sex
#' \dontrun{
#' blah = get_data_chars(year = 2019, kingco = TRUE)
#' myresult <- chars_icd_ccs_count(ph.data = blah,
#'                                 detailed = 'chemotherapy',
#'                                 group_by = c('chi_sex'))
#' print(myresult)
#' }
#'
#' @import data.table rads.data
#'
chars_icd_ccs_count <- function(ph.data = NULL,
                                icdcm_version = 10,
                                icdcm = NULL,
                                broad = NULL,
                                detailed = NULL,
                                icdcol = 'diag1',
                                group_by = NULL,
                                kingco = T,
                                mykey = 'hhsaw'){

  # Global variables used by data.table declared as NULL here to play nice with devtools::check() ----
    CMtable <- CMtable.expanded <- filter.count <- problem.icds <- broad_desc <-
      detailed_desc <- chi_geo_kc <- hospitalizations <- icdcm_code <- KeepMe <-
      icdcm_desc <- icdcm_code <- query.group <- diag1 <- intent_ignore <-
      chars_injury_matrix_count <- mechanism_ignore <- NULL

  # Check arguments & filter reference table of all ICD CM (CMtable) ----
    # ph.data ----
        ph.data.name <- deparse(substitute(ph.data))
        if(!is.null(ph.data)){
          if(!is.data.frame(ph.data)){
            stop("\n\U0001f47f 'ph.data' must be the unquoted name of a data.frame or data.table")
          }
          if(is.data.frame(ph.data) && !data.table::is.data.table(ph.data)){
            data.table::setDT(ph.data)
          }
        } else {stop("\n\U0001f47f 'ph.data', the name of a data.frame or data.table with line level CHARS data, must be specified")}

        ph.data <- data.table::setDT(data.table::copy(ph.data)) # to prevent changing of original by reference

    # icdcm_version ----
        if(!icdcm_version %in% c(9, 10) | length(icdcm_version) != 1){stop("\n \U0001f47f the `icdcm_version` argument is limited to the integers '9' OR '10'")}

    # seq_no (unique identifier) ----
        if(!'seq_no' %in% names(ph.data)){
          stop("\U2620\U0001f47f\U2620\nph.data must contain the 'seq_no' column, which is the unique identifier.")}
        if('seq_no' %in% names(ph.data) & length(unique(ph.data$seq_no)) != nrow(ph.data)){
          stop("\U2620\U0001f47f\U2620\nThe 'seq_no' is a unique patient identifier and should not be repeated across rows.")}

    # icdcm + broad + detailed ----
        if(is.null(icdcm) & is.null(broad) & is.null(detailed)){
          stop("\n\U0001f47f `icdcm`, `broad`, and `detailed` are all NULL. This doesn't make sense! Specify a value for at least one of these arguments.")
        }

        CMtable <- chars_icd_ccs(icdcm_version = icdcm_version) # reference table of all potential search terms for this function
        CMtable <- CMtable[, list(icdcm_code, icdcm_desc = icdcm, broad_desc = broad, detailed_desc = detailed)]


        filter.count <- sum(!is.null(icdcm), !is.null(broad), !is.null(detailed))

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
              stop(paste0("\n\U0001f47f Setting the argument <broad='", broad, "'>, either alone or in combinaton with the values of icdcm and detailed, filtered out all possible ICD CM codes in the reference table. Please change your argument(s)."))
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
              stop(paste0("\n\U0001f47f Setting the argument <detailed='", detailed, "'>, either alone or in combinaton with the values of icdcm and broad, filtered out all possible ICD CM codes in the reference table. Please change your argument(s)."))
            }
          }
        }

    # icdcol ----
        if(is.null(icdcol)){stop("\n\U0001f47f The `icdcol` argument cannot be NULL. If you are unsure of what enter, try using `icdcol = 'diag1`, which is the default")}
        if(isFALSE(icdcol %in% colnames(ph.data))){
          stop(paste0("\n\U0001f47f You specified icdcol='", icdcol, "', but '", icdcol, "' does not exist in `ph.data`."))
        }

        ph.data[, paste(icdcol) := toupper(get(icdcol))]

        if(length(grep("\\.|-", ph.data[[icdcol]], value = T) >0 )){
          warning(paste0("\U00026A0 There is at least one row where `icdcol` (",
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
            warning(paste0("\U00026A0 There is/are ", length(problem.icds), " row(s) where `icdcol` (",
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
    for(grr in c('icdcm', 'broad', 'detailed')){
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
      HospCounts <- data.table()
      for(QG in unique(CMtable$query.group)){
        if(is.null(group_by)){
          tempHospCounts <- ph.data[diag1 %in% unlist(CMtable[query.group == QG]$icdcm_code), list(hospitalizations = .N)]
        }
        if(!is.null(group_by)){
          tempHospCounts <- ph.data[diag1 %in% unlist(CMtable[query.group == QG]$icdcm_code), list(hospitalizations = .N), by = group_by]
        }
        tempHospCounts[, query.group := QG]
        tempHospCounts <- merge(tempHospCounts, CMtable, by = "query.group")
        tempHospCounts[, c('icdcm_code') := NULL]
        HospCounts <- rbind(HospCounts, tempHospCounts, fill = T)
      }

  # Expand reference table for each combination of group_by variables ----
      CMtable[, icdcm_code := NULL]
      if(is.null(group_by)){CMtable.expanded <- CMtable}
      if(!is.null(group_by)){
          for(nombre in group_by){
            assign(paste0('xyz_', nombre), unique(ph.data[, get(nombre)]))
          }

          template.xyz <- setDT(expand.grid(mget(ls(pattern = 'xyz_'))))
          setnames(template.xyz, gsub('^xyz_', '', names(template.xyz)))

          CMtable.expanded <- data.table()
          for(xyz.count in 1:nrow(template.xyz)){
            CMtable.expanded <- rbind(CMtable.expanded, cbind(CMtable, template.xyz[xyz.count]))
          }
      }

  # Merge counts onto the expanded table to get table with all possible combination, even when counts == 0 ----
    HospCounts <- merge(CMtable.expanded, HospCounts, by = intersect(names(HospCounts), names(CMtable.expanded)), all.x = T, all.y = T)
    HospCounts[is.na(hospitalizations), hospitalizations := 0]
    setorderv(HospCounts, c('query.group', group_by))
    HospCounts[, c("query.group") := NULL]

  # Tidy ----

  # Return data ----
  return(HospCounts)
}


# The end ----
