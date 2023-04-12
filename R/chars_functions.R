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
#' KCITSQLUTPDBH51.PH_APDEStore.chars.final_analytic that were
#' created during the CHARS ETL process.
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
  con <- odbc::dbConnect(odbc::odbc(),
                         Driver = "SQL Server",
                         Server = "KCITSQLUTPDBH51",
                         Database = "PH_APDEStore")
  chars.names <- names(DBI::dbGetQuery(con, "SELECT TOP (0) * FROM [PH_APDEStore].[chars].[final_analytic]"))

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
#' Unless you are performing a specialized analysis, please keep the default
#' setting!
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
    injury_intent <- rads_intent_ignore <- injury_mechanism <- NULL
    rads_mechanism_ignore <- rads_mechanism_motor_vehicle_traffic <- chi_geo_kc <- NULL
    yage4 <- age <- age6 <- geo_type <- geo_id <- pov200grp <- race4 <- race3 <- NULL
    chi_race_aic_hisp <- race3_hispanic <- intent_ignore <- mechanism_ignore <- NULL
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

    # kingco ----
        if(!is.logical(kingco)){stop("\n\U0001f47f `kingco` must be a logical vector of length 1, i.e,. TRUE or FALSE.")}
        if (isTRUE(kingco) & (!"chi_geo_kc" %in% names(ph.data))){
          stop("\n\U0001f47f You specified kingco=TRUE, but `ph.data` does not have the following columns that identify King County data:
                   chi_geo_kc")
        }
        if (isTRUE(kingco)){ph.data <- ph.data[chi_geo_kc == "King County"]}

  # Apply narrow or broad definition ----
      if(def == 'narrow'){ph.data <- ph.data[injury_nature_narrow == T & !is.na(injury_ecode)]}
      if(def == 'broad'){ph.data <- ph.data[injury_nature_broad == T & !is.na(injury_ecode)]}

  # Get complete list of all possible mechanisms and intents ----
      possible.intents <- c(as.character(unique(chars_injury_matrix()$intent)), 'ignore')
      possible.mechanisms <- c(as.character(unique(chars_injury_matrix()$mechanism)), 'ignore')

  # Create binary columns with individual primary intent / mechanism ----
      for(rads.intent in possible.intents){
        ph.data[, paste0("rads_intent_", rads.intent) := 0]
        ph.data[injury_intent == rads.intent, paste0("rads_intent_", rads.intent) := 1]
      }
      ph.data[, rads_intent_ignore := 1] # so code will be able to truly ignore (i.e., allow for 'any') intent when need be
      ph.data[, intent_ignore := 1] # so code will be able to truly ignore (i.e., allow for 'any') intent when need be

      for(rads.mechanism in possible.mechanisms){
        ph.data[, paste0("rads_mechanism_", rads.mechanism) := 0]
        ph.data[injury_mechanism == rads.mechanism, paste0("rads_mechanism_", rads.mechanism) := 1]
      }
      ph.data[, rads_mechanism_ignore := 1] # so code will be able to truly ignore (i.e., allow for 'any') mechanism when need be
      ph.data[, mechanism_ignore := 1] # so code will be able to truly ignore (i.e., allow for 'any') mechanism when need be

  # Identify intent of interest ----
      intent = tolower(intent)

      if("none" %in% intent){ # none means 'any intent', i.e., 'ignore' the intent
        x_intent = "ignore"
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
        x_mechanism = "ignore"
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
          # for injury_mechanism
            ph.data[, rads_mechanism_motor_vehicle_traffic := do.call(pmax, c(.SD, na.rm = T)),
                    .SDcols = paste0('rads_mechanism_', grep('mvt_', possible.mechanisms, value = T))]
          # for any injury
            ph.data[, mechanism_motor_vehicle_traffic := do.call(pmax, c(.SD, na.rm = T)),
                    .SDcols = paste0('mechanism_', grep('mvt_', possible.mechanisms, value = T))]
      }

  # Count hospitalizations for each intent_x_mechanism of interest ----
      x_combo <- data.table()
        # create matrix of all mechanisms and intents of interest
        x_grid <- data.table::setDT(expand.grid(mechanism = x_mechanism, intent = x_intent))

        # count number of hospitalizations (i.e., rows) when def == 'narrow'
        for(ii in 1:nrow(x_grid)){
          temp.ph.data <- copy(ph.data)

        # Identify whether the combination of mech & intent in x_grid has any hospitalizations in person level data
          if(isTRUE(primary_ecode)){
            temp.ph.data[, bingo := 0] # By default, assume the row does not have the given combination of mech and intent
            temp.ph.data[get(paste0("rads_mechanism_", as.character(x_grid[ii]$mechanism))) >= 1 &
                           get(paste0("rads_intent_", as.character(x_grid[ii]$intent))) >= 1, bingo := 1]
          } else {
            temp.ph.data[, bingo := 0] # By default, assume the row does not have the given combination of mech and intent
            temp.ph.data[get(paste0("mechanism_", as.character(x_grid[ii]$mechanism))) >= 1 &
                           get(paste0("intent_", as.character(x_grid[ii]$intent))) >= 1, bingo := 1]
          }

        # Aggregate (sum) the number of hospitalizations for the mech / intent combination from x_grid
            if(!is.null(group_by)){
              temp.ph.data <- temp.ph.data[, list(mechanism = as.character(x_grid[ii]$mechanism),
                                                  intent = as.character(x_grid[ii]$intent),
                                                  hospitalizations = sum(bingo)),
                                           by = group_by]}
            if(is.null(group_by)){
              temp.ph.data <- temp.ph.data[, list(mechanism = as.character(x_grid[ii]$mechanism),
                                                  intent = as.character(x_grid[ii]$intent),
                                                  hospitalizations = sum(bingo))]}

          # create grid of all possible combinations of group_by vars
            gridvars <- setdiff(names(temp.ph.data), 'hospitalizations')
            for(mygridvar in gridvars){
              assign(paste0("xtemp_", mygridvar), unique(temp.ph.data[[mygridvar]]))
            }
            complete.grid <- data.table(setDT(expand.grid(mget(paste0("xtemp_", gridvars)))))
            setnames(complete.grid, gsub("^xtemp_", "", names(complete.grid)))

          # merge temp.ph.data onto complete.grid
            temp.ph.data <- merge(complete.grid, temp.ph.data, all = T)
            temp.ph.data[is.na(hospitalizations), hospitalizations := 0]

          # append onto x_combo
            x_combo <- rbind(x_combo, temp.ph.data)
        }

  # Tidy ----
    # Additional collapse/aggregate if mechanism == 'none' ----
      if("none" %in% mechanism){
        x_combo[, mechanism := "Any mechanism"]
        x_combo <- x_combo[, list(hospitalizations = sum(hospitalizations)), by = setdiff(names(x_combo), "hospitalizations")]
      }

      x_combo[mechanism == 'ignore', mechanism := "Any mechanism"]

    # Additional collapse/aggregate if intent == 'none' ----
      if("none" %in% intent){
        x_combo[, intent := "Any intent"]
        x_combo <- x_combo[, list(hospitalizations = sum(hospitalizations)), by = setdiff(names(x_combo), "hospitalizations")]
      }

      x_combo[intent == 'ignore', intent := "Any intent"]

    # Sort columns and rows ----
        setcolorder(x_combo, c("mechanism", "intent", "hospitalizations"))
        setorderv(x_combo, c("mechanism", "intent", setdiff(names(x_combo), c("hospitalizations", "mechanism", "intent")) ))

  # Return data ----
  return(x_combo)
}

# chars_icd_ccs() ----
#' View available CHARS ICD10-cm (diagnosis) and CCS (HCUP Clinical Classification
#' Software) descriptions that can be used with \code{chars_icd_ccs_count}
#'
#' @description
#' Function to view the complete list of ICD10-cm descriptions (n > 71K!)
#' as well as level 1 (n=18), level 2 (n=136), and level 3 (n=283) CCS code descriptions.
#'
#' Output is provided in the form of a table. Use this table to inform your
#' arguments in the  \code{chars_icd_ccs_count} function.
#'
#' @note
#' If you do not specify any arguments, the function will return a table with
#' all ICD10-cm codes as well ICD10 and CCS descriptions.
#'
#' @source
#' \code{rads.data::icd_icd10cm_CHAT_2023}
#'
#' @references
#' \url{https://icd10cmtool.cdc.gov/}
#' \url{https://www.cdc.gov/nchs/icd/Comprehensive-Listing-of-ICD10-CM-Files.htm}
#' \url{https://www.hcup-us.ahrq.gov/tools_software.jsp}
#'
#' @param ref_type a character vector of length one specifying the hospital diagnosis
#' descriptions that are of interest to you. Acceptable options include: \code{'icd10'},
#' \code{'level1'}, \code{'level2'}, \code{'level3'}, & \code{'all'}.
#'
#' The default is \code{ref_type = 'all'}.
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
#' blah <- chars_icd_ccs(ref_type = 'level1')
#' head(blah)
#'
#' @import data.table rads.data
#'
chars_icd_ccs <- function(ref_type = 'all'){
  # Global variables used by data.table declared as NULL here to play nice with devtools::check() ----
  chars_list <- icd10cm <- icd10cm_desc <- ccs_lvl_1_desc <- ccs_lvl_2_desc <- ccs_lvl_3_desc  <- NULL

  if(length(ref_type) != 1){
    stop("\n \U0001f47f the `ref_type` must have a single value. Valid options are 'all', 'icd10', 'level1', 'level2', & 'level3'")}
  if(!ref_type %in% c('all', 'icd10', 'level1', 'level2', 'level3')){
    stop(paste0("\n \U0001f47f'", ref_type, "' is not a valid option for the `ref_type` argument. Valid options are 'all', 'icd10', 'level1', 'level2', & 'level3'."))}

  chars_list <- copy(rads.data::icd_icd10cm_CHAT_2023)
  if(ref_type == 'all'){chars_list <- chars_list[, list(icd10cm_code = icd10cm, icd10cm = icd10cm_desc, level1 = ccs_lvl_1_desc, level2 = ccs_lvl_2_desc, level3 = ccs_lvl_3_desc)]}
  if(ref_type == 'icd10'){chars_list <- chars_list[, list(icd10cm_code = icd10cm, icd10cm = icd10cm_desc)]}
  if(ref_type == 'level1'){chars_list <- chars_list[, list(level1 = ccs_lvl_1_desc)]}
  if(ref_type == 'level2'){chars_list <- chars_list[, list(level2 = ccs_lvl_2_desc)]}
  if(ref_type == 'level3'){chars_list <- chars_list[, list(level3 = ccs_lvl_3_desc)]}

  return(unique(chars_list))
}


# chars_icd_ccs_count() ----
#' Count (non-injury) Comprehensive Hospital Abstract Reporting System
#' (CHARS) hospitalizations
#'
#' @description
#' Generate hospitalization counts from WA State Comprehensive Hospital
#' Abstract Reporting System (CHARS) data using partial strings from the ICD10-cm
#' or CCS (levels 1 - 3) descriptions. Needs line-level CHARS data with a
#' properly formatted ICD10-cm column (e.g., the data available from
#' \code{get_data_chars()}).
#'
#' See \code{chars_icd_ccs()} for a complete list of available ICD10-cm and CCS
#' descriptions.
#'
#' @details
#' This function needs the user to enter a search string in one or more of the
#' following arguments in order to search the CHARS data for the corresponding
#' ICD10-cm codes: \code{icd10}, \code{level1}, \code{level2}, or \code{level3}.
#' Partial search terms are acceptable and they are case-insensitive. For
#' example, if you set \code{level1 = 'ous'}, the function would return counts
#' for three categories of hospitalization: "Diseases of the nervOUS system and
#' sense organs", "InfectiOUS and parasitic diseases", "Diseases of the skin and
#' subcutaneOUS tissue". It also understands simple regex syntax, including '^',
#' '$', and '|'.
#'
#' @param ph.data a data.table or data.frame. Must contain CHARS data structured
#' with one person per row and with at least one column of ICD10-cm codes.
#'
#' **NOTE!** ph.data must have a column named `seq_no`, which is a unique patient
#' level identifier.
#'
#' The default is \code{ph.data = NULL}
#'
#' @param icd10cm a character vector of length 1. An ICD10-CM description OR code.
#' It is case agnostic and works with partial strings. For example, both
#' 'rotavira' & 'A080' would provide the results for 'Rotaviral enteritis'. You
#' can also combine multiple search terms. For example, 'rotavira|choler' would
#' count all Rotaviral enteritis and cholera hospitalizations. View
#' available options with \code{View(chars_icd_ccs()[, .(icd10_code, icd10)])}.
#'
#' The default is \code{icd10 = NULL}
#'
#' @param level1 a character vector of length 1. View available options with
#' \code{chars_icd_ccs('level1')}.
#'
#' The default is \code{level1 = NULL}
#'
#' @param level2 a character vector of length 1. View available options with
#' \code{chars_icd_ccs('level2')}.
#'
#' The default is \code{level2 = NULL}
#'
#' @param level3 a character vector of length 1. View available options with
#' \code{chars_icd_ccs('level3')}.
#'
#' The default is \code{level3 = NULL}
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
#' limit the analysis to King County. **NOTE** this only works with data imported
#' with the \code{get_data_chars()} function because it needs the logical variable
#' \code{chi_geo_kc}.
#'
#' The default is \code{kingco = TRUE}.
#'
#' @return
#' Generates a table with columns for each of the search terms you entered (e.g.,
#' \code{icd10}, \code{level1}, \code{level2}, and/or \code{level3}) as well as
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
#'                                 level3 = 'chemotherapy',
#'                                 group_by = c('chi_sex'))
#' print(myresult)
#' }
#'
#' @import data.table rads.data
#'
chars_icd_ccs_count <- function(ph.data = NULL,
                                icd10cm = NULL,
                                level1 = NULL,
                                level2 = NULL,
                                level3 = NULL,
                                icdcol = 'diag1',
                                group_by = NULL,
                                kingco = T){

  # Global variables used by data.table declared as NULL here to play nice with devtools::check() ----
    CMtable <- CMtable.expanded <- filter.count <- problem.icds <- icd10_desc <- level1_desc <-
      level2_desc <- level3_desc <- chi_geo_kc <- hospitalizations <- icd10_code <- KeepMe <-
      icd10 <- icd10cm_desc <- icd10cm_code <- query.group <- diag1 <- intent_ignore <-
      chars_injury_matrix_count <- mechanism_ignore <- NULL

  # Check arguments & filter reference table of all ICD10-cm (CMtable) ----
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

    # icd10cm + level1 + level2 + level3 ----
        if(is.null(icd10cm) & is.null(level1) & is.null(level2) & is.null(level3)){
          stop("\n\U0001f47f `icd10cm`, `level1`, `level2`, and `level3` are all NULL. This doesn't make sense! Specify a value for at least one of these arguments.")
        }

        CMtable <- chars_icd_ccs() # reference table of all potential search terms for this function
        CMtable <- CMtable[, list(icd10cm_code, icd10cm_desc = icd10cm, level1_desc = level1, level2_desc = level2, level3_desc = level3)]


        filter.count <- sum(!is.null(icd10cm), !is.null(level1), !is.null(level2), !is.null(level3))

    # icd10 ----
        if(!is.null(icd10cm)){
          if(length(icd10cm) != 1){
            stop("\n\U0001f47f When specified, `icd10` must be a character vector of length one.")
            }
          CMtable <- CMtable[grepl(icd10cm, icd10cm_desc, ignore.case = T) | grepl(icd10cm, icd10cm_code, ignore.case = T)]
          if(nrow(CMtable) < 1){
            stop(paste0("\n\U0001f47f Setting the argument <icd10='", icd10cm, "'> filtered out all possible ICD10-cm codes in the reference table. Please change your argument(s)."))
          }
        }

    # level1 ----
        if(!is.null(level1)){
          if(length(level1) != 1){
            stop("\n\U0001f47f When specified, `level1` must be a character vector of length one.")
          }
          CMtable <- CMtable[grepl(level1, level1_desc, ignore.case = T)]
          if(nrow(CMtable) < 1){
            if(filter.count == 1){
              stop(paste0("\n\U0001f47f Setting the argument <level1='", level1, "'> filtered out all possible ICD10-cm codes in the reference table. Please change your argument(s)."))
            }
            if(filter.count > 1){
              stop(paste0("\n\U0001f47f Setting the argument <level1='", level1, "'>, either alone or in combinaton with the values of icd10cm, level2, and level3, filtered out all possible ICD10-cm codes in the reference table. Please change your argument(s)."))
            }
          }
        }

    # level2 ----
        if(!is.null(level2)){
          if(length(level2) != 1){
            stop("\n\U0001f47f When specified, `level2` must be a character vector of length one.")
          }
          CMtable <- CMtable[grepl(level2, level2_desc, ignore.case = T)]
          if(nrow(CMtable) < 1){
            if(filter.count == 1){
              stop(paste0("\n\U0001f47f Setting the argument <level2='", level2, "'> filtered out all possible ICD10-cm codes in the reference table. Please change your argument(s)."))
            }
            if(filter.count > 1){
              stop(paste0("\n\U0001f47f Setting the argument <level2='", level2, "'>, either alone or in combinaton with the values of icd10cm, level1, and level3, filtered out all possible ICD10-cm codes in the reference table. Please change your argument(s)."))
            }
          }
        }

    # level3 ----
        if(!is.null(level3)){
          if(length(level3) != 1){
            stop("\n\U0001f47f When specified, `level3` must be a character vector of length one.")
          }
          CMtable <- CMtable[grepl(level3, level3_desc, ignore.case = T)]
          if(nrow(CMtable) < 1){
            if(filter.count == 1){
              stop(paste0("\n\U0001f47f Setting the argument <level3='", level3, "'> filtered out all possible ICD10-cm codes in the reference table. Please change your argument(s)."))
            }
            if(filter.count >1){
              stop(paste0("\n\U0001f47f Setting the argument <level3='", level3, "'>, either alone or in combinaton with the values of icd10cm, level1, and level2, filtered out all possible ICD10-cm codes in the reference table. Please change your argument(s)."))
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
          "rads.data::icd_icd10cm_CHAT_2023 contains no hyphens or periods."
          ))
          ph.data[, paste0(icdcol) := gsub("[[:space:].]+", "", gsub("([^A-Za-z0-9 ])+", "", x = get(icdcol)))]
        }

        if(nrow(ph.data[is.na(get(icdcol)) | !grepl("^[A-Z][0-9]", get(icdcol))]) > 0){
          problem.icds <- ph.data[is.na(get(icdcol)) | !grepl("^[A-Z][0-9]", get(icdcol)), ][[icdcol]]
          warning(paste0("\U00026A0 There is/are ", length(problem.icds), " row(s) where `icdcol` (",
          icdcol, ") does not follow the proper ICD10-cm pattern. All ICDs that do not begin with a ",
          "single capital letter followed by a number have been replaced with NA."))
          ph.data[!grepl("^[A-Z][0-9]", get(icdcol)) , paste0(icdcol) := NA]
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

  # Drop unnecessary columns from reference table (CMtable) ----
    KeepMe <- c("icd10cm_code")
    for(grr in c('icd10cm', 'level1', 'level2', 'level3')){
     if(!is.null(get(grr))){
       KeepMe <- c(KeepMe, paste0(grr, "_desc"))
     }
    }

    CMtable <- CMtable[, KeepMe, with = FALSE]

  # Get counts of hospitalizations for each group of search terms in CMtable (search term referece table) ----
    # flatten CMtable for regex search ----
      CMtable = CMtable[, list(icd10cm_code = list(icd10cm_code)), by = setdiff(names(CMtable), c("icd10cm_code"))]

    # Create a 'query.group' for each combination of levels/icd10cm ----
      CMtable[, query.group := .GRP, by = setdiff(names(CMtable), "icd10cm_code")]

    # generate counts for each query.group ----
      HospCounts <- data.table()
      for(QG in unique(CMtable$query.group)){
        if(is.null(group_by)){
          tempHospCounts <- ph.data[diag1 %in% unlist(CMtable[query.group == QG]$icd10cm_code), list(hospitalizations = .N)]
        }
        if(!is.null(group_by)){
          tempHospCounts <- ph.data[diag1 %in% unlist(CMtable[query.group == QG]$icd10cm_code), list(hospitalizations = .N), by = group_by]
        }
        tempHospCounts[, query.group := QG]
        tempHospCounts <- merge(tempHospCounts, CMtable, by = "query.group")
        tempHospCounts[, c('icd10cm_code') := NULL]
        HospCounts <- rbind(HospCounts, tempHospCounts, fill = T)
      }

  # Expand reference table for each combination of group_by variables ----
      CMtable[, icd10cm_code := NULL]
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
