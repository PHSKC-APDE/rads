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
death_injury_matrix_count <- function(ph.data = NULL,
                                intent = "*",
                                mechanism = "*",
                                icdcol = "underlying_cod_code",
                                kingco = T,
                                group_by = NULL,
                                ypll_age = NULL,
                                death_age_col = NULL){
  # Global variables used by data.table declared as NULL here to play nice with devtools::check() ----
  x_intent <- x_mechanism <- x_reftable <- x_combo <- orig.coding <- orig.order <- underlying_cod_code <- NULL
  chi_geo_kc <- '.' <- deaths <- icd10 <- NULL
  calculated.age <- x_ypll <- date_of_death <- date_of_birth <-  NULL

  # Check arguments ----
    # ph.data ----
    ph.data.name <- deparse(substitute(ph.data))
    if(!is.null(ph.data)){
      if(!is.data.frame(ph.data)){
        stop("'ph.data' must be the unquoted name of a data.frame or data.table")
      }
      if(is.data.frame(ph.data) && !data.table::is.data.table(ph.data)){
        data.table::setDT(ph.data)
      }
    } else {stop("'ph.data', the name of a data.frame or data.table with line level death data, must be specified")}

    ph.data <- data.table::setDT(data.table::copy(ph.data)) # to prevent changing of original by reference

    # intent ----
    if(isFALSE(is.character(intent)) || length(intent) > 5){
      stop("`intent` must specify a character vector with a lenghth <= 5.\nTo select all options, use intent = '*'.")
    }
    myorig.intent <- copy(intent)

    # mechanism ----
    if(isFALSE(is.character(mechanism)) || length(mechanism) > 28){
      stop("`mechanism` must specify a character vector with a lenghth <= 28.\nTo select all options, use mechanism = '*'.")
    }

    # icdcol ----
    if(isFALSE(icdcol %in% colnames(ph.data))){
      stop("`icdcol` must be the name of column that exists in `ph.data`.")
    }

    ph.data[, paste(icdcol) := toupper(get(icdcol))]

    if(length(grep("\\.|-", ph.data[[icdcol]], value = T) >0 )){
      warning(paste0("
          There is at least one row where `icdcol` (", icdcol, ") contains a hyphen (-), period (.), space or some other non alpha-numeric character.
          These characters will be deleted, e.g., A85.2 will become A852.
          This is necessary because causeids in rads.data::icd10_death_injury_matrix contains no hyphens or periods."
      ))
      ph.data[, paste0(icdcol) := gsub("[[:space:].]+", "", gsub("([^A-Za-z0-9 ])+", "", x = get(icdcol)))]

    }
    if(nrow(ph.data) != nrow(ph.data[is.na(get(icdcol)) | grepl("^[A-Z].*[0-9]$", get(icdcol))])){
      problem.icds <- grep("^[A-Z].*[0-9]$", ph.data[[icdcol]], value = TRUE, invert = TRUE)
      problem.icds <- problem.icds[!is.na(problem.icds)]
      warning(paste0("
                        There is/are ", length(problem.icds), " row(s) where `icdcol` (", icdcol, ") does not follow the proper ICD pattern.
                        All ICDs that do not begin with a capital letter and end with a numeric have be replaced with NA."))
      ph.data[!grepl("^[A-Z].*[0-9]$", get(icdcol)) , paste0(icdcol) := NA]
    }

    ph.data[, paste(icdcol) := substr(get(icdcol), 1, 4)] # trim off extra digits
    ph.data[nchar(get(icdcol)) == 2, paste(icdcol) := paste0(get(icdcol), "00")] # since reference sheet has 4 character ICD10 codes, make sure death data has four digit ICD
    ph.data[nchar(get(icdcol)) == 3, paste(icdcol) := paste0(get(icdcol), "0")] # since reference sheet has 4 character ICD10 codes, make sure death data has four digit ICD

    # kingco ----
    if(!is.logical(kingco)){stop("\n\U0001f47f `kingco` must be a logical vector of length 1, i.e,. TRUE or FALSE.")}
    if (isTRUE(kingco) & (!"chi_geo_kc" %in% names(ph.data))){
      stop("\n\U0001f47f You specified kingco=TRUE, but `ph.data` does not have the following columns that identify King County data:
                     chi_geo_kc")
    }
    if (isTRUE(kingco)){ph.data <- ph.data[chi_geo_kc == "King County"]}

    # group_by ----
    if(!is.null(group_by)){
      group_col_error <- setdiff(group_by, names(ph.data))
      if(length(group_col_error) > 0){stop(paste0("\U0001f6d1\nThe following `group_by` values are not column names in `ph.data`: ", paste0(group_col_error, collapse = ', '), "."))}
    }

    # ypll_age ----
    if(isFALSE(is.null(ypll_age))) {
      if(isFALSE(is.numeric(ypll_age)) || isFALSE(all(ypll_age == floor(ypll_age)))){
        stop("
                 If `ypll_age` is specified, it must be an integer. Typical values are 65 and 85.")
      }
      if(length(ypll_age) != 1){
        stop(("
                  You can only specify one `ypll_age` at a time."))
      }
      if(ypll_age <1 | ypll_age > 99){
        stop("
                 The minumum `ypll_age` is 1 and the maximum is 99.
                 Note that both of these extremes are all but useless.
                 Typical values are 65 and 85.")
      }
    }

    # death_age_col ----
    if(isFALSE(is.null(death_age_col))) {
      if(isFALSE(death_age_col %in% colnames(ph.data))){
        stop("
                `death_age_col` must be the name of column that exists in `ph.data`.")
      }
      if(!class(ph.data[[death_age_col]]) %in% c('integer', 'numeric') || isFALSE(all(ph.data[[death_age_col]] == floor(ph.data[[death_age_col]])))){
        stop("
                If `death_age_col` is specified, it must be a column of integers in `ph.data`.")
      }
      if(is.null(ypll_age)){
        stop("
                `death_age_col` should not be specified when `ypll_age` is NULL.")
      }
    }

    if(isFALSE(is.null(ypll_age)) & is.null(death_age_col) & length(intersect(c("chi_age"), colnames(ph.data))) != 1){
      stop("
               You requested the calculation of YPLL by specifying `ypll_age` and did not provide `death_age_col`.
               The function attempted to use a column named `chi_age`, but it was not found.
               To calculate YPLL, please set death_age_col to the name of the column with the age at death.")
    }

    if( (isFALSE(is.null(ypll_age)) & is.null(death_age_col) & length(intersect(c("chi_age"), colnames(ph.data))) == 1)){
      death_age_col = 'chi_age'
      message("
              You requested the calculation of Years of Potential Life Lost (e.g., `ypll_age = 65`),
              but did provide `death_age_col`. The function found and used a column named `chi_age`
              for the YPLL calculation. If this was not your intention, please specify the correct
              column with the decendant's age with the `death_age_col` argument."
      )}

  # Identify intent of interest ----
  intent = tolower(intent)
  if("*" %in% intent){x_intent = unique(rads.data::icd10_death_injury_matrix$intent)}
  if("none" %in% intent){x_intent = unique(rads.data::icd10_death_injury_matrix$intent)} # "Any intent" will be sum of all of intents
  if(length(intersect(c("*", "none"), intent)) == 0){
    x_intent = c()
    for(i in intent){
      x_intent <- unique(c(x_intent, grep(i, unique(rads.data::icd10_death_injury_matrix$intent), value = TRUE, ignore.case = TRUE)))
    }
  }
  if(length(x_intent) == 0){stop(paste0(
    "\nYour `intent` value (", intent, ") has filtered out all of the death injury intents.\nPlease enter 'none', '*', or a new partial keyword term and try again."))}

  # Identify mechanism of interest ----
  mechanism = tolower(mechanism)
  if("*" %in% mechanism){x_mechanism = unique(rads.data::icd10_death_injury_matrix$mechanism)}
  if("none" %in% mechanism){x_mechanism = "All injury"}
  if(length(intersect(c("*", "none"), mechanism)) == 0){
    x_mechanism = c()
    for(i in mechanism){
      x_mechanism <- unique(c(x_mechanism, grep(i, unique(rads.data::icd10_death_injury_matrix$mechanism), value = TRUE, ignore.case = TRUE)))
    }
  }
  if(length(x_mechanism) == 0){stop(paste0(
    "\nYour `mechanism` value (", mechanism, ") has filtered out all of the death injury mechanisms.\nPlease enter 'none', '*', or a new partial keyword term and try again."))}


  # Count deaths for each intent_x_mechanism of interest ----
    # prep injury matrix reference table ----
    # get reference table from rads.data
    x_reftable <- copy(rads.data::icd10_death_injury_matrix)[, orig.coding := NULL]

    # subset for intent
    if(length(x_intent) == 1 && x_intent == "Any intent"){
      x_reftable[, intent := x_intent]
    } else {
      x_reftable <- x_reftable[intent %in% x_intent]
    }

    # subset for mechanism
    x_reftable <- x_reftable[mechanism %in% x_mechanism]

    # merge reference table onto death data ----
    x_combo <- merge(ph.data, x_reftable, by.x = icdcol, by.y = "icd10", all.x = F, all.y = F, allow.cartesian = TRUE)
    x_combo[, c(icdcol) := NULL]
    if("none" %in% intent){x_combo[, intent := "Any intent"]}

    # calculate death count ----
    if(is.null(ypll_age)){
      x_combo <- x_combo[, .(deaths = .N), by = c("mechanism", "intent", group_by)]
    } else {
      # create table with ypll summary
      x_ypll <- copy(x_combo)
      x_ypll[ypll_age >= get(death_age_col), paste0("ypll_", ypll_age) := ypll_age - get(death_age_col)]
      x_ypll[, c(death_age_col) := NULL]
      x_ypll <- x_ypll[, .(temp_ypll = sum(get(paste0("ypll_", ypll_age)), na.rm = TRUE)), # use temporary name because data.table doesn't accept quoted value after .(
                       by = c("mechanism", "intent", group_by)]
      setnames(x_ypll, "temp_ypll", paste0("ypll_", ypll_age))

      # create table with death summary
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
          x_combo <- x_combo[, list(deaths = sum(deaths), temp_ypll = sum(get(paste0("ypll_", ypll_age)))),
                             by = setdiff(names(x_combo), c("deaths", paste0("ypll_", ypll_age)))]
          setnames(x_combo, "temp_ypll", paste0("ypll_", ypll_age))
        }
      }

      if(mechanism == 'none'){x_combo[, mechanism := 'Any mechanism']}

    # Create rows for zero values (otherwise rows would simply be missing) ----
      # create temporary vectors of unique values of all columns EXCEPT deaths and ypll_##
        for(i in setdiff(names(x_combo), c('deaths', grep('^ypll_', names(x_combo), value = T)))){
          assign(paste0('xyz_', i), unique(x_combo[, get(i)]))
        }

        if(myorig.intent == '*'){xyz_intent <- unique(death_injury_matrix()[]$intent)}

      # create template of all combinations of x_combo values
        template.xyz <- setDT(expand.grid(mget(ls(pattern = 'xyz_'))))
        setnames(template.xyz, gsub('^xyz_', '', names(template.xyz)))

      # merge actual values onto template.xyz
        x_combo <- merge(template.xyz, x_combo, all = T)

      # Fill deaths with zeros
        x_combo[is.na(deaths), deaths := 0]

      # Fill ypll_## with zeros if needed
        if(!is.null(ypll_age)){
          ypll_name = grep('^ypll_[0-9]', names(x_combo), value = T)
          x_combo[is.na(get(ypll_name)), paste0(ypll_name) := 0]
        }

    # Sort columns and rows ----
      if(!is.null(ypll_age)){
        setcolorder(x_combo, c("mechanism", "intent", "deaths", ypll_name))
        setorderv(x_combo, c("mechanism", "intent", setdiff(names(x_combo), c("deaths", "mechanism", "intent", ypll_name)) ))
      } else{
        setcolorder(x_combo, c("mechanism", "intent", "deaths"))
        setorderv(x_combo, c("mechanism", "intent", setdiff(names(x_combo), c("deaths", "mechanism", "intent")) ))
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
#' function because it needs the logical variable \code{chi_geo_kc}.
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
death_xxx_count <- function(ph.data = NULL,
                            causeids = NULL,
                            cause = NULL,
                            icdcol = "underlying_cod_code",
                            kingco = T,
                            group_by = NULL,
                            ypll_age = NULL,
                            death_age_col = NULL,
                            nchsnum = NULL) {
  # Global variables used by data.table declared as NULL here to play nice with devtools::check() ----
    problem.icds  <-  causeid <- cause.of.death <- deaths <- '.' <- NULL
    x_reftable <- x_combo <- x_covid <- x_cause <- x_all <- x_ypll <- NULL
    chi_geo_kc <- underlying_cod_code <- icd10 <- x.causeid <- NULL
    date_of_death <- date_of_birth <- calculated.age <- orig.coding <- NULL

  # Check arguments ----
      # ph.data ----
          ph.data.name <- deparse(substitute(ph.data))
          if (!is.null(ph.data)) {
            if (!is.data.frame(ph.data)) {
              stop("'ph.data' must be the unquoted name of a data.frame or data.table")
            }
            if (is.data.frame(ph.data) &&
                !data.table::is.data.table(ph.data)) {
              data.table::setDT(ph.data)
            }
          } else {
            stop(
              "'ph.data', the name of a data.frame or data.table with line level death data, must be specified"
            )
          }

          ph.data <-
            data.table::setDT(data.table::copy(ph.data)) # to prevent changing of original by reference

      # causeids ----
          if(is.null(causeids) & is.null(cause)){stop("\U0001f6d1 You cannot have both `causeids` and `cause` == NULL.")}
          if(nchsnum == 113 & !is.null(causeids)){
              if (isFALSE(is.numeric(causeids)) ||
                  isFALSE(all(causeids == floor(causeids)))) {
                stop(
                  "`causeids` must be a vector of integers (e.g., c(1, 3, 5)). \nTo see the full list of causeids, type `death_113()`\nIf you plan to use keywords, you can set causeids = NULL."
                )
              }
              if (min(causeids) < 1 | max(causeids) > 114) {
                stop(
                  "`causeids` are limited to integers [1, 114]. \nTo see the full list of causeids, type `death_113()`"
                )
              }
          }
          if(nchsnum == 130 & !is.null(causeids)){
            if (isFALSE(is.numeric(causeids)) ||
                isFALSE(all(causeids == floor(causeids)))) {
              stop(
                "`causeids` must be a vector of integers (e.g., c(1, 3, 5)). \nTo see the full list of causeids, type `death_130()`\nIf you plan to use keywords, you can set causeids = NULL."
              )
            }
            if (min(causeids) < 1 | max(causeids) > 130) {
              stop(
                "`causeids` are limited to integers [1, 130]. \nTo see the full list of causeids, type `death_130()`"
              )
            }
          }

          causeids <- sort(unique(causeids))

      # cause ----
          if (isFALSE(is.null(cause)) & isFALSE(is.character(cause))) {
            stop(
              "`cause` must either be NULL (in which case the function uses causeids) or must be character vector with whole or partial keywords for the cause of death of interest."
            )
          }
          if (is.character(cause)) {
            causeids = NULL
          }

          if (is.null(causeids) &
              is.null(cause)) {
            stop("Both `causeids` and `cause` are NULL You must specify one or the other.")
          }

      # icdcol ----
          if (isFALSE(icdcol %in% colnames(ph.data))) {
            stop("`icdcol` must be the name of column that exists in `ph.data`.")
          }

          ph.data[, paste(icdcol) := toupper(get(icdcol))]

          if (length(grep("\\.|-", ph.data[[icdcol]], value = T) > 0)) {
            warning(
              paste0(
                "
                  There is at least one row where `icdcol` (",
                icdcol,
                ") that contains a hyphen (-), period (.), space or some other non alpha-numeric character.
                  These characters will be deleted, e.g., A85.2 will become A852.
                  This is necessary because causeids the rads.data::icd_nchs###causes tables contain no hyphens or periods."
              )
            )
            ph.data[, paste0(icdcol) := gsub("[[:space:].]+", "", gsub("([^A-Za-z0-9 ])+", "", x = get(icdcol)))]

          }
          if (nrow(ph.data) != nrow(ph.data[is.na(get(icdcol)) |
                                            grepl("^[A-Z].*[0-9]$", get(icdcol))])) {
            problem.icds <-
              grep("^[A-Z].*[0-9]$",
                   ph.data[[icdcol]],
                   value = TRUE,
                   invert = TRUE)
            problem.icds <- problem.icds[!is.na(problem.icds)]
            warning(
              paste0(
                "
                                There is/are ",
                length(problem.icds),
                " row(s) where `icdcol` (",
                icdcol,
                ") does not follow the proper ICD pattern.
                                All ICDs that do not begin with a capital letter and end with a numeric have be replaced with NA."
              )
            )
            ph.data[!grepl("^[A-Z].*[0-9]$", get(icdcol)) , paste0(icdcol) := NA]
          }

          ph.data[, paste(icdcol) := substr(get(icdcol), 1, 4)] # trim off extra digits
          ph.data[nchar(get(icdcol)) == 2, paste(icdcol) := paste0(get(icdcol), "00")] # since reference sheet has 4 character ICD10 codes, make sure death data has four digit ICD
          ph.data[nchar(get(icdcol)) == 3, paste(icdcol) := paste0(get(icdcol), "0")] # since reference sheet has 4 character ICD10 codes, make sure death data has four digit ICD

      # check that kingco is a logical ----
          if (isFALSE(is.logical(kingco))) {
            stop("`kingco` must be a logical value (i.e., T|F|TRUE|FALSE")
          }
          if (isTRUE(kingco) & (!"chi_geo_kc" %in% names(ph.data))) {
            stop(
              "You specified kingco=TRUE, but `ph.data` does not have the following columns that identify King County data:
                     chi_geo_kc"
            )
          }
          if (isTRUE(kingco)) {
            ph.data <- ph.data[chi_geo_kc == 'King County']
          }

      # group_by ----
          if(!is.null(group_by)){
            group_col_error <- setdiff(group_by, names(ph.data))
            if(length(group_col_error) > 0){stop(paste0("\U0001f6d1\nThe following `group_by` values are not column names in `ph.data`: ", paste0(group_col_error, collapse = ', '), "."))}
          }

      # ypll_age ----
          if (isFALSE(is.null(ypll_age))) {
            if (isFALSE(is.numeric(ypll_age)) ||
                isFALSE(all(ypll_age == floor(ypll_age)))) {
              stop("
                       If `ypll_age` is specified, it must be an integer. Typical values are 65 and 85.")
            }
            if (length(ypll_age) != 1) {
              stop(("
                        You can only specify one `ypll_age` at a time."))
            }
            if (ypll_age < 1 | ypll_age > 99) {
              stop(
                "
                       The minumum `ypll_age` is 1 and the maximum is 99.
                       Note that both of these extremes are all but useless.
                       Typical values are 65 and 85."
              )
            }
          }

      # death_age_col ----
          if (isFALSE(is.null(death_age_col))) {
            if (isFALSE(death_age_col %in% colnames(ph.data))) {
              stop("
                       `death_age_col` must be the name of column that exists in `ph.data`.")
            }
            if (isFALSE(class(ph.data[[death_age_col]]) %in% c("numeric", 'integer')) ||
                isFALSE(all(ph.data[[death_age_col]] == floor(ph.data[[death_age_col]])))) {
              stop("
                       If `death_age_col` is specified, it must be a column of integers in `ph.data`.
                       ")
            }
            if (is.null(ypll_age)) {
              stop("
                       `death_age_col` should not be specified when `ypll_age` is NULL.")
            }
          }
          if (isFALSE(is.null(ypll_age)) &
              is.null(death_age_col) &
              length(intersect(c("chi_age"), colnames(ph.data))) != 1) {
            stop(
              "
                     You requested the calculation of YPLL by specifying `ypll_age` and did not provide `death_age_col`.
                     The function attempted to use a column named `chi_age`, but it was not found.
                     To calculate YPLL, please set death_age_col to the name of the column with the age at death."
            )
          }
          if (isFALSE(is.null(ypll_age)) &
              is.null(death_age_col) &
              length(intersect(c("chi_age"), colnames(ph.data))) == 1) {
            death_age_col = 'chi_age'
            message(
              "
                    You requested the calculation of YPLL by specifying `ypll_age` and did not provide `death_age_col`.
                    The function found and used a column named `chi_age` for the YPLL calculation. If this was not
                    your intention, please specify the correct column with the decendant's age with the
                    `death_age_col` argument
                    "
            )
          }

  # Import reference table once ----
      if(nchsnum == 113){
        nchsref <- unique(rads.data::icd_nchs113causes[, .(cause.of.death, causeid)])} else {
          nchsref <- unique(rads.data::icd_nchs130causes[, .(cause.of.death, causeid)])
        }

  # Identify cause(s) of interest ----
      if (isFALSE(is.null(cause))) {
        cause = tolower(cause)
        x_cause = c()
        for (i in cause) {
          x_cause <- unique(c(x_cause,
                              grep(i,
                                   nchsref$cause.of.death,
                                   value = TRUE,
                                   ignore.case = TRUE)))
        }
      }
      if (isFALSE(is.null(cause)) & length(x_cause) == 0) {
        stop(
          paste0(
            "\nYour `cause` value(s) (",
            paste0("'", paste0(cause, collapse= "', '"), "'"),
            ") filtered out all of the available causes of death.
            Please enter a new keyword or keywords and try again.
            To view all available cause of death, type the following in your console:
            rads::death_", nchsnum, "()"
          )
        )
      }

  # Count deaths for each cause ----
    # prep causes of death reference table ----
      # get reference table from rads.data
      if(nchsnum == 113){x_reftable <- copy(rads.data::icd_nchs113causes)}
      if(nchsnum == 130){x_reftable <- copy(rads.data::icd_nchs130causes)}

      # subset for causeids
      if (isFALSE(is.null(causeids))) {
        x_reftable <-
          x_reftable[causeid %in% causeids] # limit to causeids from arguments
      }

      # subset for cause of death
      if (isFALSE(is.null(cause))) {
        x_reftable <-
          x_reftable[cause.of.death %in% x_cause] # limit to named causes of death from arguments
      }

      # bring causeid 17 into alignment with WA DOH (and out of alignment with CDC)
      x_reftable <-
        x_reftable[!(causeid == 17 &
                       icd10 == 'U071')] # this is COVID, which DOH extracts from unspecified infectious/parasitic

    # calculate YPLL line level if needed ----
      if (isFALSE(is.null(ypll_age))) {
        ph.data[ypll_age >= get(death_age_col), paste0("ypll_", ypll_age) := ypll_age - get(death_age_col)]
        ph.data[, c(death_age_col) := NULL]
      }

    # merge reference table onto death data ----
      x_combo <-
        merge(
          ph.data,
          x_reftable,
          by.x = icdcol,
          by.y = "icd10",
          all.x = T,
          all.y = F
        )

      x_combo[get(icdcol) %like% "U071", `:=` (cause.of.death = "COVID-19 (U07.1)",
                                               orig.coding = 'U07.1')]

      x_combo[, c(icdcol) := NULL]
      x_combo[is.na(cause.of.death), cause.of.death := "Missing/Unknown"]

  # calculate death count ----
      if (is.null(ypll_age)) {
        x_all <-
          copy(ph.data)[, .(causeid = NA_character_,
                            cause.of.death = "All causes",
                            deaths = .N),
                        by = group_by]
        x_combo <-
          x_combo[, .(deaths = .N), by = c("causeid", "cause.of.death", group_by)]

        x_combo <- rbind(x_all, x_covid, x_combo)
      } else {
        # create summary table of YPLL ----
        # all deaths
        x_all <- copy(ph.data)[, .(
          causeid = NA_character_,
          cause.of.death = "All causes",
          orig.coding = NA_character_,
          deaths = .N,
          temp_ypll = sum(get(paste0(
            "ypll_", ypll_age
          )), na.rm = TRUE)
        ),
        by = group_by]

    # NCHS causes of death
    x_combo <- copy(x_combo)[, .(deaths = .N, temp_ypll = sum(get(paste0("ypll_", ypll_age)), na.rm = TRUE)),
                             by = c("causeid", "cause.of.death", 'orig.coding', group_by)]

    # combine all_deaths + COVID-19 + NCHS###
    x_combo <- rbind(x_all, x_combo)
    rm(list = c("x_all", "x_covid"))
    setnames(x_combo, "temp_ypll", paste0("ypll_", ypll_age))
  }

  # Tidy ----
    # Drop orig.coding if exists ----
      if ("orig.coding" %in% names(x_combo)) {
        x_combo[, orig.coding := NULL]
      }

    # Create rows for zero values (otherwise rows would simply be missing) ----
    # create temporary vectors of unique values of all columns EXCEPT deaths and ypll_##
      for (i in setdiff(names(x_combo), c('deaths', 'causeid', grep('^ypll_', names(x_combo), value = T)))) {
        assign(paste0('xyz_', i), unique(x_combo[, get(i)]))
      }

      # create template of all combinations of x_combo values
      template.xyz <- setDT(expand.grid(mget(ls(pattern = 'xyz_'))))
      setnames(template.xyz, gsub('^xyz_', '', names(template.xyz)))

      # merge actual values onto template.xyz
      x_combo <- merge(template.xyz, x_combo, all = T)

      # Fill deaths with zeros
      x_combo[is.na(deaths), deaths := 0]

      # Fill ypll_## with zeros if needed
      if (!is.null(ypll_age)) {
        ypll_name = grep('^ypll_[0-9]', names(x_combo), value = T)
        x_combo[is.na(get(ypll_name)), paste0(ypll_name) := 0]
      }

      # Fill causeid if needed
      x_combo[is.na(causeid), causeid := nchsref[x_combo[is.na(causeid)], on = .(cause.of.death), x.causeid]]

    # Sort columns and rows ----
      if (!is.null(ypll_age)) {
        setcolorder(x_combo,
                    c("cause.of.death", "causeid", "deaths", ypll_name))
        setorderv(x_combo, c('cause.of.death', setdiff(
          names(x_combo),
          c("deaths", 'cause.of.death', "causeid", ypll_name)
        )))
      } else{
        setcolorder(x_combo, c("cause.of.death", "causeid", "deaths"))
        setorderv(x_combo, c('cause.of.death', setdiff(
          names(x_combo), c("deaths", 'cause.of.death', "causeid")
        )))
      }

  # Message about COVID-19 if selected causeid 17 ----
  if(nchsnum == 113){
    if (17 %in% x_combo$causeid) {
      message(
        "\U00026A0
              You selected causeid == 17 (Other and unspecified infectious and parasitic diseases...).
              COVID-19 (U07.1) has been EXCLUDED from this cause, following the example of WA DOH. Note
              however that, as of October 2020, CDC INCLUDES COVID-19 (U07.1) in causeid == 17. In
              otherwords, APDE followed WA DOH's decision since we provide a separate row for COVID-19."
      )
    }
  }

  # Return data ----
  return(x_combo)
}


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
death_113<- function(){
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
#' function because it needs the logical variable \code{chi_geo_kc}.
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
death_113_count <- function(ph.data = NULL,
                            causeids = seq(1, 113, 1),
                            cause = NULL,
                            icdcol = "underlying_cod_code",
                            kingco = T,
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
#' function because it needs the logical variable \code{chi_geo_kc}.
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
#' \url{https://secureaccess.wa.gov/doh/chat/Content/FilesForDownload/CodeSetDefinitions/NCHS130CausesInfantDeath_Codes.pdf}
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
death_130_count <- function(ph.data = NULL,
                            causeids = seq(1, 130, 1),
                            cause = NULL,
                            icdcol = "underlying_cod_code",
                            kingco = T,
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
death_other_count <- function(ph.data = NULL,
                               cause = NULL,
                               icdcol = "underlying_cod_code",
                               kingco = T,
                               group_by = NULL,
                               ypll_age = NULL,
                               death_age_col = NULL){
  # Global variables used by data.table declared as NULL here to play nice with devtools::check() ----
  problem.icds <- long113 <-  cause.of.death <- deaths <- '.' <- NULL
  x_reftable <- x_combo <- x_covid <- x_cause <- x_all <- x_ypll <- NULL
  chi_geo_kc <- underlying_cod_code <- NULL
  date_of_death <- date_of_birth <- calculated.age <- orig.coding <- NULL

  # Check arguments ----
    # ph.data ----
    ph.data.name <- deparse(substitute(ph.data))
    if(!is.null(ph.data)){
      if(!is.data.frame(ph.data)){
        stop("'ph.data' must be the unquoted name of a data.frame or data.table")
      }
      if(is.data.frame(ph.data) && !data.table::is.data.table(ph.data)){
        data.table::setDT(ph.data)
      }
    } else {stop("'ph.data', the name of a data.frame or data.table with line level death data, must be specified")}

    ph.data <- data.table::setDT(data.table::copy(ph.data)) # to prevent changing of original by reference

    # cause ----
    if(is.null(cause)){
      stop("`cause` cannot be NULL. Please specify the `cause = XXX` argument and submit again")
    }
    if(isFALSE(is.character(cause))){
      stop("`cause` must be a character vector with whole or partial keywords for the cause of death of interest.")
    }

    # icdcol ----
      if(isFALSE(icdcol %in% colnames(ph.data))){
        stop("`icdcol` must be the name of column that exists in `ph.data`.")
      }

      ph.data[, paste(icdcol) := toupper(get(icdcol))]

      if(length(grep("\\.|-", ph.data[[icdcol]], value = T) >0 )){
        warning(paste0("
              There is at least one row where `icdcol` (", icdcol, ") contains a hyphen (-), period (.), space or some other non alpha-numeric character.
              These characters will be deleted, e.g., A85.2 will become A852.
              This is necessary because causeids in rads.data::icd_other_causes_of_death contains no hyphens or periods."
        ))
        ph.data[, paste0(icdcol) := gsub("[[:space:].]+", "", gsub("([^A-Za-z0-9 ])+", "", x = get(icdcol)))]

      }
      if(nrow(ph.data) != nrow(ph.data[is.na(get(icdcol)) | grepl("^[A-Z].*[0-9]$", get(icdcol))])){
        problem.icds <- grep("^[A-Z].*[0-9]$", ph.data[[icdcol]], value = TRUE, invert = TRUE)
        problem.icds <- problem.icds[!is.na(problem.icds)]
        warning(paste0("
                            There is/are ", length(problem.icds), " row(s) where `icdcol` (", icdcol, ") does not follow the proper ICD pattern.
                            All ICDs that do not begin with a capital letter and end with a numeric have be replaced with NA."))
        ph.data[!grepl("^[A-Z].*[0-9]$", get(icdcol)) , paste0(icdcol) := NA]
      }

      ph.data[, paste(icdcol) := substr(get(icdcol), 1, 4)] # trim off extra digits
      ph.data[nchar(get(icdcol)) == 2, paste(icdcol) := paste0(get(icdcol), "00")] # since reference sheet has 4 character ICD10 codes, make sure death data has four digit ICD
      ph.data[nchar(get(icdcol)) == 3, paste(icdcol) := paste0(get(icdcol), "0")] # since reference sheet has 4 character ICD10 codes, make sure death data has four digit ICD

    # check that kingco is a logical ----
    if (isFALSE(is.logical(kingco))){stop("`kingco` must be a logical value (i.e., T|F|TRUE|FALSE")}
    if (isTRUE(kingco) & (!"chi_geo_kc" %in% names(ph.data))){
      stop("You specified kingco=TRUE, but `ph.data` does not have the following columns that identify King County data:
                     chi_geo_kc")
    }
    if (isTRUE(kingco)){ph.data <- ph.data[chi_geo_kc == 'King County']}

    # group_by ----
      if(!is.null(group_by)){
        group_col_error <- setdiff(group_by, names(ph.data))
        if(length(group_col_error) > 0){stop(paste0("\U0001f6d1\nThe following `group_by` values are not column names in `ph.data`: ", paste0(group_col_error, collapse = ', '), "."))}
      }

    # ypll_age ----
    if(isFALSE(is.null(ypll_age))) {
      if(isFALSE(is.numeric(ypll_age)) || isFALSE(all(ypll_age == floor(ypll_age)))){
        stop("
                       If `ypll_age` is specified, it must be an integer. Typical values are 65 and 85.")
      }
      if(length(ypll_age) != 1){
        stop(("
                        You can only specify one `ypll_age` at a time."))
      }
      if(ypll_age <1 | ypll_age > 99){
        stop("
                       The minumum `ypll_age` is 1 and the maximum is 99.
                       Note that both of these extremes are all but useless.
                       Typical values are 65 and 85.")
      }
    }
      if(isFALSE(is.null(ypll_age)) & is.null(death_age_col) & length(intersect(c("chi_age"), colnames(ph.data))) != 1){
        stop("
             You requested the calculation of YPLL by specifying `ypll_age` and did not provide `death_age_col`.
             The function attempted to use a column named `chi_age`, but it was not found.
             To calculate YPLL, please set death_age_col to the name of the column with the age at death.")
      }
      if(isFALSE(is.null(ypll_age)) & is.null(death_age_col) & length(intersect(c("chi_age"), colnames(ph.data))) == 1){
        death_age_col = 'chi_age'
        message("
            You requested the calculation of YPLL by specifying `ypll_age` and did not provide `death_age_col`.
            The function found and used a column named `chi_age` for the YPLL calculation. If this was not
            your intention, please specify the correct column with the decendant's age with the
            `death_age_col` argument
            "
        )}

    # death_age_col ----
    if(isFALSE(is.null(death_age_col))) {
      if(isFALSE(death_age_col %in% colnames(ph.data))){
        stop("
                       `death_age_col` must be the name of column that exists in `ph.data`.")
      }
      if(isFALSE(class(ph.data[[death_age_col]]) %in% c("numeric", 'integer')) || isFALSE(all(ph.data[[death_age_col]] == floor(ph.data[[death_age_col]])))){
        stop("
                       If `death_age_col` is specified, it must be a column of integers in `ph.data`.
                       ")
      }
      if(is.null(ypll_age)){
        stop("
                       `death_age_col` should not be specified when `ypll_age` is NULL.")
      }
    }
    if(isFALSE(is.null(ypll_age)) & is.null(death_age_col) & length(intersect(c("date_of_birth", "date_of_death"), colnames(ph.data))) != 2){
      stop("
                     You requested the calculation of YPLL by specifying `ypll_age` and did not provide `death_age_col`.
                     The function attempted to calculate the age at death using columns named 'date_of_birth' and 'date_of_death',
                     but at least one of these columns were not found.
                     To calculate YPLL, please set death_age_col to the name of the column with the age at death OR ensure that
                     `ph.data` has columns named 'date_of_birth' and 'date_of_death' with properly formatted dates.")
    }
    if(isFALSE(is.null(ypll_age)) & is.null(death_age_col) & length(intersect(c("date_of_birth", "date_of_death"), colnames(ph.data))) == 2){
      if(isFALSE(class(ph.data[['date_of_birth']]) == "Date") | isFALSE(class(ph.data[['date_of_birth']]) == "Date")){
        stop("
                       You requested the calculation of YPLL by specifying `ypll_age` and did not provide `death_age_col`.
                       The function attempted to calculate the age at death using columns named 'date_of_birth' and 'date_of_death',
                       but at least one of these columns were not of the class 'Date'. Please update your data and try running it again.")
      } else{
        ph.data[date_of_death >= date_of_birth, calculated.age := rads::calc_age(date_of_birth, date_of_death)]
        ph.data[, c("date_of_death", "date_of_birth") := NULL]
        death_age_col = "calculated.age"
      }
    }

  # Identify cause(s) of interest ----
  if(isFALSE(is.null(cause))){
    cause = tolower(cause)
    x_cause = c()
    for(i in cause){
      x_cause <- unique(c(x_cause,
                          grep(i, unique(rads.data::icd_other_causes_of_death[]$cause.of.death),
                               value = TRUE,
                               ignore.case = TRUE)))
    }
  }
  if(isFALSE(is.null(cause)) & length(x_cause) == 0){stop(paste0(
    "\nYour `cause` value (", cause, ") has filtered out all of the available causes of death.
          Please enter a new keyword or keywords and try again.
          To view all available cause of death, type the following in your console: death_other()"))}

  # Count deaths for each cause of death ----
    # prep cause of death reference table ----
      # get reference table from rads.data
      x_reftable <- copy(rads.data::icd_other_causes_of_death)[, orig.coding := NULL]
      if('source' %in% names(x_reftable)){x_reftable[, source := NULL]}

      # subset for cause of death
      if(isFALSE(is.null(cause))){
        x_reftable <- x_reftable[cause.of.death %in% x_cause] # limit to named causes of death from arguments
      }

    # calculate YPLL line level if needed ----
    if(isFALSE(is.null(ypll_age))){
      ph.data[ypll_age >= get(death_age_col), paste0("ypll_", ypll_age) := ypll_age - get(death_age_col)]
      ph.data[, c(death_age_col) := NULL]
    }

    # merge reference table onto death data ----
    # can't do a simple merge because some causes are sub-categories of others,
    # e.g., drug-overdose is a subset of drug-induced
    x_combo <- data.table() # shell to append sub-tables created for each cause of death
    for(each.cod in unique(x_reftable$cause.of.death)){
      x_combo <- rbind(x_combo,
                       merge(ph.data, x_reftable[cause.of.death == each.cod], by.x = icdcol, by.y = "icd10", all.x = T, all.y = F))
    }
    x_combo <- x_combo[!is.na(cause.of.death)]
    x_combo[, c(icdcol) := NULL]


    # calculate death count ----
    if(is.null(ypll_age)){
      x_all <- copy(ph.data)[, .(cause.of.death = "All causes", deaths = .N),
                             by = group_by]

      x_combo <- x_combo[, .(deaths = .N), by = c("cause.of.death", group_by)]

      x_combo <- rbind(x_all, x_combo)
    } else {
      # create summary table of YPLL ----
      # all deaths
      x_all <- copy(ph.data)[, list(cause.of.death = "All causes",
                                 deaths = .N,
                                 temp_ypll = sum(get(paste0("ypll_", ypll_age)), na.rm = TRUE)),
                             by = group_by]

      # NCHS causes of death
      x_combo <- copy(x_combo)[, list(deaths = .N,
                                   temp_ypll = sum(get(paste0("ypll_", ypll_age)), na.rm = TRUE)),
                               by = c('cause.of.death', group_by)]

      # combine all_deaths + NCHS_113
      x_combo <- rbind(x_all, x_combo)
      rm(list = c("x_all"))
      setnames(x_combo, "temp_ypll", paste0("ypll_", ypll_age))
    }


  # Tidy ----
    # Create rows for zero values (otherwise rows would simply be missing) ----
      # create temporary vectors of unique values of all columns EXCEPT deaths and ypll_##
        for(i in setdiff(names(x_combo), c('deaths', grep('^ypll_', names(x_combo), value = T)))){
          assign(paste0('xyz_', i), unique(x_combo[, get(i)]))
        }

      # create template of all combinations of x_combo values
        template.xyz <- setDT(expand.grid(mget(ls(pattern = 'xyz_'))))
        setnames(template.xyz, gsub('^xyz_', '', names(template.xyz)))

      # merge actual values onto template.xyz
        x_combo <- merge(template.xyz, x_combo, all = T)

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

# life_table ----
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
#' - mx: age interval specific death rate
#'
#' - qx: probability of dying in the age interval
#'
#' - lx: # of (theoretical) persons alive at the start of the age interval
#'
#' - dx: # of deaths during the age interval
#'
#' - ax: average fraction of the interval lived by those who died in the interval
#'
#' - Lx: total person years lived in the age interval
#'
#' - Tx: total person years lived beyond the start of the age interval
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

life_table <- function(ph.data = NULL,
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

  # Get name of the data.frame/data.table ----
  ph.dataname <- deparse(substitute(ph.data))

  # Check arguments ----
    # ph.data ----
      if(!is.null(ph.data)){
        if(!is.data.frame(ph.data)){
          stop("\n\U0001f47f 'ph.data' must be the unquoted name of a data.frame or data.table")
        }
        if(is.data.frame(ph.data) && !data.table::is.data.table(ph.data)){
          data.table::setDT(ph.data)
        }
      } else {stop("\n\U0001f47f 'ph.data', the name of a data.frame or data.table with population and death data, must be specified")}

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
      fill_variance <- function(ph.data.sub){
        obs.variances <- sort(ph.data.sub[!is.nan(qx_variance) & qx_variance != 0]$qx_variance)
        if(length(obs.variances) < 0.5*nrow(ph.data.sub)){
          warning(paste0("\U00026A0 You have ", nrow(ph.data.sub[is.nan(qx_variance)]), " rows where the variance of the probability of dying in the interval is NaN, probably due to zero deaths.",
                         "\n This is more than 50% of your age groups, which means your population is likely too small for meaningful life expectancy calculations.",
                         "\n The variances will be filled with the median of the measured variances, but be cautious in using / interpreting the estimates.",
                         "\n If this message is repeated, it is because this problem occurs in more than one of your tables defined by the `group_by` argument."))}
        ph.data.sub[is.nan(qx_variance), qx_variance := median(obs.variances)]
        return(ph.data.sub)
      }

    # Then apply fill.variance() function
      if(is.null(group_by)){
        ph.data <- fill_variance(ph.data.sub = ph.data)
      } else {
        ph.split <- split(ph.data, by = group_by) # create a list of tables with unique combo of group_by values
        ph.data <- rbindlist(lapply(ph.split,
                                    FUN = function(x) fill_variance(ph.data.sub = x)), use.names = T)
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

  # variance for oldest age interval cannot be calculated using the Chiang method
  # and is assumed to be zero because qx for the oldest interval == 1.00.
  # CDC follows Silcocks' approximation, not Chiang's assumption, which is what we will use here.
  # The next commented out formula appears in multiple CDC publications, but it seems incorrect
  # because (a) I see nothing like this in Silcock's paper, and (b) the variance is enormous
  # to the point of uselessness. On March 9, 2022 I received confirmation from the
  # authors that the printed formula was incorrect.
  # ph.data[irank == max(irank), ex_variance := ((lx^2)/(mx^4)) * mx_se^2]
  # The replacement formula below was derived from careful study of Silcocks' original paper
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

  ph.data[, ex_se := sqrt(ex_variance)]
  zscore = qnorm(1 - (1-ci)/2) # since two sided, need to split the alpha for upper and lower tails
  ph.data[, ex_lower := ex - ex_se * zscore]
  ph.data[, ex_upper := ex + ex_se * zscore]

  # Tidy final output ----
  # order and subset columns
  if("ax" %in% names(ph.data)){
    ordered_cols <- c(myages, mypops, mydeaths, "mx", "qx", "lx", "dx", "ax", "Lx", "Tx", "ex", "ex_lower", "ex_upper", "ex_se")
  } else{
    ph.data[, ax := get(myprops)]
    ordered_cols <- c(myages, mypops, mydeaths, myprops, "mx", "qx", "lx", "dx", "ax", "Lx", "Tx", "ex", "ex_lower", "ex_upper", "ex_se")
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

# life_table_prep () ----
#' Prepare death data for use with life_table()
#'
#' @description
#' Processes individual-level death data to create a standardized data table.
#' This table is collapsed/aggregated by age bin and optionally, by
#' demographics, for use with `rads::life_table()`.
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
life_table_prep <- function(ph.data = NULL,
                            cuts = c(0, 1, 5, 10, 15, 18, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85),
                            dobvar = "date_of_birth",
                            dodvar = "date_of_death",
                            group_by = NULL){
  # Global variables used by data.table declared as NULL here to play nice with devtools::check() ----
  orig_cols <- dob <- dod <- dob_na <- dob_na <- death_age <- tempz <- NULL
  end <- start <- interval <- age.lab <- ages <- length.interval <- NULL
  interval.start <- interval.end <- fraction <- ph.datasum <- '.' <- NULL

  # Check arguments ----
    # ph.data ----
      if(!is.null(ph.data)){
        if(!is.data.frame(ph.data)){
          stop("\n\U0001f47f 'ph.data' must be the unquoted name of a data.frame or data.table")
        }
        if(is.data.frame(ph.data) && !data.table::is.data.table(ph.data)){
          data.table::setDT(ph.data)
        }
      } else {stop("\n\U0001f47f 'ph.data', the name of a data.frame or data.table with population and death data, must be specified")}

      if(length(intersect(c("start", "end", "dob", "dod"), names(ph.data))) > 1){
        warning("ph.data has a column named 'start', 'end', 'dob', or 'dod' which was overwritten by this function. \nTo preserve your columns, rename them, and run this function again.")}

      ph.data = data.table::setDT(data.table::copy(ph.data)) # copy at start to prevent changes by reference

    # cuts ----
      if(isTRUE(any(is.na(cuts))) || is.null(cuts) || !is.numeric(cuts)){stop("\n\U0001f47f 'cuts' must be specified as a numeric vector and cannot contain any NA's.")}
      if(length(cuts) <= 1){stop("\n\U0001f47f 'cuts' should be a numeric vector of length >1 and typically of length ~20.")}
      if(min(cuts) < 0){stop("\n\U0001f47f The minimum age in 'cuts' should be zero.")}
      if(max(cuts) > 100){warning("\nYou're maximum age in 'cuts' is greater than 100. \nYou do not have to change this, but know that ages are top coded at 100.")}

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
        warning(paste0("\nThere are ", nrow(ph.data[dodvar < dobvar]), " rows where 'dodvar' is less than 'dobvar'. \nThese date pairs had their values set to NA and will only contribute indirectly to the life_table calculations."))
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
    if(dob_na > 0.01){warning("More than 1% of the date of birth values are missing. \nPlease check that your variable is of class Date or is of class character in the form 'YYYY-MM-DD' or 'YYYY/MM/DD'. \nDeaths with unknown dob will be distributed proportionately among deaths with known dates of birth.")}
    if(dod_na > 0.01){warning("More than 1% of the date of death values are missing. \nPlease check that your variable is of class Date or is of class character in the form 'YYYY-MM-DD' or 'YYYY/MM/DD'.")}

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

  # collapse/aggregate  ----
  if(!is.null(group_by)){
    collapse_cols <- c(group_by, "ages", "fraction")
  } else {collapse_cols <- c("ages", "fraction")}
  ph.data <- ph.data[, .SD, .SDcols = collapse_cols]
  ph.datasum <- ph.data[, list(deaths = .N, fraction = mean(fraction, na.rm = T)), by = setdiff(collapse_cols, "fraction")]
  data.table::setorderv(ph.datasum, c("ages", setdiff(collapse_cols, "ages")))

  # return the object from function ----
  #print("Note!! These aggregated deaths need to be merged with population data with the same demographics before running through the life_table() function.")
  return(ph.datasum)

}
