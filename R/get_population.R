#' Get OFM population estimates from SQL
#'
#' @description Simple front-end for pulling in standard OFM population data
#' from SQL
#'
#' @param kingco Logical vector of length 1. Identifies whether you want
#' population estimates limited to King County. Only impacts results for
#' geo_type in c('blk', blkgrp', 'lgd', 'scd', 'tract', 'zip').
#'
#' Default == TRUE.
#' @param years Numeric vector. Identifies which year(s) of data should be
#' pulled.
#'
#' Default == most recent available year.
#' @param ages Numeric vector. Identifies which age(s) should be pulled.
#'
#' Default == c(0:100), with 100 being the top coded value for 100:120.
#' @param genders Character vector of length 1 or 2. Identifies gender(s) should
#' be pulled. The acceptable values are 'f', 'female', 'm', and 'male'.
#'
#' Default == c('f', 'm').
#' @param races Character vector of length 1 to 7. Identifies which race(s) or
#' ethnicity should be pulled. The acceptable values are "aian",
#' "asian", "black", "hispanic", "multiple", "nhpi", and "white".
#'
#' Default == all the possible values.
#' @param race_type Character vector of length 1. Identifies whether to pull
#' race data with Hispanic as an ethnicity ("race") or Hispanic as a race
#' ("race_eth").
#'
#' Default == c("race_eth").
#' @param geo_type Character vector of length 1. Identifies the geographic level
#' for which you want population estimates. The acceptable values are: 'blk',
#' 'blkgrp', 'county', 'hra', 'kc', 'lgd' (WA State legislative districts),
#' 'region', 'seattle', 'scd' (school districts), 'tract', 'wa', and 'zip'.
#'
#' Default == "kc".
#' @param group_by Character vector of length 0 to 7. Identifies how you would
#' like the data 'grouped' (i.e., stratified). Valid options are limited to:
#' "years", "ages", "genders", "race", "race_eth", "fips_co", and "geo_id".
#'
#' Default == NULL, i.e., estimates are only grouped / aggregated by
#' geography.
#' @param round Logical vector of length 1. Identifies whether or not population
#' estimates should be returned as whole numbers.
#'
#' Default == FALSE. As of 02/2023.
#' @param mykey Character vector of length 1 OR a database connection. Identifies the keyring:: key that
#' can be used to access the Health & Human Services Analytic Workspace (HHSAW).
#'
#' Default == 'hhsaw'
#'
#' @param census_vintage Integer. One of 2010 or 2020. Refers to latest Census to influence the set of
#' population estimates
#'
#' Default == 2020
#'
#' @param geo_vintage One of 2010 or 2020. Refers to the
#'
#' @details Note the following geography limitations:
#'
#' -- 'county', 'lgd', 'scd', and 'zip' apply to all of WA State
#'
#' -- 'hra', 'kc', 'region', and 'seattle' apply to King County only
#'
#' -- 'blk', 'blkgrp', and 'tract' apply to King, Snohomish, and Pierce counties
#' only
#'
#' Note on geo_vintage:
#' ZIP codes and school districts (scd) are unaffected by geo_vintage.
#' ZIP codes are year specific when possible and school districts are mostly considered fixed.
#'
#' For all other geographies, the value should represent the vintage/era of the Census
#'
#' @importFrom data.table data.table copy setDT setnames setcolorder
#' @import rads.data
#' @importFrom keyring key_get key_list
#' @importFrom DBI dbConnect dbDisconnect dbGetQuery
#' @importFrom odbc odbc
#' @importFrom glue glue_sql_collapse glue_sql
#' @references \url{https://github.com/PHSKC-APDE/rads/wiki/get_population}
#' @return dataset as a data.table for further analysis/tabulation
#' @export
#'
#' @examples
#' \dontrun{
#'  a = get_population(geo_type = "region")
#'  print(a)
#' }

# get_population() ----
get_population <- function(kingco = T,
                           years = NA,
                           ages = c(0:100),
                           genders = c("f", "m"),
                           races = c("aian", "asian", "black", "hispanic", "multiple", "nhpi", "white"),
                           race_type = c("race_eth"),
                           geo_type = c("kc"),
                           group_by = NULL,
                           round = FALSE,
                           mykey = "hhsaw",
                           census_vintage = 2020,
                           geo_vintage = 'latest'){

    # Global variables used by data.table declared as NULL here to play nice with devtools::check() ----
    r_type <- short <- race <- name <- race_eth <- gender <- age <- geo_id <- pop <- geo_id_blk <- region <- hra <-
    server <- varname <- code <- label <- `.` <- cou_id <- cou_name <- vid <- lgd_id <- lgd_name <- scd_id <-
    scd_name <- geo_id_code <- NULL

    # Ensure years argument is accounted for
    if(is.null(years)) years <- NA

    # Logical for whether running on a server ----
      server <- grepl('server', tolower(Sys.info()['release']))

    # KC zips (copied from CHAT for 2019 data on 2021-05-18) ----
    # TODO: CHANGE THIS TO RADS DATA
      kczips <- rads.data::spatial_zip_city_region_scc$zip

    # KC School districts (copied from https://www5.kingcounty.gov/sdc/Metadata.aspx?Layer=schdst 2022/03/10) ----
      kcscds <- rads.data::spatial_school_districts

    # KC WA State House legislative districts (https://en.wikipedia.org/wiki/Washington_(state)_legislative_districts 2022/07/08) ----
      kclgds <- c(53001, 53005, 53011, 53030, 53031, 53032, 53033, 53034, 53036, 53037, 53039, 53041,
                  53043, 53045, 53046, 53047, 53048)

    # race/eth reference table ----
      ref.table <- data.table::copy(rads.data::population_wapop_codebook_values)
      ref.table <- ref.table[varname %in% c("r1r3", "r2r4", "r3", "r4")]
      ref.table[varname == "r1r3" | varname == "r3", name := "race"]
      ref.table[varname == "r2r4" | varname == "r4", name := "race_eth"]
      ref.table <- ref.table[, .(name, r_type = varname, value = code, label, short)]
      ref.table[r_type == "r3", r_type := "r1r3"]
      ref.table[r_type == "r4", r_type := "r2r4"]
      ref.table <- unique(ref.table)

    # check / clean / prep arguments ----
      # TODO: Rework so that it checks if the input object is already a DB connection
      # if not, check if the key exists and if it does create a db connection
      # but remember to close it after
      # check if keyring credentials exist for hhsaw ----
      trykey <- try(keyring::key_get(mykey, keyring::key_list(mykey)[['username']]), silent = T)
      if (inherits(trykey, "try-error")) stop(paste0("Your hhsaw keyring is not properly configured or you are not connected to the VPN. \n",
                                                      "Please check your VPN connection and or set your keyring and run the get_population() function again. \n",
                                                      paste0("e.g., keyring::key_set('hhsaw', username = 'ALastname@kingcounty.gov') \n"),
                                                      "When prompted, be sure to enter the same password that you use to log into to your laptop. \n",
                                                      "If you already have an hhsaw key on your keyring with a different name, you can specify it with the 'mykey = ...' argument \n"))
      rm(trykey)

      # check whether keyring credentials are correct / up to date ----
      if(server == FALSE){
        con <- try(con <- DBI::dbConnect(odbc::odbc(),
                                      driver = getOption('rads.odbc_version'),
                                      server = 'kcitazrhpasqlprp16.azds.kingcounty.gov',
                                      database = 'hhs_analytics_workspace',
                                      uid = keyring::key_list(mykey)[["username"]],
                                      pwd = keyring::key_get(mykey, keyring::key_list(mykey)[["username"]]),
                                      Encrypt = 'yes',
                                      TrustServerCertificate = 'yes',
                                      Authentication = 'ActiveDirectoryPassword'), silent = T)
        if (inherits(con, "try-error")) stop(paste0("Your hhsaw keyring is not properly configured and is likely to have an outdated password. \n",
                                                               "Please reset your keyring and run the get_population() function again. \n",
                                                               paste0("e.g., keyring::key_set('", mykey, "', username = 'ALastname@kingcounty.gov') \n"),
                                                               "When prompted, be sure to enter the same password that you use to log into to your laptop."))
      }else{
        message(paste0('Please enter the password you use for your laptop into the pop-up window. \n',
                       'Note that the pop-up may be behind your Rstudio session. \n',
                       'You will need to use your two factor authentication app to confirm your KC identity.'))
        con <- DBI::dbConnect(odbc::odbc(),
                              driver = getOption('rads.odbc_version'),
                              server = "kcitazrhpasqlprp16.azds.kingcounty.gov",
                              database = "hhs_analytics_workspace",
                              uid = keyring::key_list(mykey)[["username"]],
                              Encrypt = "yes",
                              TrustServerCertificate = "yes",
                              Authentication = "ActiveDirectoryInteractive")
      }



      # check kingco ----
        if( !is.logical(kingco) | length(kingco) != 1){
          stop(paste0("The `kingco` argument ('", paste(kingco, collapse = ', '), "') you entered is invalid. It must be a logcial vector (i.e., TRUE or FALSE) of length 1"))
        }

      # check years ----
        avail.years <- as.integer(DBI::dbGetQuery(conn = con, "SELECT DISTINCT year from [ref].[pop]")[]$year)

        if(all(length(years) == 1 & is.na(years))){
          years = max(avail.years)
          message(paste0("You did not specify a year so the most recent available year, ", max(avail.years), ", was selected for you. Available years include ", format_time(avail.years)))
          }

        if( !all(sapply(years, function(i) i == as.integer(i))) | min(years) < min(avail.years) | max(years) > max(avail.years)){
          stop(paste0("The `years` argument ('", paste(unique(years), collapse = ', '), "') you entered is invalid. It must be a vector of at least one 4 digit integer in ",
                      format_time(avail.years), " (e.g., `c(2017:2019)`)"))
          }
        years <- unique(years)

        avail.lgd.years <- setDT(DBI::dbGetQuery(conn = con, "SELECT DISTINCT year from [ref].[pop] where geo_type = 'lgd'"))
        if(geo_type == 'lgd' & min(years) < min(avail.lgd.years$year)){
          stop(paste0("The `years` argument ('", paste(unique(years), collapse = ', '), "') you entered is invalid. When geo_type == 'lgd', the minimum year is ", min(avail.lgd.years$year)))}

      # check ages ----
        # TODO allow pulling age groups
        if( !all(sapply(ages, function(i) i == as.integer(i))) || max(ages) > 100 || min(ages) < 0 ){
          stop(paste0("The `ages` argument ('", paste(unique(ages), collapse = ', '), "') you entered is invalid. It must be a vector of at least one age integer between 0 & 100 (e.g., `c(0:17, 65:100)`)"))}
        ages <- unique(ages)

      # check / clean genders ----
        if(sum(tolower(genders) %in% c("f", "female", "m", "male")) != length(genders)){stop(paste0("The `genders` argument ('", paste(genders, collapse = "','"), "') is limited to the following: c('f', 'female', 'm', 'male')"))}

        # TODO: CHange this to be toupper(substr(genders,1,1))
        genders_orig <- paste(genders, collapse = ', ')
        genders <- gsub("Female|female|f", "F", genders)
        genders <- gsub("Male|male|m", "M", genders)
        genders <- unique(genders)
        if(sum(genders %in% c("M", "F")) == 0){
          stop(paste0("The `genders` argument that you entered ('", genders_orig, "') is invalid. It must have one or two of the following values: `c('F', 'M')`"))
        }

      # check / clean races ----
        # TODO is this functionality really needed any more? I'd rather just have people pass names explicitly
        races_orig <- paste(races, collapse = ', ')
        races <- gsub(".*aian.*|.*indian.*", "aian", tolower(races))
        races <- gsub(".*_as.*|.*asian.*", "asian", tolower(races))
        races <- gsub(".*blk.*|.*black.*", "black", tolower(races))
        races <- gsub(".*hisp.*|.*latin.*", "hispanic", tolower(races))
        races <- gsub(".*mlt.*|.*mult.*", "multiple", tolower(races))
        races <- gsub(".*nhpi.*|.*pacific.*", "nhpi", tolower(races))
        races <- gsub(".*wht.*|.*white.*", "white", tolower(races))

        if(sum(races %in% c("aian", "asian", "black", "hispanic", "multiple", "nhpi", "white")) < 1){
          stop("The `races` argument (", races_orig, ") does not contain any valid races or ethnicities. Please include at least one of the following: `c('aian', 'asian', 'black', 'hispanic', 'multiple', 'nhpi', 'white')`")
          }
        races <- unique(races)

      # check race_type ----
        if(length(race_type) != 1 | !race_type %in% c("race", "race_eth") ){stop(paste0("The `race_type` argument ('", paste(race_type, collapse = "','"), "') is limited to one the following: c('race', 'race_eth')"))}

      # check geo_type ----
        if(length(geo_type) != 1 | !geo_type %in% c('kc', 'seattle', 'blk', 'blkgrp', 'county', 'hra', 'lgd', 'region', 'scd', 'tract', 'wa', 'zip')){
          stop(paste0("The `geo_type` argument (", paste(geo_type, collapse = ", "), ") contains an invalid entry. It must have one of the following values: `c('kc', 'seattle, 'blk', 'blkgrp', 'hra', 'region', 'tract', 'wa', 'zip')`"))
        }

        if(geo_type == "seattle"){seattle = 1; geo_type = 'region'} # Seattle is just one of four regions, so set to region and then subset results at end
        if(geo_type == "wa"){wastate = 1; geo_type = 'county'} # WA State is just the sum of all counties

        if(kingco == F & !geo_type %in% c('blk', 'blkgrp', 'lgd', 'tract', 'scd', 'zip')){
          stop("When 'kingco = F', permissible geo_types are limited to 'blk', 'blkgrp', 'scd', 'tract', and 'zip'.")
        }

        if(kingco == F & ! geo_type %in% c("lgd", "scd", "zip")){
          warning("When 'kingco = F', all permissible geo_types except for 'county', 'lgd', 'scd', 'wa', and 'zip' will provide estimates for King, Snohomish, and Pierce counties only.")
        }

      # check group_by ----
        if(!is.null(group_by)){
          if( sum(group_by %in% c('years', 'ages', 'genders', 'race', 'race_eth', 'fips_co', 'geo_id')) / length(group_by) != 1 ){
            stop(paste0("One of the `group_by` variables (", paste(group_by, collapse = ', '), ") is not valid. Valid options are: `c('years', 'ages', 'genders', 'race', 'race_eth', 'fips_co', 'geo_id')`"))
          }
          if( sum(unique(group_by) %in% c("race_eth", "race")) == 2 ){
            stop(paste0("The `group_by` argument can only contain `race_eth` (Hispanic as race) or `race` (Hispanic as ethnicity), not both. If you do not want to group by specific variables, type `group_by = NULL`"))
          }
          if(("race_eth" %in% group_by & race_type != "race_eth") | ("race" %in% group_by & race_type != "race") ){
            stop(paste0("If 'race' or 'race_eth' are specified in the `group_by` argument, it must match the value of `race_type`"))
          }
          group_by <- unique(group_by)
        }
          group_by_orig <- data.table::copy(group_by)

      # Top code age ----
          # will need to be changed if age groups are allowed
          if(max(ages) >= 100){
            sql_ages <- c(ages, 101:120)
          } else {sql_ages <- ages}

      # adjust group_by depending on which geo_type specified ----
          if(geo_type != "kc" & !("geo_id" %in% group_by)) {
            group_by <- unique(c("geo_id", group_by))
            group_by_orig <- unique(c("geo_id", group_by))
          }
          if(geo_type == "hra") {
              group_by <- setdiff(c("hra10", group_by), "geo_id")
              group_by_orig <- data.table::copy(group_by)
          }

      # adjust group_by for name differences ----
        # TODO: This seems wonky. Not sure why regular expressions are needed here
        if(!is.null(group_by)){
          group_by <- gsub("^race_eth$", "race_eth = r2r4", group_by)
          group_by <- gsub("^race$", "race = r1r3", group_by)
          group_by <- gsub("^years$", "year", group_by)
          group_by <- gsub("^ages$", "age", group_by)
          group_by <- gsub("^genders$", "gender", group_by)
        }

      # adjust geo_type as needed ----
          # TODO: HRA stuff is precomputed along with region (as 'reg')
        geo_type_orig <- data.table::copy(geo_type)
        if(geo_type %in% c("kc", "blkgrp", "hra", "tract", "region")){geo_type <- "blk"} # necessary because kc, blkgrp, tract, hra, region are aggregated up from blk
        if(geo_type %in% c("county")){geo_type <- "Cou"} #

    # generate SQL query ----

      if(is.null(group_by)){
        tmpselect <- glue::glue_sql_collapse("pop=sum(pop)")
      }else{
        tmpselect <- glue::glue_sql_collapse(c("pop=sum(pop)", group_by), sep = ', ')
      }
      tmpyears <- glue::glue_sql_collapse(years, sep = ', ')
      tmpgeo_type <- glue::glue_sql("{geo_type}", .con = con)
      tmpages <- glue::glue_sql_collapse(sql_ages, sep = ', ')
      tmpgenders <- glue::glue_sql_collapse(paste0("'", genders, "'"), sep = ', ')
      tmplgds <- glue::glue_sql_collapse(paste0("'", kclgds, "'"), sep = ", ")
      tmpscds <- glue::glue_sql_collapse(paste0("'", kcscds, "'"), sep = ", ")
      tmpzips <- glue::glue_sql_collapse(paste0("'", kczips, "'"), sep = ", ")

      if(race_type == "race_eth"){
        race_type_values <- glue::glue_sql_collapse(ref.table[r_type == "r2r4" & short %in% races]$value, sep = ', ')
        tmprace_type <- glue::glue_sql("r2r4 IN ({race_type_values})", .con = con)
      }else{
          race_type_values <- glue::glue_sql_collapse(ref.table[r_type == "r1r3" & short %in% races]$value, sep = ', ')
          tmprace_type <- glue::glue_sql("r1r3 IN ({race_type_values})", .con = con)
      }
      if(!is.null(group_by)){
        #TODO: What are all these slashes doing here?
        tmpgroup_by <- glue::glue_sql_collapse(gsub("\\[year]\\, |pop=sum\\(pop\\), |race_eth = |race = ", "", tmpselect), sep = ', ')
      }

      sql_query <- glue::glue_sql("SELECT {tmpselect}
                         FROM [ref].[pop]
                         WHERE year IN ({tmpyears})
                         AND geo_type IN ({tmpgeo_type})
                         AND age IN ({tmpages})
                         AND raw_gender IN ({tmpgenders})
                         AND {tmprace_type} ", .con = con)
      if(kingco == T & geo_type %in% c("blk", "blkgrp")){sql_query = glue::glue_sql("{sql_query} AND fips_co = 33 ", .con = con)}
      if(kingco == T & geo_type == "lgd"){sql_query = glue::glue_sql("{sql_query} AND geo_id IN ({tmplgds}) ", .con = con)}
      if(kingco == T & geo_type == "scd"){sql_query = glue::glue_sql("{sql_query} AND geo_id IN ({tmpscds}) ", .con = con)}
      if(kingco == T & geo_type == "zip"){sql_query = glue::glue_sql("{sql_query} AND geo_id IN ({tmpzips}) ", .con = con)}
      if(!is.null(group_by)){sql_query = glue::glue_sql("{sql_query} GROUP BY {tmpgroup_by} ORDER BY {tmpgroup_by}", .con = con)}

    # generate supplemental SQL query for Hispanic ethnicity ----
      # easiest solution is to replace r1r3 with r2r4 (Hispanic as race), then drop all non-Hispanic and append results to those from the main query
      hisp_eth_flag = F
      if(race_type == "race" & "hispanic" %in% races & "race" %in% group_by_orig){hisp_eth_flag = T}
      if(race_type == "race" & identical(races, "hispanic") & is.null(group_by_orig) ){hisp_eth_flag = T}

      if(hisp_eth_flag){sql_query_hisp_eth <- gsub("r1r3", "r2r4", sql_query)}
      if(hisp_eth_flag & is.null(group_by_orig)){
        sql_query_hisp_eth <- gsub("pop=sum\\(pop\\)", "pop=sum(pop), race = r2r4", sql_query_hisp_eth)
        sql_query_hisp_eth <- glue::glue_sql("{sql_query_hisp_eth} GROUP BY r2r4 ORDER BY r2r4")
        }

    # get population labels ----
      pop.lab <- data.table::setDT(DBI::dbGetQuery(con, "SELECT * FROM [ref].[pop_labels]"))

    # get population data ----
      pop.dt <- data.table::setDT(DBI::dbGetQuery(con, sql_query))

      # append Hispanic as race if / when needed
      # TODO: Don't double the query
      if(hisp_eth_flag){
        pop.dt.hisp_eth <- data.table::setDT(DBI::dbGetQuery(con, sql_query_hisp_eth))[race == 6]
        if(is.null(group_by_orig)){pop.dt = data.table::copy(pop.dt.hisp_eth)}else{
          pop.dt <- rbind(pop.dt, pop.dt.hisp_eth)
        }
      }

    # Tidy population data ----
      # add race labels ----
        if("race" %in% names(pop.dt)){pop.dt[, race := factor(race, levels = ref.table[name == "race"]$value, labels = ref.table[name == "race"]$label)]}
        if("race_eth" %in% names(pop.dt)){pop.dt[, race_eth := factor(race_eth, levels = ref.table[name == "race_eth"]$value, labels = ref.table[name == "race_eth"]$label)]}
        if(!"race" %in% names(pop.dt) & !"race_eth" %in% names(pop.dt)){pop.dt[, c(race_type) := paste(races, collapse = ", ")]}

      # add gender labels ----
        if("gender" %in% names(pop.dt)){pop.dt[, gender := factor(gender, levels = c(2, 1), labels = c("Female", "Male"))]}
        if(!"gender" %in% names(pop.dt)){pop.dt[, gender := paste0(gsub("F", "Female", gsub("M", "Male", genders)), collapse = ", ")]}

      # add label of years used in analysis ----
        if(!c("year") %in% group_by){pop.dt[, year := rads::format_time(years)]}

      # add label of ages used in analysis ----
        if(!c("age") %in% group_by){pop.dt[, age := rads::format_time(ages)]}

      # add geo_type ----
        pop.dt[, geo_type := geo_type_orig]

      # collapse or crosswalk geo_id if necessary ----
        # kc ----
          if(geo_type_orig == "kc"){
            pop.dt[, geo_id := "King County"]
            nonpopvars <- setdiff(names(pop.dt), "pop")
            pop.dt <- pop.dt[, .(pop = sum(pop)), by = nonpopvars]
          }

        # blk ----
          if(geo_type_orig == "blk" & is.null(group_by_orig)){pop.dt[, geo_id := "All blocks"]}

        # blkgrp ----
          if(geo_type_orig == "blkgrp" & "geo_id" %in% group_by_orig){
            pop.dt[, geo_id := substr(geo_id, 1, 12)]
            nonpopvars <- setdiff(names(pop.dt), "pop")
            pop.dt <- pop.dt[, .(pop = sum(pop)), by = nonpopvars]
          }
          if(geo_type_orig == "blkgrp" & is.null(group_by_orig)){pop.dt[, geo_id := "All block groups"]}

        # tract ----
          if(geo_type_orig == "tract" & "geo_id" %in% group_by_orig){
            pop.dt[, geo_id := substr(geo_id, 1, 11)]
            nonpopvars <- setdiff(names(pop.dt), "pop")
            pop.dt <- pop.dt[, .(pop = sum(pop)), by = nonpopvars]
          }
          if(geo_type_orig == "tract" & is.null(group_by_orig)){pop.dt[, geo_id := "All tracts"]}

        # region ----
          if(geo_type_orig == "region" & "geo_id" %in% group_by_orig){
            xwalk <- data.table::copy(rads.data::spatial_blocks10_to_hra_to_region)
            xwalk <- xwalk[, .(geo_id = as.character(geo_id_blk), region)]
            pop.dt <- merge(pop.dt, xwalk, by = "geo_id", all.x = T)
            nonpopvars <- setdiff(names(pop.dt), c("pop", "geo_id"))
            pop.dt <- pop.dt[, .(pop = sum(pop)), by = nonpopvars]
            setnames(pop.dt, "region", "geo_id")
          }
          if(geo_type_orig == "region" & is.null(group_by_orig)){pop.dt[, geo_id := "All regions"]}

        # seattle ----
          if(exists("seattle")){
            if(seattle == 1){
              pop.dt <- pop.dt[geo_id=="Seattle"]
              pop.dt[, geo_type := "seattle"]
            }
          }

        # county ----
          if(geo_type_orig == "county"){
            xwalk <- data.table::copy(rads.data::spatial_county_codes_to_names)
            xwalk <- xwalk[, .(geo_id_code = as.character(cou_id), geo_id = cou_name)]
            setnames(pop.dt, "geo_id", "geo_id_code")
            pop.dt <- merge(pop.dt, xwalk, by = "geo_id_code", all.x = T, all.y = T)
          }

        # hra ----
          if(geo_type_orig == "hra" & "hra10" %in% group_by_orig){
            xwalk <- data.table::copy(rads.data::spatial_hra_vid_region)
            xwalk <- xwalk[, .(geo_id_code = vid, geo_id = hra)]
            setnames(pop.dt, "hra10", "geo_id_code")
            pop.dt <- merge(pop.dt, xwalk, by = "geo_id_code", all.x = T, all.y = T)
          }

        # lgd ----
          if(geo_type_orig == "lgd"){
            xwalk <- data.table::copy(rads.data::spatial_legislative_codes_to_names)
            xwalk <- xwalk[, .(geo_id_code = as.character(lgd_id), geo_id = lgd_name)]
            setnames(pop.dt, "geo_id", "geo_id_code")
            pop.dt <- merge(pop.dt, xwalk, by = "geo_id_code", all.x = T, all.y = F)
          }

        # scd ----
          if(geo_type_orig == "scd"){
            xwalk <- data.table::copy(rads.data::spatial_school_codes_to_names)
            xwalk <- xwalk[, .(geo_id_code = as.character(scd_id), geo_id = scd_name)]
            setnames(pop.dt, "geo_id", "geo_id_code")
            pop.dt <- merge(pop.dt, xwalk, by = "geo_id_code", all.x = T, all.y = F)
          }

        # wa ----
          if(exists("wastate")){
            if(wastate == 1){
              pop.dt[, geo_type := "wa"]
              pop.dt[, geo_id := "Washington State"]
              pop.dt[, geo_id_code := NULL]
              nonpopvars <- setdiff(names(pop.dt), c("pop"))
              pop.dt <- pop.dt[, .(pop = sum(pop)), by = nonpopvars]
            }
          }

        # zip ----
          if(is.null(group_by)){
            if(geo_type_orig == "zip" & kingco == T ){
              pop.dt[, geo_id := "All KC zip codes"]
            }
            if(geo_type_orig == "zip" & kingco == F ){
              pop.dt[, geo_id := "All WA zip codes"]
            }
            if(geo_type_orig != "zip" & kingco == F){
              pop.dt[, geo_id := "King, Pierce, & Snohomish counties"]
            }
          }else{
            if(geo_type_orig == "zip" & kingco == F & !"geo_id" %in% group_by_orig ){
              pop.dt[, geo_id := "WA State"]
            }
          }

      # Collapse ages above 100 ----
      if(max(ages) == 100 & "ages" %in% group_by_orig){
        pop.dt[age >= 100, age := 100]
        if(race_type == "race_eth"){pop.dt <- pop.dt[, .(pop = sum(pop)), by = .(age, gender, race_eth, year, geo_type, geo_id)]}
        if(race_type == "race"){pop.dt <- pop.dt[, .(pop = sum(pop)), by = .(age, gender, race, year, geo_type, geo_id)]}
      }

      # round ----
        if(round == TRUE){pop.dt[, pop := rads::round2(pop, 0)]}

      # set column order ----
      ifelse("geo_id_code" %in% names(pop.dt),
             setcolorder(pop.dt, c("pop", "geo_type", "geo_id", "geo_id_code", "year", "age", "gender")),
             setcolorder(pop.dt, c("pop", "geo_type", "geo_id", "year", "age", "gender")) )

    # close connection ----
     DBI::dbDisconnect(con)

    return(pop.dt)
}
