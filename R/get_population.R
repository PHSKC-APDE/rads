#' Get OFM population estimates from SQL
#'
#' @description Simple front-end for pulling in standard population data
#'
#' @param kingco Logical vector of length 1. Identifies whether you want population estimates limited to King County.
#' @param years Numeric vector. Identifies which year(s) of data should be pulled
#' @param ages Numeric vector. Identifies which age(s) should be pulled
#' @param genders Character vector of length 1 or 2.Identifies gender(s) should be pulled.
#' @param races Character vector of length 1 to 7. Identifies which race(s) or ethnicity should be pulled
#' @param race_type Character vector of length 1. Identifies whether to pull race data with Hispanic as an ethnicity ("race") or Hispanic as a race (“race_eth”)
#' @param geo_type Character vector of length 1. Identifies the geographic level for which you want population estimates
#' @param group_by Character vector of length 0 to 7. Identifies how you would like the data 'grouped' (i.e., stratified)
#' @param round Logical vector of length 1. Identifies whether or not population estimates should be returned as whole numbers
#' @importFrom data.table data.table data.table fread setDT setnames setcolorder
#' @importFrom keyring key_get key_list
#' @importFrom DBI dbConnect dbDisconnect dbGetQuery
#' @importFrom odbc odbc
#'
#' @return dataset as a data.table for further analysis/tabulation
#' @export
#'
#' @examples
#' \dontrun{
#'  get_population(geo_type = "region", group_by = c("geo_id"))[]
#' }

# get_population() ----
get_population <- function(kingco = T,
                           years = c(2019),
                           ages = c(0:120),
                           genders = c("F", "M"),
                           races = c("aian", "asian", "black", "hispanic", "multiple", "nhpi", "white"),
                           race_type = c("race_eth"),
                           geo_type = c("kc"),
                           group_by = NULL,
                           round = T,
                           mykey = "hhsaw"){

      #global variables used by data.table declared as NULL here to play nice with devtools::check()
      r_type <- short <- race <- name <- race_eth <- gender <- age <- geo_id <- pop <- geo_id_blk <- region <- hra <- `.` <- NULL

    # KC zips (copied from CHAT for 2019 data on 2021-05-18) ----
      kczips <- c(98001, 98002, 98003, 98004, 98005, 98006, 98007, 98008, 98009, 98010, 98011, 98013, 98014, 98015, 98019, 98022,
                  98023, 98024, 98025, 98027, 98028, 98029, 98030, 98031, 98032, 98033, 98034, 98035, 98038, 98039, 98040, 98041,
                  98042, 98045, 98047, 98050, 98051, 98052, 98053, 98054, 98055, 98056, 98057, 98058, 98059, 98062, 98063, 98064,
                  98065, 98070, 98071, 98072, 98073, 98074, 98075, 98077, 98083, 98089, 98092, 98093, 98101, 98102, 98103, 98104,
                  98105, 98106, 98107, 98108, 98109, 98111, 98112, 98113, 98114, 98115, 98116, 98117, 98118, 98119, 98121, 98122,
                  98124, 98125, 98126, 98127, 98129, 98130, 98131, 98132, 98133, 98134, 98136, 98138, 98139, 98140, 98141, 98144,
                  98145, 98146, 98148, 98151, 98154, 98155, 98158, 98160, 98161, 98164, 98165, 98166, 98168, 98170, 98171, 98174,
                  98175, 98177, 98178, 98181, 98184, 98185, 98188, 98189, 98190, 98191, 98194, 98195, 98198, 98199, 98224, 98288)

    # race/eth reference table ----
      ref.table <- rbind(
        data.table(name  = rep("race", 7),
                   r_type = rep("r1_3", 7),
                   value = c("1", "2", "3", "7", "8", "5", "6"),
                   label = c("White", "Black or African American", "Native American/Alaska Native", "Asian", "Native Hawaiian/Pacific Islander", "Multiple", "Hispanic Ethnicity"),
                   short = c('white', 'black', 'aian', 'asian', 'nhpi', 'multiple', 'hispanic')),
        data.table(name  = rep("race_eth", 7),
                   r_type = rep("r2_4", 7),
                   value = c("1", "2", "3", "7", "8", "5", "6"),
                   label = c("White, non-Hispanic", "Black, non-Hispanic", "American Indian/Alaska Native, non-Hispanic", "Asian, non-Hispanic", "Native Hawaiian/Pacific Islander, non-Hispanic", "Multiple, non-Hispanic", "Hispanic Ethnicity"),
                   short = c('white', 'black', 'aian', 'asian', 'nhpi', 'multiple', 'hispanic'))
      )

    # check / clean / prep arguments ----
      # check if keyring credentials exist for hhsaw ----
      trykey <- try(keyring::key_get(mykey, keyring::key_list(mykey)[['username']]), silent = T)
      if (inherits(trykey, "try-error")) stop(paste0("Your hhsaw keyring is not properly configured or you are not connected to the VPN. \n",
                                                      "Please check your VPN connection and or set your keyring and run the get_population() function again. \n",
                                                      paste0("e.g., keyring::key_set('hhsaw', username = 'ALastname@kingcounty.gov') \n"),
                                                      "When prompted, be sure to enter the same password that you use to log into to your laptop. \n",
                                                      "If you already have an hhsaw key on your keyring with a different name, you can specify it with the 'mykey = ...' argument \n"))
      rm(trykey)

      # check whether keyring credentials are correct / up to date ----
      trykey <- try(DBI::dbConnect(odbc::odbc(),
                                    driver ='ODBC Driver 17 for SQL Server',
                                    server = 'kcitazrhpasqlprp16.azds.kingcounty.gov',
                                    database = 'hhs_analytics_workspace',
                                    uid = keyring::key_list(mykey)[["username"]],
                                    pwd = keyring::key_get(mykey, keyring::key_list(mykey)[["username"]]),
                                    Encrypt = 'yes',
                                    TrustServerCertificate = 'yes',
                                    Authentication = 'ActiveDirectoryPassword'), silent = T)
      if (inherits(trykey, "try-error")) stop(paste0("Your hhsaw keyring is not properly configured and is likely to have an outdated password. \n",
                                                             "Please reset your keyring and run the get_population() function again. \n",
                                                             paste0("e.g., keyring::key_set('", mykey, "', username = 'ALastname@kingcounty.gov') \n"),
                                                             "When prompted, be sure to enter the same password that you use to log into to your laptop."))

      # check kingco ----
      if( !is.logical(kingco) | length(kingco) != 1){
        stop(paste0("The `kingco` argument ('", paste(kingco, collapse = ', '), "') you entered is invalid. It must be a logcial vector (i.e., TRUE or FALSE) of length 1"))
      }

      # check years ----
        if( !all(sapply(years, function(i) i == as.integer(i))) | !sum(years)/length(years) > 2010 | min(years) < 2010){
          stop(paste0("The `years` argument ('", paste(unique(years), collapse = ', '), "') you entered is invalid. It must be a vector of at least one 4 digit integer > 2010 (e.g., `c(2017:2019)`)"))
          }
        years <- unique(years)

      # check ages ----
        if( !all(sapply(ages, function(i) i == as.integer(i))) | max(ages) > 120 | min(ages) < 0 ){
          stop(paste0("The `ages` argument ('", paste(unique(ages), collapse = ', '), "') you entered is invalid. It must be a vector of at least one age integer between 0 & 120 (e.g., `c(0:17, 65:120)`)"))}
        ages <- unique(ages)

      # check / clean genders ----
        if(sum(tolower(genders) %in% c("f", "female", "m", "male")) != length(genders)){stop(paste0("The `genders` argument ('", paste(genders, collapse = "','"), "') is limited to the following: c('f', 'female', 'm', 'male')"))}

        genders_orig <- paste(genders, collapse = ', ')
        genders <- gsub("Female|female|f", "F", genders)
        genders <- gsub("Male|male|m", "M", genders)
        genders <- unique(genders)
        if(sum(genders %in% c("M", "F")) == 0){
          stop(paste0("The `genders` argument that you entered ('", genders_orig, "') is invalid. It must have one or two of the following values: `c('F', 'M')`"))
        }

      # check / clean races ----
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
        if(length(geo_type) != 1 | !geo_type %in% c('kc', 'blk', 'blkgrp', 'hra', 'region', 'tract', 'zip')){
          stop(paste0("The `geo_type` argument (", paste(geo_type, collapse = ", "), ") contains an invalid entry. It must have one of the following values: `c('kc', 'blk', 'blkgrp', 'hra', 'region', 'tract', 'zip')`"))
        }

        if(kingco == F & !geo_type %in% c('blk', 'blkgrp', 'tract', 'zip')){
          stop("When 'kingco = F', permissible geo_types are limited to 'blk', 'blkgrp', 'tract', and 'zip'.")
        }

        if(kingco == F & geo_type != "zip"){
          warning("When 'kingco = F', all permissible geo_types except for 'zip' will provide estimates for King, Snohomish, and Pierce counties only.")
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
          group_by_orig <- copy(group_by)

      # adjust group_by for name differences ----
        if(!is.null(group_by)){
          group_by <- gsub("^race_eth$", "race_eth = r2_4", group_by)
          group_by <- gsub("^race$", "race = r1_3", group_by)
          group_by <- gsub("^years$", "year", group_by)
          group_by <- gsub("^ages$", "age", group_by)
          group_by <- gsub("^genders$", "gender", group_by)
        }

      # adjust geo_type as needed ----
        geo_type_orig <- copy(geo_type)
        if(geo_type %in% c("kc", "blkgrp", "hra", "tract", "region")){geo_type <- "blk"} # necessary because kc, blkgrp, tract, hra, region are aggregated up from blk

    # generate SQL query ----
      sql_select <- "pop=sum(pop)"
      if(!is.null(group_by)){sql_select <- paste0(sql_select, ", ", paste(group_by, collapse = ", "))}

      sql_where <- c("")
      sql_where <- paste0(sql_where, "year IN (", paste(years, collapse = ", "), ") ")
      sql_where <- paste0(sql_where, "AND geo_type IN ('", paste(geo_type, collapse = "', '"), "') ")
      sql_where <- paste0(sql_where, " AND age IN (", paste(ages, collapse = ", "), ") ")
      sql_where <- paste0(sql_where, " AND raw_gender IN ('", paste(genders, collapse = "', '"), "') ")
      if(race_type == "race_eth"){sql_where <- paste0(sql_where, " AND r2_4 IN (", paste(ref.table[r_type == "r2_4" & short %in% races]$value, collapse = ", "), ")" )}
      if(race_type == "race"){sql_where <- paste0(sql_where, " AND r1_3 IN (", paste(ref.table[r_type == "r1_3" & short %in% races]$value, collapse = ", "), ") ")}
      if(kingco == T & geo_type %in% c("blk", "blkgrp")){sql_where = paste0(sql_where, "AND fips_co = 33")}
      if(kingco == T & geo_type == "zip"){sql_where = paste0(sql_where, " AND geo_id IN (", paste(kczips, collapse = ", "), ")")}

      sql_group <- paste("GROUP BY",  gsub("\\[year]\\, |pop=sum\\(pop\\), |race_eth = |race = ", "", sql_select))

      sql_order <- gsub("GROUP", "ORDER", sql_group)

      if(!is.null(group_by)){sql_query <- paste("SELECT", sql_select, "FROM [ref].[pop] WHERE", sql_where, sql_group, sql_order)}
      if( is.null(group_by)){sql_query <- paste("SELECT", sql_select, "FROM [ref].[pop] WHERE", sql_where)}

    # open hhsaw connection ----
      con <- DBI::dbConnect(odbc::odbc(),
                            driver ='ODBC Driver 17 for SQL Server',
                            server = 'kcitazrhpasqlprp16.azds.kingcounty.gov',
                            database = 'hhs_analytics_workspace',
                            uid = keyring::key_list(mykey)[["username"]],
                            pwd = keyring::key_get(mykey, keyring::key_list(mykey)[["username"]]),
                            Encrypt = 'yes',
                            TrustServerCertificate = 'yes',
                            Authentication = 'ActiveDirectoryPassword')

    # get population labels ----
      pop.lab <- data.table::setDT(DBI::dbGetQuery(con, "SELECT * FROM [ref].[pop_labels]"))

    # get population data ----
      pop.dt <- data.table::setDT(DBI::dbGetQuery(con, sql_query))

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
            xwalk <- fread("https://raw.githubusercontent.com/PHSKC-APDE/reference-data/master/spatial_data/blocks10_to_region.csv")
            xwalk <- xwalk[, .(geo_id = as.character(geo_id_blk), region)]
            pop.dt <- merge(pop.dt, xwalk, by = "geo_id", all.x = T)
            nonpopvars <- setdiff(names(pop.dt), c("pop", "geo_id"))
            pop.dt <- pop.dt[, .(pop = sum(pop)), by = nonpopvars]
            setnames(pop.dt, "region", "geo_id")
          }
          if(geo_type_orig == "region" & is.null(group_by_orig)){pop.dt[, geo_id := "All regions"]}

        # hra ----
          if(geo_type_orig == "hra" & "geo_id" %in% group_by_orig){
            xwalk <- fread("https://raw.githubusercontent.com/PHSKC-APDE/reference-data/master/spatial_data/blocks10_to_region.csv")
            xwalk <- xwalk[, .(geo_id = as.character(geo_id_blk), hra)]
            pop.dt <- merge(pop.dt, xwalk, by = "geo_id", all.x = T)
            nonpopvars <- setdiff(names(pop.dt), c("pop", "geo_id"))
            pop.dt <- pop.dt[, .(pop = sum(pop)), by = nonpopvars]
            setnames(pop.dt, "hra", "geo_id")
          }
          if(geo_type_orig == "hra" & is.null(group_by_orig)){pop.dt[, geo_id := "All HRAs"]}

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

      # round ----
        if(round == T){pop.dt[, pop := rads::round2(pop, 0)]}

      # set column order ----
      setcolorder(pop.dt, c("pop", "geo_type", "geo_id", "year", "age", "gender"))

    # close connection ----
     DBI::dbDisconnect(con)

    return(pop.dt)
}

