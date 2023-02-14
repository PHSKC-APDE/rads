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
#' Default == 2022.
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
#' race data with Hispanic as an ethnicity ("race"), Hispanic as a race
#' ("race_eth"), or each race, including Hispanic, alone and in combination ('race_aic').
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
#' @param geo_vintage One of 2010 or 2020. Refers to the the Census that influenced the creation of the geographies. See details for notes.
#'
#' @param pop_table DBI::Id of the pop table. Can be used to change backends when `mykey` is a DB connection.
#'
#' @param return_query logical. Instead of computing the results, return the query for fetching the results
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
                           years = 2022,
                           ages = c(0:100),
                           genders = c("f", "m"),
                           races = c("aian", "asian", "black", "hispanic", "multiple", "nhpi", "white"),
                           race_type = c("race_eth"),
                           geo_type = c("kc"),
                           group_by = NULL,
                           round = FALSE,
                           mykey = "hhsaw",
                           census_vintage = 2020,
                           geo_vintage = 2020,
                           pop_table = DBI::Id(schema = 'ref', table = 'pop_f'),
                           return_query = FALSE){


  # A function to make subsets
  make_subset = function(con, var, items = NULL){
    if(is.null(items) || items[1] == 'All'){
      return(SQL(''))
    }else{
      thecol = Id(column = var)
      subme = glue_sql('{`thecol`} in ({items*})', .con = con)
      return(subme)
    }
  }

  # valid inputs
  validate_input = function(varname, vals, allowed_vals, additional = "", convert_to_all = TRUE){

    invalid = setdiff(vals, allowed_vals)
    if(length(invalid)>0){
      invalid = glue::glue_collapse(invalid, sep =', ', last = ', and')
      stop(glue::glue('The following values provided in `{varname}` are invalid: {invalid}.
                      {additional}'))


    }

    if(convert_to_all && all(allowed_vals %in% vals)) return('All')

    unique(vals)

  }


  # Validations ----

  ## Validate pop_table ----
  stopifnot(inherits(pop_table, 'Id'))
  stopifnot('`pop_table` must be specified with a schema and a table name' = all(c('schema', 'table') %in% (names(pop_table@name))))
  ## Validate key ----
  ## Key should be a character string that can be used to generate a database connection
  ## Also have to allow for the option of interactive authentication
  ## TODO: Allow mykey to be a database connection itself
  is.db = function(x){
    r = try(dbIsValid(mykey))
    if(inherits(r, 'try-error')){
      r = FALSE
    }
  }
  closeserver = TRUE
  if(is.character(mykey)){
    server <- grepl('server', tolower(Sys.info()['release']))
    trykey <- try(keyring::key_get(mykey, keyring::key_list(mykey)[['username']]), silent = T)
    if (inherits(trykey, "try-error")) stop(paste0("Your hhsaw keyring is not properly configured or you are not connected to the VPN. \n",
                                                   "Please check your VPN connection and or set your keyring and run the get_population() function again. \n",
                                                   paste0("e.g., keyring::key_set('hhsaw', username = 'ALastname@kingcounty.gov') \n"),
                                                   "When prompted, be sure to enter the same password that you use to log into to your laptop. \n",
                                                   "If you already have an hhsaw key on your keyring with a different name, you can specify it with the 'mykey = ...' argument \n"))
    rm(trykey)

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

    on.exit(DBI::dbDisconnect(con))

  }else if(is.db(mykey)){
    closeserver = FALSE
    con = mykey



  }else{
    stop('`mykey` is not a reference to database connection or keyring')
  }

  ## validate pop_table
  pt_chk = DBI::dbGetQuery(con, glue::glue_sql("
                 SELECT *
                 FROM INFORMATION_SCHEMA.TABLES
                 WHERE TABLE_SCHEMA = {pop_table@name['schema']}
                 AND  TABLE_NAME = {pop_table@name['table']}", .con = con))
  if(nrow(pt_chk) != 1) stop('`pop_table` does not exist in the supplied database and/or some how refers to multiple tables.')

  ## validate census_vintage ----
  census_vintage = validate_input('census_vintage', census_vintage, c(2010, 2020))
  where_census_vintage = glue::glue_sql('census_year = {census_vintage}', .con = con)

  ## validate geo_type and kingco ----
  ## if seattle, pull region where seattle
  valid_geogs = c('blk', 'blkgrp', 'tract', 'county', 'hra', 'kc', 'lgd',
                  'region', 'seattle', 'scd' , 'tract', 'wa', 'zip')
  geo_type = match.arg(geo_type, valid_geogs)
  ## create geo_type selectors and filters ----
  ## TODO: should we recompute blkgrp and tract? It'll be a lot faster
  if(geo_type %in% c('blkgrp')){
    where_geo_type = SQL("geo_type = 'blk'")
    group_geo_type = SQL('SUBSTRING(geo_id,1,12)')
    select_geo_type = glue::glue_sql('{group_geo_type} as geo_id', .con = con)
  }else if(geo_type == 'tract'){
    where_geo_type = SQL("geo_type = 'blk'")
    group_geo_type = SQL('SUBSTRING(geo_id,1,11)')
    select_geo_type = glue::glue_sql('{group_geo_type} as geo_id', .con = con)
  }else if(geo_type == 'seattle'){
    where_geo_type = SQL("geo_type = 'reg' AND geo_id = 3")
    group_geo_type = SQL('')
    select_geo_type = DBI::Id(column = 'geo_id')
  }else if(geo_type == 'wa'){
    where_geo_type = SQL("geo_type = 'cou'")
    group_geo_type = SQL('') # over the whole state
    select_geo_type = glue::glue_sql('53 as geo_id', .con = con)
  }else if(geo_type == 'kc'){
    where_geo_type = SQL("geo_type = 'cou' AND geo_id = '53033'")
    group_geo_type = SQL('') # over the whole county
    select_geo_type = DBI::Id(column = 'geo_id')
  }else{
    where_geo_type = glue_sql("geo_type = {substr(geo_type, 1,3)}", .con = con)
    group_geo_type = SQL('geo_id') # over the whole state
    select_geo_type = DBI::Id(column = 'geo_id')
  }

  ## validate kingco
  ## TODO: THIS should be precomputed/in ref.pop already
  kingco = validate_input('kingco', kingco, c(TRUE, FALSE))
  subset_by_kingco = SQL('')
  if(kingco && geo_type %in% c('blk', 'blkgrp', 'tract', 'cou')){
    subset_by_kingco = SQL("SUBSTRING(geo_id,1,5) = '53033'")
  }else if(kingco && geo_type == 'zip'){
    # TODO: THIS IS A CHANGE FROM PAST PRACTICE
    subset_by_kingco = make_subset(con, 'geo_id', as.character(rads.data::spatial_zip_city_region_scc$zip))
  }else if(kingco && geo_type == 'scd'){
    subset_by_kingco = make_subset(con, 'geo_id', as.character(rads.data::spatial_school_dist_to_region$geo_id))
  }else if(kingco && geo_type == 'lgd'){
    subset_by_kingco = make_subset(con, 'geo_id', as.character(rads.data::spatial_legislative_codes_to_names[grep('King', lgd_counties), lgd_id]))
  }

  ## validate geo_vintage ----
  geo_vintage = validate_input('geo_vintage', geo_vintage, c(2010, 2020))
  where_geo_vintage = glue_sql('geo_year >= {geo_vintage} AND geo_year<= {geo_vintage + 9}')
  # geo_vintage is not relevant for ZIPs and school districts
  if(geo_type %in% c('zip', 'scd')){
    where_geo_vintage = SQL('')
  }

  ## validate years ----
  ## integer year between 2000 and 2022
  ## TODO: This is a bottleneck. Fix it.
  # year_q = glue::glue_sql('select max(year) as maxyear from {`pop_table`}
  #                           where {where_geo_type} AND {where_census_vintage}', .con  = con)
  # year_r = dbGetQuery(con, year_q)
  # if(all(is.na(years)) || is.null(years)){
  #   years = year_r$maxyear
  # }
  years = validate_input('years', years, seq(2000, 2022))


  ## validate age ----
  ## TODO allow age groups-- ideally with database side computation
  ## it'd probably be a bunch of case_whens. OR at least the
  ## list could be turned into a bunch of clever case_whens
  ## Integers between 0 and 100. 100 gets expanded to include 105 and 110 which
  ## ofm/frankenpop uses to store 100+ stuff
  ages = validate_input('ages', ages, 0:100)
  if(max(ages)==100){
    ages = c(ages, 105,110)
  }
  ages = unique(ages)


  ## validate gender ----
  ## one of m/f.
  stopifnot('`genders` must be a character vector' = is.character(genders))
  genders = unique(toupper(substr(genders, 1,1)))
  if(!all(genders %in% c('M', 'F'))){
    stop('`genders` should be one or both of "M" and "F".
            At present, genders outside the binary do not have population estimates.')
  }
  genders = which(c('M', 'F') %in% genders)
  genders = validate_input('genders', genders, 1:2)

  ## validate races ----
  races = tolower(races)
  races = validate_input('races', races, c("aian", "asian", "black", "hispanic", "multiple", "nhpi", "white"))
  race_col = 'r2r4'
  if(race_type == 'race') race_col = 'r1r3'

  ### convert to the numeric codes used in the ref table
  ref.table <- data.table::copy(rads.data::population_wapop_codebook_values)
  ref.table <- ref.table[varname %in% c("r1r3", "r2r4", "r3", "r4")]
  ref.table[varname == "r1r3" | varname == "r3", name := "race"]
  ref.table[varname == "r2r4" | varname == "r4", name := "race_eth"]
  ref.table <- ref.table[, .(name, r_type = varname, value = code, label, short)]
  ref.table[r_type == "r3", r_type := "r1r3"]
  ref.table[r_type == "r4", r_type := "r2r4"]
  ref.table <- unique(ref.table)
  if(!all(races == 'All')) races = ref.table[r_type == race_col & short %in% races, value]


  ### TODO: With AIC pop, this will need to be be changed



  ## validate race_type ----
  race_type = match.arg(race_type, c('race', 'race_eth', 'race_aic'))

  ## validate group_by ----
  group_by = validate_input('group_by', group_by, c("years", "ages", "genders", "race", "race_eth", "geo_id"), convert_to_all = FALSE)

  # only one of race or race_eth is allowed
  if(all(c('race', 'race_eth') %in% group_by)) stop('Only one of race or race_eth can be in `group_by`')

  ## validate round ----
  stopifnot(length(round)  == 1 && is.logical(round))

  # Generate query parts ----
  ## output columns: c("pop", "geo_type", "geo_id", "year", "age", "gender", "race_eth")
  ## if race_type == race, then the column name is race
  ## TODO: race will need to be a bit more complicatd here with race_aic

  cols = data.table(coltype = c("years", "ages", "genders", race_type, "geo_id"),
                    colname = c('year', 'age_100', 'gender', race_col, 'geo_id'))

  if(race_type == 'race_aic') cols = cols[coltype != race_type]

  ## Identify groups ----
  grp_cols = cols[coltype %in% group_by, colname]
  grp_cols = setdiff(grp_cols, 'All')
  grp_cols_sql = SQL('')

  ### if groups are requested, create the relevant sql ----
  if(length(grp_cols)>0){

    #### geo_id is custom ----
    grp_cols_sql = lapply(setdiff(grp_cols, 'geo_id'), function(x){
        DBI::Id(column = x)
    })
    names(grp_cols_sql) = grp_cols

    if(any(grp_cols %in% 'geo_id')){
      grp_cols_sql[['geo_id']] <- group_geo_type
    }


    if(length(grp_cols_sql)==1) grp_cols_sql = grp_cols_sql[[1]]
    group_vars = glue_sql('GROUP BY {`grp_cols_sql`*}', .con = con)
  }else{
    group_vars = DBI::SQL('')
    # compute_pop = DBI::SQL('pop')
  }
  compute_pop = DBI::SQL('SUM(pop) as pop')

  ## select clause ----
  ### custom selection for geo_type ----
  if(any(grp_cols %in% 'geo_id')){
    grp_cols_sql[['geo_id']] <- select_geo_type
  }
  ### create select_me ----

  if(length(grp_cols) == 0){
    select_me = compute_pop
  }else{
    select_me = glue_sql_collapse(c(compute_pop, glue_sql('{`grp_cols_sql`*}', .con = con)), sep = ',')
  }

  ## Subset clauses ----
  ### create clauses ----
  subset_by_age = make_subset(con, 'age_100', ages)
  subset_by_year = make_subset(con, 'year', years)
  subset_by_gender = make_subset(con, 'gender', genders)
  subset_by_raceeth = make_subset(con, race_col, races)

  ### narrow down ----
  subs = c(where_geo_type,
           where_geo_vintage,
           where_census_vintage,
           subset_by_year,
           subset_by_age,
           subset_by_gender,
           subset_by_raceeth,
           subset_by_kingco)
  subs = subs[subs != SQL('')]
  subset_me = glue_sql_collapse(subs, sep = ' AND ')
  if(!subset_me == '') subset_me = glue_sql('where {subset_me}', .con = con)


  # Assemble query ----
  ## Query for race/eth ----
  q = glue::glue_sql(
    '
    select
    {select_me}
    from {`pop_table`} as p
    {subset_me}
     {group_vars}', .con = con
  )

  if(return_query) return(q)

  r = DBI::dbGetQuery(con, q)
  setDT(r)

  # add the columns
  if('age_100' %in% names(r)) setnames(r,'age_100', 'age')
  if(race_col %in% names(r)) setnames(r, race_col, race_type)



  return(r)

  ## Query for Hispanic/AIC ----

  # Add labels ----

  # collapse/top code age ----

  # round ----

  # set column

  # close connection ----
  # its done with on exit
  # if(closeserve) DBI::dbDisconnect(con)
  #
  # # return ----
  # return(pop.dt)
}
