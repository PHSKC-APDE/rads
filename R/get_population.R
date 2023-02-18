#' Get standard population estimates from SQL
#'
#' @description Simple front-end for pulling in standard population data
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
#' @param group_by Character vector. Identifies how you would
#' like the data 'grouped' (i.e., stratified). Valid options are limited to:
#' "years", "ages", "genders", "race", "race_eth", or "race_aic".
#' If "race" or "race_aic" are the `race_type` then they must be included in
#' `group_by`. Results are always grouped by geo_id.
#'
#' Default == NULL, i.e., estimates are only grouped / aggregated by
#' geography.
#' @param round Logical vector of length 1. Identifies whether or not population
#' estimates should be returned as whole numbers.
#'
#' Default == FALSE. As of 02/2023.
#' @param mykey Character vector of length 1 OR a database connection. Identifies
#' the keyring:: key that can be used to access the Health & Human Services
#' Analytic Workspace (HHSAW).
#'
#' Default == 'hhsaw'
#'
#' @param census_vintage Integer. One of 2010 or 2020. Refers to latest Census
#' to influence the set of population estimates
#'
#' Default == 2020
#'
#' @param geo_vintage Integer. One of 2010 or 2020. Refers to the the Census
#' that influenced the creation of the geographies. See details for notes.
#'
#' Default == same as `census_vintage`
#'
#' @param schema character. Name of the schema in the db where pop data is stored.
#' \emph{Unless you know what you are doing, do not change the default!}
#'
#' Default = 'ref'
#'
#' @param table_prefix character. Prefix of the tables in `schema` where pop data
#' is stored. The table will be selected as {schema}.pop_geo_{geo_type} unless
#' geo_type is aggregated on the fly from blocks. \emph{Unless you know what you
#' are doing, do not change the default!}
#'
#' Default = 'pop_geo_'
#'
#' @param return_query logical. Instead of computing the results, return the
#' query for fetching the results
#'
#' Default == FALSE
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
#' @importFrom stats setNames
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
                           geo_vintage = 2010,
                           schema = 'ref',
                           table_prefix = 'pop_geo_',
                           return_query = FALSE){

  # visible bindings ----
  . <- age <-  code <- colname <- coltype <- cou_id <- cou_name <- gender <-  geo_id <- geo_id_code <- NULL
  hra <- label <- lgd_counties<-  lgd_id<-  lgd_name<-  max_year<-  pop <- region <- region_id<- NULL
  scd_id <- scd_name <- setNames <- short<-  sql_col <- value<-  varname<-  vid <- NULL

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
    pop_table = DBI::Id(schema = schema, table = paste0(table_prefix, 'blk'))
    where_geo_type = SQL("")
    group_geo_type = SQL('LEFT(geo_id,12)')
    select_geo_type = glue::glue_sql('{group_geo_type} as geo_id', .con = con)
  }else if(geo_type == 'tract'){
    pop_table = DBI::Id(schema = schema, table = paste0(table_prefix, 'blk'))
    where_geo_type = SQL("")
    group_geo_type = SQL('LEFT(geo_id,11)')
    select_geo_type = glue::glue_sql('{group_geo_type} as geo_id', .con = con)
  }else if(geo_type == 'seattle'){
    pop_table = DBI::Id(schema = schema, table = paste0(table_prefix, 'reg'))
    where_geo_type = SQL("geo_type = 'reg' AND geo_id = 3")
    group_geo_type = DBI::Id(column = 'geo_id')
    select_geo_type = DBI::Id(column = 'geo_id')
  }else if(geo_type == 'wa'){
    pop_table = DBI::Id(schema = schema, table = paste0(table_prefix, 'cou'))
    where_geo_type = SQL("")
    group_geo_type = SQL('') # over the whole state
    select_geo_type = glue::glue_sql('53 as geo_id', .con = con)
  }else if(geo_type == 'kc'){
    pop_table = DBI::Id(schema = schema, table = paste0(table_prefix, 'cou'))
    where_geo_type = SQL("geo_type = 'cou' AND geo_id = '53033'")
    group_geo_type = DBI::Id(column = 'geo_id')
    select_geo_type = DBI::Id(column = 'geo_id')
  }else{
    pop_table = DBI::Id(schema = schema, table = paste0(table_prefix, tolower(substring(geo_type,1,3))))
    where_geo_type = SQL('')
    group_geo_type = SQL('geo_id') # over the whole state
    select_geo_type = DBI::Id(column = 'geo_id')
  }

  ## Validate pop_table ----
  stopifnot(inherits(pop_table, 'Id'))
  stopifnot('`pop_table` must be specified with a schema and a table name' = all(c('schema', 'table') %in% (names(pop_table@name))))
  pt_chk = DBI::dbGetQuery(con, glue::glue_sql("
                 SELECT *
                 FROM INFORMATION_SCHEMA.TABLES
                 WHERE TABLE_SCHEMA = {pop_table@name['schema']}
                 AND  TABLE_NAME = {pop_table@name['table']}", .con = con))
  if(nrow(pt_chk) != 1) stop('`pop_table` does not exist in the supplied database and/or some how refers to multiple tables. Check the schema and table_prefix argument')


  ## validate kingco ----
  ## TODO: THIS should be precomputed/in ref.pop already
  kingco = validate_input('kingco', kingco, c(TRUE, FALSE))
  subset_by_kingco = SQL('')
  if(kingco && geo_type %in% c('blk', 'blkgrp', 'tract')){
    subset_by_kingco = SQL("fips_co = '33'")
  }else if(kingco && geo_type == 'zip'){
    # TODO: THIS IS A CHANGE FROM PAST PRACTICE
    subset_by_kingco = make_subset(con, 'geo_id', as.character(rads.data::spatial_zip_city_region_scc$zip))
  }else if(kingco && geo_type == 'scd'){
    subset_by_kingco = make_subset(con, 'geo_id', as.character(rads.data::spatial_school_dist_to_region$geo_id))
  }else if(kingco && geo_type == 'lgd'){
    subset_by_kingco = make_subset(con, 'geo_id', as.character(rads.data::spatial_legislative_codes_to_names[grep('King', lgd_counties), lgd_id]))
  }
  # Per a conversation with Danny, if a user is asking for county and not kc they want all counties
  # else if(kingco && tolower(substr(geo_type,1,3)) %in% c('cou')){
  #   subset_by_kingco = SQL("geo_id = '53033'")
  # }

  ## validate geo_vintage ----
  geo_vintage = validate_input('geo_vintage', geo_vintage, c(2010, 2020))

  ## FIXME: A temporary fix for hras until the new ones get released
  if(geo_vintage == 2020 & geo_type == 'hra'){
    warning('2020 geo_vintage HRAs are not available yet. If you are getting this message April 2023 try reinstalling rads. geo_vintage changed to 2010')
    geo_vintage = 2010
  }
  if(geo_vintage == 2020 & geo_type == 'region'){
    stop('2020 geo_vintage region are not available yet. If you are getting this message April 2023 try reinstalling rads, geo_vintage changed to 2010')
    geo_vintage = 2010

  }
  # TODO: REVISIT THIS
  if(geo_vintage == 2010 & census_vintage == 2020 & geo_type %in% c('kc','county', 'wa')){
    geo_vintage = 2020
    warning('geo_vintage changed from 2010 to 2020 since the WA county boundaries did not change between 2010 and 2020')
  }

  where_geo_vintage = glue_sql('geo_year >= {geo_vintage} AND geo_year<= {geo_vintage + 9}')
  # geo_vintage is not relevant for ZIPs and school districts
  if(geo_type %in% c('zip', 'scd')){
    where_geo_vintage = SQL('')
  }



  ## validate years ----
  ## integer year between 2000 and 2022
  gt = substr(pop_table@name['table'],9,nchar(pop_table@name['table']))
  find_years_where = c(
    DBI::SQL('r_type = 97'),
    DBI::SQL('load_ref_datetime is not null'),
    DBI::SQL('delete_ref_datetime is null'),
    where_census_vintage,
    where_geo_vintage,
    glue::glue_sql('geo_type = {gt}', .con = con)
  )
  find_years_where = find_years_where[!sapply(find_years_where, function(x) x == DBI::SQL(''))]
  find_years_where = glue::glue_sql_collapse(find_years_where, sep = ' AND ')
  year_q = glue::glue_sql('select max(year) as maxyear from
                          ref.pop_metadata_etl_log
                          where {find_years_where}',.con  = con)
  year_r = dbGetQuery(con, year_q)
  if(all(is.na(years)) || is.null(years)){
    years = as.numeric(year_r$maxyear)
  }
  if(is.na(year_r$maxyear)) stop('No data for the combination of geo_type, year, geo_vintage, and census_vintage available')
  years = validate_input('years', years, seq(2000, year_r$maxyear))


  ## validate ages ----
  ## TODO allow age groups-- ideally with database side computation
  ## it'd probably be a bunch of case_whens. OR at least the
  ## list could be turned into a bunch of clever case_whens
  ## Integers between 0 and 100. 100 gets expanded to include 105 and 110 which
  ## ofm/frankenpop uses to store 100+ stuff
  ages = validate_input('ages', ages, 0:100)
  ages = unique(ages)


  ## validate genders ----
  ## one of m/f.
  stopifnot('`genders` must be a character vector' = is.character(genders))
  genders = unique(toupper(substr(genders, 1,1)))
  if(!all(genders %in% c('M', 'F'))){
    stop('`genders` should be one or both of "M" and "F".
            At present, genders outside the binary do not have population estimates.')
  }
  genders = which(c('M', 'F') %in% genders)
  genders = validate_input('genders', genders, 1:2)

  ## validate race_type ----
  race_type = match.arg(race_type, c('race', 'race_eth', 'race_aic'))

  ## validate races ----
  races = tolower(races)
  races = validate_input('races', races, c("aian", "asian", "black", "hispanic", "multiple", "nhpi", "white"))
  race_col = 'r2r4'
  if(race_type == 'race') race_col = 'r1r3'

  ### convert to the numeric codes used in the ref table
  ref.table <- data.table::copy(rads.data::population_wapop_codebook_values)
  ref.table <- ref.table[varname %in% c("r1r3", "r2r4", "r3", "r4")]
  ref.table <- unique(ref.table[, .(value = code, label, short)])
  colnames = data.frame(short = c("aian", "asian", "black", "hispanic", "multiple", "nhpi", "white"),
                        sql_col = c('race_aian', 'race_as', 'race_blk', 'race_hisp', NA, 'race_nhpi', 'race_wht'))
  stopifnot(nrow(ref.table) == 7)
  ref.table = merge(ref.table, colnames, all.x = T, by = 'short')
  if(!all(races == 'All')) races = ref.table[ short %in% races, value]

  ### race_aic is handled below

  ## validate group_by ----
  group_by = validate_input('group_by', group_by, c("years", "ages", "genders", "race", "race_eth", 'race_aic', 'geo_id'), convert_to_all = FALSE)
  group_by = unique(c('geo_id', group_by))

  ## race_type sanity checking ----
  ## confirm only one of race, race_eth, or race_aic is in the list
  if(all(c('race', 'race_eth', 'race_aic') %in% group_by)) stop('Only one of race, race_eth, or race_aic can be in `group_by`')
  ## race and race_aic must be in group by
  if(any(c('race', 'race_aic') %in% race_type)){
    if(!race_type %in% group_by){
      stop(paste('When `race_type` is', race_type, 'it must be included within group_by'))
    }
  }

  ## validate round ----
  stopifnot(length(round)  == 1 && is.logical(round))

  # Generate query parts ----
  ## output columns: c("pop", "geo_type", "geo_id", "year", "age", "gender", "race_eth")
  ## if race_type == race, then the column name is race
  ## TODO: race will need to be a bit more complicatd here with race_aic

  cols = data.table(coltype = c("years", "ages", "genders", race_type, "geo_id"),
                    colname = c('year', 'age_100', 'gender', race_col, 'geo_id'),
                    cat = c('year', 'age', 'gender', 'race', 'geo_id'))

  # Assemble query ----
  ## Query for race/eth ----
  if(race_type == 'race_eth'){
    q = build_getpop_query(con = con,
                           cols = cols,
                           pop_table = pop_table,
                           group_by = group_by,
                           group_geo_type = group_geo_type,
                           select_geo_type = select_geo_type,
                           ages = ages,
                           years = years,
                           genders = genders,
                           races = races,
                           where_geo_type,
                           where_geo_vintage,
                           where_census_vintage,
                           subset_by_kingco)
  }else if(race_type == 'race'){
    ## Query for race ----
    ### The query to data by race (ignoring ethnicity) ----
    q1 = build_getpop_query(con = con,
                           cols = cols,
                           pop_table = pop_table,
                           group_by = group_by,
                           group_geo_type = group_geo_type,
                           select_geo_type = select_geo_type,
                           ages = ages,
                           years = years,
                           genders = genders,
                           races = races,
                           where_geo_type,
                           where_geo_vintage,
                           where_census_vintage,
                           subset_by_kingco)
    q = list(q1)

    ### A query for the hispanic data ----
    if('6' %in% races){
      hcols = data.table::copy(cols)
      hcols[coltype == race_type, colname := 'race_hisp']
      q2 = build_getpop_query(con = con,
                              cols = hcols,
                              pop_table = pop_table,
                              group_by = group_by,
                              group_geo_type = group_geo_type,
                              select_geo_type = select_geo_type,
                              ages = ages,
                              years = years,
                              genders = genders,
                              races = 1, # only hispanic
                              where_geo_type,
                              where_geo_vintage,
                              where_census_vintage,
                              subset_by_kingco)

      q = list(q1,q2)
    }


  }else if(race_type == 'race_aic'){
    ## query for AIC races ----
    ### figure out what race groups need aic calculated ----
    if(races[1] == 'All'){
      aic_flag = seq_len(nrow(ref.table))
    }else{
      aic_flag = which(ref.table[, value] %in% races)
    }
    aics = ref.table[aic_flag]
    aics = aics[!is.na(sql_col)] # remove multiple race

    ### for each race, get pop ----
    q = lapply(aics[,sql_col], function(rcol){
      hcols = data.table::copy(cols)
      hcols[coltype == race_type, colname := rcol]
      build_getpop_query(
                        con = con,
                        cols = hcols,
                        pop_table = pop_table,
                        group_by = group_by,
                        group_geo_type = group_geo_type,
                        select_geo_type = select_geo_type,
                        ages = ages,
                        years = years,
                        genders = genders,
                        races = 1,
                        where_geo_type,
                        where_geo_vintage,
                        where_census_vintage,
                        subset_by_kingco
                      )


    })

  }else{
    stop('invalid race_type option snuck through')
  }

  ## run the query/queries ----
  ### return query if requested ----
  if(return_query) return(q)
  r = lapply(q, function(x){

    # run the query
    r = DBI::dbGetQuery(con, x)
    setDT(r)

    #rename/relabel the race col
    rc = ref.table[sql_col %in% names(r), ]
    stopifnot(nrow(rc)<=1)
    if(nrow(rc) == 1){
      r[,(rc[,sql_col]) := NULL]
      r[, (race_col) := rc[, value]]
    }

    r

  })

  r = rbindlist(r)


  # tidy the result ----
  ## age ----
  if('age_100' %in% names(r)){
    setnames(r,'age_100', 'age')
  }else{
    if(all(ages == 'All')) ages = 0:100
    r[, age := rads::format_time(ages)]
  }

  ## race ----
  if(race_col %in% names(r)){
    data.table::setnames(r, race_col, race_type)
    r[, (race_type) := factor(get(race_type),
                              ref.table[, value],
                              ref.table[, label])]
  }else{
    if(races == 'All') races = ref.table[, value]
    r[, (race_type) := ref.table[value %in% races, paste(label, collapse = ', ')]]
  }


  ## year ----
  if(!'year' %in% names(r)){
    if(all(years == 'all')) years = seq(2000, max_year,1)
    r[, year := rads::format_time(as.numeric(years))]
  }

  ## gender ----
  if(!'gender' %in% names(r)){
    if(all(genders == 'All')) genders = 1:2
    r[, gender := c('Female, Male')]
  }else{
    r[, gender := c('Female', 'Male')[as.numeric(gender)]]
  }

  ## geography ----
  r[, geo_type := geo_type]
  if(geo_type == 'kc') r[, geo_id := 'King County']
  if(geo_type == 'region'){
    rnames = unique(rads.data::spatial_hra_vid_region[,.(region, region_id)])
    rnames = rnames[, stats::setNames(region, region_id)]
    r[, geo_id_code := geo_id]
    r[, geo_id := rnames[as.character(geo_id)]]

  }
  if(geo_type == 'seattle') r[, geo_id := "Seattle"][,geo_type := 'Seattle']

  if(geo_type == 'county'){
    cnames = rads.data::spatial_county_codes_to_names
    cnames = cnames[,.(geo_id_code = as.character(cou_id), geo_id = cou_name)]
    cnames = stats::setNames(cnames[, geo_id], cnames[, geo_id_code])
    r[, geo_id_code := geo_id]
    r[, geo_id := cnames[as.character(geo_id)]]
  }

  if(geo_type == 'hra'){
    # FIXME: change to work with 2020 HRAs as well
    hranames = rads.data::spatial_hra_vid_region[, setNames(hra, vid)]
    r[, geo_id_code := geo_id]
    r[, geo_id := hranames[as.character(geo_id)]]
  }

  if(geo_type == 'lgd'){
    lnames = rads.data::spatial_legislative_codes_to_names[, setNames(lgd_name, lgd_id)]
    r[, geo_id_code := geo_id]
    r[, geo_id := lnames[as.character(geo_id)]]

  }

  if(geo_type == 'scd'){
    lnames = rads.data::spatial_school_codes_to_names[, setNames(scd_name, scd_id)]
    r[, geo_id_code := geo_id]
    r[, geo_id := lnames[as.character(geo_id)]]
  }

  if(geo_type == 'wa'){
    r[, geo_id := 'Washington State']
    r[, geo_id_code := 53]
  }

  if(round) r[, pop := rads::round2(pop, 0)]

  data.table::setcolorder(r, c("pop", "geo_type", "geo_id", "year", "age", "gender"))


  ## Close connection
  ## Now done with on.exit
  # if(closeserve) DBI::dbDisconnect(con)


  # Return results ----
  return(r)

}
