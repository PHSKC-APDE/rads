#' A helper function for building queries for get_pop
#'
#' @param con a database connection
#' @param cols data.table A three column data.table that describes the colname and coltype and cat
#' @param pop_table DBI::Id of the relevant database table
#' @param group_by character vector of column types to group results by. Should use types passed in cols
#' @param group_geo_type SQL defining how to handle geo type grouping (mostly for things computed on the fly from blocks)
#' @param select_geo_type SQL defining how to select geo_id
#' @param ages vector of the ages to subset by. Use "All" to not subset
#' @param years vector of years to subset results. Use "All" to not subset
#' @param genders vector of genders to subset by. Use "All" to not subset
#' @param races Vector of races to subsey by. Use 'All' to not subset
#' @param ... other SQL commands specifying subsets
#' @details This function does very little argument checking since it assumes its being run within get_population
build_getpop_query = function(con,
                              cols,
                              pop_table,
                              group_by,
                              group_geo_type,
                              select_geo_type,
                              ages,
                              years,
                              genders,
                              races,
                              ...) {

  # fix visible bindings
  coltype <- colname <- NULL

  # Define columns to group by ----
  grp_cols = cols[coltype %in% group_by, colname]
  grp_cols = setdiff(grp_cols, 'All')

  # check if there are groups
  if(length(grp_cols)>0){
    ## if so, convert the non-geo_ids to DBI::Ids and then to sql
    grp_cols_sql = lapply(setdiff(grp_cols, 'geo_id'), function(x) {
      glue::glue_sql('{`DBI::Id(column = x)`}', .con = con)
    })

    if (length(grp_cols_sql) > 0) names(grp_cols_sql) = setdiff(grp_cols, 'geo_id')

    ## add group by geotype
    if(inherits(group_geo_type, 'Id')) group_geo_type = glue::glue_sql('{`group_geo_type`}', .con = con)
    grp_cols_sql <- append(list(geo_id = group_geo_type), grp_cols_sql)

    # check to see if there are any legit ons
    blank_grp = sapply(grp_cols_sql, function(x) x == DBI::SQL(''))

    # remove ones that are just nuthin
    grp_cols_sql = grp_cols_sql[!blank_grp]
  }else{
    grp_cols_sql = list()
  }

  if(length(grp_cols_sql) >0){
    grpz = glue::glue_sql_collapse(grp_cols_sql, sep = ',')
    group_vars = glue::glue_sql('GROUP BY {grpz}', .con = con)
  }else{
    grpz = DBI::SQL('')
    group_vars = DBI::SQL('')
  }


  # Compute_pop ----
  # all get_population calls should be grouped by geo_id-- even if its redundant
  compute_pop = DBI::SQL('SUM(pop) as pop')

  ## select clause ----
  ### custom selection for geo_type ----
  if (any(grp_cols %in% 'geo_id')) {
    if (length(grp_cols_sql) >= 1) {
      grp_cols_sql[['geo_id']] <- select_geo_type
    } else{
      grp_cols_sql = select_geo_type
    }
  }

  ### make sure everything is sql
  grp_cols_sql = lapply(grp_cols_sql, function(x){
    if(inherits(x, 'Id')){
      x = glue::glue_sql('{`x`}',.con = con)
    }
    x
  })

  ### create select_me ----
  selects = c(compute_pop,grp_cols_sql)
  selects = selects[!sapply(selects, function(x) x == SQL(''))]
  select_me = glue_sql_collapse(c(compute_pop, grp_cols_sql), sep = ',')

  ## Subset clauses ----
  ### create clauses ----
  subset_by_age = make_subset(con, 'age_100', ages)
  subset_by_year = make_subset(con, 'year', years)
  subset_by_gender = make_subset(con, 'gender', genders)
  subset_by_raceeth = make_subset(con, cols[cat == 'race', colname], races)
  subs = c(subset_by_age, subset_by_year, subset_by_gender, subset_by_raceeth, ...)

  subs = subs[subs != SQL('')]
  subset_me = glue_sql_collapse(subs, sep = ' AND ')
  if (!subset_me == '') subset_me = glue_sql('where {subset_me}', .con = con)

  # Generate query ----
  q = glue::glue_sql('
      select
      {select_me}
      from {`pop_table`} as p
      {subset_me}
      {group_vars}',
                     .con = con)

  q


}
#' A function to make subsets
#' @param con a database connection
#' @param var variable name
#' @param items list of items/values to subset on
#' @return SQL representing a query subset
make_subset = function(con, var, items = NULL){
  if(is.null(items) || items[1] == 'All'){
    return(SQL(''))
  }else{
    thecol = Id(column = var)
    subme = glue::glue_sql('{`thecol`} in ({items*})', .con = con)
    return(subme)
  }
}
