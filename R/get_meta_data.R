#' Get meta data about variables available via get_data.
#'
#' @description Front end for querying meta-data tables. Currently only a few variables from "hys" are implemented.
#'
#' @param data_source Character vector. Identifies the dataset(s) to match variable names to. Default NA will report from all data-sets
#' @param var_name Character vector. Identifies variable names to report the meta data of. Default NA will return information for all variables available
#'
#' @return data.table containing 2+n columns, naming the variable, the data source, and all available meta data
#' @export
#'
#' @examples
#' \dontrun{
#'  get_meta_data(dataset = "hys")
#' }
get_meta_data <- function(data_sources = NA, variable_names = NA){
  #load available metadata table. Expand to reference to configuration call in future
  DT <- as.data.table(rads:::metadata)

  if(!is.na(data_sources)[1] & !is.na(variable_names)[1]) {
    DT <- DT[data_source %in% data_sources & variable_name %in% variable_names]
  } else if (!is.na(data_sources)[1]) {
    DT <- DT[data_source %in% data_sources]
  } else if (!is.na(variable_names)[1]) {
    DT <- DT[variable_name %in% variable_names]
  }
  return(DT)
}
