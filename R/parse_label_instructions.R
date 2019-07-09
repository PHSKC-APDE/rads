#'Parse Label Instructions
#'
#'Imports the DOH created AskHYS Question Crosswalk and converts into recode instructions. Assumes its already loaded in memory
#'
#' @param crosswalk data.frame/data.table. Ideally unadulterated from a normal import (e.g. openxlsx::read_excel)
#' @param year numeric. Identifies the year the crosswalk dataset represents
#' @param drop_na logical. Should labeling related to rows with "n/a" in the variable_name column be removed?
#' @import data.table
#' @export
#'
#' @return A set of recode instructions (e.g. list of lists)
#'
parse_label_instructions = function(crosswalk, year, drop_na = T){

  #check basic info
  stopifnot(inherits(crosswalk, 'data.frame'))

  #ensure DT without scoping shenanigans
  cw = copy(crosswalk)
  setDT(cw)

  if(!all(c('variable_name', 'data_value', 'label') %in% names(cw))){
    stop('One of `variable_name`, `data_value`, or `label` are missing')
  }

  #convert to format for recoding
  #carry through the variable name
  cw[variable_name == "", variable_name := NA]
  cw[, variable_name := zoo::na.locf(.SD), .SDcols = 'variable_name']

  if(drop_na){
    cw = cw[!variable_name  %in% c("", 'n/a', ' ', 'na'),]
  }

  #a quick final check to make sure the recode will be 1:1
  checker = cw[data_value == "" | is.na(data_value), .N, by = 'variable_name']
  if(max(checker[, N])>1){
    naughty_vars = paste(checker[N>1, variable_name], collapse = ' | ')
    stop(paste('Input: `crosswalk` contains instances where a particular variable (specified by variable_name_id) is mapped to over 1 variable label (label_id) \n
         Naughty vars:', naughty_vars))
  }

  #don't calculate recode instructions for rows that are label only
  droppies = cw[, .(.N == sum(is.na(data_value))), by = 'variable_name'][V1 == TRUE, variable_name]

  cw = cw[!variable_name %in% droppies,]

  #convert to a list of recode instructions
  ret = lapply(unique(cw[, variable_name]), function(x) create_recode(old_var = x,
                                                                      new_var = x,
                                                                      old_value = cw[variable_name == x & !is.na(data_value), data_value ],
                                                                      new_value = cw[variable_name == x & !is.na(data_value), data_value ],
                                                                      new_label = cw[variable_name == x & !is.na(data_value), label],
                                                                      start_year = year,
                                                                      end_year = year,
                                                                      var_label = cw[variable_name == x & is.na(data_value), label]))

  return(ret)

}

