#' format_enrollment_data
#'
#' Converts OSPI enrollment data spreadsheet into a standard form usable for the weight generation process.
#' This involves standardizing the namings, and reshaping the data long.
#' At the moment, students without a specified race or whose sex is not male or female are implicitly dropped.
#'
#' @param input data.table. Expected format is long on school & grade, wide on sex and race
#' @param district_code character string. Column denoting the district identifer
#' @param school_code character string. Column denoting the school identifier
#' @param grade character string. Column denoting the grade column
#' @param m_ai character string. Column(s) denoting the counts of non-hispanic AIAN males
#' @param f_ai charcter string. Column(s) denoting the counts of non-hispanic AIAN females
#' @param m_as charcter string. Column(s) denoting the counts of non-hispanic Asian males
#' @param f_as charcter string. Column(s) denoting the counts of non-hispanic Asian females
#' @param m_b charcter string. Column(s) denoting the counts of non-hispanic Black/African American males
#' @param f_b charcter string. Column(s) denoting the counts of non-hispanic Black/African American females
#' @param m_nhpi charcter string. Column(s) denoting the counts of non-hispanic NHPI males
#' @param f_nhpi charcter string. Column(s) denoting the counts of non-hispanic NHPI females
#' @param m_w charcter string. Column(s) denoting the counts of non-hispanic White males
#' @param f_w charcter string. Column(s) denoting the counts of non-hispanic White females
#' @param m_hisp charcter string. Column(s) denoting the counts of non-hispanic Hispanic males
#' @param f_hisp charcter string. Column(s) denoting the counts of non-hispanic Hispanic females
#' @param m_other charcter string. Column(s) denoting the counts of non-hispanic Other/2+ race males
#' @param f_other charcter string. Column(s) denoting the counts of non-hispanic Other/2+ race females
#' @param fill logical. If TRUE, ensure that each school+grade combination has a row for all sex and race combos
#' @return A data.table long on district, school, grade, race, and sex.
#'
#' @impot data.table
#' @export
#'
format_enrollment_data = function(input, district_code, school_code, grade, m_ai, f_ai, m_as, f_as, m_b, f_b,
                                  m_nhpi, f_nhpi, m_w, f_w, m_hisp, f_hisp, m_other, f_other, fill = TRUE){

  #Make a deep copy to avoid altering the parent environment.
  input = copy(input)

  #set as data table
  setDT(input)

  input_list_names = c('district_code', 'school_code', 'grade', 'm_ai', 'f_ai', 'm_as', 'f_as', 'm_b', 'f_b',
                       'm_nhpi', 'f_nhpi', 'm_w', 'f_w', 'm_hisp', 'f_hisp', 'm_other', 'f_other')
  input_list = list(district_code, school_code, grade, m_ai, f_ai, m_as, f_as, m_b, f_b,
                 m_nhpi, f_nhpi, m_w, f_w, m_hisp, f_hisp, m_other, f_other)

  #confirm that all inputs are character strings
  if(!all(vapply(input_list, is.character, logical(1)))){
    stop('Invalid input provided. Please insure all arguments are characters (as defined by `is.character`)')
  }

  #confirm that all the column names are in the data.table
  if(any(!unlist(input_list) %in% names(input))){
    err = paste0('The following columns do not exist in `input`: ', paste(setdiff(input_list, names(input))), collapse = ' | ')
    stop(err)
  }

  #reformat/recreate the dataset
  #Step 1: Where relevant, rename the columns (e.g. length == 1)
  ones = which(unlist(lapply(input_list, length)) == 1)
  if(length(ones)>0){
    setnames(input, unlist(input_list[ones]), input_list_names[ones])
  }

  #Step 2: For the others, add them up as new variables
  notones = seq(input_list)[-ones]
  if(length(notones)){
    input[, (input_list_names[notones]) := lapply(notones, function(x) rowSums(.SD[, mget(input_list[[x]])]))]
  }
  #step 3: Reduce DT size by dropping unneeded columns
  input = input[, input_list_names, with = F]

  #Reshape long
  input = melt(input, id.vars = c('district_code', 'school_code', 'grade'), variable.factor = F, variable.name = 'sex_race', value.name = 'n_students')
  input[, c('sex', 'race') := tstrsplit(sex_race, split = '_')]
  input[, sex_race := NULL]

  #add 0s where neccessary
  #should this be a different function?
  sch_grid = unique(input[, .(district_code, school_code, grade)])
  race_sex_grid = expand.grid(grade = unique(input[, grade]),
                              sex = unique(input[, sex]),
                              race = unique(input[, race]))
  grid = sch_grid[race_sex_grid, on = "grade", allow.cartesian = T]

  input = merge(grid, input, by = names(grid))
  input[is.na(n_students), n_students := 0]

  #Return object
  return(input)
}
