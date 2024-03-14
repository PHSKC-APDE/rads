# Get populations that correspond with standard counts ----
# CHI_generate_instructions_pop() - function to generate instructions for get_population based on structure of count data----
#' Title
#'
#' @param mycount.data
#' @param povgeo
#'
#' @return
#' @export
#'
#' @examples
chi_generate_instructions_pop <- function(mycount.data, povgeo = NA){
  pop.template <- copy(mycount.data)
  pop.template <- unique(copy(pop.template)[, .(year, cat1, cat1_varname, cat2, cat2_varname, tab)])
  pop.template[, c("start", "stop") := tstrsplit(year, split = '-') ]
  pop.template[is.na(stop), stop := start] # need to have an end date even if it is just one year

  pop.template[, race_type := 'race_eth'] # by default has race and OMB 97 with Hispanic as race

  # Drop prefix when using maternal data because do not want to create multiple alternative codings below ----
  pop.template[grepl("birthing person", cat1, ignore.case = T), cat1 := tools::toTitleCase(gsub("Birthing person's ", "", cat1))]
  pop.template[grepl("birthing person", cat2, ignore.case = T), cat2 := tools::toTitleCase(gsub("Birthing person's ", "", cat2))]

  # Create geo_type & group_by arguments ----
  omb_aic <- c("chi_race_aic_aian", "chi_race_aic_asian", "chi_race_aic_black", "chi_race_aic_his", "chi_race_aic_nhpi", "chi_race_aic_wht")

  for(catnum in c("1", "2")){
    temp.cat <- paste0("cat", catnum)
    pop.template[get(temp.cat) == "Cities/neighborhoods", geo_type := "hra"]
    pop.template[get(paste0(temp.cat, "_varname")) == "race3", c("race_type", paste0("group_by", catnum)) := 'race']
    pop.template[get(paste0(temp.cat, "_varname")) == "race4", c("race_type", paste0("group_by", catnum)) := 'race_eth']
    pop.template[get(paste0(temp.cat, "_varname")) %in% omb_aic, c("race_type", paste0("group_by", catnum)) := 'race_aic']

    # the only AIC race/eth with pop data are the standard OMB categories
    pop.template <- pop.template[!(grepl('_aic_', get(paste0(temp.cat, "_varname"))) & !get(paste0(temp.cat, "_varname")) %in% omb_aic)]

    pop.template[get(temp.cat) == "Ethnicity", c("race_type", paste0("group_by", catnum)) := 'race_eth']
    pop.template[get(temp.cat) == "Gender",  paste0("group_by", catnum) := 'genders']
    pop.template[get(temp.cat) %in% c("Race", "Race/ethnicity") & get(paste0(temp.cat, "_varname")) == 'race4',
                 paste0("group_by", catnum) := 'race_eth']
    pop.template[(get(temp.cat) == "Race" & get(paste0(temp.cat, "_varname")) == 'race3') ,
                 paste0("group_by", catnum) := 'race']
    pop.template[get(temp.cat) == "Regions" & (is.na(geo_type) | geo_type != 'hra'), `:=` (geo_type = "region")]
    pop.template[get(temp.cat) == "Big cities", `:=` (geo_type = "hra")]
    pop.template[get(temp.cat) == "Washington State", `:=` (geo_type = "wa")]
  }

  pop.template[grepl("poverty$", cat1, ignore.case = T) | grepl("poverty$", cat2, ignore.case = T), geo_type := "blk"]
  if(povgeo == 'zip'){
    pop.template[grepl("poverty$", cat1, ignore.case = T) | grepl("poverty$", cat2, ignore.case = T), geo_type := "zip"]
  }
  pop.template[(cat1 == "Regions" & cat2 == "Cities/neighborhoods") |
                 (cat2 == "Regions" & cat1 == "Cities/neighborhoods"),
               geo_type := "blk"]

  # the only AIC race/eth with population data are the OMB standard categories

  pop.template[is.na(cat2), cat2 := "NA"] # temporarily set NA to "NA" to facilitate processing with function

  pop.template[is.na(geo_type), geo_type := 'kc'] # when not specified, it is for KC

  pop.template <- unique(pop.template) # because want to minimize the calls to get_popualation to improve speed
}
