

# CHI_get_proper_pop() - function to get population for a single row specified by the output of CHI_generate_instructions_pop() ----
chi_get_proper_pop <- function(pop.template = NULL, pop.genders = NULL, pop.ages = NULL){
  # check for valid values of pop.genders ----
  if(is.null(pop.genders)){gendery = c("f", "m")
  }else{if(!tolower(pop.genders) %in% c('f', 'female', 'm', 'male')){
    stop("\n\U0001f47f if pop.genders is specified it must have one of the following values: 'F', 'f', 'Female', 'female', 'M', 'm', 'Male', or 'male'")
  } else {gendery = pop.genders}}

  # check for valid values of pop.ages ----
  if(is.null(pop.ages)){agesy = c(0:100)
  }else{if(!is.integer(pop.ages)){
    stop("\n\U0001f47f if pop.ages is specified it must be vector of integers, e.g., c(0:65)")
  } else {agesy = pop.ages}}

  # create function to generate the population table corresponding to each row of the pop.template----
  CHI_get_proper_pop_engine <- function(X, pop.template = NULL){
    # Status updates ----
    print(paste0("Process ID ", Sys.getpid(), ": Getting population ", X, " out of ", nrow(pop.template)))

    # Drop prefix when using maternal data because do not want to create multiple alternative codings below ----
    pop.template[grepl("birthing person", cat1, ignore.case = T), cat1 := tools::toTitleCase(gsub("Birthing person's ", "", cat1))]
    pop.template[grepl("birthing person", cat2, ignore.case = T), cat2 := tools::toTitleCase(gsub("Birthing person's ", "", cat2))]

    # create the group_by argument ----
    groupy <- unique(c(c("ages", "geo_id"), setdiff(c(pop.template[X, group_by1], pop.template[X, group_by2]), c(NA))))

    # use rads::get_population ----
    if(is.na(pop.template[X, geo_type])){
      tempy <- get_population(group_by = groupy,
                              race_type = pop.template[X, race_type],
                              years = pop.template[X, start]:pop.template[X, stop],
                              genders = gendery,
                              ages = agesy,
                              round = F)
    }
    if(!is.na(pop.template[X, geo_type])){
      tempy <- get_population(group_by = groupy,
                              geo_type = pop.template[X, geo_type],
                              race_type = pop.template[X, race_type],
                              years = pop.template[X, start]:pop.template[X, stop],
                              genders = gendery,
                              ages = agesy,
                              round = F)
    }

    # tidy the population data ----
    for(catnum in c("cat1", "cat2")){
      # misc ----
      tempy[, paste0(catnum) := pop.template[X, get(catnum)]]
      tempy[, paste0(catnum, "_varname") := pop.template[X, get(paste0(catnum, "_varname"))]]

      tempy[get(catnum) == "King County", paste0(catnum, "_group") := "King County"]

      tempy[get(catnum) == "Washington State", paste0(catnum, "_group") := "Washington State"]

      suppressWarnings(tempy[get(catnum) == "NA" | is.na(get(catnum)),
                             c(catnum, paste0(catnum, "_group"), paste0(catnum, "_varname")) := "NA"]) # just a random fill value for NA, which will be changed to true NA later

      tempy[get(catnum) %in% c("Cities/neighborhoods", "Regions") &  pop.template[X, geo_type] != 'blk',
            paste0(catnum, "_group") := geo_id]

      tempy[get(catnum) %in% c("Gender"), paste0(catnum, "_group") := gender]

      tempy[get(catnum) %in% c("Overall"), paste0(catnum, "_group") := "Overall"]


      # race/eth ----
      tempy[get(catnum) == "Ethnicity" | get(paste0(catnum, "_varname")) %in% c('race4'), paste0(catnum, "_group") := race_eth]
      tempy[get(catnum) == 'Race' & get(paste0(catnum, "_varname")) %in% c('race3'), paste0(catnum, "_group") := race]
      tempy[get(paste0(catnum, "_group")) == "Multiple race", paste0(catnum, "_group") := "Multiple"]
      tempy <- tempy[get(catnum) != "Ethnicity" | (get(catnum) == "Ethnicity" & get(paste0(catnum, "_group")) == 'Hispanic'), ]

      # race_aic ----
      if(pop.template[X, race_type] == 'race_aic'){
        tempy <- tempy[!(grepl('_aic_', get(paste0(catnum, "_varname"))) &
                           !((get(paste0(catnum, "_varname")) == 'chi_race_aic_aian' & race_aic == 'AIAN') |
                               (get(paste0(catnum, "_varname")) == 'chi_race_aic_asian' & race_aic == 'Asian') |
                               (get(paste0(catnum, "_varname")) == 'chi_race_aic_black' & race_aic == 'Black')|
                               (get(paste0(catnum, "_varname")) == 'chi_race_aic_his' & race_aic == 'Hispanic') |
                               (get(paste0(catnum, "_varname")) == 'chi_race_aic_nhpi' & race_aic == 'NHPI') |
                               (get(paste0(catnum, "_varname")) == 'chi_race_aic_wht' & race_aic == 'White'))
        )]
        tempy[grep('_aic', get(paste0(catnum, "_varname"))), paste0(catnum, "_group") := race_aic]
      }

      # HRAS ----
      if(tempy[1, geo_type] == 'blk' & tempy[1, get(catnum)] == 'Cities/neighborhoods'){
        temp.xwalk <- rads.data::spatial_block20_to_hra20_to_region20[, .(geo_id = GEOID20, hra20_name)]
        tempy <- merge(tempy, temp.xwalk, by = "geo_id", all.x = T, all.y = F)
        tempy[, paste0(catnum, "_group") := hra20_name]
      }

      # Regions ----
      if(tempy[1, geo_type] == 'blk' & tempy[1, get(catnum)] == 'Regions'){
        temp.xwalk <- rads.data::spatial_block20_to_hra20_to_region20[, .(geo_id = GEOID20, region_name)]
        tempy <- merge(tempy, temp.xwalk, by = 'geo_id', all.x = T, all.y = F)

        tempy[, paste0(catnum, "_group") := region_name]
      }

      if(tempy[1, geo_type] == 'hra' & tempy[1, get(catnum)] == 'Regions'){
        temp.xwalk <- rads.data::spatial_hra20_to_region20[, .(geo_id = hra20_name, region_name)]
        tempy <- merge(tempy, temp.xwalk, by = 'geo_id', all.x = T, all.y = F)

        tempy[, paste0(catnum, "_group") := region_name]
      }

      if(tempy[1, geo_type] == 'zip' & tempy[1, get(catnum)] == 'Regions'){
        zip_2_region <- rads.data::spatial_zip_to_hra20_pop
        zip_2_region <- merge(zip_2_region,
                              rads.data::spatial_hra20_to_region20[, .(hra20_name, region = region_name)],
                              by = 'hra20_name',
                              all = T)
        zip_2_region <- zip_2_region[, .(s2t_fraction = sum(s2t_fraction)), # collapse fractions down to region level
                                     .(geo_id = as.character(source_id), region)]

        tempy <- merge(tempy, zip_2_region, by = "geo_id", all.x = T, all.y = F, allow.cartesian = T)
        tempy[, pop := pop * s2t_fraction] # calculate weighted pop
        tempy[, paste0(catnum, "_group") := region]
      }

      # Big Cities ----
      if(tempy[1, get(catnum)] == 'Big cities'){
        if(tempy[1, geo_type] == 'blk'){
          blk20_hra20 <- rads.data::spatial_block20_to_hra20_to_region20[, .(geo_id = GEOID20, hra20_name)]
          tempy <- merge(tempy, blk20_hra20, by = "geo_id", all.x = T, all.y = F)
          hra20_bigcity <- rads.data::spatial_hra20_to_bigcities[, .(hra20_name, bigcity)]
          tempy <- merge(tempy, hra20_bigcity, by = 'hra20_name', all.x = T, all.y = F)
        }
        if(tempy[1, geo_type] == 'hra'){
          hra20_bigcity <- rads.data::spatial_hra20_to_bigcities[, .(hra20_name, bigcity)]
          tempy <- merge(tempy, hra20_bigcity, by.x = 'geo_id', by.y = 'hra20_name', all.x = T, all.y = F)
        }
        tempy[, paste0(catnum, "_group") := bigcity]
      }

      # age6 ----
      tempy[get(paste0(catnum, "_varname")) == "age6" & age %in% 0:17, paste0(catnum, "_group") := "<18"]
      tempy[get(paste0(catnum, "_varname")) == "age6" & age %in% 18:24, paste0(catnum, "_group") := "18-24"]
      tempy[get(paste0(catnum, "_varname")) == "age6" & age %in% 25:44, paste0(catnum, "_group") := "25-44"]
      tempy[get(paste0(catnum, "_varname")) == "age6" & age %in% 45:64, paste0(catnum, "_group") := "45-64"]
      tempy[get(paste0(catnum, "_varname")) == "age6" & age %in% 65:74, paste0(catnum, "_group") := "65-74"]
      tempy[get(paste0(catnum, "_varname")) == "age6" & age >= 75, paste0(catnum, "_group") := "75+"]

      # mage5 ----
      tempy[get(paste0(catnum, "_varname")) == "mage5" & age %in% 10:17, paste0(catnum, "_group") := "10-17"]
      tempy[get(paste0(catnum, "_varname")) == "mage5" & age %in% 18:24, paste0(catnum, "_group") := "18-24"]
      tempy[get(paste0(catnum, "_varname")) == "mage5" & age %in% 25:34, paste0(catnum, "_group") := "25-34"]
      tempy[get(paste0(catnum, "_varname")) == "mage5" & age %in% 35:44, paste0(catnum, "_group") := "35-44"]
      tempy[get(paste0(catnum, "_varname")) == "mage5" & age >=45, paste0(catnum, "_group") := "45+"]

      # yage4 ----
      tempy[get(paste0(catnum, "_varname")) == "yage4" & age %in% 0:4, paste0(catnum, "_group") := "0-4"]
      tempy[get(paste0(catnum, "_varname")) == "yage4" & age %in% 5:9, paste0(catnum, "_group") := "5-9"]
      tempy[get(paste0(catnum, "_varname")) == "yage4" & age %in% 10:14, paste0(catnum, "_group") := "10-14"]
      tempy[get(paste0(catnum, "_varname")) == "yage4" & age %in% 15:17, paste0(catnum, "_group") := "15-17"]

      # pov200grp ----
      if(tempy[1, geo_type] == 'blk' & grepl("poverty$", tempy[1, get(catnum)], ignore.case = T)){
        tempy[, geo_tract2020 := substr(geo_id, 1, 11)] # have blocks (15 char), so keep first 11 for tracts
        tempy <- merge(tempy,
                       rads.data::misc_poverty_groups[geo_type=='Tract'][, .(geo_tract2020 = geo_id, pov200grp)],
                       by = "geo_tract2020",
                       all.x = T,
                       all.y = F)
        tempy[, paste0(catnum, "_group") := pov200grp]
      }
      if( tempy[1, geo_type] == 'zip' & grepl("poverty$", tempy[1, get(catnum)], ignore.case = T)){
        tempy <- merge(tempy,
                       rads.data::misc_poverty_groups[geo_type=='ZCTA'][, .(geo_id, pov200grp)],
                       by = 'geo_id',
                       all.x = T,
                       all.y = F)
        tempy[, paste0(catnum, "_group") := pov200grp]
      }

    }

    # Drop if is.na(cat1_group)
    tempy <- tempy[!is.na(cat1_group)]

    # drop if is.na(cat2_group)
    tempy <- tempy[!(cat2 != 'NA' & (cat2_group == 'NA') | is.na(cat2_group))] # did not yet switch back to true NA at this point

    # collapse to one row per demographic combination and keep minimum needed columns ----
    tempy <- tempy[, .(pop = sum(pop)), .(chi_age = age, year, cat1, cat1_varname, cat1_group, cat2, cat2_varname, cat2_group)]

    # ensure each demographic has rows for all relevant ages & only relevant ages ----
    if(tempy[1]$cat1 == "Age"){
      if(tempy[1]$cat1_varname == 'age6'){
        tempage <- data.table(cat1 = "Age", cat1_varname = "age6", chi_age = 0:100)
        tempage[, cat1_group := cut(chi_age,
                                    breaks = c(-1, 17, 24, 44, 64, 74, 120),
                                    labels = c("<18", "18-24", "25-44", "45-64", "65-74", "75+"))]}

      if(tempy[1]$cat1_varname == 'mage5'){
        tempage <- data.table(cat1 = "Age", cat1_varname = "mage5", chi_age = 10:100)
        tempage[, cat1_group := cut(chi_age,
                                    breaks = c(9, 17, 24, 34, 44, 120),
                                    labels = c('10-17', '18-24', '25-34', '35-44', '45+'))]}

      if(tempy[1]$cat1_varname == 'yage4'){
        tempage <- data.table(cat1 = "Age", cat1_varname = "yage4", chi_age = 0:17)
        tempage[, cat1_group := cut(chi_age,
                                    breaks = c(-1, 4, 9, 14, 17),
                                    labels = c('0-4', '5-9', '10-14', '15-17'))]}

      temp.demog <- setDT(tidyr::crossing(unique(tempy[, .(year = as.character(year), cat2, cat2_varname, cat2_group)]),
                                          tempage))
    }

    if(tempy[1]$cat2 == "Age"){
      if(tempy[1]$cat2_varname == 'age6'){
        tempage <- data.table(cat2 = "Age", cat2_varname = "age6", chi_age = 0:100)
        tempage[, cat2_group := cut(chi_age,
                                    breaks = c(-1, 17, 24, 44, 64, 74, 120),
                                    labels = c("<18", "18-24", "25-44", "45-64", "65-74", "75+"))]}

      if(tempy[1]$cat2_varname == 'mage5'){
        tempage <- data.table(cat2 = "Age", cat2_varname = "mage5", chi_age = 10:100)
        tempage[, cat2_group := cut(chi_age,
                                    breaks = c(9, 17, 24, 34, 44, 120),
                                    labels = c('10-17', '18-24', '25-34', '35-44', '45+'))]}

      if(tempy[1]$cat2_varname == 'yage4'){
        tempage <- data.table(cat2 = "Age", cat2_varname = "yage4", chi_age = 0:17)
        tempage[, cat2_group := cut(chi_age,
                                    breaks = c(-1, 4, 9, 14, 17),
                                    labels = c('0-4', '5-9', '10-14', '15-17'))]}

      temp.demog <- setDT(tidyr::crossing(unique(tempy[, .(year = as.character(year), cat1, cat1_varname, cat1_group)]),
                                          tempage))
    }

    if(!"Age" %in% unique(c(tempy$cat1, tempy$cat2))){
      # all combinations of cat1 x cat2
      temp.demog <- setDT(tidyr::crossing(
        unique(tempy[, .(cat1, cat1_varname, cat1_group)]),
        unique(tempy[, .(cat2, cat2_varname, cat2_group)])
      ))
      # all combination of cat table with year and age
      temp.demog <- setDT(tidyr::crossing(
        temp.demog,
        data.table(year = as.character(pop.template[X, ]$year), chi_age = 0:100)
      ))
    }

    tempy <- suppressWarnings(merge(tempy, temp.demog, all = T))
    tempy[is.na(pop), pop := 0]

    # create combinations of cat1_group and cat2_group that have no population and set == 0 ----
    # no need with current get_population function, but keep as a placeholder / reminder

    # add tab column ----
    tempy[, tab := pop.template[X, tab]]

    # tidy when is.na(cat2) ----
    tempy[cat2 == "NA", c("cat2", "cat2_varname", "cat2_group") := NA]

    # return object ----
    return(tempy)
  }

  # use lapply to cycle over each rows and create one big final dataset ----
  tempy.allpop <- rbindlist(
    future_lapply(X = as.list(seq(1, nrow(pop.template))),
                  FUN = function(X){
                    set.seed(98104) # another attempt to set a seed
                    CHI_get_proper_pop_engine(X, pop.template = pop.template)},
                  future.seed = 98104)
  )

  tempy.allpop <- unique(tempy.allpop)

  # return object ----
  return(tempy.allpop)
}
