#' Batch execution of CHI/Tableau ready standards using R.
#'
#' @description
#' This function function generates one or more Tableau ready CHI analytic outputs
#' "//phshare01/epe_share/WORK/CHI Visualizations/Tableau Ready Output Format_v2.xlsx"
#'
#' @details
#' Give a list of variables and minimal instructions, this function outputs a data
#' frame structured for import into APDE's Tableau ready CHI database.
#'
#' @param chi_est Name of a data.table or data.frame describing variables to be analyzed, analysis to be conducted, and validation check for the input
#' @param chi_meta Name of a data.table or data.frame containing
#'
#' @return If no errors are encountered, returns a dataframe matching APDE data
#' and metadata standards. Else, error message.
#'
#' @export
#'
#' @keywords CHI, Tableau, Production
#'
#' @importFrom data.table is.data.table ':=' setDT setDF data.table setorder copy setnames setorder dcast setcolorder fread shift "%between%"
#' @import dtsurvey
#' @import future
#' @import future.apply
#'
#' @examples
#' \dontrun{

#' }

# chi_tableau_read_output header

APDE_chi_tableau_ready_output <- function(dataset, chi_meta, generate_crosstabulation = TRUE, chi_est = NULL ){
  meta <- rads::get_meta_data("hys")

  #temp reformat to originating script
  names(meta) <- c("cat", "group", "group_alias", "varname", "ng", "gval", "data_source")
  meta$data_source <- NULL
  meta$good <- TRUE


  #list of single variables to calculate against that needs to be fed
  vars <- c("sex_nocondom_lsttime",
            "drug_use_bin_30days",
            "alc_binge_bin_30days",
            "mj_drive_ride_bin_30days",
            "tob_ecig_bin_30days",
            "abusive_adult",
            "junk_soda_bin_30days",
            "dental_care_bin",
            "viol_weap_school",
            "abusive_intimate_partner",
            "mhlth_depressed_lstyr",
            "no_bkfast_tdy",
            "supportive_adult",
            "mj_use_bin_30days",
            "fruit_veg_bin_not5perday",
            "phys_60min7days_bin",
            "phys_pe5days_bin",
            "screen_ovr_3hrs_bin",
            "safe_school_bin",
            "good_grades_bin",
            "tob_2ndhand_bin",
            "mhlth_suicideidea_lstyr",
            "tob_cigar_bin_30days",
            "tob_hookah_bin_30days",
            "tob_cig_bin_30days",
            "tob_anytob_bin_30days",
            "bmi_obese_bin",
            "bmi_ovrwght_only_bin" )

  #list of single variables to calculate against
  bys <- c("chi_race_aic_aian",
           "chi_race_aic_cambodian",
           "chi_race_aic_chinese",
           "chi_race_aic_filipino",
           "chi_race_aic_indian",
           "chi_race_aic_japanese",
           "chi_race_aic_korean",
           "chi_race_aic_asianother",
           "chi_race_aic_vietnamese",
           "chi_race_aic_asian",
           "chi_race_aic_black",
           "chi_grade_orig",
           "chi_race_aic_his",
           "chi_race_aic_nhpi",
           "chi_race_aic_oth",
           "chi_race_eth8",
           "chi_sex",
           "chi_sexorien_3",
           "chi_race_aic_wht",
           "chi_geo_region",
           "chi_geo_kc")

  #list of variables to create time trend information against
  ttbys = c("chi_race_aic_aian",
            "chi_race_aic_asian",
            "chi_race_aic_black",
            "chi_grade_orig",
            "chi_race_aic_his",
            "chi_race_aic_nhpi",
            "chi_race_aic_oth",
            "chi_race_eth8",
            "chi_sex",
            "chi_race_aic_wht",
            "chi_geo_region",
            "chi_geo_kc"  )


  #create crosstabs to be calculated. This uses all variables except geographies and crosstab on their own identity
  ctabs = CJ(cat1_varname = bys, cat2_varname = bys)
  ctabs = ctabs[cat1_varname != cat2_varname & cat1_varname != 'chi_geo_kc' & cat2_varname != 'chi_geo_kc']

  #hard load data for testing script
  hysinit <- get_data("hys", year = c(seq(2004,2018,2),2021),ar =T)
  hys = hysinit[chi_grade_orig %in% c(8, 10, 12) & chi_geo_kc == 1 & !is.na(chi_sex)]
  hys = dtsurvey(hys[!is.na(wt_sex_grade_kc), -('_id')], psu = 'psu', strata = 'chi_grade_orig', weight = "wt_sex_grade_kc")




  #create function to calculate the various tables desired
  domath = function(hys, v, bys, ctabs, ttbys, meta){

    ##Identify years for current update of the system
    #what two cycles do the data represent (even if in some combos this is not true)?
    grp_yrs = unique(hys[!is.na(get(v)), .SD, .SDcols = c(v, 'chi_year')][, chi_year])
    grp_yrs = grp_yrs[(length(grp_yrs)-1):length(grp_yrs)]
    ##create a human readable description of the data range of the current point estimates
    gyl = paste(grp_yrs, collapse = ' & ')

    ##create county wide point estimate of the current variable
    #calculate point estimate
    kc = rads::calc(hys, what = v, where = chi_year %in% grp_yrs, metrics = c('mean', 'denominator', 'numerator', 'rse'), proportion = T, time_var = 'chi_year')
    #assign metadata
    kc[, tab := '_kingcounty']
    kc[, cat1 := 'King County']
    kc[, cat1_group := 'King County']
    kc[, cat1_varname := 'chi_geo_kc']
    kc[, cat1_group_alias := 'King County']
    kc[, c('cat2', 'cat2_group', 'cat2_varname', 'cat2_group_alias') := list(cat1, cat1_group, cat1_varname, cat1_group_alias) ]
    kc[, chi_year := gyl]

    #calculate trendline data
    #demgroups and time trends
    dgs = lapply(bys, function(x){
      r= rads::calc(hys, what = v, by = x, where = chi_year %in% grp_yrs, metrics = c('mean', 'denominator', 'numerator', 'rse'), proportion = T, time_var = 'chi_year')
      r[, tab := 'demgroups']
      r[, chi_year := gyl]

      if(x %in% ttbys){
        r2 = rads::calc(hys, what = v, by = c(x, 'chi_year'), metrics = c('mean', 'denominator', 'numerator', 'rse'), proportion = T)
        r2[, tab := 'trends']
        r = rbind(r, r2)
      }

      #rename variable "x" (by variable currently being manipulated) to "cat1_group"
      setnames(r, x, 'cat1_group') #will need to be updated to something human readable

      #assume that any variable that has a category of 1 is actually binary, and we only need to keep the affirmative observations
      if(any(r[, cat1_group] %in% 1)) r=  r[cat1_group == 1] ##???? if any cat1_group were identified, remove anything that isn't

      #create cat1_varname variable using the by variable (bys) identified
      r[, cat1_varname := x]
      r= r[!is.na(cat1_group)]

      r

    })
    dgs = rbindlist(dgs)
    dgs[, c('cat2', 'cat2_group', 'cat2_varname', 'cat2_group_alias') := list(NA_character_, NA_character_, NA_character_, NA_character_) ]
    #remove demgroups on kc wide geography
    dgs = dgs[!(tab == 'demgroups' & cat1_varname == 'chi_geo_kc')]


    #merge on cat information
    #note, cat1_group
    dgs = merge(dgs, meta, all.x = T, by.x = c('cat1_varname', 'cat1_group'), by.y = c('varname', 'gval'))
    stopifnot(all(dgs[, good]))
    dgs[, c('ng', 'good', 'cat1_group') := NULL]
    setnames(dgs, c('cat', 'group', 'group_alias'), c('cat1', 'cat1_group', 'cat1_group_alias'))

    #weird fix for chi_geo_kc
    dgs[tab == 'trends' & cat1_varname=='chi_geo_kc', c('cat2', 'cat2_group', 'cat2_varname', 'cat2_group_alias') := list(cat1, cat1_group, cat1_varname, cat1_group_alias)]


    #crosstabs
    cts = lapply(seq_len(nrow(ctabs)), function(x){
      by1 = ctabs[x, cat1_varname]
      by2 = ctabs[x, cat2_varname]

      r= rads::calc(hys, what = v, where = chi_year %in% grp_yrs, by = c(by1, by2), metrics = c('mean', 'denominator', 'numerator', 'rse'), proportion = T, time_var = 'chi_year')

      #cat1 fixes
      setnames(r, by1, 'cat1_group') #will need to be updated to something human readable
      if(any(r[, cat1_group] %in% 1)) r=  r[cat1_group == 1]
      r[, cat1_varname :=by1]
      r= r[!is.na(cat1_group)]

      #cat2 fixes
      setnames(r, by2, 'cat2_group') #will need to be updated to something human readable
      if(any(r[, cat2_group] %in% 1)) r=  r[cat2_group == 1]
      r[, cat2_varname :=by2]
      r= r[!is.na(cat2_group)]

      r[, chi_year := gyl]

      r


    })
    cts = rbindlist(cts)
    cts[, tab := 'crosstabs']

    #merge on cat information for cat1
    cts = merge(cts, meta, all.x = T, by.x = c('cat1_varname', 'cat1_group'), by.y = c('varname', 'gval'))
    stopifnot(all(cts[, good]))
    cts[, c('ng', 'good', 'cat1_group') := NULL]
    setnames(cts, c('cat', 'group', 'group_alias'), c('cat1', 'cat1_group', 'cat1_group_alias'))

    #merge on cat information for cat2
    cts = merge(cts, meta, all.x = T, by.x = c('cat2_varname', 'cat2_group'), by.y = c('varname', 'gval'))
    stopifnot(all(cts[, good]))
    cts[, c('ng', 'good', 'cat2_group') := NULL]
    setnames(cts, c('cat', 'group', 'group_alias'), c('cat2', 'cat2_group', 'cat2_group_alias'))

    #get results sorted
    res = rbind(kc, dgs, cts)


    setnames(res,
             c('mean', 'mean_se', 'mean_lower', 'mean_upper', 'variable', 'chi_year'),
             c('result', 'se', 'lower_bound', 'upper_bound', 'indicator_key', 'year'))


    res[, data_source := 'hys']
    res[, level := NULL]
    res[, run_date := Sys.Date()]
    res[, year := gsub(', ', ' & ', year, fixed = T)]

    #overalls for crosstabs
    newover = res[tab == 'demgroups']
    newover[, tab := 'crosstabs']
    newover[, c('cat2', 'cat2_varname', 'cat2_group') := list('Overall', NA_character_, 'Overall')]
    res = rbind(res, newover)

    #compare to KC
    kc_comp = res[cat1_varname == 'chi_geo_kc', .(year, kcr = result, lower_bound_kc = lower_bound, upper_bound_kc = upper_bound)]

    res = merge(res, kc_comp, all.x = T, by = 'year')
    res[upper_bound < lower_bound_kc, comparison_with_kc := 'lower']
    res[lower_bound > upper_bound_kc, comparison_with_kc := 'higher']
    res[upper_bound >= lower_bound_kc | lower_bound <= upper_bound_kc, comparison_with_kc := 'no different']
    res[comparison_with_kc %in% c('higher', 'lower'), significance := '*' ]
    res[, c('kcr', 'lower_bound_kc', 'upper_bound_kc') := NULL]

    res[is.nan(result), result := NA ]

    return(res)
  }

  FormatedAnalysisOriginal = rbindlist(future.apply::future_lapply(vars, function(v) domath(hys, v, bys, ctabs, ttbys, meta)))


###########################################################
###########################################################
###########################################################
#########################NEW CODE##########################
###########################################################
###########################################################
###########################################################


  #data to work with
  #hard load data for testing script
  data <- get_data("hys", year = c(seq(2004,2018,2),2021),ar =T)
  data <- data[chi_grade_orig %in% c(8, 10, 12) & chi_geo_kc == 1 & !is.na(chi_sex)]
  data <- dtsurvey(data[!is.na(wt_sex_grade_kc), -('_id')], psu = 'psu', strata = 'chi_grade_orig', weight = "wt_sex_grade_kc")

  #base variables
  variables <- c("sex_nocondom_lsttime",
                 "drug_use_bin_30days",
                 "alc_binge_bin_30days",
                 "mj_drive_ride_bin_30days",
                 "tob_ecig_bin_30days",
                 "abusive_adult",
                 "junk_soda_bin_30days",
                 "dental_care_bin",
                 "viol_weap_school",
                 "abusive_intimate_partner",
                 "mhlth_depressed_lstyr",
                 "no_bkfast_tdy",
                 "supportive_adult",
                 "mj_use_bin_30days",
                 "fruit_veg_bin_not5perday",
                 "phys_60min7days_bin",
                 "phys_pe5days_bin",
                 "screen_ovr_3hrs_bin",
                 "safe_school_bin",
                 "good_grades_bin",
                 "tob_2ndhand_bin",
                 "mhlth_suicideidea_lstyr",
                 "tob_cigar_bin_30days",
                 "tob_hookah_bin_30days",
                 "tob_cig_bin_30days",
                 "tob_anytob_bin_30days",
                 "bmi_obese_bin",
                 "bmi_ovrwght_only_bin" )

  #varibales for bivariate analysis
  bivariables <- c("chi_race_aic_aian",
                   "chi_race_aic_cambodian",
                   "chi_race_aic_chinese",
                   "chi_race_aic_filipino",
                   "chi_race_aic_indian",
                   "chi_race_aic_japanese",
                   "chi_race_aic_korean",
                   "chi_race_aic_asianother",
                   "chi_race_aic_vietnamese",
                   "chi_race_aic_asian",
                   "chi_race_aic_black",
                   "chi_grade_orig",
                   "chi_race_aic_his",
                   "chi_race_aic_nhpi",
                   "chi_race_aic_oth",
                   "chi_race_eth8",
                   "chi_sex",
                   "chi_sexorien_3",
                   "chi_race_aic_wht",
                   "chi_geo_region",
                   "chi_geo_kc")

  #variables to calculate crosstabulations against
  #create crosstabs to be calculated. This uses all variables except geographies and crosstab on their own identity
  ctabs = CJ(cat1_varname = bivariables, cat2_varname = bivariables)
  ctabs = ctabs[cat1_varname != cat2_varname & cat1_varname != 'chi_geo_kc' & cat2_varname != 'chi_geo_kc']
  crosstabs <- ctabs

  #list of variables to use in time trend analysis
  timeTrendBivariables <- c("chi_race_aic_aian",
                            "chi_race_aic_asian",
                            "chi_race_aic_black",
                            "chi_grade_orig",
                            "chi_race_aic_his",
                            "chi_race_aic_nhpi",
                            "chi_race_aic_oth",
                            "chi_race_eth8",
                            "chi_sex",
                            "chi_race_aic_wht",
                            "chi_geo_region",
                            "chi_geo_kc"  )


  #metadata from prior code that should be phased out sooner rather than later! Should be user input
  meta <- rads::get_meta_data("hys")

  #temp reformat to originating script
  names(meta) <- c("cat", "group", "group_alias", "varname", "ng", "gval", "data_source")
  meta$data_source <- NULL
  meta$good <- TRUE


  #List of needed functions:
  #Trendline batcher
  #Bivariate batcher
  #crosstab batcher
  #tableau formater


  APDE_CHI_TRO_time_trend_analysis <- function(data, variables, bivariables) {
    #return a data structure containing calc results for each combination of variables and bivariables across range of available timepoints
    #
    .internal_time_trend_calc <- function(v, bivariables, time_var, data) {

      all_calc_results <- (future.apply::future_lapply(bivariables, function(bivariable) {
        calc_result = rads::calc(data, what = v, by = c(bivariable, time_var), metrics = c('mean', 'denominator', 'numerator', 'rse'), proportion = T)
        calc_result
      }))
      return(all_calc_results)
    }

    returner <- future.apply::future_lapply(variables, function(v) .internal_time_trend_calc(v, bivariables, "chi_year" , data))
    returner

  }


  APDE_TRO_FORMATING <- function(DT,
                                 yearVariable,
                                 indicatorKeyVariable,
                                 resultVariable,
                                 numeratorVariable,
                                 denominatorVariable,
                                 seVariable,
                                 lowerBoundVariable,
                                 upperBoundVariable,
                                 rseVariable,
                                 tabVariable,
                                 cat1Variable,
                                 cat1_groupVariable,
                                 cat1_varnameVariable,
                                 cat1_group_aliasVariable,
                                 cat2Variable = NA,
                                 cat2_groupVariable = NA,
                                 cat2_varnameVariable = NA,
                                 cat2_group_aliasVariable = NA,
                                 data_sourceVariable,
                                 run_dateVariable,
                                 comparison_with_KCVariable,
                                 significanceVariable) {
    #Will check if value exists in the names of a dataframe, or a new value is provided
    #if a value is a name, it will rename that vector in the dataframe to the standard name (order matters) and apply any known cleaning
    #otherwise, will create the needed vector with the standard name and give all instances of it the value provided

    #make target datatable of correct size
    outputNames <- c("year",
                     "indicator_key",
                     "result",
                     "numerator",
                     "denominator",
                     "se",
                     "lower_bound",
                     "upper_bound",
                     "rse",
                     "tab",
                     "cat1",
                     "cat1_group",
                     "cat1_varname",
                     "cat1_group_alias",
                     "cat2",
                     "cat2_group",
                     "cat2_varname",
                     "cat2_group_alias",
                     "data_source",
                     "run_date",
                     "comparison_with_kc",
                     "significance")
    Tableau_Ready_DT <- matrix(rep(NA, time = 22*nrow(DT)), ncol = 22, byrow= TRUE)
    colnames(Tableau_Ready_DT) <- outputNames
    Tableau_Ready_DT <- data.table::as.data.table(Tableau_Ready_DT)

    #process and add year variable
    if(yearVariable %in% names(DT)) {
      Tableau_Ready_DT$year <- DT[, ..yearVariable]
    }
    Tableau_Ready_DT$year <- as.character(Tableau_Ready_DT$year)


    #process and add indicator_key
    if(indicatorKeyVariable %in% names(DT)) {
      Tableau_Ready_DT$indicator_key <- DT[, ..indicatorKeyVariable]
    }
    Tableau_Ready_DT$indicator_key <- as.character(Tableau_Ready_DT$indicator_key)

    #process and add result
    if(resultVariable %in% names(DT)) {
      Tableau_Ready_DT$result <- DT[, ..resultVariable]
    }
    Tableau_Ready_DT[is.nan(result), result := NA]

    #process and add numerator
    if(numeratorVariable %in% names(DT)) {
      Tableau_Ready_DT$numerator <- DT[, ..numeratorVariable]
    }


    #process and add denominator
    if(denominatorVariable %in% names(DT)) {
      Tableau_Ready_DT$denominator <- DT[, ..denominatorVariable]
    }

    #process and add se
    if(seVariable %in% names(DT)) {
      Tableau_Ready_DT$se <- DT[, ..seVariable]
    }

    #process and add lower_bound
    if(lowerBoundVariable %in% names(DT)) {
      Tableau_Ready_DT$lower_bound <- DT[, ..lowerBoundVariable]
    }

    #process and add upper_bound
    if(upperBoundVariable %in% names(DT)) {
      Tableau_Ready_DT$upper_bound <- DT[, ..upperBoundVariable]
    }

    #process and add rse
    if(rseVariable %in% names(DT)) {
      Tableau_Ready_DT$rse <- DT[, ..rseVariable]
    }

    #process and add tab
    Tableau_Ready_DT$tab <- rep(tabVariable, nrow(DT))
    Tableau_Ready_DT$tab <- as.character(Tableau_Ready_DT$tab)

    #process and add cat1
    Tableau_Ready_DT$cat1 <- rep(cat1Variable, nrow(DT))

    #process and add cat1_group
    Tableau_Ready_DT$cat1_group <- rep(cat1_groupVariable, nrow(DT))

    #process and add cat1_varname
    Tableau_Ready_DT$cat1_varname <- rep(cat1_varnameVariable, nrow(DT))

    #process and add cat1_group_alias
    Tableau_Ready_DT$cat1_group_alias <- rep(cat1_group_aliasVariable, nrow(DT))

    #process and add cat2
    Tableau_Ready_DT$cat2 <- rep(cat2Variable, nrow(DT))
    Tableau_Ready_DT$cat2 <- as.character(Tableau_Ready_DT$cat2)

    #process and add cat2_group
    Tableau_Ready_DT$cat2_group <- rep(cat2_groupVariable, nrow(DT))
    Tableau_Ready_DT$cat2_group <- as.character(Tableau_Ready_DT$cat2_group)

    #process and add cat2_varname
    Tableau_Ready_DT$cat2_varname <- rep(cat2_varnameVariable, nrow(DT))
    Tableau_Ready_DT$cat2_varname <- as.character(Tableau_Ready_DT$cat2_varname)

    #process and add cat2_group_alias
    Tableau_Ready_DT$cat2_group_alias <- rep(cat2_group_aliasVariable, nrow(DT))
    Tableau_Ready_DT$cat2_group_alias <- as.character(Tableau_Ready_DT$cat2_group_alias)

    #process and add data_source
    Tableau_Ready_DT$data_source <- rep(data_sourceVariable, nrow(DT))
    Tableau_Ready_DT$data_source <- as.character(Tableau_Ready_DT$data_source)

    #process and add run_date
    Tableau_Ready_DT$run_date <- rep(run_dateVariable, nrow(DT))

    #process and add comparison_with_kc
    Tableau_Ready_DT$comparison_with_kc <- rep(comparison_with_KCVariable, nrow(DT))
    Tableau_Ready_DT$comparison_with_kc <- as.character(Tableau_Ready_DT$comparison_with_kc)

    #process and add significance
    Tableau_Ready_DT$significance <- rep(significanceVariable, nrow(DT))
    Tableau_Ready_DT$significance <- as.character(Tableau_Ready_DT$significance)

    return(Tableau_Ready_DT)
  }

  APDE_TRO_KCCompare <- function(DT) {
    #takes a properly formatted tableau ready data.table
    #returns the same data.table with comparison to KC for each item
    #NOTE currently only check cat1 and cat1group, doesn't support bivariates yet

    DT$comparison_with_kc <- "no different"
    for(indicator in unique(DT$indicator_key)) {
      #for each indicator
      for(categorical in unique(DT[indicator_key == indicator,]$cat1_varname)) {
        #for each bivariate
        for(group in unique(DT[indicator_key == indicator & cat1_varname == categorical,]$cat1_group))
          #for each bivariate group
          for(ayear in unique(DT[indicator_key == indicator & cat1_varname == categorical & cat1_group == group,]$year)){
            #for each year
            if(!is.na(DT[indicator_key == indicator & cat1_group == "King County" & year == ayear & tab == "trends",]$result)) {
              #if not an NA result, append description relative to KC wide
              if(DT[indicator_key == indicator & cat1_varname == categorical & cat1_group == group & year == ayear & tab == "trends",]$upper_bound <
                 DT[indicator_key == indicator & cat1_group == "King County" & year == ayear & tab == "trends",]$lower_bound) {
                DT[indicator_key == indicator & cat1_varname == categorical & cat1_group == group & year == ayear & tab == "trends",]$comparison_with_kc <- "lower"
              }
              if(DT[indicator_key == indicator & cat1_varname == categorical & cat1_group == group & year == ayear & tab == "trends",]$lower_bound >
                 DT[indicator_key == indicator & cat1_group == "King County" & year == ayear & tab == "trends",]$upper_bound) {
                DT[indicator_key == indicator & cat1_varname == categorical & cat1_group == group & year == ayear & tab == "trends",]$comparison_with_kc <- "higher"
              }

            } else if(!is.na(DT[indicator_key == indicator & cat1_varname == categorical & cat1_group == group & year == ayear & tab == "trends",]$result)) {
              print("warning, missing KC but not missing bivariate")
            }
          }
      }
    }
    return(DT)
  }


  ########################################
  ########################################
  ########USAGE###########################
  ########################################
  ########################################



  ###################################
  #test call to trends function to generate list of DT's containing calc output
  #this returns  calc output for all "trends" that then need to be formated
  trend_result_list_of_lists <- APDE_CHI_TRO_time_trend_analysis(data, variables, timeTrendBivariables)

  #to format this, we need to walk through the resulting data structure. The structure is 2 dimensional matrix of data frames.
  #the first dimension is the primary variable, the second dimension is the bivariate
  #we walk through each data table, clean it as necessary, and the reparameterize it for tableau using the "APDE_TRO_FORMATING()" helper function


  #trend_resultunlist_backup <- trend_resultunlist

  #approach for walking through data structure and updating to final format
  remove(returnDF)


  for(listofDT in trend_result_list_of_lists) {

    for(DT in listofDT) {

      #create temporary DT to work with
      temp <- DT

      #temp <- trend_resultunlist[[1]][[1]] #working in loop ver

      #get category 1 variable, which is the name of the variable column, which happens to be first variable in calc returned DT
      variableNameLookup <- names(temp)[1]
      #remove negation of binary observations
      if(any(unlist(temp[, 1]) %in% 1)) temp =  temp[get(variableNameLookup) == 1,]

      #remove obvious NA's
      temp <- temp[!is.na(get(variableNameLookup))]

      #create variables to pass to table function
      ####### this only works with provided metadata table.
      variableValueLookup <- unique(unlist(temp[,1]))
      #if(length(variableValuelookup) !=1) stop("unexpectedly have too many 'indicator key' values")

      for(singleVariableValueLookup in variableValueLookup) {
        tempCat <- meta[varname == variableNameLookup & gval == singleVariableValueLookup,]$cat
        tempCatGroup <- meta[varname == variableNameLookup & gval == singleVariableValueLookup,]$group

        tempCatGroupAlias <- meta[varname == variableNameLookup & gval == singleVariableValueLookup]$group_alias

        names(temp)[1] <- "cat1_group"


        formatedOutput <- APDE_TRO_FORMATING(temp[cat1_group == singleVariableValueLookup,],
                                             "chi_year",
                                             "variable",
                                             "mean",
                                             "numerator",
                                             "denominator",
                                             "mean_se",
                                             "mean_lower",
                                             "mean_upper",
                                             "rse",
                                             "trends",
                                             tempCat,
                                             tempCatGroup,
                                             variableNameLookup,
                                             tempCatGroupAlias,
                                             NA,
                                             NA,
                                             NA,
                                             NA,
                                             "hys",
                                             Sys.Date(),
                                             #"no different",
                                             NA,
                                             NA)
                                             #"*")
        if(nrow(temp[cat1_group == singleVariableValueLookup,]) == 0) {
          print("input was 0 length")
          print(temp)
        }

        #setnames(temp, names(temp)[1], "cat1_group")
        if(exists("returnDF")) {
          returnDF <- rbind(returnDF, formatedOutput)
        } else {
          returnDF <- formatedOutput
        }

      }
    }
  }


  #assign comparison to KC
  returnDF <- APDE_TRO_KCCompare(returnDF)



  #compare output of original and new code

  test <- returnDF
  test2 <- FormatedAnalysisOriginal[tab=="trends",]
  test$missmatch <- 0
  test2$missmatch <- 0

  countdif <- 0
  countsame <- 0
  for(indicator in unique(test$indicator_key)) {
    #for each indicator
    for(categorical in unique(test[indicator_key == indicator,]$cat1_varname)) {
      #for each bivariate
      for(group in unique(test[indicator_key == indicator & cat1_varname == categorical,]$cat1_group))
        #for each bivariate group
        for(ayear in unique(test[indicator_key == indicator & cat1_varname == categorical & cat1_group == group,]$year)){
          #for each year
          if(!is.na(test[indicator_key == indicator & cat1_group == "King County" & year == ayear & tab == "trends",]$result)) {
            #if not an NA result, append description relative to KC wide
            if(!identical(test[indicator_key == indicator & cat1_varname == categorical & cat1_group == group & year == ayear & tab == "trends", !c("comparison_with_kc", "run_date")], test2[indicator_key == indicator & cat1_varname == categorical & cat1_group == group & year == ayear & tab == "trends", !c("comparison_with_kc", "run_date")])) {
              #note, the above text ignores run_date and comparison_with_kc. comaprison ignored because the original code seems to implement this incorrectly.
              countdif <- countdif + 1

              test[indicator_key == indicator & cat1_varname == categorical & cat1_group == group & year == ayear & tab == "trends",]$missmatch <- 1
              test2[indicator_key == indicator & cat1_varname == categorical & cat1_group == group & year == ayear & tab == "trends",]$missmatch <- 1
            } else {
              countsame <- countsame + 1
            }
          }

        }
    }
  }
  print(countdif)
  print(countsame)


  #####calculating _kingcounty" tab#####




  ####calculating demgroups tab####




  ####calculating crosstabs tab####








  ########################################
  ########################################
  ########OLDstuff to fix#################
  ########################################
  ########################################


  ##create county wide point estimate of all variable
  #calculate point estimate
  for(variable in variables) {
    ##Identify years of output and generate year tag for TRO format
    #what two cycles do the data represent (even if in some combos this is not true)?
    grp_yrs = unique(data[!is.na(get(variable)), .SD, .SDcols = c(variables, 'chi_year')][, chi_year])
    grp_yrs = grp_yrs[(length(grp_yrs)-1):length(grp_yrs)]
    #create a human readable description of the data range of the current point estimates
    gyl = paste(grp_yrs, collapse = ' & ')
  }

  #receives a data set and a variable name indicating sequence
  #returns a vector of unique time items in sequence for the variable of interest
  .find_number_of_indicator_observations <- function(data, ivar, t_var) {
    data <- as.data.table(data)
    DT <- data[!is.na(get(ivar)), .SD, .SDcols = c(ivar, t_var)]
    time_var_list <- unlist(unique(DT[,..t_var]))
    return(time_var_list)

  }

  APDE_CHI_TRO_crosstabular_analysis <- function(data, indicators, variables1, variables2, binary_variables, expected_series_length = 1) {
    #failure checks
    if(nrow(variable1) != nrow(variable2)) { stop("list of crosstabular variables must be symetrical")}

    #preparing output datatable

    calculate_a_crosstab <- function(indicator) {
      rads::calc(data, what = indicator, where =)

    }

    #notes on future https://cran.r-project.org/web/packages/future/vignettes/future-3-topologies.html
    future.apply::future_lapply()
    cts = lapply(seq_len(nrow(ctabs)), function(x){
      by1 = ctabs[x, cat1_varname]
      by2 = ctabs[x, cat2_varname]

      r= rads::calc(hys, what = v, where = chi_year %in% grp_yrs, by = c(by1, by2), metrics = c('mean', 'denominator', 'numerator', 'rse'), proportion = T, time_var = 'chi_year')

      #cat1 fixes
      setnames(r, by1, 'cat1_group') #will need to be updated to something human readable
      if(any(r[, cat1_group] %in% 1)) r=  r[cat1_group == 1]
      r[, cat1_varname :=by1]
      r= r[!is.na(cat1_group)]

      #cat2 fixes
      setnames(r, by2, 'cat2_group') #will need to be updated to something human readable
      if(any(r[, cat2_group] %in% 1)) r=  r[cat2_group == 1]
      r[, cat2_varname :=by2]
      r= r[!is.na(cat2_group)]

      r[, chi_year := gyl]

      r


    })
    cts = rbindlist(cts)
    cts[, tab := 'crosstabs']

    #merge on cat information for cat1
    cts = merge(cts, meta, all.x = T, by.x = c('cat1_varname', 'cat1_group'), by.y = c('varname', 'gval'))
    stopifnot(all(cts[, good]))
    cts[, c('ng', 'good', 'cat1_group') := NULL]
    setnames(cts, c('cat', 'group', 'group_alias'), c('cat1', 'cat1_group', 'cat1_group_alias'))

    #merge on cat information for cat2
    cts = merge(cts, meta, all.x = T, by.x = c('cat2_varname', 'cat2_group'), by.y = c('varname', 'gval'))
    stopifnot(all(cts[, good]))
    cts[, c('ng', 'good', 'cat2_group') := NULL]
    setnames(cts, c('cat', 'group', 'group_alias'), c('cat2', 'cat2_group', 'cat2_group_alias'))


    #binary variables don't need to calculate their 0.
    #does removing the zeros before calc perform faster (probably)
    #a binary variable should check both sides and remove

    #for non trendline analyses, we should confirm number of time variables to calculate over and that enough data were provided

  }




}


