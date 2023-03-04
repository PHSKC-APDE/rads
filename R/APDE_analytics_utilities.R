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
  crosstabvariables <- ctabs

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

  APDE_CHI_TRO_kingcounty_analysis <- function(data, variables) {
    #return a data structure containing king county wide calc results for a list of variables grouped by dates
    .internal_kingcounty_calc <- function(v, data) {
      #identifies latest 2 years in current data set and assumes they are the years to use for our estimates
      grp_yrs = unique(data[!is.na(get(v)), .SD, .SDcols = c(v, 'chi_year')][, chi_year])
      grp_yrs = grp_yrs[(length(grp_yrs)-1):length(grp_yrs)]
      ##create a human readable description of the data range of the current point estimates
      gyl = paste(grp_yrs, collapse = ' & ')

      ##create county wide point estimate of the current variable
      #calculate point estimate
      DT = rads::calc(data, what = v, where = chi_year %in% grp_yrs, metrics = c('mean', 'denominator', 'numerator', 'rse'), proportion = T, time_var = 'chi_year')

      formatedOutput <- APDE_TRO_FORMATING(DT,
                                           "chi_year",
                                           "variable",
                                           "mean",
                                           "numerator",
                                           "denominator",
                                           "mean_se",
                                           "mean_lower",
                                           "mean_upper",
                                           "rse",
                                           "_kingcounty",
                                           "King County",
                                           "King County",
                                           "chi_geo_kc",
                                           "King County",
                                           "King County",
                                           "King County",
                                           "chi_geo_kc",
                                           "King County",
                                           "hys",
                                           Sys.Date(),
                                           NA,
                                           NA)

    }

    ListOfDT <- future.apply::future_lapply(variables, function(v) .internal_kingcounty_calc(v, data))

    returner <- do.call("rbind", ListOfDT)


    returner
  }

  APDE_CHI_TRO_demgroups_analysis <- function(data, variables, bivariables) {
    #return a data structure containing bivariate analysis
    .internal_demgroups_calc <- function(v, bivariables, data) {
      #identifies latest 2 years in current data set and assumes they are the years to use for our estimates
      grp_yrs = unique(data[!is.na(get(v)), .SD, .SDcols = c(v, 'chi_year')][, chi_year])

      grp_yrs = grp_yrs[(length(grp_yrs)-1):length(grp_yrs)]
      ##create a human readable description of the data range of the current point estimates
      gyl = paste(grp_yrs, collapse = ' & ')

      ##create county wide point estimate of the current variable
      #calculate point estimate
      DT = rads::calc(data, what = v, by = bivariables, where = chi_year %in% grp_yrs, metrics = c('mean', 'denominator', 'numerator', 'rse'), proportion = T, time_var = 'chi_year')

      formatedOutput <- APDE_TRO_FORMATING(DT,
                                           "chi_year",
                                           "variable",
                                           "mean",
                                           "numerator",
                                           "denominator",
                                           "mean_se",
                                           "mean_lower",
                                           "mean_upper",
                                           "rse",
                                           "demgroups",
                                           "King County",
                                           "King County",
                                           "chi_geo_kc",
                                           "King County",
                                           "King County",
                                           "King County",
                                           "chi_geo_kc",
                                           "King County",
                                           "hys",
                                           Sys.Date(),
                                           NA,
                                           NA)

    }

    ListOfDT <- future.apply::future_lapply(variables, function(v) .internal_kingcounty_calc(v, data))

    returner <- do.call("rbind", ListOfDT)


    returner
  }

  APDE_CHI_TRO_analysis <- function(data, variables, bivariables = NA, meta, type = "") {
    analysisOptions <- c("_wastate","_kingcounty","bigcities","demgroups","crosstabs","trends")
    if(!(type %in% analysisOptions)) {
      stop("analysis type is required. Options are: \"_wastate\",\"kingcounty\",\"bigcities\",\"demgroups\",\"crosstabs\",\"trends\".")
    }
    if(!(type %in% c("_kingcounty")) & is.na(bivariables)) {
      stop(" \"_wastate\", \"bigcities\", \"demgroups\", \"crosstabs\", \"trends\" analysees require one or more bivariables.")
    }

    #return a data structure containing calc results for each combination of variables and bivariables across range of available timepoints
    #
    .internal_trends_calc <- function(v, bivariables, data) {

      all_calc_results <- (future.apply::future_lapply(bivariables, function(bivariable) {
        calc_result = rads::calc(data, what = v, by = c(bivariable, "chi_year"), metrics = c('mean', 'denominator', 'numerator', 'rse'), proportion = T)
        calc_result
      }))
      return(all_calc_results)
    }

    .internal_demgroups_calc <- function(v, bivariables, data) {


      all_calc_results <- (future.apply::future_lapply(bivariables, function(bivariable) {
        #identifies latest 2 years in current data set and assumes they are the years to use for our estimates
        grp_yrs = unique(data[!is.na(get(v)), .SD, .SDcols = c(v, 'chi_year')][, chi_year])

        grp_yrs = grp_yrs[(length(grp_yrs)-1):length(grp_yrs)]
        ##create a human readable description of the data range of the current point estimates
        gyl = paste(grp_yrs, collapse = ' & ')

        ##create county wide point estimate of the current variable
        #calculate point estimate
        calc_result = rads::calc(data, what = v, by = bivariables, where = chi_year %in% grp_yrs, metrics = c('mean', 'denominator', 'numerator', 'rse'), proportion = T, time_var = 'chi_year')
        calc_result

      }))
      return(all_calc_results)
    }

    if(type == "trends") {
      calc_result_list_of_lists <- future.apply::future_lapply(variables, function(v) .internal_time_trend_calc(v, bivariables, data))
      tab <- "trends"
    } else if(type == "demgroups") {
      calc_result_list_of_lists <- future.apply::future_lapply(variables, function(v) .internal_demgroup_calc(v, bivariables, data))
      tab <- "demgroups"
    }

    for(listofDT in calc_result_list_of_lists) {

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
        temp

        for(singleVariableValueLookup in variableValueLookup) {
          tempCat <- meta[varname == variableNameLookup & gval == singleVariableValueLookup,]$cat
          tempCatGroup <- meta[varname == variableNameLookup & gval == singleVariableValueLookup,]$group

          tempCatGroupAlias <- meta[varname == variableNameLookup & gval == singleVariableValueLookup]$group_alias

          names(temp)[1] <- "cat1_group"


          formatedOutput <- APDE_TRO_FORMATING(temp[cat1_group == singleVariableValueLookup,],
                                               NA,
                                               tab,
                                               "chi_year",
                                               NA,
                                               "cat1_group",
                                               NA,
                                               "variable",
                                               NA,
                                               NA,
                                               NA,
                                               NA,
                                               "mean",
                                               "mean_lower",
                                               "mean_upper",
                                               "mean_se",
                                               "rse",
                                               NA,
                                               "NULL",
                                               NA,
                                               NA,
                                               NA,
                                               "numerator",
                                               "denominator",
                                               "1",
                                               NA,
                                               Sys.Date())


          if(nrow(temp[cat1_group == singleVariableValueLookup,]) == 0) {
            print("input was 0 length")
            print(temp)
          }

          if(exists("returner")) {
            returner <- rbind(returner, formatedOutput)
          } else {
            returner <- formatedOutput
          }

        }
      }
    }
    returner

  }


  APDE_TRO_FORMATING <- function(DT,
                                 data_sourceVariable = NA,
                                 indicatorKeyVariable = NA,
                                 tabVariable = NA,
                                 yearVariable = NA,
                                 cat1Variable = NA,
                                 cat1_groupVariable = NA,
                                 cat1_group_aliasVariable = NA,
                                 cat1_varnameVariable = NA,
                                 cat2Variable = NA,
                                 cat2_groupVariable = NA,
                                 cat2_group_aliasVariable = NA,
                                 cat2_varnameVariable = NA,
                                 resultVariable = NA,
                                 lowerBoundVariable = NA,
                                 upperBoundVariable = NA,
                                 seVariable = NA,
                                 rseVariable = NA,
                                 comparison_with_KCVariable = NA,
                                 time_trendsVariable = NA, #be sure this saves as provided string, should be "NULL" to spec.
                                 significanceVariable = NA,
                                 cautionVariable = NA,
                                 suppressionVariable = NA,
                                 numeratorVariable = NA,
                                 denominatorVariable = NA,
                                 chiVariable = NA,
                                 source_dateVariable = NA,
                                 run_dateVariable = NA) {
    #Will check if value exists in the names of a dataframe, or a new value is provided
    #if a value is a name, it will rename that vector in the dataframe to the standard name (order matters) and apply any known cleaning
    #otherwise, will create the needed vector with the standard name and give all instances of it the value provided

    #make target datatable of correct size
    outputNames <- c("data_source",
                     "indicator_key",
                     "tab",
                     "year",
                     "cat1",
                     "cat1_group",
                     "cat1_group_alias",
                     "cat1_varname",
                     "cat2",
                     "cat2_group",
                     "cat2_group_alias",
                     "cat2_varname",
                     "result",
                     "lower_bound",
                     "upper_bound",
                     "se",
                     "rse",
                     "comparison_with_kc",
                     "time_trends",
                     "significance",
                     "caution",
                     "suppression",
                     "numerator",
                     "denominator",
                     "chi",
                     "source_date",
                     "run_date")
    Tableau_Ready_DT <- matrix(rep(NA, time = length(outputNames)*nrow(DT)), ncol = length(outputNames), byrow= TRUE)
    colnames(Tableau_Ready_DT) <- outputNames
    Tableau_Ready_DT <- data.table::as.data.table(Tableau_Ready_DT)

    #process and add data source
    if(data_sourceVariable %in% names(DT)) {
      Tableau_Ready_DT$data_source <- DT[, ..data_sourceVariable]
    }
    Tableau_Ready_DT[is.nan(data_sourceVariable), data_sourceVariable := NA]

    #process and add indicator_key
    if(indicatorKeyVariable %in% names(DT)) {
      Tableau_Ready_DT$indicator_key <- DT[, ..indicatorKeyVariable]
    }
    Tableau_Ready_DT$indicator_key <- as.character(Tableau_Ready_DT$indicator_key)

    #process and add tab
    Tableau_Ready_DT$tab <- rep(tabVariable, nrow(DT))
    Tableau_Ready_DT$tab <- as.character(Tableau_Ready_DT$tab)

        #process and add year variable
    if(yearVariable %in% names(DT)) {
      if(!grepl("&", kc$chi_year)) {
        #test if dates are default calc output (only commas separating multiple dates) and if so, add "&" after last comma if any
        Tableau_Ready_DT$year <- sub(",([^,]*)$", ", &\\1", DT[, ..yearVariable])
      } else {
        Tableau_Ready_DT$year <- DT[, ..yearVariable]
      }
    } else {
      #if the year is not a variable name, use the passed value to fill in the year
      Tableau_Ready_DT$year <- yearVariable
    }
    Tableau_Ready_DT$year <- as.character(Tableau_Ready_DT$year)

    #process and add cat1
    Tableau_Ready_DT$cat1 <- rep(cat1Variable, nrow(DT))

    #process and add cat1_group
    Tableau_Ready_DT$cat1_group <- rep(cat1_groupVariable, nrow(DT))

    #process and add cat1_group_alias
    Tableau_Ready_DT$cat1_group_alias <- rep(cat1_group_aliasVariable, nrow(DT))

    #process and add cat1_varname
    Tableau_Ready_DT$cat1_varname <- rep(cat1_varnameVariable, nrow(DT))

    #process and add cat2
    Tableau_Ready_DT$cat2 <- rep(cat2Variable, nrow(DT))
    Tableau_Ready_DT$cat2 <- as.character(Tableau_Ready_DT$cat2)

    #process and add cat2_group
    Tableau_Ready_DT$cat2_group <- rep(cat2_groupVariable, nrow(DT))
    Tableau_Ready_DT$cat2_group <- as.character(Tableau_Ready_DT$cat2_group)

    #process and add cat2_group_alias
    Tableau_Ready_DT$cat2_group_alias <- rep(cat2_group_aliasVariable, nrow(DT))
    Tableau_Ready_DT$cat2_group_alias <- as.character(Tableau_Ready_DT$cat2_group_alias)

    #process and add cat2_varname
    Tableau_Ready_DT$cat2_varname <- rep(cat2_varnameVariable, nrow(DT))
    Tableau_Ready_DT$cat2_varname <- as.character(Tableau_Ready_DT$cat2_varname)

    #process and add result
    if(resultVariable %in% names(DT)) {
      Tableau_Ready_DT$result <- DT[, ..resultVariable]
    }
    Tableau_Ready_DT[is.nan(result), result := NA]


    #process and add lower_bound
    if(lowerBoundVariable %in% names(DT)) {
      Tableau_Ready_DT$lower_bound <- DT[, ..lowerBoundVariable]
    }

    #process and add upper_bound
    if(upperBoundVariable %in% names(DT)) {
      Tableau_Ready_DT$upper_bound <- DT[, ..upperBoundVariable]
    }

    #process and add se
    if(seVariable %in% names(DT)) {
      Tableau_Ready_DT$se <- DT[, ..seVariable]
    }

    #process and add rse
    if(rseVariable %in% names(DT)) {
      Tableau_Ready_DT$rse <- DT[, ..rseVariable]
    }

    #process and add comparison_with_kc
    Tableau_Ready_DT$comparison_with_kc <- rep(comparison_with_KCVariable, nrow(DT))
    Tableau_Ready_DT$comparison_with_kc <- as.character(Tableau_Ready_DT$comparison_with_kc)

    #process and add timetrends indicator
    if(time_trendsVariable %in% names(DT)) {

    } else {
      Tableau_Ready_DT$time_trends <- rep("NULL", nrow(DT))
      Tableau_Ready_DT$time_trends <- as.character(Tableau_Ready_DT$time_trends)
    }

    #process and add significance
    Tableau_Ready_DT$significance <- rep(significanceVariable, nrow(DT))
    Tableau_Ready_DT$significance <- as.character(Tableau_Ready_DT$significance)

    #process and add caution var
    Tableau_Ready_DT$caution <- rep(cautionVariable, nrow(DT))
    Tableau_Ready_DT$caution <- as.character(Tableau_Ready_DT$caution)

    #process and add suppression indicator var
    Tableau_Ready_DT$suppression <- rep(suppressionVariable, nrow(DT))
    Tableau_Ready_DT$suppression <- as.character(Tableau_Ready_DT$suppression)

    #process and add numerator
    if(numeratorVariable %in% names(DT)) {
      Tableau_Ready_DT$numerator <- DT[, ..numeratorVariable]
    }

    #process and add denominator
    if(denominatorVariable %in% names(DT)) {
      Tableau_Ready_DT$denominator <- DT[, ..denominatorVariable]
    }

    #process and add chi indicator
    Tableau_Ready_DT$chi <- rep(chiVariable, nrow(DT))
    Tableau_Ready_DT$chi <- as.character(Tableau_Ready_DT$chi)

    #process and add date_source
    Tableau_Ready_DT$date_source <- rep(date_sourceVariable, nrow(DT))
    Tableau_Ready_DT$date_source <- as.character(Tableau_Ready_DT$date_source)

    #process and add run_date
    Tableau_Ready_DT$run_date <- rep(run_dateVariable, nrow(DT))




    return(Tableau_Ready_DT)
  }


  ########################################
  ########################################
  ########USAGE###########################
  ########################################
  ########################################


  ####calculating time trends######
  returnDF_TT <- APDE_CHI_TRO_analysis(data, variables, timeTrendBivariables, meta,type = "trends" )

  ####calculating demgroups tab####
  returnDF_DG <- APDE_CHI_TRO_analysis(data, variables, bivariables, meta, type = "demgroups")

  ####calculating crosstabs tab####


  ####calculating _kingcounty" tab#####
  returnDF_KC <- APDE_CHI_TRO_kingcounty_analysis(data, variables)


  returnDF <- rbind(returnDF_TT, returnDF_DG, returnDF_KC)


  #compare output of original and new code
  returnDF <- compare_estimate(mydt = returnDF,
                           id_vars = c("indicator_key", "year"),
                           key_where = cat1_group ==  "King County" & tab != "crosstabs",

                           new_col = "comparison_with_kc",
                           tidy = TRUE)
}

