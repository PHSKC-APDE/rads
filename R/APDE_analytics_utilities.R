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

    kc_comp = res[cat1_varname == 'chi_geo_kc', .(year, kcr = result, lower_bound_kc = lower_bound, upper_bound_kc = upper_bound)]

    #compare to KC
    res = merge(res, kc_comp, all.x = T, by = 'year')
    res[upper_bound < lower_bound_kc, comparison_with_kc := 'lower']
    res[lower_bound > upper_bound_kc, comparison_with_kc := 'upper']
    res[upper_bound >= lower_bound_kc | lower_bound <= upper_bound_kc, comparison_with_kc := 'no different']
    res[comparison_with_kc %in% c('higher', 'lower'), significance := '*' ]
    res[, c('kcr', 'lower_bound_kc', 'upper_bound_kc') := NULL]

    res[is.nan(result), result := NA ]

    return(res)
  }

  FormatedAnalysisOriginal = rbindlist(future.apply::future_lapply(vars, function(v) domath(hys, v, bys, ctabs, ttbys, meta)))



  ########assumptions of an APDE CHI TRO###################


  #########in function variables
  data <- hys
  variables <- vars
  bivariables <- bys
  crosstabs <- ctabs
  meta <- meta

  #List of needed functions:
  #Trendline calculator
  #Bivariate batcher
  #crosstab batcher

  APDE_TRO_FORMATING <- function(data, tab_var, cat1_var, cat1_group_var, cat2_var = NULL) {
    #PRACTICE< DETELE
    data <- calc_result
    tab_var <- "trends"
    cat1_group_var <- bivariable

        #create tab column
    data[, tab := tab_var]

    #turn the column identified as cat1_group into the data of a column named cat1_group
    setnames(data, cat1_var, "cat1_group")
    data[, cat1_varname := cat1_var]

    if(is.null(cat2_var)) {
      data[, c('cat2', 'cat2_group', 'cat2_varname', 'cat2_group_alias') := list(NA_character_, NA_character_, NA_character_, NA_character_) ]

    }


  }

  APDE_CHI_TRO_time_trend_analysis <- function(data, variables, bivariables, KC = TRUE, process = "calc") {
    #takes data set manageable by calc fuction, list of independent variables,
    Tableau_Ready_DT <- data.table::data.table("year" = as.character(),
                                               "indicator_key" = as.character(),
                                               "result" = as.numeric(),
                                               "numerator" = as.numeric(),
                                               "denominator" = as,integer(),
                                               "se" = as.numeric(),
                                               "lower_bound" = as.numeric(),
                                               "upper_bound" = as.numeric(),
                                               "rse" = as.numeric(),
                                               "tab" = as.character(),
                                               "cat1" = as.character(),
                                               "cat1_group" = as.character(),
                                               "cat1_varname" = as.character(),
                                               "cat1_group_alias" = as.character(),
                                               "cat2" = as.character(),
                                               "cat2_group" = as.character(),
                                               "cat2_varname" = as.character(),
                                               "cat2_group_alias" = as.character(),
                                               "data_source" = as.character(),
                                               "run_date" = as.character(),
                                               "comparison_with_kc" = as.character(),
                                               "significance" = as.character())


    #call defined process for each combination of variables and by variables across range of available timepoints
    .internal_time_trend_calc <- function(v, bivariables, time_var, data) {

      all_results <- rbindlist(future.apply::future_lapply(bivariables, function(bivariable) {
        calc_result = rads::calc(data, what = v, by = c(bivariable, time_var), metrics = c('mean', 'denominator', 'numerator', 'rse'), proportion = T)
        calc_result_backup <- calc_result
        #old, all internal, approach
        {
          setnames(calc_result, bivariable, "cat1_group")
          calc_result[, cat1_varname := bivariable]
        }
        calc_results_first <- calc_result

        calc_result <- calc_result_backup
        #external call approach
        calc_result <- APDE_TRO_FORMATING(calc_result, "trends", bivariable )

        if(any(calc_result[, cat1_group] %in% 1)) {
          calc_result <- calc_result[cat1_group == 1,]
        } else {
          calc_result <- calc_result[!is.na(cat1_group)]
        }


        return(tab_rdy_result)
      }))
      return(all_results)
    }

    test <- rbindlist(future.apply::future_lapply(variables, function(v) .internal_time_trend_calc(v, bivariables, "chi_year" ,data)))
    test2 <- APDE_TRO_FORMATING(test, "trends",)

  }
  {
    #note have historically always created a KC trend as well
    dgs = lapply(bys, function(x){

      if(x %in% ttbys){
        r2 = rads::calc(hys, what = v, by = c(x, 'chi_year'), metrics = c('mean', 'denominator', 'numerator', 'rse'), proportion = T)
        r2[, tab := 'trends']
        r = rbind(r, r2)
      }

      #generate rename variable "x" (by variable currently being manipulated) to "cat1_group"
      setnames(r2, x, 'cat1_group') #will need to be updated to something human readable
      if(any(r2[, cat1_group] %in% 1)) r2=  r2[cat1_group == 1] ##???? if any cat1_group were identified, remove anything that isn't

      #create cat1_varname variable using the by variable (bys) identified
      r2[, cat1_varname := x]
      r2= r2[!is.na(cat1_group)]

      r

    })
    #dgs = rbindlist(dgs)
    r2[, c('cat2', 'cat2_group', 'cat2_varname', 'cat2_group_alias') := list(NA_character_, NA_character_, NA_character_, NA_character_) ]
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


  }

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
    Tableau_Ready_DT <- data.table::data.table("year" = as.character(),
                                               "indicator_key" = as.character(),
                                               "result" = as.numeric(),
                                               "numerator" = as.numeric(),
                                               "denominator" = as,integer(),
                                               "se" = as.numeric(),
                                               "lower_bound" = as.numeric(),
                                               "upper_bound" = as.numeric(),
                                               "rse" = as.numeric(),
                                               "tab" = as.character(),
                                               "cat1" = as.character(),
                                               "cat1_group" = as.character(),
                                               "cat1_varname" = as.character(),
                                               "cat1_group_alias" = as.character(),
                                               "cat2" = as.character(),
                                               "cat2_group" = as.character(),
                                               "cat2_varname" = as.character(),
                                               "cat2_group_alias" = as.character(),
                                               "data_source" = as.character(),
                                               "run_date" = as.character(),
                                               "comparison_with_kc" = as.character(),
                                               "significance" = as.character())

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



  calculate_King_County <- function(x, v, years) {
    kc = rads::calc(x, what = v, where = chi_year %in% years, metrics = c('mean', 'denominator', 'numerator', 'rse'), proportion = T, time_var = 'chi_year')
    #assign metadata
    kc[, tab := '_kingcounty']
    kc[, cat1 := 'King County']
    kc[, cat1_group := 'King County']
    kc[, cat1_varname := 'chi_geo_kc']
    kc[, cat1_group_alias := 'King County']
    kc[, c('cat2', 'cat2_group', 'cat2_varname', 'cat2_group_alias') := list(cat1, cat1_group, cat1_varname, cat1_group_alias) ]
    kc[, chi_year := gyl]

    return(kc)
  }


  #compare output of original and new code
  if(identical(FormatedAnalysisOriginal, FormatedAnalysis)) {
    stop("function not performing as expected")
  }

  return(FormatedAnalysis)



}


