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

  #create crosstabs to be calculated. This uses all variables except geographies and crosstab on their own identity
  ctabs = CJ(cat1_varname = bys, cat2_varname = bys)
  ctabs = ctabs[cat1_varname != cat2_varname & cat1_varname != 'chi_geo_kc' & cat2_varname != 'chi_geo_kc']

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

      #generate rename variable "x" (by variable currently being manipulated) to "cat1_group"
      setnames(r, x, 'cat1_group') #will need to be updated to something human readable
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

  FormatedAnalysis = rbindlist(future_lapply(vars, function(v) domath(LineData, v, bys, ctabs, ttbys, meta)))
  return(FormatedAnalysis)

}


