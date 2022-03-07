#' Generate a standard life table
#'
#' @description
#' Generates a standard life table given a data.frame or data.table with basic
#' attributes.
#'
#' @references
#' Chiang, Chin Long & World Health Organization. (1979).
#' Life table and mortality analysis / Chin Long Chiang.
#' World Health Organization. https://apps.who.int/iris/handle/10665/62916
#'
#' Silcocks PB, Jenner DA, Reza R. Life expectancy as a summary of mortality in
#' a population: Statistical considerations and suitability for use by health
#' authorities. J Epidemiol Community Health 55(1):38–43. 2001
#'
#' @param mydt a data.table or data.frame. Must contain aggregated deaths and
#' corresponding populations, as well as the age interval and the average
#' fraction of years lived in the interval by those who die in the interval.
#' @param age_interval character vector of length one identifying a column
#' specifying the beginning and end of each age interval separated by a hyphen.
#' Note, the start of each interval should be the end of the previous
#' interval, e.g., '5-10', '10-15', '15-20', etc. Think of this as short-hand
#' for [5, 10), [10, 15), [15, 20). The final interval should be open ended with
#' the starting value followed by a '+' (e.g., '85+', '90+', etc.). The maximum
#' age cannot exceed 100. Leave the value blank (i.e., NA) for the total deaths
#' at an unknown age. These deaths will be distributed proportionately over the
#' other age groups.
#' @param mydeaths character vector of length one identifying a numeric column
#' with the total deaths for the given age interval in the given year(s).
#' @param mypop character vector of length one identifying a numeric column
#' with the total population in the age intervals corresponding to mydeaths.
#' This is technically the mid-year population. In practice we usually
#' use OFM population estimates.
#' @param myfrac character vector of length one identifying a numeric column
#' with the average proportion of the interval lived by those who died in the
#' interval. For example, if those who died in '80-85' lived an average of 1000
#' days past their 80th birthday, myfrac would be 0.54 (1000/(365.25*5)).
#' @param ci numeric of length one. Must be a number between 0 and 1, default is
#' 0.95.
#'
#' @return a data.table with the pre-existing columns plus the
#' standard life table columns
#' @details
#' The function returns the following life table columns:
#'
#' - mx: age interval specific death rate
#'
#' - qx: probability of dying in the age interval
#'
#' - lx: # of (theoretical) persons alive at the start of the age interval
#'
#' - dx: # of deaths during the age interval
#'
#' - ax: average fraction of the interval lived by those who died in the interval
#'
#' - Lx: total person years lived in the age interval
#'
#' - Tx: total person years lived beyond the start of the age interval
#'
#' - ex: expectation of life (a.k.a., life expectancy) at the start of the age
#' interval
#'
#' @export
#' @name life_table
#' @examples
#'  temp1 <- data.table::data.table(
#'           ages = c("0-1", "1-2", "2-3", "3-4", "4-5", "5-10", "10-15",
#'                    "15-20", "20+"),
#'           dead = c(391, 30, 17, 14, 8, 41, 73, 202, 28919),
#'           population = c(25640, 25104, 25507, 24998, 24038, 131202, 128490,
#'                        127951, 1747871),
#'           fraction = c(0.09, rep(0.5, 8)))
#'  temp1[]
#'  temp2 <- life_table(mydt = temp1,
#'                       age_interval = "ages",
#'                       mydeaths = "dead",
#'                       mypop = "population",
#'                       myfrac = "fraction")
#'  temp2[]
#'
#' @import data.table
#' @importFrom stats qnorm
#'

life_table <- function(mydt = NULL,
                       age_interval = "age_interval",
                       mydeaths = "deaths",
                       mypop = "pop",
                       myfrac = "ax",
                       ci = 0.95){

  # Global variables used by data.table declared as NULL here to play nice with devtools::check() ----
    istart <- iend <- irank <- ilength <- mx <- qx <- lx <- dx <- Lx <- Tx <- ex <- NULL
    ax <- mx_upper <- mx_lower <- mx_se <- qnorm <- qx_variance <- px_variance <- NULL
    ex_temp <- ex_temp_cumsum <- ex_variance <- ex_se <- ex_lower <- ex_upper <- NULL
    ordered_cols <- NULL

  # Get name of the data.frame/data.table ----
    mydtname <- deparse(substitute(mydt))

  # Check arguments ----
      if(!is.null(mydt)){
        if(!is.data.frame(mydt)){
          stop("'mydt' must be the name of a data.frame or data.table")
        }
        if(is.data.frame(mydt) && !data.table::is.data.table(mydt)){
          data.table::setDT(mydt)
        }
      } else {stop("'mydt', the name of a data.frame or data.table with population and death data, must be specified")}

      if(!age_interval %in% names(mydt)){
        stop(paste0("'age_interval' (", age_interval, ") is not the name of a column in 'mydt'."))}
      if(nrow(mydt[!is.na(age_interval)]) != nrow(mydt[!is.na(age_interval) & get(age_interval) %like% "[0-9]-[0-9]|[0-9]\\+"])){
        stop(paste0("The values in 'age_interval' (i.e., ", age_interval, ") must be in the form #-# or #+, e.g., '10-15' or '85+'"))}
      if(nrow(mydt[get(age_interval) %like% "[0-9]\\+"]) != 1){
        stop(paste0("The final age in 'age_interval' (i.e., ", age_interval, ") must be in the form #+, e.g., '85+' or '90+'"))}
      if(nrow(mydt) != length(unique(mydt[[age_interval]]))){stop(paste0("The values in 'age_interval' (i.e., ", age_interval, ") must be unique"))}

      if(!mypop %in% names(mydt)){
        stop(paste0("'mypop' (", mypop, ") is not the name of a column in 'mydt'."))}
      if(!is.numeric(mydt[[mypop]])){
        stop(paste0("'mypop' (i.e., ", mypop, ") must be of class == numeric"))}

      if(!mydeaths %in% names(mydt)){
        stop(paste0("'mydeaths' (", mydeaths, ") is not the name of a column in 'mydt'."))}
      if(!is.numeric(mydt[[mydeaths]])){
        stop(paste0("'mydeaths' (i.e., ", mydeaths, ") must be of class == numeric"))}
      if( nrow(mydt[is.na(age_interval) & !is.na(mydeaths)]) > 1 ){
        stop(paste0("'mydt' (i.e., ", mydtname, ") can only have 1 row with deaths where the age_interval is NA."))}

      if(!myfrac %in% names(mydt)){
        stop(paste0("'myfrac' (", myfrac, ") is not the name of a column in 'mydt'."))}
      if(!is.numeric(mydt[[myfrac]])){
        stop(paste0("'myfrac' (i.e.,", myfrac, ") must be of class == numeric"))}
      if(nrow(mydt[!get(myfrac) %between% 0:1]) > 0){
        stop(paste0("'myfrac' (i.e., ", ax, ") should be a proportion (i.e., it must be between 0 & 1)"))}

      if( !class(ci) %in% c("numeric")){
        stop(paste0("`ci` (", ci, ") should be a two digit decimal between 0.00 & 0.99"))}
      if(nrow(mydt[!ci %between% 0:1]) > 0){
        stop(paste0("`ci` (", ci, ") should be a two digit decimal between 0.00 & 0.99"))}

  # Copy mydt to prevent changing original by reference ----
    mydt <- copy(mydt)

  # Get name of pre-existing variables ----
      orig_cols <- copy(names(mydt))

  # Split age_interval to create intervals ----
    mydt[,c("istart", "iend") := tstrsplit(gsub("\\+", "", get(age_interval)), "-")]
    mydt[, c("istart", "iend") := lapply(.SD, as.integer), .SDcols = c("istart", "iend")]
    mydt[, irank := rank(istart)]
    setorder(mydt, irank) # critical that table is sorted from youngest to oldest
    mydt[, ilength := iend - istart]
    mydt[is.na(iend), ilength := 100-istart] # adjustment for final interval

  # Distribute deaths with unknown age proportionately among deaths with known ages ----
    if(nrow(mydt[is.na(age_interval) & !is.na(mydeaths)]) > 0){
        deaths.unk.age <- mydt[is.na(get(age_interval))][[mydeaths]] # count num of deaths with unknown age
        mydt <- mydt[!is.na(get(age_interval))] # delete rows from summary table with unknown age
        mydt[, paste0(mydeaths) := get(mydeaths) + (deaths.unk.age * get(mydeaths)/(sum(mydt[[mydeaths]])-deaths.unk.age))] # distribute unknown death
    }

  # Check that beginning of each interval == end of previous interval (overlap by one digit) ----
    if( nrow(mydt[shift(iend, n = 1L, type = "lag") == istart]) != (nrow(mydt)-1)){ # -1 bc first interval starts at 0
      stop(paste0("The values in 'age_interval' (i.e., ", age_interval, ") are misspecified.
                    The start of each interval must be the end of the previous interval"))
    }

  # Calculate metrics for life table ----
    # ax ... the proportion (i.e., fraction) of person-years lived in the interval by those who died in the interval ----
      # Note that CDC approximates with 0.5 for 1 year intervals, but I have real data so will use that instead
      mydt[irank == max(irank), paste0(myfrac) := NA]

    # mx ... calculate the age specific death rate ----
      # mx = #_deaths_in_age_group / #_person_years_lived_in_age_group
      # mydt[, mx := deaths / ((ilength*(pop-deaths)) + (ilength*ax*deaths))] # Chiang ch 2, formula 1.2
      mydt[, mx := get(mydeaths)/get(mypop)] # Chiang 2.4 ... "age specific death rate can be estimated from ..."
      mydt[mx > 1, mx := 1] # due to small numbers, it is possible for #deaths>#pop, especially for single old age groups. Probability > 100% illogical.

      # mydt[, temp_deaths := get(mydeaths)][temp_deaths == 0, temp_deaths := 1] # Can't have zero deaths when calculating lower CI
      # mydt[, mx_lower := qgamma(0.025, temp_deaths) / get(mypop)][get(mydeaths) == 0, mx_lower := 0] # exact Poisson lower CI
      # mydt[, temp_deaths := NULL]
      mydt[, mx_upper := qgamma(0.975, get(mydeaths) + 1) / get(mypop)] # exact Poisson upper CI
      mydt[, mx_se := (mx_upper - mx) / qnorm(0.975)] # reverse_engineer poisson standard error
      mydt[, mx_upper := NULL]

    # qx ... probability of dying in the interval ----
      mydt[, qx := ilength*mx / (1 + ((1-get(myfrac))*ilength*mx))] # Chiang formula 1.4 & 2.3
      mydt[irank == max(irank) | qx > 1, qx := 1] # probability of death for those in final age group is always 100%

    # lx ... # alive at the start of the age interval ----
      mydt[1, lx := 100000]
      for(ii in seq(2, nrow(mydt), 1)){
        mydt[ii, lx := mydt[ii-1]$lx * (1-mydt[ii-1]$qx) ]
      }

    # dx ... # deaths in age interval ----
      mydt[, dx := qx * lx] # same as mydt[, dx := lx - shift(lx, n = 1L, type = "lead")]

    # Lx ... calculate person-years lived in age interval ----
      # mydt[!grepl("^0", age.range), Lx := lx - (0.5*dx)] # approximation, approximation doesn't apply to first year
      mydt[, Lx := ilength*(lx - dx) + (ilength*get(myfrac)*dx)] # Chiang formula 2.3 & 2.7
      mydt[irank == max(irank), Lx := dx / mx] # Chiang formula 3.10, for final interval which is open ended

    # Tx ... calculate total number of person-years lived over start of age interval ----
      # this is a sum of all Lx for the same age range or older
      for(ii in seq(1, nrow(mydt), 1) ){
        mydt[ii, Tx := sum(mydt[ii:nrow(mydt)]$Lx)]
      }
      mydt[irank == max(irank), Tx := Lx] # Chiang formula 3.12, for final interval which is open ended

    # ex ... expectation of life (aka life expectancy) at start of age interval ----
      mydt[, ex := Tx / lx]
      mydt[irank == max(irank), ex := 1/mx] # Chiang formula 3.12, for final interval which is open ended

  # Calculate uncertainty for life expectancy ----
      mydt[, qx_variance := ((qx^2)*(1-qx)) / get(mydeaths)] # Chiang 2.2 variance of qx
      mydt[, px_variance := qx_variance] # Chiang 3.6, variance prob(survival) == variance of prob(death)
      mydt[, ex_temp := (lx^2) * ((((1-get(myfrac))*ilength) + shift(ex, 1L, type = "lead"))^2) * px_variance] # Chiang page 137

      # reverse cumulative sum, so flip, get cumsum, then flip back
      setorder(mydt, -irank)
      mydt[!is.na(ex_temp), ex_temp_cumsum := cumsum(ex_temp)] # reverse cumulative sum
      setorder(mydt, irank)

      # divide ex_temp_cumsum by lx^2 to get sample variance
      mydt[, ex_variance := ex_temp_cumsum / lx^2]

      # variance for oldest age interval cannot be calculated using the Chiang method
      # and is assumed to be zero because qx for the oldest interval == 1.00.
      # CDC follows Silcocks' approximation, not Chiang's assumption, which is what we will use here.
      # The next commented out formula appears in multiple CDC publications, but it seems incorrect
      # because (a) I see nothing like this in Silcock's paper, and (b) the variance is enormous
      # to the point of uselessness
        # mydt[irank == max(irank), ex_variance := ((lx^2)/(mx^4)) * mx_se^2]
      # The replacement formula below was derived from careful study of Silcocks' original paper
      mydt[irank == max(irank), ex_variance := (0.5*mydt[irank == max(irank)-1]$Lx) * (4 / get(mydeaths)*(mx^2))]

      mydt[, ex_se := sqrt(ex_variance)]
      zscore = qnorm(1 - (1-ci)/2) # since two sided, need to split the alpha for upper and lower tails
      mydt[, ex_lower := ex - ex_se * zscore]
      mydt[, ex_upper := ex + ex_se * zscore]

  # Tidy final output ----
    # order and subset columns
    if("ax" %in% names(mydt)){
      ordered_cols <- c(age_interval, mypop, mydeaths, "mx", "qx", "lx", "dx", "ax", "Lx", "Tx", "ex", "ex_lower", "ex_upper", "ex_se")
    } else{
      mydt[, ax := get(myfrac)]
      ordered_cols <- c(age_interval, mypop, mydeaths, myfrac, "mx", "qx", "lx", "dx", "ax", "Lx", "Tx", "ex", "ex_lower", "ex_upper", "ex_se")
    }
    ordered_cols <- c(setdiff(orig_cols, ordered_cols), ordered_cols)
    mydt <- mydt[, ordered_cols, with = FALSE]

    # rounding
    mydt[, c("lx", "dx", "Lx", "Tx", mydeaths) := lapply(.SD, rads::round2, 0), .SDcols = c("lx", "dx", "Lx", "Tx", mydeaths)]
    mydt[, c("qx", "mx") := lapply(.SD, rads::round2, 5), .SDcols = c("qx", "mx")]
    mydt[, c("ex_se") := lapply(.SD, rads::round2, 5), .SDcols = c("ex_se")]
    mydt[, c("ax", "ex", "ex_lower", "ex_upper") := lapply(.SD, rads::round2, 2), .SDcols = c("ax", "ex", "ex_lower", "ex_upper")]

  # Return object from function ----
    return(mydt)
}

