#' CHI Generate Metadata
#'
#' @description
#' function to generate metadata table combining existing metadata and latest estimates.
#'
#' @param meta.old Previous metadata table
#' @param est.current current year's tableau ready output with completed estimates
#'
#' @return
#' @export
#'
chi_generate_metadata <- function(meta.old = NULL,
                                  est.current = NULL){
  # get new metadata ----
  meta.new <- unique(est.current[tab == "metadata",
                                 .(indicator_key,
                                   latest_yearx = as.integer(year),
                                   latest_year_resultx = result,
                                   run_datex = run_date,
                                   latest_year_countx = as.integer(numerator),
                                   latest_year_kc_popx = as.integer(denominator))])

  # merge new metadata onto old metadata ----
  meta.new <- merge(meta.old, meta.new, by = c("indicator_key"), all = T)

  # update with newest data ----
  # only replace old data when there is new data because may stop calculating indicators, in which case, would want to keep old data
  meta.new[!is.na(latest_yearx), latest_year := as.numeric(latest_yearx)]
  meta.new[!is.na(latest_year_resultx), latest_year_result := latest_year_resultx]
  meta.new[, run_date := as.Date(gsub("-", "", run_datex), "%Y%m%d")]
  meta.new[!is.na(latest_year_countx), latest_year_count := latest_year_countx]
  meta.new[!is.na(latest_year_kc_popx), latest_year_kc_pop := latest_year_kc_popx]
  meta.new[, c("latest_yearx", "latest_year_resultx", "run_datex", "latest_year_countx", "latest_year_kc_popx") := NULL]

  # update valid_years ----
  meta.new[as.integer(latest_year) > suppressWarnings(as.integer(substrRight(valid_years, 1, 4))),
           valid_years := suppressWarnings(paste(as.integer(substr(valid_years, 1, 4)):as.integer(latest_year), collapse = " "))]

  # Ensure there are no missing important metadata cells ----
  missing.per.col <- sapply(meta.new, function(x) sum(is.na(x)))
  if(sum(missing.per.col) > 0){
    stop("You are missing at least one critical meta.new[] value.")
  }

  # order metadata table ----
  setorder(meta.new, indicator_key)

  # return table ----
  return(meta.new)
}
