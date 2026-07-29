#' Reference table of available geographic crosswalks
#'
#' A reference table of available geographic crosswalks used by [list_ref_xwalk()]
#' and [get_xwalk()]. It informs which geographic pairs are valid, their location
#' in `rads.data::`, and any relevant explanatory notes.
#'
#' @format
#' A data.table with 16 rows and 6 columns: `input`, `output`, `object`,
#' `inputvar`, `outputvar`, and `notes`.
#'
#' @source Manually curated and maintained in `data-raw/ref_get_xwalk.csv`.
#'
#' @examples
#' head(ref_get_xwalk)
#'
#'
#' @name ref_get_xwalk
"ref_get_xwalk"
