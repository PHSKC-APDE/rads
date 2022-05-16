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

chi_tableau_ready_output <- function(chi_est = NULL, chi_meta = NULL){

}


