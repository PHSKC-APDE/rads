#' Check if the installed rads package version is the latest available
#'
#' This function checks if the installed version of the rads package is the latest
#' available on GitHub. It compares the local version with the version in the
#' DESCRIPTION file on the main branch of the GitHub repository.
#'
#' @param print_message Logical. Whether to print messages about version status.
#'   Default is TRUE.
#'
#' @return Invisibly returns a list with the following components:
#'   \item{is_current}{Logical. TRUE if the installed version is the latest, FALSE otherwise.}
#'   \item{local_version}{The version of the installed rads package.}
#'   \item{remote_version}{The version on GitHub, or NULL if it couldn't be determined.}
#'   \item{status}{Character string. The status of the check (e.g., "success", "error").}
#'   \item{message}{Character string. Detailed message about the check result.}
#'
#' @examples
#' \dontrun{
#' # Check if rads is up to date
#' check_version()
#' }
#'
#' @export
check_version <- function(print_message = TRUE) {

  # Initialize return values
  result <- list(
    is_current = TRUE,
    local_version = NULL,
    remote_version = NULL,
    status = "error",
    message = ""
  )


  tryCatch({
    # Check if httr is available
    if (!requireNamespace("httr", quietly = TRUE)) {
      result$message <- "httr package is needed to check for updates and is not installed."
      if (print_message) {
        message(result$message)
      }
      return(invisible(result))
    }

    # Construct URL to raw DESCRIPTION file
    url <- "https://raw.githubusercontent.com/PHSKC-APDE/rads/main/DESCRIPTION"
    resp <- httr::GET(url)

    if (httr::status_code(resp) == 200) { # 200 means is okay / was successful
      desc_text <- httr::content(resp, "text")

      # Parse the DESCRIPTION file
      # use Debian Control Format (.dcf) because the DESCRIPTION file in R uses dcf
      desc_vals <- tryCatch(read.dcf(textConnection(desc_text)), error = function(e) NULL)

      if (!is.null(desc_vals) && "Version" %in% colnames(desc_vals)) {
        remote_version <- package_version(desc_vals[1, "Version"])
        result$remote_version <- remote_version

        # Get local version
        local_version <- utils::packageVersion("rads")
        result$local_version <- local_version

        # Compare versions
        if (remote_version > local_version) {
          result$is_current <- FALSE
          update_msg <- ifelse(requireNamespace("devtools", quietly = TRUE),
                               'To update, run: devtools::install_github("PHSKC-APDE/rads", auth_token = NULL)',
                               'To update, install devtools and then run: devtools::install_github("PHSKC-APDE/rads", auth_token = NULL)'
          )

          result$message <- paste0(
            "A newer version is available: ", remote_version, "\n",
            "Your version: ", local_version, "\n",
            update_msg
          )

          if (print_message) {
            message("-----------------------------------------------------")
            message("\u26A0\ufe0f A newer version of rads is available: ", remote_version)
            message("Your version: ", local_version)
            message('\U0001f504 ', update_msg)
            message("-----------------------------------------------------")
          }
        } else {
          result$message <- paste0("Package rads is up to date (version ", local_version, ").")
          if (print_message) {
            message(result$message)
          }
        }

        result$status <- "success"
      } else {
        result$message <- "Could not parse version information from DESCRIPTION file."
        if (print_message) {
          message(result$message)
        }
      }
    } else {
      result$message <- paste0("Could not retrieve version info from GitHub (status: ", httr::status_code(resp), ")")
      if (print_message) {
        message(result$message)
      }
    }
  }, error = function(e) {
    result$message <- paste0("Version check failed: ", conditionMessage(e))
    if (print_message) {
      message(result$message)
    }
  })

  invisible(result)
}
