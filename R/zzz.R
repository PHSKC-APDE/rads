.onAttach <- function(libname, pkgname) {
  tryCatch({
    if (!requireNamespace("httr", quietly = TRUE)) {
      packageStartupMessage("rads update check skipped: \n'httr' package is needed to check for updates and is not installed.")
      return()
    }

    url <- "https://raw.githubusercontent.com/PHSKC-APDE/rads/main/DESCRIPTION"
    resp <- httr::GET(url)

    if (httr::status_code(resp) == 200) { # 200 means is okay / was successful
      desc_text <- httr::content(resp, "text")

      # use Debian Control Format (.dcf) because the DESCRIPTION file in R uses dcf
      desc_vals <- tryCatch(read.dcf(textConnection(desc_text)), error = function(e) NULL)

      remote_version <- NULL
      if (!is.null(desc_vals) && "Version" %in% colnames(desc_vals)) {
        remote_version <- package_version(desc_vals[1, "Version"])
      }

      local_version <- utils::packageVersion(pkgname)

      if (!is.null(remote_version) && remote_version > local_version) {
        packageStartupMessage("-----------------------------------------------------")
        packageStartupMessage("\u26A0\ufe0f A newer version of rads is available: ", remote_version)
        packageStartupMessage("Your version: ", local_version)
        update_msg <- ifelse(requireNamespace("devtools", quietly = TRUE),
                             '\U0001f504 To update, run: devtools::install_github("PHSKC-APDE/rads", auth_token = NULL)',
                             '\U0001f504 To update, install devtools and then run: devtools::install_github("PHSKC-APDE/rads", auth_token = NULL)'
        )
        packageStartupMessage(update_msg)
        packageStartupMessage("-----------------------------------------------------")
      }
    } else {
      packageStartupMessage("Could not retrieve version info from GitHub (status: ", httr::status_code(resp), ")")
    }
  }, error = function(e) {
    packageStartupMessage("Version check skipped: could not connect to GitHub")
  })
}
