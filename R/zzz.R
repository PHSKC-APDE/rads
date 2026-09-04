.onAttach <- function(libname, pkgname) {
  if (interactive()) {
    getNamespace("rads")$check_version()
  }
}

