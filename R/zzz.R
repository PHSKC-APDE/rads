.onAttach <- function(libname, pkgname) {
  if (interactive()) {
    check_version()
  }
}
