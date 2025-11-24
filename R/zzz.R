.onAttach <- function(libname, pkgname) {
  if (interactive()) {
    check_version()
  }
}

.onLoad = function(libname, pkgname){

  #find the option to use
  types = unique(odbc::odbcListDrivers()$name)

  #find the best ODBC driver, if available
  odbc_drv = grep('ODBC Driver', types, value = T)
  if(length(odbc_drv) >0){
    odbc_drv = sort(odbc_drv)
    selected_driver <- odbc_drv[length(odbc_drv)] # keep driver with the highest version number
  }else{
    selected_driver <- "SQL Server"
    warning('\u26A0\ufe0f No usable ODBC driver detected.\n',
            'To use functions requiring a database connection please install an ODBC driver.\n',
            '"Sql Server" is being used as default which may not work super well. \nGood luck!')
  }

  options(
    rads.odbc_version = selected_driver
  )
}

