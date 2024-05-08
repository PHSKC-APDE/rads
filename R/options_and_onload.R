.onLoad = function(libname, pkgname){

  #find the option to use
  types = unique(odbc::odbcListDrivers()$name)

  #find the best ODBC driver, if available
  odbc_drv = grep('ODBC Driver', types, value = T)
  odbc_drv = sort(odbc_drv)
  if(length(odbc_drv) >0){
    ov = odbc_drv[length(odbc_drv)]
  }else{
    ov = "SQL Server"
    warning('No usable ODBC driver detected. To use functions requiring a database connection please install an ODBC driver. "Sql Server" is being used as default which may not work super well. Good luck.')
  }

  options(
    rads.odbc_version = ov
  )
}
