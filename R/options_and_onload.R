.onLoad = function(libname, pkgname){

  #find the option to use
  types = unique(odbc::odbcListDrivers()$name)

  #find the best ODBC driver, if available
  odbc_drv = grep('ODBC Driver', types, value = T)
  odbc_drv = sort(odbc_drv)
  if(length(odbc_drv) >0){
    ov = odbc_drv[length(odbc_drv)]
  }else{
    warning('No usable ODBC driver detected. To use functions requiring a database connection please install an ODBC driver: https://docs.microsoft.com/en-us/sql/connect/odbc/download-odbc-driver-for-sql-server?view=sql-server-ver15')
  }
  #set some options
  ov = "ODBC Driver 17 for SQL Server"
  options(
    rads.odbc_version = ov
  )
}
