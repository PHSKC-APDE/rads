.onLoad = function(libname, pkgname){

  #find the option to use
  types = unique(odbc::odbcListDrivers()$name)

  #find the best ODBC driver, if available
  odbc_drv = grep('ODBC Driver', types, value = T)
  odbc_drv = sort(odbc_drv)
  if(length(odbc_drv) >0){
    ov = odbc_drv[length(odbc_drv)]
  }else if('SQL Server' %in% types){
    ov = 'SQL Server'
  }else{
    warning('No ODBC driver detected')
  }

  #set some options
  options(
    rads.odbc_version = ov
  )
}
