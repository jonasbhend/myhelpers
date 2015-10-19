#' nc_time
#'
#' read netcdf times and convert to RDates
#'
#' @param nc netcdf file connection
#'
#' @seealso \code{\link{nc_open}} for opening netcdf files,
#'   \code{\link{as.Date}} and \code{\link{as.POSIXct}} for information on R
#'   Date format
#'
#' @keywords utils
#' @export
nc_time <- function(nc){
  t.i <- grep('tim', names(nc$dim))
  t.units <- nc$dim[[t.i]]$units
  t.mul <- c(second=1,
             minute=60,
             hour=3600,
             day=1,
             month=1,
             year=1) ## special cases for days-years
  mul.i <- pmatch(substr(t.units,1,3), names(t.mul))
  if (length(mul.i) == 1){
    if (mul.i < 4){
      t.out <- as.POSIXct(gsub('.*since ', '', t.units), tz='UTC') + t.mul[mul.i]*nc$dim[[t.i]]$val
    } else if (mul.i == 4){
      t.out <- as.POSIXct(as.Date(gsub('.*since ', '', t.units), tz='UTC') + round(nc$dim[[t.i]]$val), tz='UTC')
    }
  } else {
    stop('Time coordinate format not implemented yet')
  }
  return(t.out)
}
