#' Read NetCDF Times and Convert to POSIXct
#'
#' Based on the time dimension of the NetCDF connecction supplied,
#' the date-time string is converted to internal POSIXct format.
#'
#' @param nc netcdf file connection
#'
#' @seealso \code{\link{nc_open}} for opening netcdf files,
#'   \code{\link{as.Date}} and \code{\link{as.POSIXct}} for information on R
#'   Date format
#'
#' @examples
#' nc <- list(dim=list(time=NULL))
#' nc$dim$time <- list(units='seconds since 1979-01-01 00:00:00',
#'                     vals=c(0,31535999, 31536000))
#' nc_time(nc)
#'
#' nc$dim$time <- list(units='days since 1979-01-01',
#'                     vals=c(0.5, 364.99, 365))
#' as.Date(nc_time(nc))
#'
#' nc$dim$time <- list(units='months since 1979-01-30',
#'                     vals=0:24)
#' nc_time(nc)
#'
#' @keywords utils
#' @export
nc_time <- function(nc){
  t.i <- grep('tim', names(nc$dim))
  t.units <- nc$dim[[t.i]]$units
  t.mul <- c(second=1,
             minute=60,
             hour=3600,
             day=24*3600,
             month=1,
             year=1) ## special cases for days-years
  t.vals <- nc$dim[[t.i]]$val
  mul.i <- pmatch(substr(t.units,1,3), names(t.mul))
  refdate <- as.POSIXct(gsub('.*since ', '', t.units), tz='UTC')
  if (length(mul.i) == 1 & is.numeric(t.mul)){
    if (mul.i <= 4){
      t.out <- refdate + t.vals * t.mul[mul.i]
    } else {
      if (any(t.vals[!is.na(t.vals)] %% 1 != 0)){
        stop("Non-integer months or years are not supported")
      }
      t.out <- rep(refdate, length(t.vals))
      if (mul.i == 5) month(t.out) <- month(t.out) + t.vals
      if (mul.i == 6) month(t.out) <- month(t.out) + t.vals
      ## fix end of month problem
      for (dd in 1:3){
        if (any(is.na(t.out))){
          newref <- refdate
          day(newref) <- day(refdate) - dd
          ina <- is.na(t.out)
          t.out[ina] <- newref
          if (mul.i == 5) month(t.out[ina]) <- month(t.out[ina]) + t.vals[ina]
          if (mul.i == 6) year(t.out[ina]) <- year(t.out[ina]) + t.vals[ina]
        }
      }
    }
  } else {
    stop('Time coordinate format not implemented yet')
  }
  return(t.out)
}
