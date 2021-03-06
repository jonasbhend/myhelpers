% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nc_time.R
\name{nc_time}
\alias{nc_time}
\title{Read NetCDF Times and Convert to POSIXct}
\usage{
nc_time(nc)
}
\arguments{
\item{nc}{netcdf file connection}
}
\description{
Based on the time dimension of the NetCDF connecction supplied,
the date-time string is converted to internal POSIXct format.
}
\examples{
nc <- list(dim=list(time=NULL))
nc$dim$time <- list(units='seconds since 1979-01-01 00:00:00',
                    vals=c(0,31535999, 31536000))
nc_time(nc)

nc$dim$time <- list(units='days since 1979-01-01',
                    vals=c(0.5, 364.99, 365))
as.Date(nc_time(nc))

nc$dim$time <- list(units='months since 1979-01-30',
                    vals=0:24)
nc_time(nc)

}
\seealso{
\code{\link{nc_open}} for opening netcdf files,
  \code{\link{as.Date}} and \code{\link{as.POSIXct}} for information on R
  Date format
}
\keyword{utils}
