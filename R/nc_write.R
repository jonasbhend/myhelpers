#' nc_write
#'
#' Quickly write content to netcdf following a netcdf template file
#'
#' @param nctempfile path to netcdf template (see details)
#' @param file path to output netcdf file
#' @param varname variable name to write output to (if set to \code{NULL},
#'   the first variable in the template file will be used)
#' @param data array (or vector) containing the data to be written to the output
#'   file
#' @param append logical, should data be appended to existing netcdf file?
#' @param ... additional parameters passed to \code{\link{ncvar_put}}
#'
#' @details This function copies an already existing NetCDF file to the new
#' location and writes data to the variable indicated by \code{varname}. The
#' dimensions and all other attributes are copied from \code{nctempfile}.
#'
#' @note The climate data operators \code{cdo} command line tool is used to
#' produce an empty netcdf file
#'
#'
#' @export
nc_write <- function(nctempfile, file, varname, data, append=FALSE, ...){

  nctemplate <- nc_open(nctempfile, readunlim=FALSE, suppress_dimvals=TRUE)
  on.exit(nc_close(nctemplate))

  ## set variable name if not specified
  if (is.null(varname)) varname <- names(nctemplate$var)[1]
  ## check if an accordingly named variable exists
  if (!any(names(nctemplate$var) == varname)) stop('Variable to write does not exist')

  ## open netcdf file (and close on exit of function)
  on.exit(nc_close(ncout))
  if (append & file.exists(file)){
    ncout <- nc_open(filename=file, write=TRUE, readunlim=FALSE, suppress_dimvals=TRUE)
  } else {
    if (nctemplate$dim$tim$len == dim(data)[length(dim(data))]){
      system(paste('cdo -s setrtomiss,-1e20,1e20', nctempfile, file))
    } else {
      fdates <- system(paste('cdo -s showdate', nctempfile), TRUE)
      fdates <- gsub("  ", " ", gsub("  ", " ", fdates))
      fdates <- strsplit(fdates, ' ')[[1]]
      fdates <- fdates[nchar(fdates) == 10]
      system(paste('cdo -s -L setrtomiss,-1e20,1e20', paste0('-seldate,',fdates[1], ',', fdates[dim(data)[length(dim(data))]]), nctempfile, file))
    }
    ncout <- nc_open(filename=file, write=TRUE, readunlim=FALSE, suppress_dimvals=TRUE)
  }

  ## write the data
  ncvar_put(ncout, varid=varname, vals=data, ...)

}
