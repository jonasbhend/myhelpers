#' @name read_forecasts
#'
#' @title Read Seasonal Forecasts from Repository
#'
#' @description \code{read_forecasts} reads and collates (a slice of) forecasts.
#' \code{get_fcfiles} assembles the available forecast files for a collection of
#' forecasts (e.g. for a specific index, grid, granularity, and bias correction
#' method).
#'
#' @param fcfiles a vector of file paths to the forecast file collection
#' @param index name of variable (index) to be read
#' @param lon,lat the longitude and latitude to be read for point forecasts,
#' or the longitude and latitude limits for a subset of the forecast domain
#' @param loi,lai the grid indices to be read (alternative to \code{lon, lat})
#' @param expand logical, should forecast arrays be expanded if differing
#'   numbers of ensemble members are available in hindcast set?
#'
#' @details Alternatives are for the forecasts to be retrieved from the research
#'   directory (\code{source = 'euporias'}) or from the direcotry with
#'   operational forecasts (\code{source = 'operational'}). In the latter case,
#'   the grid, method, and granularity arguments are void as these will always
#'   be set to native, none and monthly respectively.
#'
#' @note
#' Reading in subsets only works for continous subset not wrapping
#' around the domain. That is, for data on a global grid with positive
#' longitudes, a European domain cannot be extracted.
#'
#' @keywords util
#' @export
read_forecasts <- function(fcfiles, index=NA,
                          lon=NULL, lat=NULL, loi=NULL, lai=NULL,
                          expand=TRUE){

  ## get the index value
  if (is.na(index) & 'index' %in% names(attributes(fcfiles))){
    index <- attr(fcfiles, 'index')
  } else {
    stop("Index / variable name has to be specified")
  }

  ## build a quick dictionary
  varlist <- list(tasmin=c('MN2T24', 'mn2t24', 'tmin', 'tasmin', 'tn'),
                  tasmax=c('MX2T24', 'mx2t24', 'tmax', 'tasmax', 'tx'),
                  tas=c('MEAN2T24', 'mean2t24', 'tas', 'tg', 't2m', 167),
                  pr=c('TOT_PREC', 'pr', 'tp', 'rr', 228, 'tpra', 'tprate'),
                  dtr=c('dtr'))
  inames <- c(index, varlist[[index]])

  ## open forecast files
  fc.con <- lapply(fcfiles, nc_open)

  ## check variable name is present in all files
  fc.pars <- lapply(fc.con, function(x) names(x$var)[names(x$var) %in% inames])
  stopifnot(sapply(fc.pars, length) > 0)

  ## check dimensionality is the same
  ndims <- unlist(Map(function(x,y) length(x$var[[y]]$size), x=fc.con, y=fc.pars))
  stopifnot(min(ndims) == max(ndims))
  ndim <- ndims[1]

  ## check whether subset should be read in
  if (is.null(lon) & is.null(lat) & is.null(loi) & is.null(lai)){
    start <- count <- NA
  } else {

    if (is.null(loi) & is.null(lai)){
      lons <- fc.con[[1]]$dim$lon$vals
      lats <- fc.con[[1]]$dim$lat$vals
      if (length(lon) == 1){
        londiff <- (lons - lon + 180) %% 360 - 180
        loi <- which.min(abs(londiff))
      } else if (length(lon) == 2){
        loi <- which(lons >= lon[1] & lons <= lon[2])
      }
      if (length(lat) == 1){
        lai <- which.min(abs(lats - lat))
      } else if (length(lat) == 2){
        lai <- which(lats >= min(lat) & lats <= max(lat))
      }
    }
    start <- c(min(loi), min(lai), rep(1, ndim - 2))
    count <- c(length(loi),length(lai),rep(-1, ndim - 2))
  }

  # get forecast time information
  fc.time <- lapply(fc.con, nc_time)
  ftmp <- Map(function(x,y) ncvar_get(x, y, start=start, count=count),
              x=fc.con, y=fc.pars)
  if (expand == TRUE){
    fdims <- apply(sapply(ftmp, dim), 1, max)
    dfun <- expandNA
  } else {
    fdims <- apply(sapply(ftmp, dim), 1, min)
    dfun <- shrink
  }
  fcst <- abind(lapply(ftmp, dfun, fdims),
                along=length(dim(ftmp[[1]])) + 1)
  devnull <- lapply(fc.con, nc_close)
  attr(fcst, 'time') <- fc.time
  return(fcst)
}

#' @rdname read_forecasts
#'
#' @param model Name of forecasting system
#' @param grid Name of grid to be read
#' @param method Name of method for debiasing of forecast
#' @param init Month of initialization (e.g. '05')
#' @param granularity granularity of forecast data to be read
#' @param source where should forecasts be retrieved from (see details)?
#' @export

get_fcfiles <- function(model='ecmwf-system4',
                        grid='global2',
                        index='tas',
                        method='none',
                        init='05',
                        granularity='monthly',
                        source = c("euporias", "operational")){

  source <- match.arg(source)
  if (source == 'euporias'){
    fcfiles <- list.files(paste("/store/msclim/bhendj/EUPORIAS", model, grid,
                                granularity, index, method, sep='/'),
                          pattern=paste0('....', init, '01_', index, '_'), full.names=TRUE)
  } else {
    stopifnot(index %in% c("tas", "pr"))
    oind <- ifelse(index == 'tas', "167", "228")
    fcfiles <- list.files("/store/msclim/sysclim/ecmwf/system4/monthly/sa/netcdf",
                          pattern=paste0('^....', init, '01_', oind, '.nc'), full.names=TRUE)
  }
  if (length(fcfiles) == 0) stop("No files found")
  attr(fcfiles, 'index') <- index

  return(fcfiles)
}


