#' @name read_ncdf
#'
#' @aliases read_single get_fcfiles
#'
#' @title Read Data from a NetCDF
#'
#' @description \code{read_ncdf} reads and collates (a slice of) data from a
#'   single netcdf file or a collection of netcdf files. \code{read_single} is
#'   the underlying function that does the heavy lifting on the single files
#'   (not exported, for programming purposes only). \code{get_fcfiles} assembles
#'   the available forecast files for a collection of forecasts (e.g. for a
#'   specific index, grid, granularity, and bias correction method). Reading
#'   subsets of NetCDF files allows you to subset the file by longitude and
#'   latitude, by grid index, by time, or as a compact representation of a
#'   selection of grid points (e.g. land only).
#'
#' @param x a vector of file paths to the forecast file collection
#' @param expand logical, should forecast arrays be expanded if differing
#'   numbers of ensemble members are available in hindcast set?
#' @param n.cores number of cores for parallelization
#' @param ... parameters passed on to \code{\link{read_single}}
#'
#' @keywords util
#'
#' @seealso read_single
#'
#' @export
read_ncdf <- function(x, expand=FALSE, n.cores = 1, ...){

  if (length(x) == 1){

    fcst <- read_single(x, ...)

  } else {

    cl <- makeForkCluster(n.cores)
    on.exit(stopCluster(cl))

    ftmp <- clusterApplyLB(cl, x, read_single, ...)
    ## get rid of NULLs
    ftmp <- ftmp[sapply(ftmp, length) > 0]
    if (length(ftmp) == 0) return(NULL)

    fdims <- sapply(ftmp, dim)
    if (all(sapply(fdims, is.null))) fdims <- sapply(ftmp, length)
    if (is.vector(fdims)) fdims <- t(fdims)
    if (expand == TRUE){
      fdims <- apply(fdims, 1, max)
      dfun <- expandNA
    } else {
      fdims <- apply(fdims, 1, min)
      dfun <- shrink
    }
    fcst <- abind(clusterApplyLB(cl, ftmp, dfun, fdims),
                  along=max(length(dim(ftmp[[1]])), 1) + 1)

    ## reconcile attributes
    attns <- setdiff(names(attributes(ftmp[[1]])), c('dim', 'time'))
    for (attn in attns){
      attr(fcst, attn) <- attr(ftmp[[1]], attn)
    }
    attr(fcst, 'time') <- unlist(lapply(ftmp, attr, 'time')) -
      as.numeric(as.POSIXct('1981-01-01')) + as.POSIXct('1981-01-01')

  }

  return(fcst)
}

#' @rdname read_ncdf
#'
#' @param index Variable name to be read
#' @param lon,lat the longitude and latitude to be read for point forecasts,
#' or the longitude and latitude limits for a subset of the forecast domain
#' @param loi,lai the grid indices to be read (alternative to \code{lon, lat})
#' @param mask logical vector with grid point indices to be read (see details).
#' @param tlim vector of start and end dates to be read.
#' @param ti time indices to be read
#' @param time vector of dates to be read. The comparison is performed on
#' on the same format as \code{time} (i.e. if \code{is.Date(time)}, the
#' comparison is done transforming the ncdf time to \code{as.Date})
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
read_single <- function(x, index=NA,
                        lon=NULL, lat=NULL,
                        loi=NULL, lai=NULL,
                        mask=NULL,
                        tlim=NULL, ti=NULL, time=NULL){
  ## check bounds
  stopifnot(sum(sapply(list(time, ti, tlim), is.null)*1) >= 2)

  ## open netcdf file
  nc <- nc_open(x)
  on.exit(nc_close(nc))

  ## build a quick dictionary
  varlist <- list(tasmin=c('MN2T24', 'mn2t24', 'tmin', 'tasmin', 'tn'),
                  tasmax=c('MX2T24', 'mx2t24', 'tmax', 'tasmax', 'tx'),
                  tas=c('MEAN2T24', 'mean2t24', 'tas', 'tg', 't2m', 167),
                  pr=c('TOT_PREC', 'pr', 'tp', 'rr', 228, 'tpra', 'tprate'))

  ## figure out what variable name to read
  if (is.na(index)){
    ## quick and dirty fix
    vnames <- names(nc$var)
    if (any(vnames %in% unlist(varlist))){
      varname <- (vnames[vnames %in% unlist(varlist)])[1]
      index <- names(varlist)[sapply(varlist, function(x) any(x %in% varname))]
    } else {
      index <- varname <- setdiff(vnames, c('time_bnds', 'rotated_pole'))[1]
    }
  } else {
    ## find variable name
    inames <- c(index, varlist[[index]])
    varname <- names(nc$var)[names(nc$var) %in% inames]
    stopifnot(length(varname) == 1)
  }

  ## check dimensionality
  dims <- nc$var[[varname]]$size
  ndim <- length(dims)
  stopifnot(ndim >= 2)

  ## spatial subsetting
  lons <- nc$dim$lon$vals
  lats <- nc$dim$lat$vals
  start <- rep(1, ndim)
  count <- rep(-1, ndim)

  ## check whether subset should be read in
  if (!is.null(mask)){
    stopifnot(length(mask) == prod(dims[1:2]) | length(mask) == dims[1])
  } else if (!is.null(lon)| !is.null(lat) | !is.null(loi) | !is.null(lai)) {

    ## get subset
    if (is.null(loi)) {
      loi <- seq(along=lons)
    } else if (min(loi) < 1 | max(loi) > length(lons)) {
      return(NULL)
    }
    if (is.null(lai)) {
      lai <- seq(along=lats)
    } else if (min(lai) < 1 | max(lai) > length(lats)){
      return(NULL)
    }

    if (!is.null(lon)){
      if (length(lon) == 1){
        londiff <- (lons - lon + 180) %% 360 - 180
        loi <- which.min(abs(londiff))
      } else if (length(lon) == 2){
        loi <- which(lons >= lon[1] & lons <= lon[2])
      }
      if (length(loi) == 0) return(NULL)
    }
    if (!is.null(lat)){
      if (length(lat) == 1){
        lai <- which.min(abs(lats - lat))
      } else if (length(lat) == 2){
        lai <- which(lats >= min(lat) & lats <= max(lat))
      }
      if (length(lai) == 0) return(NULL)
    }


    start[1:2] <- c(ifelse(!is.null(loi), min(loi), 1),
                    ifelse(!is.null(lai), min(lai), 1))
    count[1:2] <- c(ifelse(!is.null(loi), length(loi), -1),
                    ifelse(!is.null(lai), length(lai), -1))
  }

  # get time information
  ntime <- nc_time(nc)
  if (is.null(ti)) {
    ti <- rep(TRUE, length(ntime))
  } else if (is.numeric(ti)) {
    ti <- seq(1, length(ntime)) %in% ti
  } else {
    stopifnot(length(ti) == length(ntime))
  }

  ## subset in time
  if (!is.null(tlim)){
    if (is.Date(tlim)){
      ti2 <- as.Date(ntime) >= tlim[1] & as.Date(ntime) <= tlim[2]
    } else {
      ti2 <- ntime >= as.POSIXct(tlim[1]) & ntime <= as.POSIXct(tlim[2])
    }
    if (sum(ti2) == 0) return(NULL)
    start[ndim] <- min(which(ti2))
    count[ndim] <- sum(ti2)
    rm(ti2)
  } else if (!is.null(time)){
    if (is.Date(time)){
      ti <- as.Date(ntime) %in% time
    } else {
      ti <- ntime %in% as.POSIXct(time)
    }
  }

  ## exception handling if no time steps present
  if (all(!ti) | !is.finite(start[ndim]) | count[ndim]  == 0){
    return(NULL)
  }

  ntmp <- ncvar_get(nc, varname, start=start, count=count)
  if (!is.null(mask)){
    spacedimi <- seq(1, ifelse(length(mask) == nrow(ntmp), 1, 2))
    ntmp <- array(ntmp[rep(c(mask), length.out=length(ntmp))],
                  c(sum(mask), dim(ntmp)[-spacedimi]))
  }
  if (!all(ti)){
    ntmp <- drop(array(ntmp[rep(c(ti), each=length(ntmp)/length(ti))],
                  c(dim(ntmp)[-length(dim(ntmp))], sum(ti))))
  }


  ## write attributes
  attr(ntmp, 'index') <- index
  attr(ntmp, 'varname') <- varname
  if (!is.null(tlim)){
    attr(ntmp, 'time') <- ntime[start[ndim] + seq(1, count[ndim]) - 1]
  } else {
    attr(ntmp, 'time') <- ntime[ti]
  }
  if (!is.null(mask)){
    attr(ntmp, 'lon') <- rep(lons, length=length(mask))[mask]
    attr(ntmp, 'lat') <- rep(lats, each=length(mask)/length(lats))[mask]
  } else if (all(start[1:2] == 1) & all(count[1:2] < 0)){
    attr(ntmp, 'lon') <- lons
    attr(ntmp, 'lat') <- lats
  } else {
    attr(ntmp, 'lon') <- lons[start[1] + seq(1,count[1]) - 1]
    attr(ntmp, 'lat') <- lats[start[2] + seq(1, count[2]) - 1]
  }

  return(ntmp)
}


#' @rdname read_ncdf
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
                        source = c("euporias", "operational", "demo")){

  source <- match.arg(source)
  if (source == 'euporias'){
    fcfiles <- list.files(paste("/store/msclim/bhendj/EUPORIAS", model, grid,
                                granularity, index, method, sep='/'),
                          pattern=paste0(if (granularity == 'monthly') paste0(index, '_'), '....', init, '01_', if (granularity != 'monthly') paste0(index, '_')), full.names=TRUE)
  } else if (source == "operational") {
    stopifnot(index %in% c("tas", "pr"))
    oind <- ifelse(index == 'tas', "167", "228")
    fcfiles <- list.files("/store/msclim/sysclim/ecmwf/system4/monthly/sa/netcdf",
                          pattern=paste0('^....', init, '01_', oind, '.nc'), full.names=TRUE)
  } else {
    fcfiles <- system.file("extdata", c("tas_2000.nc", "tas_2001.nc", "tas_2002.nc"), package='myhelpers')
  }
  if (length(fcfiles) == 0) stop("No files found")
  attr(fcfiles, 'index') <- index

  return(fcfiles)
}


