#' Read Seasonal Forecasts from Repository
#'
#' Similar to \code{\link{read_scores}}, \code{read_forecast} reads forecasts
#' from the repository.
#'
#' @param model Name of forecasting system
#' @param index Name of index (variable) to be read
#' @param grid Name of grid to be read
#' @param method Method string for debiasing of forecast
#' @param init Month of initialization (e.g. '05')
#' @param lon vector with the longitude to be read in
#' @param lat vector with the latitude to be read in
#' @param loi vector with grid index to be read in (alternative to \code{lon})
#' @param lai vector with grid index to be read in (alternative to \code{lat})
#' @param granularity granularity of forecast data to be read
#' @param source where should forecasts be retrieved from (see details)?
#'
#' @details Alternatives are for the forecasts to be retrieved from the research
#'   directory (\code{source = 'euporias'}) or from the direcotry with
#'   operational forecasts (\code{source = 'operational'}). In the latter case,
#'   the grid, method, and granularity arguments are void as these will always
#'   be set to native, none and monthly respectively.
#'
#' @keywords util
#' @export
read_forecast <- function(model='ecmwf-system4', index='tas', grid='global2',
                          method='none', init='05', lon=8.55, lat=47.37,
                          loi=NULL, lai=NULL, granularity='monthly',
                          source=c('euporias', 'operational')){

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
  fc.con <- lapply(fcfiles, nc_open)
  if (is.null(loi) & is.null(lai)){
    lons <- fc.con[[1]]$dim$lon$vals
    lats <- fc.con[[1]]$dim$lat$vals
    londiff <- (lons - lon + 180) %% 360 - 180
    loi <- which.min(abs(londiff))
    lai <- which.min(abs(lats - lat))
  }
  fc.time <- lapply(fc.con, nc_time)
  fcst <- sapply(fc.con, function(x){
    varname <- names(x$var)[grep(index, names(x$var))]
    if (length(varname) == 0) varname <- names(x$var)[1]
    ncvar_get(x, varname, start=c(loi[1], lai[1],1,1), count=c(length(loi), length(lai), -1,-1))
  }, simplify='array')
  sapply(fc.con, nc_close)
  if (is.list(fcst)){
    minnens <- min(sapply(fcst, nrow))
    fcst <- sapply(fcst, function(x) if (is.matrix(x)) x[1:minnens,] else x[,,1:minnens,,drop=F], simplify='array')
  }
  attr(fcst, 'time') <- fc.time
  return(fcst)
}
