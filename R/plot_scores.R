#' plot skill scores from data frame
#'
#' Draw map of skill scores stored in a data frame. Incomplete rows and
#' columns are expanded (e.g. missing values, land sea mask, etc.)
#'
#' @param df data frame with one score to plot (one instance)
#' @param value.var variable name to be displayed
#' @param dpath parent directory for grid files
#' @param type user defined type for colour shading. Either individual grid boxes (\code{image})
#' or shaded contours (\code{contour}) are plotted.
#' @param levels breaks for colour scale
#' @param colours user defined colours (one less than levels)
#' @param na.colour user defined colour for missing values (avoided when \code{NULL})
#' @param add.map map is added if \code{TRUE}
#' @param map.interior should country boundaries be plotted?
#' @param database name of map database to be used
#' @param title text to be displayed in topleft corner of plot
#' @param ... additional arguments passed to \code{\link[graphics]{image}}
#'
#' @keywords util
#' @export
plot_scores <- function(df, value.var='value',
                        dpath='/store/msclim/bhendj/EUPORIAS',
                        type=c('image', 'contour'),
                        levels=NULL,
                        colours=NULL,
                        na.colour=NULL,
                        add.map=TRUE,
                        map.interior=FALSE,
                        database='world',
                        title='',
                        ...){

  ## check that only one score instance is supplied
  stopifnot(nrow(df) == length(unique(df$gridID)))
  stopifnot(value.var %in% names(df))

  ## get the type argument
  type <- match.arg(type)
  ## get the relevant information from the grid files
  grid <- df$grid[1]
  ## get land sea mask of indices to be read
  lsmfile <- paste0(dpath, '/grids/', grid, '_lsm.nc')
  ## if (length(lsmfile) != 1) browser()
  stopifnot(file.exists(lsmfile))
  nc <- nc_open(lsmfile)
  lolaname <- sapply(nc$var[['FR_LAND']]$dim, function(x) x$name)[1:2]
  lon <- nc$dim[[lolaname[1]]]$vals
  lat <- nc$dim[[lolaname[2]]]$vals
  nc_close(nc)

  ## for plotting readjust longitudes
  if (any(lon > 180)){
    lon[lon > 180] <- lon[lon > 180] - 360
    df$lon[df$lon > 180] <- df$lon[df$lon > 180] - 360
  }

  ## sort the variable into the grid
  score <- array(NA, c(length(lon), length(lat)))
  score[cbind(match(df$lon, lon), match(df$lat, lat))] <- df[[value.var]]

  if (!is.null(na.colour)){
    NAs <- array(0, c(length(lon), length(lat)))
    NAs[cbind(match(df$lon, lon), match(df$lat, lat))] <- is.na(df[[value.var]])*1
  }


  ## set up colours if not defined
  if (is.null(levels)){
    levels <- pretty(c(score, -score), 12)
  }
  if (is.null(colours)){
    ncols <- length(levels) - 1
    colours <- mchcol(n=ncols)
  }

  if (type == 'image'){
    image(sort(lon), sort(lat), score[order(lon), order(lat)],
          breaks=levels, col=colours, xlab='', ylab='', axes=FALSE, ...)
  } else if (type == 'contour') {
    image(sort(lon), sort(lat), NA*score[order(lon), order(lat)],
          breaks=levels, col=colours, xlab='', ylab='', axes=FALSE, ...)
    .filled.contour(sort(lon), sort(lat), score[order(lon), order(lat)],
          levels=levels, col=colours)

  }
  if (!is.null(na.colour)) image(sort(lon), sort(lat), NAs[order(lon), order(lat)],
                                 breaks=seq(-0.5, 1.5), col=c(NA, na.colour),
                                 axes=FALSE, xlab='', ylab='', add=T)

  if (add.map){
    mm <- map(database=database, interior=map.interior, plot=FALSE,
              xlim=if (length(grep("eobs", grid)) == 1) c(-60,60) else NULL,
              ylim=if (length(grep("eobs", grid)) == 1) c(10,85) else NULL)
    # transform the map coordinates to rotated coords
    if (length(grep('eobs', grid)) == 1){
      mmrot <- geocors.trafo(na.omit(mm$x), na.omit(mm$y), from.type='lonlat',
                             to.type='rotpol', to.pars=list(plon=-162, plat=39.25))
      expand <- lapply(mm[c('x', 'y')], function(x){
        xout <- rep(NA, length(x))
        xout[!is.na(x)] <- seq(1,sum(!is.na(x)))
        return(xout)})
      mm <- list(x=mmrot$rlon[expand$y], y=mmrot$rlat[expand$x])
    }
    lines(mm)
  }

  box()

  ## plot title
  text(par('usr')[1] + diff(par('usr')[1:2])*0.02,
       par('usr')[4] - diff(par('usr')[3:4])*0.02,
       title, adj=c(0,1), cex=par('cex.lab')*1.2, font=2)


  ## return levels and colours for further use
  invisible(list(levels=levels, colours=colours))

}
