#' plot a colour bar
#'
#' Adds a colour bar with the specified colours and levels
#'
#' @param levels a vector with the levels to be used for the distinct colours or a list with components \code{levels} and \code{colours}
#' @param colours a vector of the colours to be used with the respective levels
#' @param side side at which axis labels are to be shown
#' @param units text for units of colourbar (at right, top end of colour bar
#' @param hadj.units horizontal adjustment of units string
#' @param ... additional arguments passed to \code{\link{axis}}
#'
#' @keywords utilities
#' @export
colourbar <- function(levels, colours=NULL, side=1,
                      units='', hadj.units=0.5, ...){
  if (is.list(levels)){
    colours <- levels$col
    levels <- levels$lev
  }
  ncols <- length(colours)
  nlevs <- length(levels)
  stopifnot(ncols == nlevs - 1)
  if (side %in% c(1,3)){
    image(1:ncols, 1, as.matrix(1:ncols),
          breaks=seq(0.5, ncols+0.5), col=colours,
          axes=FALSE, xlab='', ylab='')
    abline(v=seq(1.5, ncols))
  } else {
    image(1, 1:ncols, t(1:ncols),
          breaks=seq(0.5, ncols+0.5), col=colours,
          axes=FALSE, xlab='', ylab='')
    abline(h=seq(1.5, ncols))
  }
  box()
  axis(side, at=seq(1.5, ncols), labels=levels[-c(1, nlevs)], ...)
  axis(side, at=ncols+0.5, labels=units, tick=FALSE, hadj=hadj.units, ...)
}
