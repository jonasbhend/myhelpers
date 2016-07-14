#' Plot Filled Contours
#'
#' Contrary to \code{\link[graphics]{filled.contour}}, this function provides
#' the atmoic function for contour shading in analogy with
#' \code{\link[graphics]{image}} and \code{\link[graphics]{contour}}.
#'
#' @param x,y Locations of grid cells for values of \code{z}.
#' @param z Matrix containing the values to be plotted.
#' @param levels Numeric vector of levels at which to draw contours.
#' @param col Vector of colours to use for shading in between contours.
#' @param xlim,ylim,zlim x- y- and z-limits for the plot.
#' @param add Logical, hsould plot be added to existing plot?
#' @param axes Logical, should axes and bounding box be drawn?
#' @param type One of 'contour' or 'image' for contour and image plots respectively.
#' @param col.axis,col.box Colour for axes and bounding box.
#' @param add.boundary Logical, should filled contours be separated with a
#'   boundary line (drawn by \code{\link[graphics]{contour}})?
#' @param col.contour,lty.contour,lwd.contour Colour, line type, and line width
#'   for boundary contours.
#' @param drawlabels Logical, contours are labelled if \code{TRUE}.
#' @param ... Additional arguments to \code{\link{plot.window}},
#'   \code{\link{title}}, \code{\link{Axis}} and \code{\link{box}}.
#'
#' @keywords plot
#'
#' @examples
#' x <- seq(1,100)
#' y <- seq(1,80)
#' z <- outer(x,y, function(x,y) sin(x/10) + cos(y / 50))
#'
#' # contour plot
#' filled_contour(x,y,z)
#'
#' @export
filled_contour <- function(x=seq(0,1,length.out=nrow(z)),
                           y=seq(0,1,length.out=ncol(z)),
                           z,
                           levels = pretty(zlim, 12),
                           col = NULL,
                           xlim = range(x),
                           ylim = range(y),
                           zlim = range(z[is.finite(z)]),
                           add = FALSE,
                           axes = TRUE,
                           type=c("contour", "image"),
                           col.axis = 1,
                           col.box = 1,
                           add.boundary = TRUE,
                           col.contour = 'grey',
                           lty.contour = 1,
                           lwd.contour = 1,
                           drawlabels=FALSE,
                           ...
                           ) {

  type <- match.arg(type)
  if (missing(z)) {
    if (!missing(x)) {
      if (is.list(x)) {
        z <- x$z
        y <- x$y
        x <- x$x
      }
      else {
        z <- x
        x <- seq.int(0, 1, length.out = nrow(z))
      }
    }
    else stop("no 'z' matrix specified")
  }
  else if (is.list(x)) {
    y <- x$y
    x <- x$x
  }

  ## reorder x, y and z
  z <- z[order(x), order(y)]
  x <- x[order(x)]
  y <- y[order(y)]

  if (any(diff(x) <= 0) || any(diff(y) <= 0))
    stop("increasing 'x' and 'y' values expected")
  if (!is.matrix(z) || nrow(z) <= 1L || ncol(z) <= 1L)
    stop("no proper 'z' matrix specified")
  if (!add) {
    localPlotWindow <- function(xlim, ylim, ..., main, sub,
                                xlab, ylab, outer, line) plot.window(xlim, ylim,
                                                                     ...)
    localTitle <- function(..., log) title(...)
    plot.new()
    localPlotWindow(xlim, ylim, xaxs='i', yaxs='i', ...)
    localTitle(...)
  }

  if (is.null(col)) col <- mchcol(n=length(levels) - 1)

  if (type == 'contour'){
    .filled.contour(x,y,z,levels=levels, col=col)
    if (add.boundary){
      contour(x, y, z, levels=levels, add=T, drawlabels=drawlabels,
              col=col.contour, lty=lty.contour, lwd=lwd.contour)
    }
  } else {
    image(x,y, z, breaks=levels, col=col, add=T)
  }

  if (!add) {
    localAxis <- function(..., col.axis, bg, pch, cex, lty, lwd) Axis(...)
    localBox <- function(..., col.box, bg, pch, cex, lty, lwd) box(...)
    if (axes) {
      localAxis(x, side = 1, ...)
      localAxis(y, side = 2, ...)
      localBox(...)
    }
  }

  out <- list(levels=levels, colours=col)
  invisible(out)
}
