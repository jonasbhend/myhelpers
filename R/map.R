#' Draw geographical maps
#'
#' Fix the interior polygons in the standard map
#'
#' @param database map database to use see \code{\link[maps]{map}}
#' @param interior If \code{FALSE}, interior segments are not drawn.
#'   An interior segment is a line segment of the map that bounds two
#'   of the polygons to be drawn. This argument is ignored if fill is \code{TRUE}.
#' @param regions Specific region to plot  see \code{\link[maps]{map}}
#' @param add logical, should map be added to existing plot?
#' @param ... parameters passed to \code{\link[maps]{map}} from the \code{maps} package
#'
#'
#' @seealso \code{\link[maps]{map}} from the \code{maps} package
#' @keywords utilities
#' @export
map <- function(database='world', interior=TRUE, regions=".", add=FALSE, ...){
  ## plus data for plot=FALSE
  mm <- list(first=maps::map(database=database, interior=interior, regions=regions, add=add, ...))
  if (!interior){
    mm[[2]] <- maps::map(database=database, regions=".*ake.*", add=T, ...)
    mm[[3]] <- maps::map(database=database, regions="Caspian", add=T, ...)
    mout <- Reduce(function(z1,z2) Map(function(x,y) c(x, NA, y), z1, z2), mm)
  } else {
    mout <- mm[[1]]
  }
  invisible(mout)
}
