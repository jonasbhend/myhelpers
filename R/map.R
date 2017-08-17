#' Draw geographical maps
#'
#' By default adds lakes to the standard map
#'
#' @param database map database to use see \code{\link[maps]{map}}
#' @param ... parameters passed to \code{\link[maps]{map}} from the \code{maps} package
#' @param add logical, should map be added to existing plot?
#' @param add.lakes logical, should lakes be added?
#'
#'
#' @seealso \code{\link[maps]{map}} from the \code{maps} package
#' @keywords utilities
#' @importFrom maps map
#' @export
map <- function(database= "world", ..., add=FALSE, add.lakes=TRUE){
  ## plus data for plot=FALSE
  mm <- list(first=maps::map(database=database, add=add, ...))
  if (add.lakes & database != 'lakes') {
    mm[[2]] <- maps::map(database="lakes", add=TRUE, ...)
    mout <- Reduce(function(z1,z2) Map(function(x,y) c(x, NA, y), z1, z2), mm)
  } else {
    mout <- mm[[1]]
  }
  invisible(mout)
}
