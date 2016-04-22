#' @name expandNA
#' @aliases shrink
#'
#' @title
#' Expand arrays with missing values and subset arrays
#'
#' @description
#' This function expands a multi-dimensional array to the dimensionality indicated by filling in missing values. No new dimensions are created but existing dimensions are enlarged. The algorithm is considerably faster if only the last dimension is changed.
#'
#' @param x array with input data
#' @param dims vector with dimensions describing the array extent (dimension \code{i} of output ranges from \code{1:dims[i]})
#'
#' @details
#' Expansion is performed such that the original data in \code{x} sits in the lower left corner of the array (i.e. for each dimension the indices \code{1:dim(x)}). Correspondingly, shrinking is performed by selected the lower-left corner as a subset.
#'
#' @return
#' Output array of dimension \code{dims}
#'
#' @examples
#' tt <- outer(1:4, 1:3, paste, sep=',')
#' expandNA(tt, c(5,5))
#' shrink(tt, c(2,2))
#'
#' @export
expandNA <- function(x, dims){
  d <- dim(x)
  xnames <- if (length(d) == 1) names(x) else dimnames(x)
  if (is.null(d)) d <- length(x)
  stopifnot(length(d) == length(dims))
  if (all(d == dims)){
    return(x)
  } else{
    xout <- array(NA, dims)
    if (length(d) == 1 | all(dims[-length(d)] == d[-length(d)])){
      xout[seq(along=x)] <- x
    } else {
      dimnames(xout) <- Map(paste0,
                            letters[seq(along=dims)],
                            lapply(dims, seq, from=1))
      dimnames(x) <- Map(paste0,
                         letters[seq(along=d)],
                         lapply(d, seq, from=1))
      afill(xout) <- x
    }
    if (length(d) == 1){
      names(xout)[seq(along = x)] <- names(x)
    } else {
      dimnames(xout) <- NULL
      if (any(!is.null(xnames))){
        for (i in which(!sapply(xnames, is.null))){
          dimnames(xout)[[i]] <- c(xnames[[i]], rep("", dims[i] - length(xnames[[i]])))
        }
      }
    }
    return(xout)
  }
}

#' @rdname expandNA
#' @export

shrink <- function(x, dims){
  d <- dim(x)
  if (is.null(d)) d <- length(x)
  stopifnot(length(d) == length(dims))
  if (all(d == dims)){
    return(x)
  } else if (length(dims) == 1) {
    return(x[seq(1,dims)])
  } else {
    return(asub(x, lapply(dims, seq, from=1)))
  }
}

