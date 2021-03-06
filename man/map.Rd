% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/map.R
\name{map}
\alias{map}
\title{Draw geographical maps}
\usage{
map(database = "world", ..., add = FALSE, add.lakes = TRUE)
}
\arguments{
\item{database}{map database to use see \code{\link[maps]{map}}}

\item{...}{parameters passed to \code{\link[maps]{map}} from the \code{maps} package}

\item{add}{logical, should map be added to existing plot?}

\item{add.lakes}{logical, should lakes be added?}
}
\description{
By default adds lakes to the standard map
}
\seealso{
\code{\link[maps]{map}} from the \code{maps} package
}
\keyword{utilities}
