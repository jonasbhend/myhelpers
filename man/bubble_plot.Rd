% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bubble_plot.R
\name{bubble_plot}
\alias{bubble_plot}
\alias{map_forecast}
\alias{bubble_plot}
\alias{skillLegend}
\title{Plot forecasts (with forecast skill)}
\usage{
map_forecast(lon, lat, x, color = NULL, breaks = NULL, skill = NULL,
  type = c("area", "edge"), plot = TRUE, ...)

bubble_plot(lon, lat, x, color = NULL, breaks = NULL, skill = NULL,
  type = "area", labels = NULL, ...)

skillLegend(args, legend.pos = "bottomleft", width = 0.2, height = 0.15,
  inset = 0.01, skill.levs = c(0.2, 0.5, 0.8), skill.col = "grey",
  byrow = TRUE, bbox = FALSE, font.main = par("font.main"),
  cex.main = par("cex.main"), cex = par("cex.lab"),
  font = par("font.lab"))
}
\arguments{
\item{lon}{longitudes of the forecast array}

\item{lat}{latitutdes of forecast array}

\item{x}{three-dimensional array of forecast probabilities (in percent) with
dimensions forecast categories x lon x lat}

\item{color}{matrix of color for breaks (see details)}

\item{breaks}{matrix of breaks (see details)}

\item{skill}{two or three-dimensional array of forecast skill with dimenisions
lon x lat (2d) or forecast cat. x lon x lat (3d)}

\item{type}{specify whether skill information should be represented by grid
point area (\code{type = "area"}, the default) or grid point width and
height (\code{type = "edge"}).}

\item{plot}{logical, should plot be generated or just a dry run be performed
to enquire parameters?}

\item{...}{additional parameters passed to \code{\link[graphics]{image}}}

\item{labels}{Vector of category labels for legend (e.g. below normal,
normal, above normal)}

\item{args}{output from map_project. List with at least type
and londiff and latdiff vectors}

\item{legend.pos}{position of inset legend on plot}

\item{width}{width of legend expressed as a fraction of the plot}

\item{height}{height of legend expressed as a fraction}

\item{inset}{additional spacing to offset legend from border}

\item{skill.levs}{skill levels to be plotted}

\item{skill.col}{colour for skill bubbles}

\item{byrow}{logical, should skill labels be shown in one row?}

\item{bbox}{logcial, should bounding box be added?}

\item{font.main}{font of the legend header}

\item{cex.main}{character expansion of legend header}

\item{cex}{character expansion of legend text}

\item{font}{font of legend text}
}
\description{
Plot category forecasts with shading according to the dominant
  forecast category. Alternatively, point size reflective of forecast skill
  can be used. \code{map_forecast} plots the forecasts (with points sizes
  corresponding to forecast skill), \code{bubble_plot} also plots the legends.
}
\details{
color and breaks for the forecast are supplied as a matrix with
  the number of columns corresponding to the forecast categories. If no
  breaks are supplied, breaks are set automatically with the lowest
  probability at \code{ceiling(1/n*10)*10}. If category forecasts with
  unequal climatological expected frequency are used, breaks should be
  provided.
}
\examples{
\dontrun{
tmp <- bubble_plot(lon, lat, fcst, skill=skill)
map(add=T) ## add in map for forecast
skillLegend(tmp) ## add in inset legend for skill
}

}
\keyword{utilities}
