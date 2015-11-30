#' @name bubble_plot
#' @aliases map_forecast
#'
#' @title Plot forecasts (with forecast skill)
#'
#' @description Plot category forecasts with shading according to the dominant
#'   forecast category. Alternatively, point size reflective of forecast skill
#'   can be used. \code{map_forecast} plots the forecasts (with points sizes
#'   corresponding to forecast skill), \code{bubble_plot} also plots the legends.
#'
#'
#' @param lon longitudes of the forecast array
#' @param lat latitutdes of forecast array
#' @param x three-dimensional array of forecast probabilities (in percent) with
#'   dimensions forecast categories x lon x lat
#' @param color matrix of color for breaks (see details)
#' @param breaks matrix of breaks (see details)
#' @param skill two or three-dimensional array of forecast skill with dimenisions
#'   lon x lat (2d) or forecast cat. x lon x lat (3d)
#' @param type specify whether skill information should be represented by grid
#'   point area (\code{type = "area"}, the default) or grid point width and
#'   height (\code{type = "edge"}).
#' @param plot logical, should plot be generated or just a dry run be performed
#'   to enquire parameters?
#' @param ... additional parameters passed to \code{\link[graphics]{image}}
#'
#' @details color and breaks for the forecast are supplied as a matrix with
#'   the number of columns corresponding to the forecast categories. If no
#'   breaks are supplied, breaks are set automatically with the lowest
#'   probability at \code{ceiling(1/n*10)*10}. If category forecasts with
#'   unequal climatological expected frequency are used, breaks should be
#'   provided.
#'
#' @keywords utilities
#' @export
map_forecast <- function(lon, lat, x, color=NULL, breaks=NULL, skill=NULL,
                         type=c('area', 'edge'), plot=TRUE, ...){

  type <- match.arg(type)
  ncat <- nrow(x)

  ## figure out breaks if needed
  if (is.null(breaks)) {
    breaks <- outer(seq(ceiling(1/ncat*10)*10, 100,10), rep(0, ncat), '+')
  }
  nbreaks <- nrow(breaks)

  ## figure out color
  if (is.null(color)){
    color <- outer(seq(1, nbreaks), 1:ncat, function(x,y) {
      hcl(rev(seq(10,240,length=ncat))[y],
          l=seq(90,20,length=nbreaks)[x],
          c=seq(30,90,length=nbreaks)[x])
    })
    color[1,] <- grey(0.9)
    if (ncat %% 2 == 1) color[,ncat %/% 2 + 1] <- grey(seq(0.9, 0.2, length=nbreaks))
  }

  ## adjust for plotting of colour scales
  if (max(breaks) <= 100){
    xbreaks <- unique(c(0, t(t(breaks) + seq(0,(ncat - 1)*100, 100))))
  } else {
    xbreaks <- unique(c(0, breaks))
  }
  xcols <- c(color)
  stopifnot(length(xbreaks) == length(xcols) + 1)

  ## find dominant forecast category
  xplot <- apply(x, 2:3, function(y) if (all(!is.na(y))) max(y) + which.max(y) * 100 - 100 else NA)

  if (is.null(skill)){
    if (plot) image(lon, lat, xplot, col=xcols, breaks=xbreaks, ...)
  } else {

    ## allow for univariate skill metrices to be used (e.g. RPSS)
    if (length(dim(skill)) == 2){
      if (all(dim(skill) == dim(x)[-1])){
        skill <- aperm(array(skill, c(dim(skill), nrow(x))), c(3,1,2))
      }
    }

    stopifnot(dim(skill) == dim(x))
    if (plot) image(lon, lat, xplot*NA, col=xcols, breaks=xbreaks, ...)

    ## get the skill scores corresponding to the dominant category
    maxprob <- apply(x, 2:3, function(y) if (all(!is.na(y))) which.max(y) else NA)
    xskill <- skill[1,,]*NA
    for (i in 1:ncat) xskill[which(maxprob == i)] <- (skill[i,,])[which(maxprob == i)]
    xskill <- pmax(xskill, 0)

    ## plot the points adjusted for skill
    ## set up a data.frame
    xall <- data.frame(lon=rep(lon, ncol(xskill)),
                       lat=rep(lat, each=nrow(xskill)),
                       colour=xcols[as.numeric(cut(xplot, xbreaks))],
                       skill=c(xskill))

    ## only plot points with non-zero skill
    xall <- subset(xall, !is.na(skill) & skill > 0)
    londiff <- median(diff(sort(unique(lon))))*c(-0.5, -0.5, 0.5, 0.5, -0.5, NA)
    latdiff <- median(diff(sort(unique(lat))))*c(-0.5, 0.5, 0.5, -0.5, -0.5, NA)

    ## select skill representation type
    if (type == "area"){
      expskill <- 0.5
    } else {
      expskill <- 1
    }

    lon.edges <- outer(rep(0, 6), xall$lon, '+') + outer(londiff, xall$skill**expskill, '*')
    lat.edges <- outer(rep(0, 6), xall$lat, '+') + outer(latdiff, xall$skill**expskill, '*')
    ## plot points
    if (plot) polygon(c(lon.edges), c(lat.edges), col=xall$colour, border=NA)
  }

  outlist <- list(breaks=breaks, color=color, type=type)
  if (!is.null(skill)) {
    outlist$skill <- xskill
    outlist$londiff <- londiff
    outlist$latdiff <- latdiff
  }
  invisible(outlist)
}

#' @rdname bubble_plot
#'
#' @param labels Vector of category labels for legend (e.g. below normal,
#'   normal, above normal)
#'
#' @export
bubble_plot <- function(lon, lat, x, color=NULL, breaks=NULL,
                        skill=NULL, type='area', labels=NULL, ...){
  ncat <- nrow(x)
  layout(matrix(c(rep(ncat + 1, ncat), seq(1,ncat)), 2, ncat, byrow=TRUE),
         heights=c(5, lcm(2.5)))

  ## plot the main plot
  par(mar=rep(0.2, 4))
  tmp <- map_forecast(lon=lon, lat=lat, x=x,
                      color=color, breaks=breaks,
                      skill=skill, type=type, axes=F,
                      xlab="", ylab="", plot=FALSE, ...)

  if (is.null(labels)){
    if (ncat == 3){
      labels <- c("Below normal", "Normal", "Above normal")
    } else {
      labels <- paste('Cat.', 1:ncat)
    }
  }
  ## plot colourbars
  par(mar=c(2,0.2,2,0.2))
  for (i in 1:ncat){
    colourbar(levels=unique(c(0,tmp$breaks[,i], 100)),
              color=tmp$color[,i], units='%')
    axis(3, at=0.5, labels[i], hadj=0, font=2, tick=F, line=-0.5)
  }

  ## plot the main plot
  par(mar=rep(0.2, 4))
  tmp <- map_forecast(lon=lon, lat=lat, x=x,
                      color=color, breaks=breaks,
                      skill=skill, type=type, axes=F,
                      xlab="", ylab="", ...)
  box()
  invisible(tmp)
}

#' @rdname bubble_plot
#' @param args output from map_project. List with at least type
#'   and londiff and latdiff vectors
#' @param legend.pos position of inset legend on plot
#' @param width width of legend expressed as a fraction of the plot
#' @param height height of legend expressed as a fraction
#' @param inset additional spacing to offset legend from border
#' @param skill.levs skill levels to be plotted
#' @param skill.col colour for skill bubbles
#'
#' @examples
#' \dontrun{
#' tmp <- bubble_plot(lon, lat, fcst, skill=skill)
#' map(add=T) ## add in map for forecast
#' skillLegend(tmp) ## add in inset legend for skill
#' }
#'
#' @export
skillLegend <- function(args, legend.pos="bottomleft",
                        width=0.2, height=0.15, inset=0.01,
                        skill.levs=c(0.2, 0.5, 0.8), skill.col='grey'){
  pusr <- par("usr")
  pusr[1:2] <- pusr[1:2] + c(1,-1)*inset*diff(pusr[1:2])
  pusr[3:4] <- pusr[3:4] + c(1,-1)*inset*diff(pusr[3:4])
  bwidth <- diff(pusr[1:2])*width
  bheight <- diff(pusr[3:4])*height
  if (length(grep("bottom", legend.pos)) == 1){
    ypos <- pusr[3] + c(0, bheight)
  } else if (length(grep("top", legend.pos)) == 1) {
    ypos <- pusr[4] - c(bheight, 0)
  } else {
    ypos <- mean(pusr[3:4]) + c(-0.5, 0.5)*bheight
  }
  if (length(grep("left", legend.pos)) == 1){
    xpos <- pusr[1] + c(0, bwidth)
  } else if (length(grep("right", legend.pos)) == 1){
    xpos <- pusr[2] - c(bwidth, 0)
  } else {
    xpos <- mean(pusr[1:2]) + c(-0.5,0.5)*bwidth
  }
  rect(xpos[1], ypos[1], xpos[2], ypos[2], col='white', border='black')

  ## add in skill bubbles
  nlevs <- length(skill.levs)
  yskill <- ypos[1] + diff(ypos)*(1:nlevs / (nlevs + 2))
  xskill <- rep(xpos[1] + 0.1*diff(xpos), nlevs)
  ## select skill representation type
  if (args$type == "area"){
    expskill <- 0.5
  } else {
    expskill <- 1
  }
  x.edges <- outer(rep(0, 6), xskill, '+') + outer(args$londiff, skill.levs**expskill, '*')
  y.edges <- outer(rep(0, 6), yskill, '+') + outer(args$latdiff, skill.levs**expskill, '*')

  polygon(x.edges, y.edges, col=skill.col, border=NA)
  text(min(x.edges, na.rm=T), ypos[1] + diff(ypos)*(nlevs + 1) / (nlevs + 2),
       "Skill of forecast", adj=c(0, 0.5))
  text(xskill + 0.1*diff(xpos), yskill, skill.levs, adj=c(0, 0.5))
}
