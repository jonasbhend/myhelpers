#' plot a colour bar
#'
#' Add watermark to plot
#'
#' @param txt text string to display, defaults to (c) MeteoSwiss <date>
#' @param cex character expansion factor
#'
#' @importFrom graphics par text
#' @keywords utilities
#' @export
addWatermark <- function(txt=NULL, cex=par('cex.axis')*0.8){
  if (is.null(txt)){
    txt <- paste("\uA9 MeteoSwiss,", format(Sys.time(), '%F %H:%M'))
  }
  text(par('usr')[2] - 0.01*diff(par('usr')[1:2]),
       par('usr')[3],
       txt,
       adj=c(1,-0.5),
       cex=cex)
}
