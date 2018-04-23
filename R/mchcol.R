#' Make MeteoSwiss Colour Tables Accessible
#'
#' Interpolate standard, fixed-length MeteoSwiss colour tables to varying number
#' of colours (end points are fixed).
#'
#' @param colfun name of function from package \pkg{colkd} providing colour
#'   tables
#' @param n length of colours to output
#' @param midcol colour to be used for central element in diverging colour
#'   scales with uneven number of colours (defaults to white).
#'
#' @note In case the package \code{colkd} is not available,
#'   \code{\link[colorspace]{diverge_hcl}} from package \code{colorspace} is
#'   used to generate colour tables instead.
#'
#' @keywords util
#' @export
mchcol <- function(colfun="colkd.temp.anom", n=11, midcol='#FFFFFF'){
  if (requireNamespace("colkd", quietly = TRUE)){
    cfun <- get(colfun, envir = environment(colkd::colkd.temp.anom))
    cc.rgb <- grDevices::col2rgb(cfun())
    cc.interp <- apply(cc.rgb, 1, function(x) stats::approx(x=seq(0, 1, length=length(x)),
                                                            y=x,
                                                            xout=seq(0,1,length=n))$y)
    cout <- grDevices::rgb(cc.interp, maxColorValue=255)
  } else {
    cout <- colorspace::diverge_hcl(n, h=c(240, 10), c=80, l=40)
  }

  if (n %% 2 == 1){
    cout[(n + 1)/2] <- midcol
  }
  return(cout)
}
