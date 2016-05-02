#' Make MeteoSwiss colourtables accessible
#'
#' Interpolate standard, fixed-length MeteoSwiss colour tables to
#' varying number of colours (end points are fixed).
#'
#' @param colfun name of function from package \pkg{colkd}
#'   providing colour tables
#' @param n length of colours to output
#' @param midcol colour to be used for central element in diverging
#'   colour scales with uneven number of colours (defaults to white).
#'
#' @keywords util
#' @export
mchcol <- function(colfun="colkd.temp.anom", n=11, midcol='#FFFFFF'){
  cfun <- match.fun(colfun)
  cc.rgb <- col2rgb(cfun())
  cc.interp <- apply(cc.rgb, 1, function(x) approx(x=seq(0, 1, length=length(x)),
                                                   y=x,
                                                   xout=seq(0,1,length=n))$y)
  cout <- rgb(cc.interp, maxColorValue=255)
  if (n %% 2 == 1){
    cout[(n + 1)/2] <- midcol
  }
  return(cout)
}
