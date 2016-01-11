#' read_scores
#'
#' Read skill scores to data frame
#'
#' @param models character vector of the names of forecasting systems
#' @param scores character vector of skill scores to be read
#' @param indexes character vector of variable names
#' @param methods character vector of bias correction methods
#' @param initmonths character vector of initialization months
#' @param grids character vector of grids
#' @param granularities character vector of temporal resolutions
#' @param ccrs logical should recalibrated indices be used?
#' @param detrends logical should detrended series be used?
#' @param leads index of lead times, or a character string specifying
#' what lead times to use (currently only \code{"last"} is supported)
#' @param reference name of reference model against which the forecast skill
#' is evaluated. If set to \code{NULL} (the default), forecasts are
#' verified against a climatological forecast.
#' @param dpath head of directory tree
#'
#' @keywords util
#' @export
read_scores <- function(models='ecmwf-system4',
                        scores=c('EnsCorr', 'FairRpss'), indexes='tas',
                        methods='none', initmonths="05", grids="global2",
                        granularities="seasonal", ccrs=c(FALSE, TRUE),
                        detrends=FALSE, leads=2,
                        reference=NULL,
                        dpath="/store/msclim/bhendj/EUPORIAS"){
  stopifnot(file.exists(dpath))
  skill.long <- list()
  ## do the analysis by grid
  for (grid in grids){
    ## get land sea mask of indices to be read
    lsmfile <- list.files(paste0(dpath, '/grids'), paste0('^', grid, '_lsm.nc'), full.names=TRUE)
    stopifnot(length(lsmfile) == 1)
    nc <- nc_open(lsmfile)
    lsm <- ncvar_get(nc, "FR_LAND")
    lolaname <- sapply(nc$var[['FR_LAND']]$dim, function(x) x$name)[1:2]
    lon <- rep(nc$dim[[lolaname[1]]]$vals, nc$dim[[lolaname[2]]]$len)
    lat <- rep(nc$dim[[lolaname[2]]]$vals, each=nc$dim[[lolaname[1]]]$len)
    nc_close(nc)

    skill <- expand.grid(model=models, index=indexes, method=methods,
                         initmon=initmonths,
                         score=scores, granularity=granularities, ccr=ccrs,
                         detrend=detrends, lead=leads)
    skill$grid <- grid
    skillnames <- names(skill)
    skill <- cbind(skill, matrix(NA, nrow(skill), length(lsm)))

    ## convert to filename and filepath
    if (is.null(reference)){
      filepaths <- paste(dpath, 'skill_scores', grid, skill$granularity, skill$index, sep='/')
      filenames <- paste0('^', skill$index, c('', '_detrend')[skill$detrend*1 + 1],
                          c('', '_CCR')[skill$ccr*1 + 1], '_',
                          skill$method, '_.*', skill$model, '_vs_.*initmon', skill$initmon, '.nc$')
    } else {
      filepaths <- paste(dpath, 'skill_against_reference', grid, skill$granularity, skill$index, sep='/')
      filenames <- paste0('^', skill$index, c('', '_detrend')[skill$detrend*1 + 1],
                          c('', '_CCR')[skill$ccr*1 + 1], '_',
                          skill$method, '_.*', skill$model, '-ref-', reference, '_vs_.*_.*initmon', skill$initmon, '.nc$')
    }
    ## read in scores
    for (i in 1:nrow(skill)){
      infile <- list.files(filepaths[i], filenames[i], full.names=TRUE)
      if (length(infile) == 1){
        nc <- nc_open(infile)
        if (skill$score[i] %in% names(nc$var)){
          dtmp <- try(ncvar_get(nc, as.character(skill$score[i])), silent=TRUE)
          if (class(dtmp) != 'try-error'){
            if (skill$lead[i] == 'last') {
              lead <- dim(dtmp)[3]
              print(lead)
            } else {
              lead <- as.numeric(as.character(skill$lead))
            }
            dtmp <- dtmp[,,lead]
            lolaname <- sapply(nc$var[[as.character(skill$score[i])]]$dim, function(x) x$name)[1:2]
            lon2 <- rep(nc$dim[[lolaname[1]]]$vals, nc$dim[[lolaname[2]]]$len)
            lat2 <- rep(nc$dim[[lolaname[2]]]$vals, each=nc$dim[[lolaname[1]]]$len)
            stopifnot(round(lon2,3) == round(lon,3), round(lat2,3) == round(lat,3))
            if (skill$score[i] %in% c('EnsRmse', 'FairCrps', 'FairSprErr')){
              skill[i,ncol(skill) - rev(seq(lsm) - 1)] <- pmax(c(dtmp), 0)
            } else {
              skill[i,ncol(skill) - rev(seq(lsm) - 1)] <- c(dtmp)
            }
          }
        }
        nc_close(nc)
      }
    } # end of loop on rows of skill

    skill.long[[grid]] <- melt(skill, id.vars=skillnames, variable.name='gridID')
    skill.long[[grid]][['gridID']] <- as.numeric(as.character(skill.long[[grid]][['gridID']]))
    ## add in land sea mask, latitudes and longitudes
    skill.long[[grid]][['lsm']] <- c(lsm)[skill.long[[grid]][['gridID']]]
    skill.long[[grid]][['lon']] <- c(lon)[skill.long[[grid]][['gridID']]]
    skill.long[[grid]][['lat']] <- c(lat)[skill.long[[grid]][['gridID']]]
    skill.long[[grid]][['reference']] <- reference
  } # end of loop on grids

  ## output single data frame if only one grid is read
  if (length(grids) == 1) skill.long <- skill.long[[1]]

  return(skill.long)
}
