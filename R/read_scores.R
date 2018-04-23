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
#' @param obs character vector of verifying observations (e.g. ERA-INT)
#' @param periods character of periods over which the verification has been carried out (e.g. 1981-2014)
#' @param seasmethods logical should recalibrated indices be used?
#' @param detrends logical should detrended series be used?
#' @param leads index of lead times, or a character string specifying
#' what lead times to use (currently only \code{"last"} is supported)
#' @param reference name of reference model against which the forecast skill
#' is evaluated. If set to \code{NULL} (the default), forecasts are
#' verified against a climatological forecast.
#' @param dpath head of directory tree
#' @param cleanup logical, should only non-missing combinations be retained?
#'
#' @keywords util
#' @importFrom ncdf4 nc_open nc_close ncvar_get
#' @importFrom reshape2 melt
#' @export
read_scores <- function(models='ecmwf-system4',
                        scores=c('EnsCorr', 'FairRpss'), indexes='tas',
                        methods='none', initmonths="05", grids="global2",
                        granularities="seasonal", obs=NA,
                        periods=NA,
                        seasmethods='none',
                        detrends=FALSE, leads=2,
                        reference=NULL,
                        dpath="/store/msclim/bhendj/EUPORIAS", cleanup=FALSE){
  stopifnot(file.exists(dpath))
  skill.long <- list()
  ## do the analysis by grid
  for (grid in grids){
    ## get land sea mask of indices to be read
    lsmfile <- list.files(paste0(dpath, '/grids'), paste0('^', grid, '_lsm.nc'), full.names=TRUE)
    stopifnot(length(lsmfile) == 1)
    nc <- ncdf4::nc_open(lsmfile)
    lsm <- ncdf4::ncvar_get(nc, "FR_LAND")
    lolaname <- sapply(nc$var[['FR_LAND']]$dim, function(x) x$name)[1:2]
    lon <- rep(nc$dim[[lolaname[1]]]$vals, nc$dim[[lolaname[2]]]$len)
    lat <- rep(nc$dim[[lolaname[2]]]$vals, each=nc$dim[[lolaname[1]]]$len)
    nc <- ncdf4::nc_close(nc)

    skill <- expand.grid(model=models, index=indexes, method=methods,
                         initmon=initmonths,
                         score=scores, granularity=granularities,
                         seasmethod=seasmethods,
                         detrend=detrends, lead=leads, obs=obs,
                         period=periods, stringsAsFactors=FALSE)
    skill$grid <- grid
    skillnames <- names(skill)
    skill <- cbind(skill, matrix(NA, nrow(skill), length(lsm)))

    ## convert to filename and filepath
    ## use temporary method string to handle all cases
    ## check whether full method string is provided, else
    mstring <- skill$method
    iind <- setdiff(seq(along=mstring), grep("_[0-9]*-[0-9]*_", mstring))
    if (length(iind) > 0){
      mper <- paste0('_', ifelse(is.na(skill$period[iind]), '[0-9].*', skill$period[iind]))
      mobs <- paste0('_', ifelse(is.na(skill$obs[iind]), '.*', skill$obs[iind]))
      mstring[iind] <- paste0(skill$method[iind], mper, mobs)
      mstring[grep('none_', mstring)] <- skill$method[grep("none_", mstring)]
    }
    if (is.null(reference)){
      filepaths <- paste(dpath, 'skill_scores', grid, skill$granularity, skill$index, sep='/')
      filenames <- paste0('^', skill$index, c('', '_detrend')[skill$detrend*1 + 1],
                          ifelse(skill$seasmethod == 'none',
                                 '',
                                 paste0('_', toupper(skill$seasmethod))),
                          '_',
                          mstring, '_', skill$model, '_vs_',
                          ifelse(is.na(skill$obs), '.*', skill$obs),
                          '_', ifelse(is.na(skill$period), '.*-.*', skill$period),
                          '_initmon', skill$initmon, '.nc$')
    } else {
      filepaths <- paste(dpath, 'skill_against_reference', grid, skill$granularity, skill$index, sep='/')
      filenames <- paste0('^', skill$index, c('', '_detrend')[skill$detrend*1 + 1],
                          ifelse(skill$seasmethod == 'none',
                                 '',
                                 paste0('_', toupper(skill$seasmethod))),
                          '_',
                          mstring, '_', skill$model, '-ref-', reference, '_vs_',
                          ifelse(is.na(skill$obs), '.*', skill$obs),
                          '_', ifelse(is.na(skill$period), '.*-.*', skill$period),
                          '_initmon', skill$initmon, '.nc$')
    }

    ## read in scores
    oldinfile <- "0"
    for (i in order(paste(filepaths, filenames, sep='/'))){
      infile <- list.files(filepaths[i], filenames[i], full.names=TRUE)
      if (length(infile) == 1){
        ## deparse infile for method string
        method <- gsub("detrend_", "", gsub(paste0(toupper(skill$seasmethod[i]), "_"), "", gsub(paste0(skill$index[i], '_'), '', gsub(paste0('_', skill$model[i], '.*'), '', basename(infile)))))
        skill$method[i] <- method

        ## check whether infile is already openend
        if (infile == oldinfile){
          nc <- ncold
        } else {
          if (!is.null(names(nc))) nc <- ncdf4::nc_close(nc)
          nc <- ncdf4::nc_open(infile)
        }
        oldinfile <- infile
        ncold <- nc
        if (skill$score[i] %in% names(nc$var)){
          dtmp <- try(ncdf4::ncvar_get(nc, as.character(skill$score[i])), silent=TRUE)
          if (class(dtmp) != 'try-error'){
            if (length(dim(dtmp)) == 2){
              warning(paste0("No lead times in", infile, "Are you sure you are reading in the right lead times?"))
            } else {
              if (skill$lead[i] == 'last') {
                lead <- dim(dtmp)[3]
              } else {
                lead <- as.numeric(as.character(skill$lead))
              }
              dtmp <- dtmp[,,lead]
            }
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
      }
    } # end of loop on rows of skill
    if (!is.null(names(nc)))  nc <- ncdf4::nc_close(nc)

    if (cleanup){
      ## remove rows of skill with all values missing
      allmissing <- apply(skill[,ncol(skill) - rev(seq(lsm) - 1)], 1, function(x) all(is.na(x)))
      if (any(allmissing)) skill <- skill[-which(allmissing), ]
    }

    ## fix method strings
    methods <- gsub("_....-...._.*", "", methods)
    skill$method <- gsub("_....-...._.*", "", skill$method)

    skill.long[[grid]] <- reshape2::melt(skill, id.vars=skillnames, variable.name='gridID')
    skill.long[[grid]][['gridID']] <- as.numeric(as.character(skill.long[[grid]][['gridID']]))
    ## add in land sea mask, latitudes and longitudes
    skill.long[[grid]][['lsm']] <- c(lsm)[skill.long[[grid]][['gridID']]]
    skill.long[[grid]][['lon']] <- c(lon)[skill.long[[grid]][['gridID']]]
    skill.long[[grid]][['lat']] <- c(lat)[skill.long[[grid]][['gridID']]]
    skill.long[[grid]][['reference']] <- reference
    ## convert method string to factors
    skill.long[[grid]][['method']] <- factor(skill.long[[grid]][['method']], unique(skill.long[[grid]][['method']])[pmatch(unique(methods), unique(skill.long[[grid]][['method']]))])
  } # end of loop on grids

  ## output single data frame if only one grid is read
  if (length(grids) == 1) skill.long <- skill.long[[1]]

  return(skill.long)
}
