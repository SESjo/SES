#' idPixel
#' 
#' Given a grig, attribute to the dives the pixel number they belong to.
#' 
#' @param stat A statdives object.
#' @param grid The grid to look pixel into.
#' @param ses A ses object. 'stat' is ignored when given.
#' @param append Should the entire statdives object be returned.
#' @details The pixel number is row number of the 'grid' data frame.
#' @family SESspacial
#' @export
#' @examples
#' path <- system.file("extdata", package="SES")
#' pathname <- file.path(path, "ker_0083x1d_catsat_vgpm_20111101_MTLPB.nc")
#' expl <- importSEAPOpred(ncfile=pathname)
#' pathname <- file.path(path, "2011-16_SES_example_accelero.mat")
#' ses <- importSES(pathname)
#' ses$stat <- idPixel(ses$stat, expl[ , 1:2])
#' # ses$tdr <- addVar("Pixel.id", from="stat", to="tdr", ses=ses)
idPixel <- function(stat, grid, ses=NULL, append=TRUE) {
	if (!is.null(ses)) {
		stat == "stat" ||  stop("'stat' must be set to 'stat' or left blank when 'ses' is given.")
		stat <- eval(parse(text=paste0(substitute(ses), '$stat')))
	}
	statVars <- userHeader(c("Lat", "Lon"), type="stat")
	findVars(statVars, stat, type="check")
	findVars(c("Lat", "Lon"), grid, type="check")
	
	# Find nearest superior values of a grid  variable
	nearestBins <- function(col){
		gridcol <- grid[ , col] ; datacol <- stat[ , statVars[col]]
		delta <- outer(unique(gridcol), datacol, `-`) ; delta[delta > 0] <- -Inf ; delta[is.na(delta)] <- - Inf
		res <- median(diff(unique(gridcol), 1)) ; ok <- abs(apply(delta, 2, max)) <= abs(res)
		delta <- unique(gridcol)[apply(delta, 2, which.max)] ; delta[!ok] <- NA
		delta
	}
	vals <- list()
	vals[c("Lat", "Lon")] <- lapply(c("Lat", "Lon"), FUN=nearestBins)
	
	# Identify the corresponding pixel number and return results
	findPix <- function(lat, lon, grid) {
		if (is.na(lat) || is.na(lon)) return(NA)
		return(as.integer(which(grid$Lat == lat & grid$Lon == lon)))
	}
	if (!append) return(mapply(findPix, vals$Lat, vals$Lon, MoreArgs=list(grid=grid)))
	stat$Pixel.id <- mapply(findPix, vals$Lat, vals$Lon, MoreArgs=list(grid=grid))
	return(stat)
}

#' ncgrid
#' 
#' Extract the grid from a NetCDF file.
#' 
#' @param ncfile The to read the grid from.
#' @param connection An existing connexion with a file can be used instead of \code{ncfile} argument.
#' @param latname The variable name of the meridional dimension of the grid.
#' @param lonname The variable name of the zonal dimension of the grid.
#' @param zname The optional name of a third dimension of the grid e.g. 'depthu'.
#' @return Retuns a data frame with combinations of latitude and longitude grid breaks.
#' @family SESspacial
#' @export
#' @import ncdf
#' @examples
#' path <- system.file("extdata", package="SES")
#' pathname <- file.path(path, "ker_0083x1d_catsat_vgpm_20111101_MTLPB.nc")
#' expl <- ncgrid(ncfile=pathname)
ncgrid <- function(ncfile, connection, 
                   latname = 'latitude', lonname = 'longitude', zname = NULL){
  if (missing(connection)) {
  	con <- open.ncdf(ncfile)
  } else {
  	con <- connection
  }
  if (missing(connection)) close.ncdf(con)
  lat <- con$dim[[latname]]$vals
  lon <- con$dim[[lonname]]$vals
  if (is.null(zname)){
    return(expand.grid(Lon=lon, Lat=lat))
  } else {
    args <- list(Lon=lon, Lat=lat)
    args[[zname]] <- con$dim[[zname]]$vals
    return(do.call(expand.grid, args))
  }
}
