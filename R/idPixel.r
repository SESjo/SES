#' idPixel
#' @description Attribute to dives the pixel number they belong to.
#' @param stat A statdives object.
#' @param grid The grid to look pixel into.
#' @param ses A ses object. 'stat' is ignored when given.
#' @param append Should the entire statdives object be returned.
#' @param append.grid Should the variable 'Pixel.id' added to grid
#' @details The pixel number is row number of the 'grid' data frame.
#' @seealso \code{\link{addLoc}} \code{\link{importSEAPOpred}} \code{\link{importSES}}
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
  existsVars(c("Lat", "Lon"), stat)
	existsVars(c("Lat", "Lon"), grid)
  
  # Find nearest superior values of grid
  nearestBins <- function(col){
    gridcol <- grid[ , col] ; datacol <- stat[ , col]
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
