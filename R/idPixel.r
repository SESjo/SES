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
  existsVars(c("Dive.id", "Lat", "Lon"), stat)
	existsVars(c("Lat", "Lon"), grid)

	pix <- function(lat, lon, grid) {
    if (is.na(lat) || is.na(lon)) return(NA)
		d.lat <- grid$Lat - lat ; d.lat[d.lat > 0] <- -Inf
		latSupLim <- grid$Lat[which.max(d.lat)]
		d.lon <- grid$Lon - lon ; d.lon[d.lon > 0] <- -Inf
		lonSupLim <- grid$Lon[which.max(d.lon)]
		if (abs(max(d.lat)) <= median(diff(unique(grid$Lat), 1)) || abs(max(d.lon)) <= median(diff(grid$Lon, 1))){
			return(as.integer(which(grid$Lat == latSupLim & grid$Lon == lonSupLim)))
		} else {
			return(NA)
		}
	}
		
	if (!append) return(mapply(pix, stat$Lat, stat$Lon, MoreArgs=list(grid=grid)))
	stat$Pixel.id <- mapply(pix, stat$Lat, stat$Lon, MoreArgs=list(grid=grid))
	return(stat)
}
