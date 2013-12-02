#' addLoc
#' @description Copy locations (Latitude and Longitude) from an an object to another within a ses.
#' @keywords Lat Lon
#' @param from Source of location information ('tdr' or 'statdives' class).
#' @param to Where to add location inforation ('tdr' or 'statdives' class).
#' @param ses A SES from which use the data. If used 'from' and 'to' must be taken in "tdr" and "stat".
#' @param append logical. If TRUE, the function returns the entire object 'to' with the added column. if FALSE it returns the columns.
#' @return Object given at the 'to' argument, updated with location information.
#' @seealso \code{\link{addVar}} \code{\link{importSES}}
#' @author Yves
#' @export
#' @examples
#' path <- system.file("extdata", package="SES")
#' pathname <- file.path(path, "2011-16_SES_example_accelero.mat")
#' ses <- importSES(pathname)
#' ses$tdr <- addLoc(ses$stat, ses$tdr)
#' head(ses$tdr)
addLoc <- function(from, to, ses=NULL, append=TRUE){
	if (!is.null(ses)) {
		from <- eval(parse(text=paste0(substitute(ses), '$', from)))
		to <- eval(parse(text=paste0(substitute(ses), '$', to)))
	}
  existsVars(c("Lat", "Lon"), from)
  
	if (!append) return(lapply(c("Lat", "Lon"), addVar, from, to, append=FALSE))
	to[ , c("Lat", "Lon")] <- lapply(c("Lat", "Lon"), addVar, from, to, append=FALSE)
	return(to)
}
