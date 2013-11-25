#' addLoc
#' @description Add location Latitude and Longitude from an an obeject to another within a ses
#' @keywords Lat Lon
#' @param from Source of location information ('tdr' or 'statdives' class).
#' @param to Where to add location inforation ('tdr' or 'statdives' class).
#' @param ses A SES from which use the data. If used 'from' and 'to' must be taken in "tdr" and "stat".
#' @return Object given at the 'to' argument, updated with location information.
#' @seealso \code{\link{importSES}}
#' @author Yves
#' @export
addLoc <- function(from, to, ses=NULL){
	
	if (!is.null(ses)) {
		from <- eval(parse(text=paste0(substitute(ses), '$', from)))
		to <- eval(parse(text=paste0(substitute(ses), '$', to)))
	}
	argtdr <- match("tdr", c(class(from), class(to)) %wo% "data.frame", nomatch=0)
	any(grepl("Dive.id", names(from))) || stop("'From' dataset must contain 'Dive.id' variable (Primary key).")
	any(grepl("Dive.id", names(to))) || stop("'From' dataset must contain 'Dive.id' variable (Primary key).")
	any(grepl("Lat", names(from))) || stop("'From' dataset must contain 'Lat' variable.")
	any(grepl("Lon", names(from))) || stop("'From' dataset must contain 'Lat' variable.")
	if (argtdr == 1){
		to <- merge(to, unique(from[ , c("Dive.id", "Lat", "Lon")]), by="Dive.id")
		class(to) <- c("statdives", "data.frame")
	} else if (argtdr == 2){
	  dvs <- seqs(to$Dive.id)
	  dvs$value[dvs$value != 0] <- from$Lat ; to$Lat <- rep(dvs$value, dvs$length)
	  dvs$value[dvs$value != 0] <- from$Lon ; to$Lon <- rep(dvs$value, dvs$length)
	}else{
		stop("Input object must be of class 'tdr', and 'statdives'.")
	}
	return(to)
}