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
	any(grepl("Dive.id", from))  || stop("'From' dataset must contain 'Dive.id' variable (Primary key).")
	any(grepl("Dive.id", to))  || stop("'From' dataset must contain 'Dive.id' variable (Primary key).")
	any(grepl("Lat", from))  || stop("'From' dataset must contain 'Lat' variable.")
	any(grepl("Lon", from))  || stop("'From' dataset must contain 'Lat' variable.")
	if (argtdr == 1){
		to <- merge(to, unique(from[ , c("Dive.id", "Lat", "Lon")]), by="Dive.id")
		class(to) <- c("statdives", "data.frame")
	} else if (argtdr == 2){
		dives <- seqs(to$Dive.id) ; dives <- dives[dives$value != 0, ]
		coords <- data.frame(Lat=rep(0, nrow-from), Lon=rep(0, nrow(to)))
		tmpfun <- function(st, ed, val) {
			coords$Lat[st:ed] <- from$Lat[which(from$Dive.id == val)]
			coords$Lon[st:ed] <- from$Lon[which(from$Dive.id == val)]
		}
		coords <- Map(tmpfun,  dives$start, dives$end, dives$value)
		to <- cbind(to, coords)
		class(to) <- c("tdr", "data.frame")
	}else{
		stop("Input object must be of class 'tdr', and 'statdives'.")
	}
	return(to)
}