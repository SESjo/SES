#' isDay
#' @description Use time and loc info to find if  events occured during the day or the night.
#' @param time 
#' @param loc
#' @param stat A statdives object can be used instead of the two previous arguments.
#' @param elevlim Sun elevation the thresholds to distinguish between day and night
#' @param append Should the entire updated object be returned (if 'stat' argument was used).
#' @details SEAPODYm criteria elevlim=c(-18, 18). Transition periods filled with NAs.
#' @seealso \code{\link{sunPosition}}
#' @author Yves
isDay <- function(time, loc, stat=NULL, elevlim=c(-18, 18), append=TRUE) {
	if (!is.null(stat)){
		any(grepl("Time", names(stat))) || stop("A 'Time' variable must be provided in 'stat'")
		time <- stat[ , "Time"]
		any(grepl("Lat", names(stat))) || stop("A 'Lat' variable must be provided in 'stat'")
		any(grepl("Lon", names(stat))) || stop("A 'Lon' variable must be provided in 'stat'")
		loc <- stat[ , c("Lat", "Lon")]
	}
	
	if (any(is.na(time))) stop("NA not allowed in 'time' argument")
	if (any(is.na(loc))) {
		locNA <- apply(is.na(loc), 1, sum) != 0
		sunAngle <- rep(NA, length(time))
		sunAngle[!locNA] <- sunPosition(time=time[!locNA], loc=loc[!locNA, ])$el
	}
	else {
	  locNA <- rep(FALSE, length(time))
		sunAngle <- sunPosition(time=time, loc=loc)$el
	}
	is.Day <- rep(NA, length(time))
	is.Day[!locNA & sunAngle > elevlim[2]] <- TRUE
	is.Day[!locNA & sunAngle < elevlim[1]] <- FALSE
	if (!is.null(stat) & append) {stat$is.Day <- is.Day ; return(stat)}
	else {return(is.Day)}
}
