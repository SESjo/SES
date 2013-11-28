#' isDay
#' @description Use time and loc info to find if  events occured during the day or the night.
#' @param Time 
#' @param loc
#' @param stat A statdives object can be used instead of the two previous arguments.
#' @param elevlim Sun elevation the thresholds to distinguish between day and night
#' @param append Should the entire updated object be returned (if 'stat' argument was used).
#' @details SEAPODYm criteria elevlim=c(-18, 18). Transition periods filled with NAs.
#' @seealso \code{\link{sunPosition}}
#' @author Yves
#' @export
isDay <- function(time, loc, stat=NULL, elevlim=c(-18, 18), append=TRUE) {
	if (!is.null(stat)){
    findVars(c("Time", "Lat", "Lon"), stat)
		loc <- data.frame(Lat=Lat, Lon=Lon)
	}
	
	if (any(is.na(Time))) stop("NA not allowed in 'Time' argument")
	if (any(is.na(loc))) {
		locNA <- apply(is.na(loc), 1, sum) != 0
		sunAngle <- rep(NA, length(Time))
		sunAngle[!locNA] <- sunPosition(Time=Time[!locNA], loc=loc[!locNA, ])$el
	}
	else {
	  locNA <- rep(FALSE, length(Time))
		sunAngle <- sunPosition(Time=Time, loc=loc)$el
	}
	is.Day <- rep(NA, length(Time))
	is.Day[!locNA & sunAngle > elevlim[2]] <- TRUE
	is.Day[!locNA & sunAngle < elevlim[1]] <- FALSE
	if (!is.null(stat) & append) {stat$is.Day <- is.Day ; return(stat)}
	else {return(is.Day)}
}
