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
#' @examples
#' testPts <- data.frame(Lat = c(-41,-3,3, 41), 
#'                      Lon = c(0, 0, 0, 0))
#' time <- data.frame(Year=rep(2012, 4), Month=rep(12, 4), Day=rep(22, 4),
#'                    Hour=10:13, Minute=rep(0, 4), Second=rep(0,4))
#' isDay(time, testPts)
#' isDay(convertTime(time, to="posx"), testPts)
isDay <- function(Time, loc, stat=NULL, elevlim=c(-18, 18), append=TRUE) {
	if (!is.null(stat)){
		statVars <- userHeader(c("Time", "Lat", "Lon"), type="stat")
		findVars(statVars, stat, varnames=c("Time", "Lat", "Lon"))
		loc <- data.frame(Lat=Lat, Lon=Lon)
	}
	
	if (any(is.na(Time))) stop("NA not allowed in 'Time' argument")
	if (any(is.na(loc))) {
		locNA <- apply(is.na(loc), 1, sum) != 0
		sunAngle <- try(rep(NA, nrow(Time)), silent=TRUE)
		if (is.error(sunAngle)) sunAngle <- rep(NA, length(Time))
		sunAngle[!locNA] <- sunPosition(time=Time[!locNA], loc=loc[!locNA, ])$el
	}
	else {
		locNA <- try(rep(FALSE, nrow(Time)), silent=TRUE)
		if (is.error(locNA)) locNA <- rep(FALSE, length(Time))
		sunAngle <- sunPosition(time=Time, loc=loc)$el
	}
	is.Day <- rep(NA, length(locNA))
	is.Day[!locNA & sunAngle > elevlim[2]] <- TRUE
	if (!is.null(stat) & append) {stat$is.Day <- is.Day ; return(stat)}
	else {return(is.Day)}
}
