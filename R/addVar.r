#' addVar
#' @description Copy a variable from an object to another within a ses.
#' @keywords Lat Lon
#' @param var The name of the variable to be copied.
#' @param from Source of location information ('tdr' or 'statdives' class).
#' @param to Where to add location inforation ('tdr' or 'statdives' class).
#' @param ses A SES from which use the data. If used 'from' and 'to' must be taken in "tdr" and "stat".
#' @param append logical. If TRUE, the function returns the entire object 'to' with the added column. if FALSE it returns the column.
#' @return Object given at the 'to' argument, updated with location information.
#' @details Missing values (NAs) of the choosen variable are transformed in zeros when pasted to a 'tdr' object.
#' @seealso \code{\link{addLoc}}
#' @author Yves
#' @export
addVar <- function(var, from, to, ses=NULL, append=TRUE){
	if (!is.null(ses)) {
		from <- eval(parse(text=paste0(substitute(ses), '$', from)))
		to <- eval(parse(text=paste0(substitute(ses), '$', to)))
	}
	argtdr <- match("tdr", c(class(from), class(to)) %wo% "data.frame", nomatch=0)
  existsVars("Dive.id", from)
	existsVars("Dive.id", to)
  
	if (argtdr == 1){
		to <- merge(to, unique(from[ , c("Dive.id",  var)]), by="Dive.id")
		if (!append) return(to[, var])
		class(to) <- c("statdives", "data.frame")
	} else if (argtdr == 2){
		if (any(is.na(from[ , var]))) {
			warning("Missing values (NAs) pasted as zeros")
			from[ , var] <- replaceMissing(from[ , var], na.0=NA, na.1=0)
		}
		dvs <- per(to$Dive.id)
		dvs$value[dvs$value != 0] <- from[ , var] 
		if (!append) return(rep(dvs$value, dvs$length))
		to[ , var] <- rep(dvs$value, dvs$length)
	}else{
		stop("Input object must be of class 'tdr', and 'statdives'.")
	}
	return(to)
}