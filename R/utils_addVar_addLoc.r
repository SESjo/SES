#' addVar
#' 
#' Copy values of variable from an object to another within a ses.
#' 
#' @param var The name of the variable to be copied.
#' @inheritParams addLoc
#' @return Object given at the 'to' argument, updated with location information.
#' @details Missing values (NAs) of the choosen variable are transformed in zeros when pasted 
#' to a 'tdr' object.
#' @seealso \code{\link{addLoc}}
#' @export
addVar <- function(var, from, to, ses=NULL, append=TRUE){
	if (!is.null(ses)) {
		from <- eval(parse(text=paste0(substitute(ses), '$', from)))
		to <- eval(parse(text=paste0(substitute(ses), '$', to)))
	}
	argtdr <- match("tdr", c(class(from), class(to)) %wo% "data.frame", nomatch=0)
	
	dvidFrom <- userHeader("Dive.id", type=class(from)[1])
	findVars(dvidFrom, from, type="check")
	dvidTo <- userHeader("Dive.id", type=class(to)[1])
	findVars(dvidTo, to, type="check")
	
	if (argtdr == 1){
		to <- merge(to, unique(from[ , c(dvidFrom,  var)]), by.x=dvidTo, by.y=dvidFrom)
		if (!append) return(to[, var])
		class(to) <- c("statdives", "data.frame")
	} else if (argtdr == 2){
		if (any(is.na(from[ , var]))) {
			message("Missing values (NAs) pasted as zeros")
			from[ , var] <- replaceMissing(from[ , var], na.0=NA, na.1=0)
		}
		dvs <- per(to[ , dvidTo])
		dvs$value[dvs$value != 0] <- from[ , var] 
		if (!append) return(rep(dvs$value, dvs$length))
		to[ , var] <- rep(dvs$value, dvs$length)
	}else{
		stop("Input objects must be of class 'tdr', and 'statdives'.")
	}
	return(to)
}

#' addLoc
#' 
#' Copy locations (Latitude and Longitude) from onre table to another within a 'ses' object
#' 
#' @param from Source of location information ('tdr' or 'statdives' object).
#' @param to Where to add location inforation (respectively a 'statdives' or 'tdr' object).
#' @param ses A SES from which use the data. If used 'from' and 'to' must be taken in "tdr" and "stat".
#' @param append logical. If TRUE, the function returns the entire object 'to' with the 
#' added column. if FALSE it returns the columns.
#' @return Object given at the 'to' argument, updated with location information.
#' @seealso \code{\link{addVar}} \code{\link{importSES}}
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
	
	findDefaultVars(c("Lat", "Lon"), from, type.obj=class(from)[1]), type="check")
if (!append) return(lapply(vars, addVar, from, to, append=FALSE))
to[ , vars] <- lapply(vars, addVar, from, to, append=FALSE)
return(to)
}
