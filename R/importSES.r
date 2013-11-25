#' ImportSES
#' @description Import an individual from a .mat file to R workspace is a standart way
#' @keywords Import SES .mat
#' @param matfile Path to .mat file. 
#' @param type Type of data to import: "tdr" only TDR data, "stat" Only Statdives data. "Bot" both.
#' @return An object of class "ses". Includes: ID of the SES - TDR and or Stats of dive as requested with 'type' argument.
#' @seealso SESformat (df) to edit output format (column and class of columns)
#' @author Yves
#' @export
importSES <- function (matfile, type=c("both", "tdr", "stat")){
	
	old.opt <- options("warn") ; options(warn=-1)
	
	tdrfmt <- c("ptimes_num"="Time", "corrected_depth"="Depth", "external_temperature"="Temp", "ligth_level"="Light", "capture"="is.Catch", "captureX"="is.Catch.x", "captureY"="is.Catch.y", "captureZ"="is.Catch.z", "num_evts"="Catch.id", "no_jour"="Day.id", "no_dive"="Dive.id", "type_dive"="Dive.type", "phase_dive"="Dive.step")
	statfmt <- c("ptimes_num"="Time")
	tdrkeep <- c("Time", "Depth", "Light", "Catch.id", "Dive.id")
	statkeep <- c("Dive.id", "Depth.max", "Dive.duration", "nCatch", "Lat", "Lon")
	SESformat <- list(tdrfmt=tdrfmt, statfmt=statfmt, tdrkeep=tdrkeep, statkeep=statkeep)
	rm("tdrfmt", "statfmt", "tdrkeep", "statkeep")
	
	require("R.matlab") 
	matdata <- readMat(matfile) ; options(old.opt)
	res <- list(Ind.id=SESname(matfile), 
				tdr=data.frame(), stat=data.frame())
	class(res) <- c("SES", "list")
	class(res$tdr) <- c("tdr", "data.frame")
	class(res$stat) <- c("statdives", "data.frame")
	
	if (type != "stat"){
		res$tdr <- as.data.frame(matdata$tdrcor2)
		headers <- unlist(matdata$tdrcor2txt)
		names(res$tdr) <- unname(SESformat$tdrfmt[headers])
		warning(paste0("The desired variable(s) ", 
					   paste(SESformat$keeptdr[match(SESformat$keeptdr, names(res$tdr), nomatch=0)], collapse=" & "), 
					   " is(are) not available in tdrcor2."))
		res$tdr <- res$tdr[, match(SESformat$tdrkeep, names(res$tdr), nomatch=0)]
		res$tdr$Time <- as.POSIXct((res$tdr$Time - 719529)*24*3600, tz="UTC", origin="1970-01-01")
		res$tdr[, grep("is.", names(res$tdr))] <- as.logical(res$tdr[, grep("is.", names(res$tdr))])
		res$tdr[] <- lapply(res$tdr, ReplaceMissing) # Replace matlab's NaN by NA
		type == "tdr" && return(res)}
	
	if (type != "tdr"){
		res$stat <- as.data.frame(matdata$statdives)
		headers <- unlist(matdata$statdivestxt)
		names(res$stat) <- unname(SESformat$statfmt[headers])
		warning(paste0("The desired variable(s) ", 
					   paste(SESformat$statkeep[match(SESformat$statkeep, names(res$stat), nomatch=0)], collapse=" & "), 
					   " is(are) not available in statdives."))
		res$stat <- res$stat[, match(SESformat$statkeep, names(res$stat), nomatch=0)]
		res$stat[] <- lapply(res$stat, ReplaceMissing) # Replace matlab's NaN by NA
		type == "stat" && return(res)}
}

