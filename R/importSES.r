#' ImportSES
#' @description Import an individual from a .mat file to R workspace is a standart way
#' @param matfile Path to .mat file. 
#' @param type Type of data to import: "tdr" only TDR data, "stat" Only Statdives data. "Bot" both.
#' @return An object of class "ses". Includes: ID of the SES - TDR and or Stats of dive as requested with 'type' argument.
#' @details The .mat file must be of of version v7 or less (last MATLAB version v7.3).
#' @seealso Edit 'SESformat' in the source code to modify the output format (type of columns)
#' @author Yves
#' @export
#' @import R.matlab
#' @examples
#' path <- system.file("extdata", package="SES")
#' pathname <- file.path(path, "2011-16_SES_example_accelero.mat")
#' ses <- importSES(pathname)
importSES <- function (matfile, type="both"){
	
	old.opt <- options("warn") ; options(warn=-1)
	
	if (length(matfile) == 1){
		matdata <- readMat(matfile) ; options(old.opt)
		res <- list(Ind.id=SESname(matfile), 
					tdr=data.frame(), stat=data.frame())
		class(res) <- c("ses", "list")
		
		if (type != "stat"){
			if (any(grepl("tdrcor2", names(matdata)))){
				matdata2 <- matdata
			}else{
				matdata2 <- matdata[[grep("tdrcor2", lapply(matdata, names))]]
			}
			res$tdr <- as.data.frame(matdata2$tdrcor2)
			res$tdr <- renames(type="tdr", obj=res$tdr, objtxt=matdata2$tdrcor2txt)
			res$tdr$Time <- datenum2posx(res$tdr$Time)
			res$tdr[, grep("is.", names(res$tdr))] <- as.logical(res$tdr[, grep("is.", names(res$tdr))])
			res$tdr[] <- lapply(res$tdr, replaceMissing) # Replace matlab's NaN by NA
		}
		class(res$tdr) <- c("tdr", "data.frame")
		if (type == "tdr") return(res)
		
		if (type != "tdr"){
			if (any(grepl("statdives", names(matdata)))){
				matdata2 <- matdata
			}else{
				matdata2 <- matdata[[grep("statdivestxt", lapply(matdata, names))]]
			}
			res$stat <- as.data.frame(matdata2$statdives)
			res$stat <- renames(type="stat", obj=res$stat, objtxt=matdata2$statdivestxt)
			res$stat$Time <- datenum2posx(res$stat$Time)
			res$stat[] <- lapply(res$stat, replaceMissing) # Replace matlab's NaN by NA
		}
		class(res$stat) <- c("statdives", "data.frame")
		if (type == "stat") return(res)
		
		return(res)
	}else{
		res <- list(Ind.id=SESname(matfile[1]), 
					tdr=data.frame(), stat=data.frame())
		class(res) <- c("ses3D", "ses", "list")
		matfile <-  matfile[order(file.info(matfile)$size)]
		for(infile in matfile){
			matdata <- readMat(infile) 
			locs <- try(data.frame(Dive.id=matdata$dive.id.geor, 
								   Lat.i=matdata$gps.geor[ , 1],
								   Lon.i=matdata$gps.geor[ , 2],
								   Lat.f=matdata$gps.geor[ , 3],
								   lon.f=matdata$gps.geor[ , 4]))
			if (is.error(locs)){next}
			else{matfile <- matfile[-which(matfile == infile)] ; break}
		}
		if (is.error(locs)){stop("Multiple matfile input is reserved to 3D dives data.")}
		for(infile in matfile){
			matdata <- readMat(infile) 	
			res$stat <- try(renames(type="stat3D", obj=as.data.frame(matdata$data), objtxt=matdata$titre.colonne))
			if (is.error(res$stat)){next}
			else{
				res$stat$Dive.id <- seq_along(res$stat[ , 1])
				res$stat <- merge(locs, res$stat, by="Dive.id", all.y=TRUE)
				class(res$stat) <- c("statdives3D", "statdives", "data.frame")
				if(type == "stat") return(res)
				matfile <- matfile[-which(matfile == infile)] 
				break
			}
		}
		matdata <- readMat(matfile) 	
		res$tdr <- renames("tdr3D", as.data.frame(Reduce(rbind, matdata$GeoRefLatLong[ ])), matdata$GeoRefLatLong.titles)
		res$tdr$Dive.id <- rep(locs$Dive.id, sapply(matdata$GeoRefLatLong, nrow))
		class(res$tdr) <- c("tdr3D", "tdr", "data.frame")
		if (type == "tdr") res$stat <- NULL
		return(res)
	}
}


#' renames
#' @description Extract and rename tdr and statdives variables according to formatSES instructions.
#' @param type The type of dataset
#' @param obj The object to process.
#' @param objtxt The original names. tdrcortxt or statdivestxt in the .mat file.
#' @export
#' @keywords internal
#' @author Yves
renames <- function(type=c("tdr", "stat", "stat3D", "tdr3D"), obj, objtxt){
	findVars(type, formatSES, varnames="fmt", substring=FALSE)
	headers <- unlist(objtxt)
	newHeaders <- unname(fmt[headers, "alias"])
	if (ncol(obj) != length(newHeaders)){
		warning("The number of variables differs between 'statdives' and 'statdivestxt'. The nth first variable names are assumed to be the good ones.")
	}
	names(obj) <- newHeaders[1:ncol(obj)]
	if (any(match(fmt$alias[fmt$keep], names(obj), nomatch=0) == 0)){
		warning(paste0("The desired variable(s) ", 
					   paste(fmt$alias[fmt$keep][is.na(match(fmt$alias[fmt$keep], names(obj)))], collapse=" & "), 
					   " is(are) not available in statdives."))
	}
	obj <- obj[ , unique(match(fmt$alias[fmt$keep], names(obj), nomatch=0))] # unique needed to avoid duplicated columns with partial matching (e.g. lat and latitude)
	obj
}