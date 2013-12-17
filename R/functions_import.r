#' ImportSES
#' 
#' Import an individual from a .mat file to R workspace is a standart way. The .mat file must be of 
#' version v7 or older (last MATLAB version v7.3): \code{R.matlab} requirement. Edit 
#' \code{formatSES} object to modify the importation preferences. See \code{\link{print.fmtSES}} 
#' documentation to learn how.
#' 
#' @param matfile Path to .mat file(s). If \code{matfiles} is an atomic vector, then the input is
#' interpreted as a request to load a '3D' ses. Use \code{lapply()} to import several ses at once. 
#' @param type To choose among \code{tdr} (only TDR data), \code{stat} (only Statdives data) and 
#' \code{both} (for both of them).
#' 
#' @return An object of class \code{ses}. Includes: ID of the individual - TDR and/or dive statistics (according to the \code{type} argument).
#' 
#' @family settings
#' @import R.matlab
#' 
#' @export
#' @examples
#' path <- system.file("extdata", package="SES")
#' pathname <- file.path(path, "2011-16_SES_example_accelero.mat")
#' ses <- importSES(pathname)
importSES <- function (matfile, type=c("both", "tdr", "stat")){
	
	old.opt <- options("warn") ; options(warn=-1)
	
	if (length(matfile) == 1){
		matdata <- readMat(matfile) ; options(old.opt)
		res <- list(Ind.id=SESname(matfile), 
					tdr=data.frame(), stat=data.frame())
		class(res) <- c("ses", "list")
		
		desiredData <- c("tdrcor2", "tdrcor2txt", "statdives", "statdivestxt")
		type <- switch(match.arg(type), both = "*", tdr = "tdr", stat = "stat")
		desiredData <- desiredData[grep(type, desiredData)]
		findVars(desiredData, matdata, substring=FALSE, ignore.case=FALSE, ignore.depth.error=TRUE)
		rm(matdata)
		
		if (type != "stat"){
			res$tdr <- as.data.frame(tdrcor2)
			res$tdr <- renames(type="tdr", obj=res$tdr, objtxt=tdrcor2txt)
			rm(list=desiredData[grep("tdr", desiredData)]) ; gc()
			res$tdr$Time <- datenum2posx(res$tdr$Time)
			res$tdr[, grep("is.", names(res$tdr))] <- as.logical(res$tdr[, grep("is.", names(res$tdr))])
			res$tdr[] <- lapply(res$tdr, replaceMissing) # Replace matlab's NaN by NA
		}
		class(res$tdr) <- c("tdr", "data.frame")
		if (type == "tdr") return(res)
		
		if (type != "tdr"){
			res$stat <- as.data.frame(statdives)
			res$stat <- renames(type="stat", obj=res$stat, objtxt=statdivestxt)
			rm(list=desiredData[grep("stat", desiredData)])
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
			if (inherits(locs, "try-error")){next}
			else{matfile <- matfile[-which(matfile == infile)] ; break}
		}
		if (inherits(locs, "try-error")){stop("Multiple matfile input is reserved to 3D dives data.")}
		for(infile in matfile){
			matdata <- readMat(infile) 	
			res$stat <- try(renames(type="stat3D", obj=as.data.frame(matdata$data), objtxt=matdata$titre.colonne))
			if (inherits(res$stat, "try-error")){next}
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

#' importSEAPOpred
#' 
#' Load a NetCDF file matching with a date and a directory. When the time resolution of the biomass
#' NetCDF files is > 1day, then file whose date is the closest from \code{date} argument is loaded.
#' 
#' @param date The date (of type \code{POSIXct}) to load.
#' @param dir The directory to look into.
#' @param ncfile Alternatively to previous arguments, a specific NetCDF file can be given directlty.
#' @return Returns a data frame with latitude, longitude and biomass of the six functional groups.
#' @family SESspacial
#' @import ncdf
#' @export
#' @examples
#' path <- system.file("extdata", package="SES")
#' pathname <- file.path(path, "ker_0083x1d_catsat_vgpm_20111101_MTLPB.nc")
#' expl <- importSEAPOpred(ncfile=pathname)
#' require("fields")
#' brk <- c(quantile(expl$Meso.b, probs=seq(0, 0.98, length.out=30)), max(expl$Meso.b))
#' image.plot(as.image(expl$Meso.b, x=expl[, 1:2], nx=750, ny=250), breaks=brk, nlevel=30, zlim=brk[c(1,30)])
importSEAPOpred <- function(date, dir, ncfile=NULL) {
	
	if (is.null(ncfile)){
		ncfiles <- list.files(dir, "*.nc", full.names=TRUE)
		ncres <- median(diff(text2posx(ncfiles), lag=1, units="day"))
		if (ncres == 1){
			ncfile <- ncfiles[which(text2posx(ncfiles) == date)]
		}else if (ncres == 7){
			ncfile <- ncfiles[which.min(abs(as.numeric(text2posx(ncfiles) - date)))]
		}else {warning(paste0("Unusual time resolution of NetCDF files:", ncres))}
	}
	
	if (length(ncfile) != 1) {
		stop(paste0("On " , as.character(date), ", none or several NetCDF files. \n", paste0(basename(ncfile), collapse="\n")))
		return(NA)
	} else {
		con <- open.ncdf(ncfile)
		grps <- names(con$var)
		headers <- c("epi_mnk_pb"="Epi.b", "meso_mnk_pb"="Meso.b", "mmeso_mnk_pb"="mMeso.b", "bathy_mnk_pb"="Bathy.b", "mbathy_mnk_pb"="mBathy.b", "hmbathy_mnk_pb"="hmBathy.b")
		grid <- ncgrid(ncfile, connection=con)
		grid <- cbind(grid, replicate(length(grps), rep(0, nrow(grid))))
		names(grid) <- c("Lon", "Lat", headers[grps])
		for (grp in grps){
			grid[ , headers[grp]] <- as.numeric(get.var.ncdf(con, varid=grp))
		}
		close.ncdf(con)
	}
	
	return(grid)
	
}

#' importChl
#' 
#' Load a NetCDF file matching with a date and a directory.
#' 
#' @param date The date (class POSIXct) to load.
#' @param dir The directory to look into.
#' @param ncfile Alternatively to previous arguments, a NetCDF file can be given directlty.
#' @return Returns a data frame with latitude, longitude and the associate [Chl].
#' @family chl
#' @import ncdf
#' @export
importChl <- function(date, dir, ncfile=NULL){
	if (is.null(ncfile)){
		ncfiles <- list.files(dir, "*.nc", full.names=TRUE)
		ncfile <- ncfiles[which(text2posx(ncfiles) == date)]
		if (length(ncfile) == 0){
			warning(paste0("On " , as.character(date), ", no NetCDF files. \n"))
			return(NA)
		}
	}
	if (length(ncfile) != 1) {
		warning(paste0("On '" , as.character(date), "', several NetCDF files. \n", paste0(basename(ncfile), collapse="\n")))
		return(NA)
	} else {
		con <- open.ncdf(ncfile)
		grid <- ncgrid(ncfile, connection=con)
		grid$Chl <- as.numeric(get.var.ncdf(con, varid="chl"))
		close.ncdf(con)
	}
	return(grid)
}
