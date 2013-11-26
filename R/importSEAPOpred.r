#' importSEAPOpred
#' @description Load a NetCDF file matching with a date and a directory.
#' @param data The date (class POSIXct) to load.
#' @param dir The directory to look into.
#' @param ncfile Alternatively to previous arguments, a NetCDF file can be given directlty.
#' @return Retuns a data frame with latitude, longitude and biomass of the six functional groups.
#' @author Yves
#' @export
#' @examples
#' path <- system.file("extdata", package="SES")
#' pathname <- file.path(path, "ker_0083x1d_catsat_vgpm_20111101_MTLPB.nc")
#' expl <- importSEAPOpred(ncfile=pathname)
#' require("fields")
#' brk <- c(quantile(expl$Meso.b, probs=seq(0, 0.98, length.out=30)), max(expl$Meso.b))
#' image.plot(as.image(expl$Meso.b, x=expl[, 1:2], nx=750, ny=250), breaks=brk, nlevel=30, zlim=brk[c(1,30)])
importSEAPOpred <- function(date, dir, ncfile=NULL) {
	
	require("ncdf")
	if (is.null(ncfile)){
		ncfiles <- list.files(dir, "*.nc", full.names=TRUE)
		ncfile <- ncfiles[which(text2posx(ncfiles) == date)]
	}
	
	if (length(ncfile) != 1) {
		stop(paste0("On" , date, ", none or several NetCDF files. \n", paste0(basename(ncfile), collapse="\n")))
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