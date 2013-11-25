#' importSEAPOpred
#' @description Load a NetCDF file matching with a date and a directory.
#' @param data The date (class POSIXct) to load.
#' @param dir The directory to look into.
#' @return Retuns a data frame with latitude, longitude and biomass of the six functional groups.
#' @author Yves
importSEAPOpred <- function(date, dir) {
  
  require("ncdf")
  ncfiles <- list.files(dir, "*.nc", full.names=TRUE)
  ncfile <- ncfiles[which(text2posx(ncfiles) == date)]
  
  if (length(ncfile) != 1) {
    return(NA)
  } else {
    con <- open.ncdf(ncfile)
    lat <- con$dim$latitude$vals
    lon <- con$dim$longitude$vals
    grps <- names(con$var)
    headers <- c("epi_mnk_pb"="Epi.b", "meso_mnk_pb"="Meso.b", "mmeso_mnk_pb"="mMeso.b", "bathy_mnk_pb"="Bathy.b", "mbathy_mnk_pb"="mBathy.b", "hmbathy_mnk_pb"="hmBathy.b")
    grid <- expand.grid(lat, lon)
    grid <- cbind(grid, replicate(length(grps), rep(0, nrow(grid))))
    names(grid) <- c("Lat", "Lon", headers[grps])
    for (grp in grps){
      grid[ , headers[grp]] <- as.numeric(t(get.var.ncdf(con, varid=grp)))
    }
    # Add to test
    # image(get.var.ncdf(con, varid=grp))
    #  image.plot(as.image(Z=grid$hmBathy.b, x=grid[, 2:1]))
    close.ncdf(con)
  }

  return(grid)
  
}