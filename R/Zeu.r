#' mZeu2layer
#' @description Assign to a record the SEAPODYM layer it beyond using the depth expressed in multiple of Zeu.
#' @param mZeu
#' @details Layers limits taken to be 1.5, 4.5 and 10.5 time Zeu.
#' @author Yves
#' @export
mZeu2layer <- function(mZeu){
  # Find the layer to wich a depth expressed in mZeu belongs
  Layer <- rep(NA, length(mZeu))
  Layer[mZeu > 4.5] <- "Bathy" ; Layer[is.na(Layer) & mZeu > 1.5] <- "Meso" ; Layer[is.na(Layer)] <- "Epi"
  return(as.character(Layer))
}


#' modelMorel
#' @description Predict euphotic depth (m) from the surface chlorophyll concentration
#' @param chl
#' @param stat A statdives object. 'chl' argument is ignored. The values are taken from this statdive object.
#' @param append Should the variable be returned with the entire statdives object ?
#' @references Morel and Berthon (1989)
#' @author Yves
#' @export
modelMorel <- function(chl, stat=NULL, append=TRUE){
  
  if (!is.null(stat)){
    any(grepl("Chl", names(stat))) || stop("'stat' dataset must contain a 'Chl' variable.")
    chl <- stat$Chl
  }
  Zeu <- rep(NA, length(chl))
  cond <- !is.na(chl) ; chl <- chl[cond]
  chl.tot.strat <- 38*chl^(0.425)
  chl.tot.strat[chl > 1] <- 40.2*chl[chl > 1]^(0.507)
  Zeu[cond] <- 200*chl.tot.strat^(-0.293)
  if (!is.null(stat) && append){
    stat$Zeu <- Zeu
    return(stat)
  } else {return(Zeu)}
}


#' extractChl
#' @description Create [Chl]_surf variable by extracting relevant values from NetCDF files
#' @param stat
#' @param chldir The directory where to find the NetCDF files with [chl] values.
#' @param append Should the variable be returned with the entire statdives object ?
#' @author Yves
#' @export
extractChl <- function(stat, chldir, append=TRUE) {
  
  any(grepl("Lat", names(stat))) || stop("'stat' dataset must contain a 'Lat' variable.")
  any(grepl("Lon", names(stat))) || stop("'stat' dataset must contain a 'Lon' variable.")
  any(grepl("Date", names(stat))) || stop("'stat' dataset must contain a 'Date' variable.")
  
  chl <- rep(NA, nrow(stat))
  chlgrid <- ncgrid(list.files(chldir, "*.nc", full.names=TRUE)[1])
  chlPix <- idPixel(stat, chlgrid, append=FALSE)
  
  for (date in unique(stat$Date)){
    chlDate <- importChl(date, chldir)
    if (all(is.na(chlDate))) next
    chl[stat$Date == date] <- chlDate$Chl[stat$Pixel.id[stat$Date == date]]
  }
  
  return(chl)
}


#' importChl
#' @description Load a NetCDF file matching with a date and a directory.
#' @param date The date (class POSIXct) to load.
#' @param dir The directory to look into.
#' @param ncfile Alternatively to previous arguments, a NetCDF file can be given directlty.
#' @return Retuns a data frame with latitude, longitude and the associate [Chl].
#' @author Yves
#' @export
importChl <- function(date, dir, ncfile=NULL){
  
  require("ncdf", quietly=TRUE)
  if (is.null(ncfile)){
    ncfiles <- list.files(dir, "*.nc", full.names=TRUE)
    ncfile <- ncfiles[which(text2posx(ncfiles) == date)]
    if (length(ncfile) == 0){
      warning(paste0("On " , date, ", no NetCDF files. \n"))
      return(NA)
    }
  }
  
  if (length(ncfile) != 1) {
    warning(paste0("On " , date, ", several NetCDF files. \n", paste0(basename(ncfile), collapse="\n")))
    return(NA)
  } else {
    con <- open.ncdf(ncfile)
    grid <- ncgrid(ncfile, connection=con)
    grid$Chl <- as.numeric(get.var.ncdf(con, varid="chl"))
    close.ncdf(con)
  }
  return(grid)
  
}