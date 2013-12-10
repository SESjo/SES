#' extractBiom
#' @description Create Biomass variable by extracting relevant values from NetCDF files
#' @param stat
#' @param biomdir The directory where to find the NetCDF files with biomass values.
#' @details When the time resolution of the biomass NetCDF files is > 1day, then the biomass is extracted in the pixel where the averaged daily location of seal belongs (Beware, implies day/night same location).
#' @author Yves
#' @export
extractBiom <- function(stat, tdr, biomdir) {
  
  findVars(c("Lat", "Lon", "Date", "Pixel.id"), stat)
  findVars(c("Layer", "is.Day", "Pixel.id", "Date"), tdr, varnames=c("tdrLayer", "tdris.Day", "tdrPixel.id", "tdrDate"))

  biom <- rep(NA, nrow(tdr))
  
  ncfiles <- list.files(biomdir, "*.nc", full.names=TRUE)
  ncres <- median(diff(text2posx(ncfiles), lag=1, units="day"))
  message(paste("Time resolution of micronekton biomass input is", ncres, "day(s)"))
  if (ncres == 7){
    tmp <- aggregate(cbind(Lat, Lon), by=list(Date=Date), mean)
    biomgrid <- ncgrid(ncfiles[1])
    tmp <- idPixel(tmp, biomgrid)
    pixelstot <- na.omit(unique(tmp))
  }
  
  for (date in unique(Date)){
    biomDate <- importSEAPOpred(date, biomdir)
    if (all(is.na(biomDate))) next
    if (ncres == 1){pixels <- na.omit(unique(Pixel.id[Date == date]))}
    else if (ncres == 7){pixels <- na.omit(unique(pixelstot$Pixel.id[pixelstot$Date == date]))}
    
    for (pix in pixels){
      if (ncres == 1){cond <- tdrDate == date & tdrPixel.id == pix}
      else if (ncres == 7){cond <- tdrDate == date} # Pixel.id is recomputed according to the daily averaged locations
      layers <- unique(tdrLayer[cond])
      is.day <- unique(tdris.Day[cond])
      for (layer in layers){
        for (day in is.day){
          val <- layerBiom(biomDate[pix, 3:8], layers=layer, is.day=day)
          biom[cond & tdrLayer==layer & tdris.Day==day] <- val
        }
      } 
    }
  }
  return(unlist(biom))
}


layerBiom <- function(grp, all.col=FALSE, layers=NULL, is.day=NULL){
  # Compute the biomass in each layer during the day and night periods
  # grp = c(epi, meso, mmeso, bathy, mbathy, hmbathy)
  # all(lay.biom(1:6)$Biom == c(4, 10, 7, 15, 1, 5)) # Checking, must be TRUE
  tab <- expand.grid(Layer=c("Bathy", "Epi", "Meso"), is.Day=c(FALSE, TRUE))
  tab$Biom <- rep(NA, nrow(tab))
  tab$Biom[tab$is.Day] <- c(sum(grp[4:6]), grp[1], sum(grp[2:3]))
  tab$Biom[!tab$is.Day] <- c(grp[4], sum(grp[c(1,3,6)]), sum(grp[c(2,5)]))
  if (!is.null(layers)) tab <- tab[tab$Layer %in% layers, ]
  if (!is.null(is.day)) tab <- tab[tab$is.Day %in% is.day, ]
  if (all.col) return(tab)
  else return(tab$Biom)
}
