#' extractBiom
#' @description Create Biomass variable by extracting relevant values from NetCDF files
#' @param stat
#' @param biomdir The directory where to find the NetCDF files with biomass values.
#' @param append Should the variable be returned with the entire statdives object ?
#' @author Yves
#' @export
extractBiom <- function(stat, tdr, biomdir, append=TRUE) {
  
  any(grepl("Lat", names(stat))) || stop("'stat' dataset must contain a 'Lat' variable.")
  any(grepl("Lon", names(stat))) || stop("'stat' dataset must contain a 'Lon' variable.")
  any(grepl("Date", names(stat))) || stop("'stat' dataset must contain a 'Date' variable.")
  any(grepl("Pixel.id", names(tdr))) || stop("'stat' dataset must contain a 'Pixel.id' variable.")  
  any(grepl("Layer", names(tdr))) || stop("'tdr' dataset must contain a 'Layer' variable.")  
  any(grepl("Pixel.id", names(tdr))) || stop("'tdr' dataset must contain a 'Pixel.id' variable.")  
  
  biom <- rep(NA, nrow(tdr))
  
  for (date in unique(stat$Date)){
    biomDate <- importSEAPOpred(date, biomdir)
    if (all(is.na(biomDate))) next
    pixels <- na.omit(unique(stat$Pixel.id[stat$Date == date]))
    
    for (pix in pixels){
      cond <-  tdr$Date == date & tdr$Pixel.id == pix
      layers <- unique(tdr$Layer[cond])
      is.day <- unique(tdr$is.Day[cond])
      for (layer in layers){
        for (day in is.day){
          val <- layerBiom(biomDate[pix, 3:8], layers=layer, is.day=day)
          biom[cond & tdr$Layer==layer & tdr$is.Day==day] <- val
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