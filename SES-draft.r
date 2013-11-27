library(devtools)
load_all("../SES/")

matfiles <- list.files("../../Data/acc", full.names=TRUE, recursive=TRUE)
matfiles <- matfiles[grep("/20[0-9]{2}-[0-9]{2}/", matfiles)]
matfiles <- matfiles[grep("accelero.mat", matfiles)]

ncdir <- "C:/Users/stationcalcul/Desktop/Yves/Data/biom_137"
biomgrid <- ncgrid(list.files(ncdir, "*.nc", full.names=TRUE)[1])

for (infile in matfiles) {
  
  # Import a seal dataset
  cat("Processing ", SESname(infile), " ...\n")
  ses <- importSES(infile)
  
  # Compute new variables associated with dives
  ses$stat <- isDay(stat=ses$stat)
  ses$stat <- na.omit(ses$stat)
  ses$stat <- idPixel(ses$stat, biomgrid) # to speed up ++
  ses$stat$Date <- convertTime(ses$stat$Time, to="posx", width=3)
  
  biom <- importSEAPOpred(date, ncdir)

  # Add these variable to tdr data
  vars <- c("Date", "Lat", "Lon", "Pixel.id", "is.Day")
  ses$tdr[ , vars] <- lapply(vars, addVar, from="stat", to="tdr", ses=ses, append=FALSE)

}

# SESformat 	correct
# importSEAPOpred(file= )
# format.tdr
# format.statdives
# as.SES / is.SES
# as.tdr / is.tdr
# as.statdive / is.statdive

grp2layer <- function(stat, biom) {
  
  df <- expand.grid(Dive.id=stat$Dive.id, Layer=c("Epi", "Meso", "Bathy"))
  df <- df[order(df$Dive.id), ]
  
  tab <- apply(biom[1:3, 3:8], 1, layerBiom)
  
  
  
  biom <- importSEAPOpred(date, ncdir)
  
  
}

layerBiom <- function(grp, all.col=FALSE){
  # Compute the biomass in each layer during the day and night periods
  # grp = c(epi, meso, mmeso, bathy, mbathy, hmbathy)
  # all(lay.biom(1:6)$Biom == c(4, 10, 7, 15, 1, 5)) # Checking, must be TRUE
  tab <- expand.grid(Layer=c("Bathy", "Epi", "Meso"), is.Day=c(FALSE, TRUE))
  tab$Biom <- rep(NA, nrow(tab))
  tab$Biom[tab$is.Day] <- c(sum(grp[4:6]), grp[1], sum(grp[2:3]))
  tab$Biom[!tab$is.Day] <- c(grp[4], sum(grp[c(1,3,6)]), sum(grp[c(2,5)]))
  if (all.col) return(tab)
  else return(tab$Biom)
}