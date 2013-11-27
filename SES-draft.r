library(devtools)

# Set up workspace and data directories ####
setwd("C:/Users/stationcalcul/Desktop/Yves/R/SES")
rm(list=ls())
# mkdirs("./output_CPUEvsBiomass")
# install.package("./SES_1.0.tar.gz", repos=NULL, type="source")
# library(SES)
load_all("../SES/")

# SES data - 22 individuals
matfiles <- list.files("../../Data/acc", pattern="*accelero.mat", full.names=TRUE, recursive=TRUE)
matfiles <- matfiles[grep("/20[0-9]{2}-[0-9]{2}/", matfiles)]

# SEAPODYM predictions of micronekton biomass
biomdir <- "C:/Users/stationcalcul/Desktop/Yves/Data/biom_137"

# CATSAT Surface [Chl] data
chldir <- "C:/Users/stationcalcul/Desktop/Yves/Data/chl_ker/"



# Building dataset from raw data ####
for (infile in matfiles) {
  
  # Import a seal dataset
  cat("Processing ", SESname(infile), " ...\n")
  ses <- importSES(infile)
  
  # Compute new variables associated with dives
  ses$stat <- isDay(stat=ses$stat)
  biomgrid <- ncgrid(list.files(biomdir, "*.nc", full.names=TRUE)[1])
  ses$stat <- idPixel(ses$stat, biomgrid) # to speed up
  ses$stat$Date <- convertTime(ses$stat$Time, to="posx", width=3)
  ses$stat$Chl <- extractChl(ses$stat, chldir)
  ses$stat <- modelMorel(stat=ses$stat)
  
  # Add these variable to TDR data
  vars <- c("Date", "Lat", "Lon", "Pixel.id", "is.Day", "Zeu")
  ses$tdr[ , vars] <- lapply(vars, addVar, from="stat", to="tdr", ses=ses, append=FALSE)
  ses$tdr$Layer <- mZeu2layer(ses$tdr$Depth / ses$tdr$Zeu)
  
  # Add Layer and micronekton biomass
  # biom <- importSEAPOpred(date, ncdir)
  
  
  # Remove unusable data from TDR
  cond <- with(ses$tdr, Date!=0 & Dive.id!=0 & Lat!=0 & Pixel.id!=0 & Layer!="0")
  ses$tdr <- ses$tdr[cond, ]
  
  # Compute new variables
  Catch.numb  <- aggregate(Catch.id ~ Date + Pixel.id + is.Day + Layer, data=ses$tdr,
                           function(x) length(unique(x)))
  Time.spent  <- aggregate(Time ~ Date + Pixel.id + is.Day + Layer, data=ses$tdr, length)
  Biomass     <- aggregate(Biomass ~ Date + Pixel.id + is.Day + Layer, data=ses$tdr, unique)
  
  # Finalizing current individual ...
  data <- merge(merge(Catch.numb, Time.spent), Biomass)
  names(data) <- c("Date", "Pixel.id", "is.Day", "Layer", "Catch.numb", "Time.spent", "Biomass")
  data$Ind.id <- rep(SESname(infile), nrow(data))
  data$CPUE <- data$Catch.numb / data$Time.spent
  save(data, paste0("./output_CPUEvsBiomass/", SESname(infile), ".RData"))
  rm(c("ses", "Catch.numb", "Time.spent", "Biomass")) ; gc(verbose=FALSE)
  
}

# Concatenate all individuals
sesfiles <- paste0("./output_CPUEvsBiomass/", SESname(matfiles), ".RData")
df <- data.frame()
for (infile in sesfiles) {
  load(infile)
  df <- rbind(df, data)
}



# Analysis ####

# Draft area ####

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