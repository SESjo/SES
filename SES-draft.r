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
  ses$stat$is.Day <- isDay(ses$stat)
  ses$stat <- idPixel(biomgrid, ses$stat)
  
  # Add these variable to tdr data
  vars <- c("Lat", "Lon", "Pixel.id", "is.Day")
  ses$tdr[ , vars] <- lapply(vars, addVar, from="stat", to="tdr", ses=ses, append=FALSE)

}

# SESformat 	correct
# importSEAPOpred(file= )
# format.tdr
# format.statdives
# as.SES / is.SES
# as.tdr / is.tdr
# as.statdive / is.statdive