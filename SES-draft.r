library(devtools)
load_all("../SES/")

matfiles <- list.files("../../Data/acc", full.names=TRUE, recursive=TRUE)
matfiles <- matfiles[grep("/20[0-9]{2}-[0-9]{2}/", matfiles)]
matfiles <- matfiles[grep("accelero.mat", matfiles)]

ncdir <- "C:/Users/stationcalcul/Desktop/Yves/Data/biom_137"
biomgrid <- ncgrid(list.files(ncdir, "*.nc", full.names=TRUE)[1])

for (infile in matfiles) {
  
  cat("Processing ", SESname(infile), " ...\n")
  ses <- importSES(infile)
  ses$tdr <- addLoc(from=ses$stat, to=ses$tdr)
  ses$stat$is.Day <- dayNight(ses$stat$Time, ses$stat[ , c("Lat", "Lon")])
  ses$stat <- pixels(biomgrid, ses$stat)
  ses$tdr <- addVar("Pixel.id", from="stat", to="tdr", ses=ses)
  ses$tdr <- addVar("is.Day", from="stat", to="tdr", ses=ses)
}

# SESformat 	correct
# importSEAPOpred(file= )
# format.tdr
# format.statdives
# as.SES / is.SES
# as.tdr / is.tdr
# as.statdive / is.statdive