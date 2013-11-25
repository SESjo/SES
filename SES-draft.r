library(devtools)
load_all("../SES/")

matfiles <- list.files("../../Data/acc", full.names=TRUE, recursive=TRUE)
matfiles <- matfiles[grep("/20[0-9]{2}-[0-9]{2}/", matfiles)]
matfiles <- matfiles[grep("accelero.mat", matfiles)]

for (infile in matfiles) {
  cat(SESname(infile), "\n")
  ses <- importSES(infile)
  ses$tdr <- addLoc(from=ses$stat, to=ses$tdr)
  
}

# SESformat 	correct
# importSEAPOpred(file= )
# format.tdr
# format.statdives
# as.SES / is.SES
# as.tdr / is.tdr
# as.statdive / is.statdive