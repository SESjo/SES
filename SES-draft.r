library(devtools)
load_all("../SES/")

matfiles <- list.files("../../Data/acc", full.names=TRUE, recursive=TRUE)
matfiles <- matfiles[grep("/20[0-9]{2}-[0-9]{2}/", matfiles)]
matfiles <- matfiles[grep("accelero.mat", matfiles)]

for (ses in matfiles) {
  
  myses <- importSES(ses)
  
  myses$tdr <- addLoc(from=myses$stat, to=myses$tdr)
  
}

# SESformat 	correct
# importSEAPOpred(file= )
 
# as.SES / is.SES
# as.tdr / is.tdr
# as.statdive / is.statdive