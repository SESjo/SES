#' ImportSES
#' @description Import an individual from a .mat file to R workspace is a standart way
#' @keywords Import SES .mat
#' @param matfile Path to .mat file. 
#' @param type Type of data to import: "tdr" only TDR data, "stat" Only Statdives data. "Bot" both.
#' @return An object of class "ses". Includes: ID of the SES - TDR and or Stats of dive as requested with 'type' argument.
#' @details The .mat file must be of of version v7 or less (last MATLAB version v7.3).
#' @seealso Edit 'SESformat' in the source code to modify the output format (type of columns)
#' @author Yves
#' @export
#' @import R.matlab
#' @examples
#' path <- system.file("extdata", package="SES")
#' pathname <- file.path(path, "2011-16_SES_example_accelero.mat")
#' ses <- importSES(pathname)
importSES <- function (matfile, type="both"){
  
  old.opt <- options("warn") ; options(warn=-1)
   
  matdata <- readMat(matfile) ; options(old.opt)
  res <- list(Ind.id=SESname(matfile), 
              tdr=data.frame(), stat=data.frame())
  
  if (type != "stat"){
    if (any(grepl("tdrcor2", names(matdata)))){
      matdata2 <- matdata
    }else{
      matdata2 <- matdata[[grep("tdrcor2", lapply(matdata, names))]]
    }
    res$tdr <- as.data.frame(matdata2$tdrcor2)
    headers <- unlist(matdata2$tdrcor2txt)
    names(res$tdr) <- unname(formatSES$tdr[headers, "alias"])
    if (any(match(formatSES$tdr$alias[formatSES$tdr$keep], names(res$tdr), nomatch=0) == 0)){ warning(paste0("The desired variable(s) ", 
                   paste(formatSES$tdr$alias[formatSES$tdr$keep][is.na(match(formatSES$tdr$alias[formatSES$tdr$keep], names(res$tdr)))], collapse=" & "), 
                   " is(are) not available in tdrcor2."))
    }
    res$tdr <- res$tdr[, match(formatSES$tdr$alias[formatSES$tdr$keep], names(res$tdr), nomatch=0)]
    res$tdr$Time <- as.POSIXct((res$tdr$Time - 719529)*24*3600, tz="UTC", origin="1970-01-01")
    res$tdr[, grep("is.", names(res$tdr))] <- as.logical(res$tdr[, grep("is.", names(res$tdr))])
    res$tdr[] <- lapply(res$tdr, replaceMissing) # Replace matlab's NaN by NA
  }
  class(res$tdr) <- c("tdr", "data.frame")
  if (type == "tdr") return(res)
  
  if (type != "tdr"){
    if (any(grepl("statdives", names(matdata)))){
      matdata2 <- matdata
    }else{
      matdata2 <- matdata[[grep("statdivestxt", lapply(matdata, names))]]
    }
    res$stat <- as.data.frame(matdata2$statdives)
    headers <- unlist(matdata2$statdivestxt)
    newHeaders <- unname( formatSES$stat[headers, "alias"])
    if (ncol(res$stat) != length(newHeaders)){
      warning("The number of variables differs between 'statdives' and 'statdivestxt'. The nth first variable names are assumed to be the good ones.")
    }
    names(res$stat) <- newHeaders[1:ncol(res$stat)]
    if (any(match(formatSES$stat$alias[formatSES$stat$keep], names(res$stat), nomatch=0) == 0)){warning(paste0("The desired variable(s) ", 
                   paste(formatSES$stat$alias[formatSES$stat$keep][is.na(match(formatSES$stat$alias[formatSES$stat$keep], names(res$stat)))], collapse=" & "), 
                   " is(are) not available in statdives."))
    }
    res$stat <- res$stat[, match(formatSES$stat$alias[formatSES$stat$keep], names(res$stat), nomatch=0)]
    res$stat$Time <- as.POSIXct((res$stat$Time - 719529)*24*3600, tz="UTC", origin="1970-01-01")
    res$stat[] <- lapply(res$stat, replaceMissing) # Replace matlab's NaN by NA
  }
  class(res$stat) <- c("statdives", "data.frame")
  if (type == "stat") return(res)

  class(res) <- c("ses", "list")
  return(res)
}
