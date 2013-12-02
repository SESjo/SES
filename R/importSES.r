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
#' @examples
#' path <- system.file("extdata", package="SES")
#' pathname <- file.path(path, "2011-16_SES_example_accelero.mat")
#' ses <- importSES(pathname)
importSES <- function (matfile, type="both"){
  
  old.opt <- options("warn") ; options(warn=-1)
  
  tdrfmt <- c("ptimes_num"="Time", "corrected_depth"="Depth", "external_temperature"="Temp", "ligth_level"="Light", "capture"="is.Catch", "captureX"="is.Catch.x", "captureY"="is.Catch.y", "captureZ"="is.Catch.z", "num_evts"="Catch.id", "no_jour"="Day.id", "no_dive"="Dive.id", "type_dive"="Dive.type", "phase_dive"="Dive.step")
  statfmt <- c("dive_numb"="Dive.id", "start_ptime"= "Time", "start_index"="Start.idx", "dive_duration"="Dive.dur", "max_depth"="Depth.max", "time_spent_surface_after"="Surftime.after","desc_duration"="Desc.dur", "bottom_duration"="Bottom.dur", "asc_duration"="Asc.dur", "bottom_dur_res"="BT.res", "desc_speed"="Desc.spd", "asc_speed"="Asc.spd",	             
               "sinuosity"="Sinuosity", "dive_class"="Dive.type", "rem"="Note", "no_capture"="Catch.numb", "no_bottom_capture"="BCatch.numb", "temp_200m"="T200m", "light_150"="Light150m", "lum_surface"="IR0", "lat"="Lat", "lon"="Lon","angle"="Angle", "jour/nuit"="is.Day", "diff_midi_solaire"="Diff.Zenit", "latitude"="Lat", "longitude"="Lon",
               "angle_Descent"="Angle.desc", "angle_Ascent"="Angle.asc", "IR150"="lum150m", "temp200m"="T200")
  tdrkeep <- c("Time", "Depth", "Light", "Catch.id", "Dive.id")
  statkeep <- c("Dive.id", "Time", "Start.idx", "Depth.max", "Dive.dur", "Catch.numb", "Lat", "Lon", "Sinuosity")
  SESformat <- list(tdrfmt=tdrfmt, statfmt=statfmt, tdrkeep=tdrkeep, statkeep=statkeep)
  rm("tdrfmt", "statfmt", "tdrkeep", "statkeep")
  
  require("R.matlab") 
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
    names(res$tdr) <- unname(SESformat$tdrfmt[headers])
    if (any(match(SESformat$tdrkeep, names(res$tdr), nomatch=0) == 0)){ warning(paste0("The desired variable(s) ", 
                   paste(SESformat$tdrkeep[is.na(match(SESformat$tdrkeep, names(res$tdr)))], collapse=" & "), 
                   " is(are) not available in tdrcor2."))
    }
    res$tdr <- res$tdr[, match(SESformat$tdrkeep, names(res$tdr), nomatch=0)]
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
    newHeaders <- unname(SESformat$statfmt[headers])
    if (ncol(res$stat) != length(newHeaders)){
      warning("The number of variables differs between 'statdives' and 'statdivestxt'. The nth first variable names are assumed to be the good ones.")
    }
    names(res$stat) <- newHeaders[1:ncol(res$stat)]
    if (any(match(SESformat$statkeep, names(res$stat), nomatch=0) == 0)){warning(paste0("The desired variable(s) ", 
                   paste(SESformat$statkeep[is.na(match(SESformat$statkeep, names(res$stat)))], collapse=" & "), 
                   " is(are) not available in statdives."))
    }
    res$stat <- res$stat[, match(SESformat$statkeep, names(res$stat), nomatch=0)]
    res$stat$Time <- as.POSIXct((res$stat$Time - 719529)*24*3600, tz="UTC", origin="1970-01-01")
    res$stat[] <- lapply(res$stat, replaceMissing) # Replace matlab's NaN by NA
  }
  class(res$stat) <- c("statdives", "data.frame")
  if (type == "stat") return(res)

  class(res) <- c("ses", "list")
  return(res)
}
