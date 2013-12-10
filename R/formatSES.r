#' @title SES import preferences
#' @name formatSES
#' @description User preferences about SES importation.
#' @format Two data frames, one for each of TDR and statdives datasets, storing the names to give to the initial matlab variable (alias) and whether or not they should be imported from the .mat file (keep).
#' @export
path <- system.file("extdata", package="SES")
formatSES <- try(list(tdr=read.csv(file.path(path, "formatSES.tdr.csv"), sep=";", stringsAsFactors=FALSE, row.names="X"),
					  stat=read.csv(file.path(path, "formatSES.stat.csv"), sep=";", stringsAsFactors=FALSE, row.names="X"),
					  tdr3D=read.csv(file.path(path, "formatSES.tdr3D.csv"), sep=";", stringsAsFactors=FALSE, row.names="X"),
					  stat3D=read.csv(file.path(path, "formatSES.stat3D.csv"), sep=";", stringsAsFactors=FALSE, row.names="X")),
                 silent=TRUE)
if (is.error(formatSES)){
	formatSES <- structure(list(tdr = structure(
							list(alias = c("Time", "Depth", 
							   "Temp", "Light", "is.Catch", "is.Catch.x", "is.Catch.y", "is.Catch.z", 
							   "Catch.id", "Day.id", "Dive.id", "Dive.type", "Dive.step"), keep =
							 c(TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, 
							   FALSE, FALSE)), .Names = c("alias", "keep"), 
							row.names = c("ptimes_num", "corrected_depth", "external_temperature", "ligth_level", "capture", 
							  "captureX", "captureY", "captureZ", "num_evts", "no_jour", "no_dive", 
							  "type_dive", "phase_dive"), class = "data.frame"), stat = structure(list(
							  alias = c("Dive.id", "Time", "Start.idx", "Dive.dur", "Depth.max", 
							    "Surftime.after", "Desc.dur", "Bottom.dur", "Asc.dur", "BT.res", 
							    "Desc.spd", "Asc.spd", "Sinuosity", "Dive.type", "Note", 
							    "Catch.numb", "BCatch.numb", "T200m", "Light150m", "IR0", 
							    "Lat", "Lon", "Angle", "is.Day", "Diff.Zenit", "Lat", "Lon", 
							    "Angle.desc", "Angle.asc", "lum150m", "T200"), keep = c(TRUE, 
							    TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, 
							    FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, 
							    FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, 
							    FALSE, FALSE, FALSE)), .Names = c("alias", "keep"), row.names = c("dive_numb", 
							      "start_ptime", "start_index", "dive_duration", "max_depth", "time_spent_surface_after", 
							      "desc_duration", "bottom_duration", "asc_duration", "bottom_dur_res", 
							      "desc_speed", "asc_speed", "sinuosity", "dive_class", "rem", 
							      "no_capture", "no_bottom_capture", "temp_200m", "light_150", 
							      "lum_surface", "lat", "lon", "angle", "jour/nuit", "diff_midi_solaire", 
							      "latitude", "longitude", "angle_Descent", "angle_Ascent", "IR150", 
							      "temp200m"), class = "data.frame"), stat3D = structure(list(alias = structure(c(14L, 
							      7L, 4L, 3L, 24L, 23L, 6L, 12L, 2L, 1L, 19L, 20L, 26L, 27L, 28L, 
							      25L, 17L, 16L, 15L, 5L, 8L, 21L, 22L, 9L, 18L, 11L, 13L, 10L), .Label = c("Angle.asc", 
							        "Angle.desc", "Asc.dur", "Bott.dur", "Catch.numb.bott", "Depth.max", 
							        "Desc.dur", "Direction", "Direction.currant", "Dist.diff", "Dist.gps", 
							        "Dist.h", "Dist.model", "Dive.dur", "Effort.asc", "Effort.bott", 
							        "Effort.desc", "is.diveGeoref", "Sinuosity.h", "Sinuosity.v", 
							        "Speed", "Speed.currant", "Surff.dur", "Surfi.dur", "varinconnnue4", 
							        "varinconnue1", "varinconnue2", "varinconnue3"), class = "factor"), 
							      keep = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, 
							       TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, 
							       TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE
							      ), stringAsFactors = c(FALSE, FALSE, FALSE, FALSE, FALSE, 
							         FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, 
							         FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, 
							         FALSE, FALSE, FALSE, FALSE, FALSE)), .Names = c("alias", 
							         "keep", "stringAsFactors"), row.names = c("diveduration", "desc_duration", 
							           "bot_duration", "asc_duration", "presurf_dur", "postsurf_dur", 
							           "profmax", "distance_horiz", "angdrad", "angrrad", "tortuoh", 
							           "tortuop", "sum_ecartcaps", "moy_ecartcaps", "sum_ecartpit", 
							           "moy_ecartpit", "effTOT_desc", "effTOT_bot", "effTOT_asc", "capturesbot", 
							           "dir", "spd", "spd_courantmercator", "dir_courantmercator", "plongee_georef", 
							           "distance_gps", "distance_model3d", "diff_distancereel-distancemodel"
							         ), class = "data.frame"), tdr3D = structure(list(alias = structure(c(7L, 
							          2L, 4L, 6L, 3L, 5L, 1L, 8L, 9L, 10L, 11L), .Label = c("Depth", 
							            "Distance", "Lat", "Lat.rad", "Lon", "Lon.rad", "Time", "var1", 
							            "var2", "var3", "var4"), class = "factor"), keep = c(TRUE, TRUE, 
							             FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE), 
							          stringAsFactors = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, 
							          FALSE, FALSE, FALSE, FALSE, FALSE)), .Names = c("alias", 
							          "keep", "stringAsFactors"), row.names = c("DateTime", "Distance", 
							            "LatRad", "LongRad", "Latitude", "Longitude", "Depth", "DRCalcSpd", 
							            "NewX", "NewY", "Bering"), class = "data.frame")), .Names = c("tdr", 
							              "stat", "stat3D", "tdr3D"))
}
for (i in seq_along(formatSES)){
  class(formatSES[[i]]) <- c("fmtSES", class(formatSES[[i]]))
}