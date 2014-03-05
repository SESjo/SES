#' @title SESsettings
#' @name SESsettings
#' @usage SESsettings
#' @description Environment to keep SES importation preferences (\code{formatSES}). To edit first run \code{attach(SESsettings)} and then \code{format$tdr} etc to proceed.
#' @export
if (any(grepl("SESsettings", search()))) detach('SESsettings')
SESsettings <- new.env(parent=.GlobalEnv)
path <- try(system.file("extdata", package="SES"), silent=TRUE)
assign("formatSES", try(list(tdr=read.csv(file.path(path, "formatSES.tdr.csv"), sep=";", stringsAsFactors=FALSE),
							 stat=read.csv(file.path(path, "formatSES.stat.csv"), sep=";", stringsAsFactors=FALSE),
							 tdr3D=read.csv(file.path(path, "formatSES.tdr3D.csv"), sep=";", stringsAsFactors=FALSE),
							 stat3D=read.csv(file.path(path, "formatSES.stat3D.csv"), sep=";", stringsAsFactors=FALSE)),
						silent=TRUE),
	   envir=SESsettings)

if (!inherits(get("formatSES", envir=SESsettings), "try-error")){
# Occurs if .csv file was modified using Excel
#        rownamesChecking <- with(SESsettings, sapply(formatSES, length) == 5)
#        if (!all(rownamesChecking)){
#          for (type in names(rownamesChecking)[!rownamesChecking]){
#            
# #            path <- try(system.file("extdata", package="SES"), silent=TRUE)
# #            formatSES[[type]] <- read.csv(file.path(path, "formatSES.stat.csv"), 
# #                                          sep=";", stringsAsFactors=FALSE, row.names=1)
# #            names(formatSES[[type]]) <- c("userAlias", "suggestedAlias", "keep", 
# #                                          "description", "applyFun")
# #            print("I was here")
#            
#          }
#        }
} else {
	warning("Rescue 'FormatSES' was loaded. Default headers will be used in imports.")
	assign("formatSES", structure(list(userAlias = c("Dive.id", "Time", "Start.idx", 
	                                                 "Dive.dur", "Depth.max", "Surff.dur", "Desc.dur", "Bottom.dur", 
	                                                 "Asc.dur", "BT.res", "Desc.spd", "Asc.spd", "Sinuosity", "is.Drift", 
	                                                 "Note", "Catch.numb", "BCatch.numb", "T200m", "Light150m", "IR0", 
	                                                 "Lat", "Lon", "Angle.sun", "is.Day", "Diff.Zenit", "Lat", "Lon", 
	                                                 "Angle.desc", "Angle.asc", "IR150", "T200m", "Density", "absAsc.spd", 
	                                                 "absDesc.spd", "Asc.eff", "Desc.eff", "Surfi.dur", "Str.glob", 
	                                                 "Str.hz", "Str.vt"), suggestedAlias = c("Dive.id", "Time", "Start.idx", 
	                                                                                         "Dive.dur", "Depth.max", "Surff.dur", "Desc.dur", "Bottom.dur", 
	                                                                                         "Asc.dur", "BT.res", "Desc.spd", "Asc.spd", "Sinuosity", "is.Drift", 
	                                                                                         "Note", "Catch.numb", "BCatch.numb", "T200m", "Light150m", "IR0", 
	                                                                                         "Lat", "Lon", "Angle.sun", "is.Day", "Diff.Zenit", "Lat", "Lon", 
	                                                                                         "Angle.desc", "Angle.asc", "IR150", "T200m", "Density", "absAsc.spd", 
	                                                                                         "absDesc.spd", "Asc.eff", "Desc.eff", "Surfi.dur", "Str.glob", 
	                                                                                         "Str.hz", "Str.vt"), keep = c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, 
	                                                                                                                       FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, 
	                                                                                                                       TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, 
	                                                                                                                       TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, 
	                                                                                                                       TRUE, FALSE, TRUE, TRUE, TRUE), description = c("Dive number", 
	                                                                                                                                                                       "Time when dive started (matlab numeric format)", "Line number of dive start in TDR dataset", 
	                                                                                                                                                                       "Dive duration (s)", "Maximum depth reached in dive (m)", "Duration of the surface period just following the dive (s)", 
	                                                                                                                                                                       "Duration of the descent step of the dive (s)", "Duration of the bottom step of the dive (s)", 
	                                                                                                                                                                       "Duration of the ascent step of the dive (s)", "Residual of the linear model Bottom time vs. Depth + Dive duration (ref. Bailleul PhD)", 
	                                                                                                                                                                       "Vertical speed during descent (m/s)", "Vertical speed during ascent (m/s)", 
	                                                                                                                                                                       "Sinuosity of vertical trajectory at bottom (deviation from straight lines between start/end of bottom and depth max)", 
	                                                                                                                                                                       "Result of dive classification (ref. Dragon PhD). 1 = Drift dives. Avoid using other categories.", 
	                                                                                                                                                                       "Comment about the dive", "Prey catch attempt number (0 = not catch). One or several records with even number = 1 catch.", 
	                                                                                                                                                                       "Prey catch attempt number relatively to the bottom of this dive.", 
	                                                                                                                                                                       "Temperature at 200 m (C deg.)", "Light intensity at 150 m (ref Thomas Jaud et al, 2012, Plos One)  (units?)", 
	                                                                                                                                                                       "Light intensity at surface (ref Thomas Jaud et al, 2012, Plos One)  (units?)", 
	                                                                                                                                                                       "Latitude (deg.)", "Longitude (deg.)", "Sun elevation angle (deg)", 
	                                                                                                                                                                       "Is The dive occurring during daytrime (threshold on sun elevation -6 deg)", 
	                                                                                                                                                                       "Time difference to zenith (hours)", "Latitude (deg.)", "Longitude (deg.)", 
	                                                                                                                                                                       "Angle in the descent step  (deg)", "Angle in the ascent step (deg)", 
	                                                                                                                                                                       "Light intensity at 150 m (ref Thomas Jaud et al, 2012, Plos One)", 
	                                                                                                                                                                       "Temperature at 200 m (C deg.)", "Density of the animal's body (g/L)", 
	                                                                                                                                                                       "Average of the estimated absolute speed in the ascent step (m/s)", 
	                                                                                                                                                                       "Average of the estimated absolute speed in the descent step (m/s)", 
	                                                                                                                                                                       "Estimated swimming effort in the ascent step (m/s^3)", "Estimated swimming effort in the descent step (m/s^3)", 
	                                                                                                                                                                       "Duration of the surface period just preceeding the dive (s)", 
	                                                                                                                                                                       "Bottom step 3D straightness index (see Benhamou S. 2004. J Theorical Biol.) (no unit)", 
	                                                                                                                                                                       "Bottom step 2D straightness index on the horizontal plane (no unit)", 
	                                                                                                                                                                       "Bottom step 1D straightness index on the vertical dimention (no unit)"
	                                                                                                                       ), applyFun = c("as.integer", "datenum2posx", "as.integer", "as.double", 
	                                                                                                                                       "as.double", "as.double", "as.double", "as.double", "as.double", 
	                                                                                                                                       "as.double", "as.double", "as.double", "as.double", "class2logical", 
	                                                                                                                                       "as.character", "as.double", "as.double", "as.double", "as.double", 
	                                                                                                                                       "as.double", "as.double", "as.double", "as.double", "as.logical", 
	                                                                                                                                       "as.double", "as.double", "as.double", "as.double", "as.double", 
	                                                                                                                                       "as.double", "as.double", "as.numeric", "as.numeric", "as.numeric", 
	                                                                                                                                       "as.numeric", "as.numeric", "as.numeric", "as.numeric", "as.numeric", 
	                                                                                                                                       "as.numeric")), .Names = c("userAlias", "suggestedAlias", "keep", 
	                                                                                                                                                                  "description", "applyFun"), class = c("fmtSES", "data.frame"), row.names = c("dive_numb", 
	                                                                                                                                                                                                                                               "start_ptime", "start_index", "dive_duration", "max_depth", "time_spent_surface_after", 
	                                                                                                                                                                                                                                               "desc_duration", "bottom_duration", "asc_duration", "bottom_dur_res", 
	                                                                                                                                                                                                                                               "desc_speed", "asc_speed", "sinuosity", "dive_class", "rem", 
	                                                                                                                                                                                                                                               "no_capture", "no_bottom_capture", "temp_200m", "light_150", 
	                                                                                                                                                                                                                                               "lum_surface", "lat", "lon", "angle", "jour/nuit", "diff_midi_solaire", 
	                                                                                                                                                                                                                                               "latitude", "longitude", "angle_Descent", "angle_Ascent", "lum150m", 
	                                                                                                                                                                                                                                               "temp200m", "masse_Volumique", "vitesse_Ascent", "vitesse_Descent", 
	                                                                                                                                                                                                                                               "effort_Ascent", "effort_Descent", "time_spent_surface_before", 
	                                                                                                                                                                                                                                               "str_gl", "str_hz", "str_vt")),
		   envir=SESsettings)
}
with(SESsettings,
	 for (i in seq_along(formatSES)){
	 	class(formatSES[[i]]) <- c("fmtSES", class(formatSES[[i]]))
	 }
)
attach(SESsettings)
