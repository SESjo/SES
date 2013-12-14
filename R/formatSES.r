#' @title \code{importSES()}: User preferences
#' @alias .fmtSES
#' @name .fmtSES
#' @description User preferences about SES formatting on import. To edit, simply type \code{formatSES} (or \code{formatSES$tdr} etc...) on console.
#' @usage .fmtSES
#' @format Four data frames, one for each of TDR and statdives datasets (classic or 3D), storing the names to give to the initial matlab variable (alias), whether or not they should be imported from the .mat file (keep) and a brief decription of them.
#' @seealso \code{\link{importSES}}
#' @author Yves
if (any(grepl(".fmtSES", search()))) detach(.fmtSES)
.fmtSES <- new.env(parent=.GlobalEnv)
path <- system.file("extdata", package="SES")
assign("formatSES", try(list(tdr=read.csv(file.path(path, "formatSES.tdr.csv"), sep=";", stringsAsFactors=FALSE),
							 stat=read.csv(file.path(path, "formatSES.stat.csv"), sep=";", stringsAsFactors=FALSE),
							 tdr3D=read.csv(file.path(path, "formatSES.tdr3D.csv"), sep=";", stringsAsFactors=FALSE),
							 stat3D=read.csv(file.path(path, "formatSES.stat3D.csv"), sep=";", stringsAsFactors=FALSE)),
						silent=TRUE),
	   envir=.fmtSES)
if (is.error(get("formatSES", envir=.fmtSES))){
	warning("Rescue 'FormatSES' was loaded. Default headers will be used in imports.")
	assign("formatSES", structure(list(tdr = structure(list(userAlias = c("Time", "Depth", 
																		  "Temp", "Light", "is.Catch", "is.Catch.x", "is.Catch.y", "is.Catch.z", 
																		  "Catch.id", "Day.id", "Dive.id", "Dive.type", "Dive.step"), suggestedAlias = c("Time", 
																		  																			   "Depth", "Temp", "Light", "is.Catch", "is.Catch.x", "is.Catch.y", 
																		  																			   "is.Catch.z", "Catch.id", "Day.id", "Dive.id", "Dive.type", "Dive.step"
																		  ), keep = c(TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, 
																		  			TRUE, FALSE, TRUE, FALSE, FALSE), description = c("Date and time (matlab numeric format)", 
																		  															  "Depth (m) (corrected according to surface offsets of device)", 
																		  															  "Temperature (deg. C)", "Light intensity (ref. Vacquie-Garcia et al, 2012, J. Plos One to convert it in Wcm^2)", 
																		  															  "Is the record associated with a prey catch attempt (Beware not equivalent to a catch)", 
																		  															  "Is the record  > threshold on x axes of the accelerometer", 
																		  															  "Is the record  > threshold on y axes of the accelerometer", 
																		  															  "Is the record  > threshold on z axes of the accelerometer", 
																		  															  "Prey catch attempt number (0 = not catch). One or several records with even number = 1 catch.", 
																		  															  "Day number (Beware unknown threshold)", "Dive number. 0 in surface periods (or invalid dives ?)", 
																		  															  "Result of dive classification (ref. Dragon PhD). 1 = Drift dives. Avoid using other categories.", 
																		  															  "Dive step classification (1 = Descent / 2 = Bottom / 3 = Ascent)."
																		  			)), .Names = c("userAlias", "suggestedAlias", "keep", "description"
																		  			), class = c("fmtSES", "data.frame"), row.names = c("ptimes_num", 
																		  																"corrected_depth", "external_temperature", "ligth_level", "capture", 
																		  																"captureX", "captureY", "captureZ", "num_evts", "no_jour", "no_dive", 
																		  																"type_dive", "phase_dive")), stat = structure(list(userAlias = c("Dive.id", 
																		  																																 "Time", "Start.idx", "Dive.dur", "Depth.max", "Surff.dur", "Desc.dur", 
																		  																																 "Bottom.dur", "Asc.dur", "BT.res", "Desc.spd", "Asc.spd", "Sinuosity", 
																		  																																 "Dive.type", "Note", "Catch.numb", "BCatch.numb", "T200m", "Light150m", 
																		  																																 "IR0", "Lat", "Lon", "Angle", "is.Day", "Diff.Zenit", "Lat", 
																		  																																 "Lon", "Angle.desc", "Angle.asc", "IR150", "T200m"), suggestedAlias = c("Dive.id", 
																		  																																 																		"Time", "Start.idx", "Dive.dur", "Depth.max", "Surff.dur", "Desc.dur", 
																		  																																 																		"Bottom.dur", "Asc.dur", "BT.res", "Desc.spd", "Asc.spd", "Sinuosity", 
																		  																																 																		"Dive.type", "Note", "Catch.numb", "BCatch.numb", "T200m", "Light150m", 
																		  																																 																		"IR0", "Lat", "Lon", "Angle", "is.Day", "Diff.Zenit", "Lat", 
																		  																																 																		"Lon", "Angle.desc", "Angle.asc", "IR150", "T200m"), keep = c(TRUE, 
																		  																																 																																	  TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, 
																		  																																 																																	  FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, 
																		  																																 																																	  TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, 
																		  																																 																																	  FALSE), description = c("Dive number", "Time when dive started (matlab numeric format)", 
																		  																																 																																	  						"Time when dive ended (matlab numeric format)", "Dive duration (s)", 
																		  																																 																																	  						"Maximum depth reached in dive (m)", "Duration of the surface period just following the dive (s)", 
																		  																																 																																	  						"Duration of the descent step of the dive (s)", "Duration of the bottom step of the dive (s)", 
																		  																																 																																	  						"Duration of the ascent step of the dive (s)", "Residual of the linear model Bottom time vs. Depth + Dive duration (ref. Bailleul PhD)", 
																		  																																 																																	  						"Vertical speed during descent (m/s)", "Vertical speed during ascent (m/s)", 
																		  																																 																																	  						"Sinuosity of vertical trajectory at bottom (deviation from straight lines between start/end of bottom and depth max)", 
																		  																																 																																	  						"Result of dive classification (ref. Dragon PhD). 1 = Drift dives. Avoid using other categories.", 
																		  																																 																																	  						"Note about the dive", "Prey catch attempt number (0 = not catch). One or several records with even number = 1 catch.", 
																		  																																 																																	  						"Prey catch attempt number relatively to the bottom of this dive.", 
																		  																																 																																	  						"Temperature at 200 m (C deg.)", "Light intensity at 150 m (ref Thomas Jaud et al, 2012, Plos One)  (units?)", 
																		  																																 																																	  						"Light intensity at surface (ref Thomas Jaud et al, 2012, Plos One)  (units?)", 
																		  																																 																																	  						"Latitude (deg.)", "Longitude (deg.)", "Angle (which one ???)  (units ?)", 
																		  																																 																																	  						"Is The dive occurring during daytrime (unknown threshold on sun elevation)", 
																		  																																 																																	  						"???  (units ?)", "Latitude (deg.)", "Longitude (deg.)", "Angle in the descent step  (units ?)", 
																		  																																 																																	  						"Angle in the ascent step (units ?)", "Light intensity at 150 m (ref Thomas Jaud et al, 2012, Plos One)", 
																		  																																 																																	  						"Temperature at 200 m (C deg.)")), .Names = c("userAlias", "suggestedAlias", 
																		  																																 																																	  																	  "keep", "description"), class = c("fmtSES", "data.frame"), row.names = c("dive_numb", 
																		  																																 																																	  																	  																		 "start_ptime", "start_index", "dive_duration", "max_depth", "time_spent_surface_after", 
																		  																																 																																	  																	  																		 "desc_duration", "bottom_duration", "asc_duration", "bottom_dur_res", 
																		  																																 																																	  																	  																		 "desc_speed", "asc_speed", "sinuosity", "dive_class", "rem", 
																		  																																 																																	  																	  																		 "no_capture", "no_bottom_capture", "temp_200m", "light_150", 
																		  																																 																																	  																	  																		 "lum_surface", "lat", "lon", "angle", "jour/nuit", "diff_midi_solaire", 
																		  																																 																																	  																	  																		 "latitude", "longitude", "angle_Descent", "angle_Ascent", "lum150m", 
																		  																																 																																	  																	  																		 "temp200m")), tdr3D = structure(list(userAlias = c("Time", "Distance", 
																		  																																 																																	  																	  																		 												   "Lat.rad", "Lon.rad", "Lat", "Lon", "Depth", "var1", "var2", 
																		  																																 																																	  																	  																		 												   "var3", "var4"), suggestedAlias = c("Time", "Distance", "Lat.rad", 
																		  																																 																																	  																	  																		 												   									"Lon.rad", "Lat", "Lon", "Depth", "var1", "var2", "var3", "var4"
																		  																																 																																	  																	  																		 												   ), keep = c(TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, 
																		  																																 																																	  																	  																		 												   			FALSE, FALSE, FALSE), description = c("Date and time (matlab numeric format)", 
																		  																																 																																	  																	  																		 												   												  "Distance travelled (3D) from the last record (m?) ?", "Latitude (radian)", 
																		  																																 																																	  																	  																		 												   												  "Longitude (radian)", "Latitude (degree)", "Longitude (degree)", 
																		  																																 																																	  																	  																		 												   												  "Depth (m)", "???", "???", "???", "???")), .Names = c("userAlias", 
																		  																																 																																	  																	  																		 												   												  													  "suggestedAlias", "keep", "description"), class = c("fmtSES", 
																		  																																 																																	  																	  																		 												   												  													  													"data.frame"), row.names = c("DateTime", "Distance", "LatRad", 
																		  																																 																																	  																	  																		 												   												  													  																				 "LongRad", "Latitude", "Longitude", "Depth", "DRCalcSpd", "NewX", 
																		  																																 																																	  																	  																		 												   												  													  																				 "NewY", "Bering")), stat3D = structure(list(userAlias = c("Dive.dur", 
																		  																																 																																	  																	  																		 												   												  													  																				 														  "Desc.dur", "Bott.dur", "Asc.dur", "Surfi.dur", "Surff.dur", 
																		  																																 																																	  																	  																		 												   												  													  																				 														  "Depth.max", "Dist.h", "Angle.desc", "Angle.asc", "Sinuosity.h", 
																		  																																 																																	  																	  																		 												   												  													  																				 														  "Sinuosity.v", "varinconnue1", "varinconnue2", "varinconnue3", 
																		  																																 																																	  																	  																		 												   												  													  																				 														  "varinconnnue4", "Effort.desc", "Effort.bott", "Effort.asc", 
																		  																																 																																	  																	  																		 												   												  													  																				 														  "Catch.numb.bott", "Direction", "Speed", "Speed.current", "Direction.current", 
																		  																																 																																	  																	  																		 												   												  													  																				 														  "is.diveGeoref", "Dist.gps", "Dist.model", "Dist.diff"), suggestedAlias = c("Dive.dur", 
																		  																																 																																	  																	  																		 												   												  													  																				 														  																			"Desc.dur", "Bott.dur", "Asc.dur", "Surfi.dur", "Surff.dur", 
																		  																																 																																	  																	  																		 												   												  													  																				 														  																			"Depth.max", "Dist.h", "Angle.desc", "Angle.asc", "Sinuosity.h", 
																		  																																 																																	  																	  																		 												   												  													  																				 														  																			"Sinuosity.v", "varinconnue1", "varinconnue2", "varinconnue3", 
																		  																																 																																	  																	  																		 												   												  													  																				 														  																			"varinconnnue4", "Effort.desc", "Effort.bott", "Effort.asc", 
																		  																																 																																	  																	  																		 												   												  													  																				 														  																			"Catch.numb.bott", "Direction", "Speed", "Speed.current", "Direction.current", 
																		  																																 																																	  																	  																		 												   												  													  																				 														  																			"is.diveGeoref", "Dist.gps", "Dist.model", "Dist.diff"), keep = c(TRUE, 
																		  																																 																																	  																	  																		 												   												  													  																				 														  																																			  TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, 
																		  																																 																																	  																	  																		 												   												  													  																				 														  																																			  TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, 
																		  																																 																																	  																	  																		 												   												  													  																				 														  																																			  TRUE, TRUE, TRUE, TRUE, TRUE), description = c("Duration of dive (s)", 
																		  																																 																																	  																	  																		 												   												  													  																				 														  																																			  											   "Duration of the descent step of the dive (s)", "Duration of the bottom step (s)", 
																		  																																 																																	  																	  																		 												   												  													  																				 														  																																			  											   "Duration of the ascent step (s)", "Duration of the surface period just preceeding the dive (s)", 
																		  																																 																																	  																	  																		 												   												  													  																				 														  																																			  											   "Duration of the surface period just following the dive (s)", 
																		  																																 																																	  																	  																		 												   												  													  																				 														  																																			  											   "Maximum depth reached in dive (m)", "Distance travelled while diving on the (latitude, longitude) plan (m?)", 
																		  																																 																																	  																	  																		 												   												  													  																				 														  																																			  											   "Descent angle (radian)", "Ascent angle (radian)", "Horizontal tortuosity (ref. Benhamou 2004) (unit?)", 
																		  																																 																																	  																	  																		 												   												  													  																				 														  																																			  											   "Vertical tortuosity (ref. Benhamou 2004)  (unit?)", "???", "???", 
																		  																																 																																	  																	  																		 												   												  													  																				 														  																																			  											   "???", "???", "Swimming effort cumulated over the descent step of the dive (jerk)", 
																		  																																 																																	  																	  																		 												   												  													  																				 														  																																			  											   "Swimming effort cumulated over the bottom step of the dive (jerk)", 
																		  																																 																																	  																	  																		 												   												  													  																				 														  																																			  											   "Swimming effort cumulated over the ascent step of the dive (jerk)", 
																		  																																 																																	  																	  																		 												   												  													  																				 														  																																			  											   "Number of prey catch attempts in the bottom step", "Average direction of the seal in the dive ? (unit?)", 
																		  																																 																																	  																	  																		 												   												  													  																				 														  																																			  											   "Average speed (horizontal) of the seal in the dive ? (unit?)", 
																		  																																 																																	  																	  																		 												   												  													  																				 														  																																			  											   "Average speed (horizontal) of the current ? (unit?)", "Average direction of the current in the dive ? (unit?)", 
																		  																																 																																	  																	  																		 												   												  													  																				 														  																																			  											   "Is the dive associated with GPS locations in both of preceeding and following surface periods", 
																		  																																 																																	  																	  																		 												   												  													  																				 														  																																			  											   "Distance travelled between GPS locations (units?)", "Distance travelled in the reconstructed dive (units?)", 
																		  																																 																																	  																	  																		 												   												  													  																				 														  																																			  											   "Difference between these distance (model attempts to minimize it) (units?)"
																		  																																 																																	  																	  																		 												   												  													  																				 														  																																			  )), .Names = c("userAlias", "suggestedAlias", "keep", "description"
																		  																																 																																	  																	  																		 												   												  													  																				 														  																																			  ), class = c("fmtSES", "data.frame"), row.names = c("diveduration", 
																		  																																 																																	  																	  																		 												   												  													  																				 														  																																			  													"desc_duration", "bot_duration", "asc_duration", "presurf_dur", 
																		  																																 																																	  																	  																		 												   												  													  																				 														  																																			  													"postsurf_dur", "profmax", "distance_horiz", "angdrad", "angrrad", 
																		  																																 																																	  																	  																		 												   												  													  																				 														  																																			  													"tortuoh", "tortuop", "sum_ecartcaps", "moy_ecartcaps", "sum_ecartpit", 
																		  																																 																																	  																	  																		 												   												  													  																				 														  																																			  													"moy_ecartpit", "effTOT_desc", "effTOT_bot", "effTOT_asc", "capturesbot", 
																		  																																 																																	  																	  																		 												   												  													  																				 														  																																			  													"dir", "spd", "spd_courantmercator", "dir_courantmercator", "plongee_georef", 
																		  																																 																																	  																	  																		 												   												  													  																				 														  																																			  													"distance_gps", "distance_model3d", "diff_distancereel-distancemodel"
																		  																																 																																	  																	  																		 												   												  													  																				 														  																																			  ))), .Names = c("tdr", "stat", "tdr3D", "stat3D")),
		   envir=.fmtSES)
}
with(.fmtSES,
	 for (i in seq_along(formatSES)){
	 	class(formatSES[[i]]) <- c("fmtSES", class(formatSES[[i]]))
	 }
)
attach(.fmtSES)
