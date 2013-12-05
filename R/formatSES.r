#' @title SES import preferences
#' @name formatSES
#' @description User preferences about SES importation.
#' @format Two data frames, one for each of TDR and statdives datasets, storing the names to give to the initial matlab variable (alias) and whether or not they should be imported from the .mat file (keep).
#' @export
tdrfmt <- c("ptimes_num"="Time", "corrected_depth"="Depth", "external_temperature"="Temp", "ligth_level"="Light", "capture"="is.Catch", "captureX"="is.Catch.x", "captureY"="is.Catch.y", "captureZ"="is.Catch.z", "num_evts"="Catch.id", "no_jour"="Day.id", "no_dive"="Dive.id", "type_dive"="Dive.type", "phase_dive"="Dive.step")
statfmt <- c("dive_numb"="Dive.id", "start_ptime"= "Time", "start_index"="Start.idx", "dive_duration"="Dive.dur", "max_depth"="Depth.max", "time_spent_surface_after"="Surftime.after","desc_duration"="Desc.dur", "bottom_duration"="Bottom.dur", "asc_duration"="Asc.dur", "bottom_dur_res"="BT.res", "desc_speed"="Desc.spd", "asc_speed"="Asc.spd",	             
			 "sinuosity"="Sinuosity", "dive_class"="Dive.type", "rem"="Note", "no_capture"="Catch.numb", "no_bottom_capture"="BCatch.numb", "temp_200m"="T200m", "light_150"="Light150m", "lum_surface"="IR0", "lat"="Lat", "lon"="Lon","angle"="Angle", "jour/nuit"="is.Day", "diff_midi_solaire"="Diff.Zenit", "latitude"="Lat", "longitude"="Lon",
			 "angle_Descent"="Angle.desc", "angle_Ascent"="Angle.asc", "IR150"="lum150m", "temp200m"="T200")
tdrkeep <- c("Time", "Depth", "Light", "Catch.id", "Dive.id")
statkeep <- c("Dive.id", "Time", "Start.idx", "Depth.max", "Dive.dur", "Catch.numb", "Lat", "Lon", "Sinuosity")
formatSES <- list(
	tdr=data.frame(alias=tdrfmt, keep= tdrfmt %in% tdrkeep, stringsAsFactors=FALSE),
	stat=data.frame(alias=statfmt, keep= statfmt %in% statkeep, stringsAsFactors=FALSE))
