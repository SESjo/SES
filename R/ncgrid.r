#' ncgrid
#' @description Extract the grid from a NetCDF file
#' @param ncfile The to read the grid from.
#' @return Retuns a data frame with combinations of latitude and longitude grid breaks.
#' @author Yves
#' @export
#' @examples
#' path <- system.file("extdata", package="SES")
#' pathname <- file.path(path, "ker_0083x1d_catsat_vgpm_20111101_MTLPB.nc")
#' expl <- ncgrid(ncfile=pathname)
ncgrid <- function(ncfile, connection){
  if (missing(connection)) {
  	require("ncdf", quietly=TRUE)
  	con <- open.ncdf(ncfile)
  } else {
  	con <- connection
  }
  lat <- con$dim$latitude$vals
  lon <- con$dim$longitude$vals
  if (missing(connection)) close.ncdf(con)
  return(expand.grid(Lon=lon, Lat=lat))
}
