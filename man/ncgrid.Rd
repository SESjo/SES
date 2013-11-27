\name{ncgrid}
\alias{ncgrid}
\title{ncgrid}
\usage{
  ncgrid(ncfile, connection)
}
\arguments{
  \item{ncfile}{The to read the grid from.}
}
\value{
  Retuns a data frame with combinations of latitude and
  longitude grid breaks.
}
\description{
  Extract the grid from a NetCDF file
}
\examples{
path <- system.file("extdata", package="SES")
pathname <- file.path(path, "ker_0083x1d_catsat_vgpm_20111101_MTLPB.nc")
expl <- ncgrid(ncfile=pathname)
}
\author{
  Yves
}

