\name{ncgrid}
\alias{ncgrid}
\title{ncgrid}
\usage{
ncgrid(ncfile, connection, latname = "latitude", lonname = "longitude",
  zname = NULL)
}
\arguments{
  \item{ncfile}{The to read the grid from.}

  \item{connection}{An existing connexion with a file can
  be used instead of \code{ncfile} argument.}

  \item{latname}{The variable name of the meridional
  dimension of the grid.}

  \item{lonname}{The variable name of the zonal dimension
  of the grid.}

  \item{zname}{The optional name of a third dimension of
  the grid e.g. 'depthu'.}
}
\value{
Retuns a data frame with combinations of latitude and
longitude grid breaks.
}
\description{
Extract the grid from a NetCDF file.
}
\examples{
path <- system.file("extdata", package="SES")
pathname <- file.path(path, "ker_0083x1d_catsat_vgpm_20111101_MTLPB.nc")
expl <- ncgrid(ncfile=pathname)
}
\seealso{
Other SESspacial: \code{\link{idPixel}};
\code{\link{importSEAPOpred}}
}

