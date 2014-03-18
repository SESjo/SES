\name{importMercator}
\alias{importMercator}
\title{importMercator}
\usage{
importMercator(date, dir, ncfile = NULL, type = "UV", latname = "y",
  lonname = "x")
}
\arguments{
  \item{type}{The list of variable to import: subset of
  'UVST'. 'U' and 'V' are the zonal and meridional velocity
  components of the current. 'S' is salinity and 'T' stands
  for temperature.}

  \item{ncfile}{The to read the grid from.}

  \item{latname}{The variable name of the meridional
  dimension of the grid.}

  \item{lonname}{The variable name of the zonal dimension
  of the grid.}

  \item{date}{The date (of type \code{POSIXct}) to load.}

  \item{dir}{The directory to look into.}
}
\description{
Import velocity field of mecator currents from a NetCDF
file.
}
\seealso{
Other SESspacial: \code{\link{idPixel}};
\code{\link{importSEAPOpred}}; \code{\link{ncgrid}}
}

