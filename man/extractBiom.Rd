\name{extractBiom}
\alias{extractBiom}
\title{extractBiom}
\usage{
extractBiom(stat, tdr, biomdir)
}
\arguments{
  \item{stat}{The 'statdives' object.}

  \item{tdr}{The 'tdr' object.}

  \item{biomdir}{The directory where to find the NetCDF
  files with biomass values.}
}
\description{
Create Biomass variable by extracting relevant values from
NetCDF files. When the time resolution of the biomass
NetCDF files is > 1day, then the biomass is extracted in
the pixel where the averaged daily location of seal belongs
(Beware, implies day/night same location).
}

