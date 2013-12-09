\name{extractBiom}
\alias{extractBiom}
\title{extractBiom}
\usage{
  extractBiom(stat, tdr, biomdir, append = TRUE)
}
\arguments{
  \item{stat}{}

  \item{biomdir}{The directory where to find the NetCDF
  files with biomass values.}

  \item{append}{Should the variable be returned with the
  entire statdives object ?}
}
\description{
  Create Biomass variable by extracting relevant values
  from NetCDF files
}
\details{
  When time resolution is > 1day, then the biomass is
  extracted in the pixel where the averaged daily location
  of seal belongs.
}
\author{
  Yves
}

