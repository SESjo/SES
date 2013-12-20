\name{extractChl}
\alias{extractChl}
\title{extractChl}
\usage{
  extractChl(stat, chldir, append = TRUE)
}
\arguments{
  \item{stat}{The 'statdives' object.}

  \item{chldir}{The directory where to find the NetCDF
  files with [chl] values.}

  \item{append}{Should the variable be returned with the
  entire statdives object ?}
}
\description{
  Create [Chl]_surf variable by extracting relevant values
  from NetCDF files.
}
\seealso{
  Other chl: \code{\link{importChl}},
  \code{\link{mZeu2layer}}, \code{\link{modelMorel}}
}

