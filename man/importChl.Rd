\name{importChl}
\alias{importChl}
\title{importChl}
\usage{
  importChl(date, dir, ncfile = NULL)
}
\arguments{
  \item{date}{The date (class POSIXct) to load.}

  \item{dir}{The directory to look into.}

  \item{ncfile}{Alternatively to previous arguments, a
  NetCDF file can be given directlty.}
}
\value{
  Returns a data frame with latitude, longitude and the
  associate [Chl].
}
\description{
  Load a NetCDF file matching with a date and a directory.
}
\seealso{
  Other chl: \code{\link{extractChl}},
  \code{\link{mZeu2layer}}, \code{\link{modelMorel}}
}

