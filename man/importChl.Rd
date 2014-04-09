\name{importChl}
\alias{importChl}
\title{Import a CHL map from a NetCDF file.}
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
Import a CHL map from a NetCDF file.
}
\seealso{
Other chl: \code{\link{extractChl}};
\code{\link{mZeu2layer}}; \code{\link{modelMorel}}
}

