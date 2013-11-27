\name{importSEAPOpred}
\alias{importSEAPOpred}
\title{importSEAPOpred}
\usage{
  importSEAPOpred(date, dir, ncfile = NULL)
}
\arguments{
  \item{data}{The date (class POSIXct) to load.}

  \item{dir}{The directory to look into.}

  \item{ncfile}{Alternatively to previous arguments, a
  NetCDF file can be given directlty.}
}
\value{
  Retuns a data frame with latitude, longitude and biomass
  of the six functional groups.
}
\description{
  Load a NetCDF file matching with a date and a directory.
}
\examples{
path <- system.file("extdata", package="SES")
pathname <- file.path(path, "ker_0083x1d_catsat_vgpm_20111101_MTLPB.nc")
expl <- importSEAPOpred(ncfile=pathname)
require("fields")
brk <- c(quantile(expl$Meso.b, probs=seq(0, 0.98, length.out=30)), max(expl$Meso.b))
image.plot(as.image(expl$Meso.b, x=expl[, 1:2], nx=750, ny=250), breaks=brk, nlevel=30, zlim=brk[c(1,30)])
}
\author{
  Yves
}

