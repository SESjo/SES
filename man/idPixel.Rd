\name{idPixel}
\alias{idPixel}
\title{idPixel}
\usage{
  idPixel(stat, grid, ses = NULL, append = TRUE)
}
\arguments{
  \item{stat}{A statdives object.}

  \item{grid}{The grid to look pixel into.}

  \item{ses}{A ses object. 'stat' is ignored when given.}

  \item{append}{Should the entire statdives object be
  returned.}

  \item{append.grid}{Should the variable 'Pixel.id' added
  to grid}
}
\description{
  Attribute to dives the pixel number they belong to.
}
\details{
  The pixel number is row number of the 'grid' data frame.
}
\examples{
path <- system.file("extdata", package="SES")
pathname <- file.path(path, "ker_0083x1d_catsat_vgpm_20111101_MTLPB.nc")
expl <- importSEAPOpred(ncfile=pathname)
pathname <- file.path(path, "2011-16_SES_example_accelero.mat")
ses <- importSES(pathname)
ses$stat <- idPixel(ses$stat, expl[ , 1:2])
# ses$tdr <- addVar("Pixel.id", from="stat", to="tdr", ses=ses)
}
\seealso{
  \code{\link{addLoc}} \code{\link{importSEAPOpred}}
  \code{\link{importSES}}
}

