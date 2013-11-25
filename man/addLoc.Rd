\name{addLoc}
\alias{addLoc}
\title{addLoc}
\usage{
  addLoc(from, to, ses = NULL, append = TRUE)
}
\arguments{
  \item{from}{Source of location information ('tdr' or
  'statdives' class).}

  \item{to}{Where to add location inforation ('tdr' or
  'statdives' class).}

  \item{ses}{A SES from which use the data. If used 'from'
  and 'to' must be taken in "tdr" and "stat".}

  \item{append}{logical. If TRUE, the function returns the
  entire object 'to' with the added column. if FALSE it
  returns the columns.}
}
\value{
  Object given at the 'to' argument, updated with location
  information.
}
\description{
  Copy locations (Latitude and Longitude) from an an object
  to another within a ses.
}
\examples{
path <- system.file("extdata", package="SES")
pathname <- file.path(path, "2011-16_SES_example_accelero.mat")
ses <- importSES(pathname)
ses$tdr <- addLoc(ses$stat, ses$tdr)
}
\author{
  Yves
}
\seealso{
  \code{\link{addVar}} \code{\link{importSES}}
}
\keyword{Lat}
\keyword{Lon}

