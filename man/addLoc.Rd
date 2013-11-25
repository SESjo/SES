\name{addLoc}
\alias{addLoc}
\title{addLoc}
\usage{
  addLoc(from, to, ses = NULL)
}
\arguments{
  \item{from}{Source of location information ('tdr' or
  'statdives' class).}

  \item{to}{Where to add location inforation ('tdr' or
  'statdives' class).}

  \item{ses}{A SES from which use the data. If used 'from'
  and 'to' must be taken in "tdr" and "stat".}
}
\value{
  Object given at the 'to' argument, updated with location
  information.
}
\description{
  Add location Latitude and Longitude from an an obeject to
  another within a ses
}
\author{
  Yves
}
\seealso{
  \code{\link{importSES}}
}
\keyword{Lat}
\keyword{Lon}

