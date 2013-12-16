\name{sunPos}
\alias{sunPos}
\title{sunPos}
\usage{
  sunPos(Year, Month, Day, Hour = 12, Minute = 0,
    Second = 0, lat = 46.5, long = 6.5)
}
\arguments{
  \item{Year}{Atomic numeric vector with year information.}

  \item{Month}{Atomic numeric vector with month
  information.}

  \item{Day}{Atomic numeric vector with day information.}

  \item{Hour}{Atomic numeric vector with hour information.}

  \item{Minute}{Atomic numeric vector with minute
  information.}

  \item{Second}{Atomic numeric vector with second
  information.}
}
\description{
  Compute sun azimuth and elevation given a location and a
  date/time. Algorithm taken from:
  \url{http://stackoverflow.com/questions/8708048/position-of-the-sun-given-time-of-Day-latitude-and-longitude.}.
  Update version: Jan 6 '12 at 21:40 by "Josh O'Brien".
}
\seealso{
  \code{\link{sunPosition}}

  Other generalUtils: \code{\link{SESname}},
  \code{\link{convertTime}}, \code{\link{per}},
  \code{\link{replaceMissing}}, \code{\link{sunPosition}}
}
\keyword{internal}

