\name{sunPosition}
\alias{sunPosition}
\title{sunPosition}
\usage{
sunPosition(Year, Month, Day, Hour = 12, Minute = 0, Second = 0,
  time = NULL, Lat = 46.5, Lon = 6.5, loc = NULL)
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

  \item{time}{An object with date and time information,
  thus previous arguments are ignored. Can be of POSIXct or
  data.frame classes. If data frame order of columns must
  be from Year to Second (6 columns).}

  \item{Lat}{Atomic numeric vector with latitude
  information.}

  \item{Lon}{Atomic numeric vector with longitude
  information.}

  \item{loc}{A data frame including with Lat and Lon can
  replace the last two variables.}
}
\description{
Compute sun azimuth and elevation given a location and a
date/time.
}
\details{
Based on:
\url{http://stackoverflow.com/questions/8708048/position-of-the-sun-given-time-of-Day-latitude-and-longitude.}.
Update version: Jan 6 '12 at 21:40 by "Josh O'Brien".
}
\examples{
testPts <- data.frame(Lat = c(-41,-3,3, 41),
                     Lon = c(0, 0, 0, 0))
# Sun's position as returned by the NOAA Solar Calculator,
NOAA <- data.frame(elevNOAA = c(72.44, 69.57, 63.57, 25.6),
                   azNOAA = c(359.09, 180.79, 180.62, 180.3))
# Sun's position as returned by sunPosition()
sunPos <- sunPosition(Year = 2012, Month = 12, Day = 22,
                      Hour = 12, Minute = 0, Second = 0,
                      Lat = testPts$Lat, Lon = testPts$Lon)
# Comparison
cbind(testPts, NOAA, sunPos)
# Another syntax
time <- data.frame(Year=rep(2012, 4), Month=rep(12, 4), Day=rep(22, 4),
                   Hour=10:13, Minute=rep(0, 4), Second=rep(0,4))
sunPosition(time=time, loc=testPts)
}
\seealso{
\code{\link{isDay}}

Other generalUtils: \code{\link{SESname}};
\code{\link{convertTime}}; \code{\link{nval}};
\code{\link{per}}; \code{\link{replaceMissing}};
\code{\link{sunPos}}
}

