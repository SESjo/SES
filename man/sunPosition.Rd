\name{sunPosition}
\alias{sunPosition}
\title{sunPosition}
\usage{
  sunPosition(Year, Month, Day, Hour = 12, Minute = 0,
    Second = 0, time = NULL, Lat = 46.5, Lon = 6.5,
    loc = NULL)
}
\arguments{
  \item{Year}{}

  \item{Month}{}

  \item{Day}{}

  \item{Hour}{}

  \item{Minute}{}

  \item{Second}{}

  \item{time}{An object with date and time information,
  thus previous arguments are ignored. Can be of POSIXct or
  data.frame classes. If data frame order of columns must
  be from Year to Second (6 columns).}

  \item{lat}{}

  \item{lon}{}
}
\description{
  Compute sun azimuth and elevation given a location and a
  date/time.
}
\details{
  Algorithm taken from:
  http://stackoverflow.com/questions/8708048/position-of-the-sun-given-time-of-Day-latitude-and-longitude.
  Update: the Jan 6 '12 at 21:40 by "Josh O'Brien"
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
                      Lat = testPts$Lat, Lon = testPts$Lon))
# Comparison
cbind(testPts, NOAA, sunPos)
# Another syntax
time <- data.frame(Year=rep(2012, 4), Month=rep(12, 4), Day=rep(22, 4),
                   Hour=10:13, Minute=rep(0, 4), Second=rep(0,4))
sunPosition(time=time, loc=testPts)
}
\author{
  Yves
}
\seealso{
  \code{\link{isDay}}
}

