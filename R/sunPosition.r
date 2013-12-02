#' sunPosition
#' @description Compute sun azimuth and elevation given a location and a date/time.
#' @param Year
#' @param Month
#' @param Day
#' @param Hour
#' @param Minute
#' @param Second
#' @param time An object with date and time information, thus previous arguments are ignored. Can be of POSIXct or data.frame classes. If data frame order of columns must be from Year to Second (6 columns).
#' @param lat
#' @param lon
#' @details Algorithm taken from: http://stackoverflow.com/questions/8708048/position-of-the-sun-given-time-of-Day-latitude-and-longitude. Update: the Jan 6 '12 at 21:40 by "Josh O'Brien"
#' @seealso \code{\link{isDay}}
#' @author Yves
#' @export
#' @examples
#' testPts <- data.frame(Lat = c(-41,-3,3, 41), 
#'                      Lon = c(0, 0, 0, 0))
#' # Sun's position as returned by the NOAA Solar Calculator,
#' NOAA <- data.frame(elevNOAA = c(72.44, 69.57, 63.57, 25.6),
#'                    azNOAA = c(359.09, 180.79, 180.62, 180.3))
#' # Sun's position as returned by sunPosition()
#' sunPos <- sunPosition(Year = 2012, Month = 12, Day = 22,
#'                       Hour = 12, Minute = 0, Second = 0,
#'                       Lat = testPts$Lat, Lon = testPts$Lon))
#' # Comparison
#' cbind(testPts, NOAA, sunPos)
#' # Another syntax
#' time <- data.frame(Year=rep(2012, 4), Month=rep(12, 4), Day=rep(22, 4),
#'                    Hour=10:13, Minute=rep(0, 4), Second=rep(0,4))
#' sunPosition(time=time, loc=testPts)
sunPosition <- function(Year, Month, Day, Hour=12, Minute=0, Second=0, time=NULL,
                        Lat=46.5, Lon=6.5, loc=NULL) {
  
  if (!is.null(loc)) {
    findVars(c("Lat", "Lon"), loc)
  }
  if (!is.null(time)) {
    if (inherits(time, "POSIXt")) {
      ymd <- posx2ymd(time)
    }else {
      if (ncol(time) != 6) stop("'time' must have 6 columns.")
      ymd <- replaceMissing(time, na.0=NA, 0)
      names(ymd) <- c("Year", "Month", "Day", "Hour", "Minute", "Second")
    }
    return(with(ymd, .sunPosition(Year, Month, Day, Hour, Minute, Second, Lat, Lon)))
  }
  .sunPosition(Year, Month, Day, Hour, Minute, Second, Lat, Lon)
}




.sunPosition <- function(Year, Month, Day, Hour=12, Minute=0, Second=0,
                        lat=46.5, long=6.5) {
  
  # From: http://stackoverflow.com/questions/8708048/position-of-the-sun-given-time-of-Day-latitude-and-longitude
  # Update: the Jan 6 '12 at 21:40 by "Josh O'Brien"
  
  twopi <- 2 * pi
  deg2rad <- pi / 180
  
  # Get Day of the Year, e.g. Feb 1 = 32, Mar 1 = 61 on leap Years
  Month.Days <- c(0,31,28,31,30,31,30,31,31,30,31,30)
  Day <- Day + cumsum(Month.Days)[Month]
  leapDays <- Year %% 4 == 0 & (Year %% 400 == 0 | Year %% 100 != 0) & 
    Day >= 60 & !(Month==2 & Day==60)
  Day[leapDays] <- Day[leapDays] + 1
  
  # Get Julian date - 2400000
  Hour <- Hour + Minute / 60 + Second / 3600 # Hour plus fraction
  delta <- Year - 1949
  leap <- trunc(delta / 4) # former leapYears
  jd <- 32916.5 + delta * 365 + leap + Day + Hour / 24
  
  # The input to the Atronomer's almanach is the difference between
  # the Julian date and JD 2451545.0 (noon, 1 January 2000)
  time <- jd - 51545.
  
  # Ecliptic coordinates
  
  # Mean longitude
  mnlong <- 280.460 + .9856474 * time
  mnlong <- mnlong %% 360
  mnlong[mnlong < 0] <- mnlong[mnlong < 0] + 360
  
  # Mean anomaly
  mnanom <- 357.528 + .9856003 * time
  mnanom <- mnanom %% 360
  mnanom[mnanom < 0] <- mnanom[mnanom < 0] + 360
  mnanom <- mnanom * deg2rad
  
  # Ecliptic longitude and obliquity of ecliptic
  eclong <- mnlong + 1.915 * sin(mnanom) + 0.020 * sin(2 * mnanom)
  eclong <- eclong %% 360
  eclong[eclong < 0] <- eclong[eclong < 0] + 360
  oblqec <- 23.439 - 0.0000004 * time
  eclong <- eclong * deg2rad
  oblqec <- oblqec * deg2rad
  
  # Celestial coordinates
  # Right ascension and declination
  num <- cos(oblqec) * sin(eclong)
  den <- cos(eclong)
  ra <- atan(num / den)
  ra[den < 0] <- ra[den < 0] + pi
  ra[den >= 0 & num < 0] <- ra[den >= 0 & num < 0] + twopi
  dec <- asin(sin(oblqec) * sin(eclong))
  
  # Local coordinates
  # Greenwich mean sidereal time
  gmst <- 6.697375 + .0657098242 * time + Hour
  gmst <- gmst %% 24
  gmst[gmst < 0] <- gmst[gmst < 0] + 24.
  
  # Local mean sidereal time
  lmst <- gmst + long / 15.
  lmst <- lmst %% 24.
  lmst[lmst < 0] <- lmst[lmst < 0] + 24.
  lmst <- lmst * 15. * deg2rad
  
  # Hour angle
  ha <- lmst - ra
  ha[ha < -pi] <- ha[ha < -pi] + twopi
  ha[ha > pi] <- ha[ha > pi] - twopi
  
  # Latitude to radians
  lat <- lat * deg2rad
  
  # Azimuth and elevation
  el <- asin(sin(dec) * sin(lat) + cos(dec) * cos(lat) * cos(ha))
  az <- asin(-cos(dec) * sin(ha) / cos(el))
  
  # For logic and names, see Spencer, J.W. 1989. Solar Energy. 42(4):353
  cosAzPos <- (0 <= sin(dec) - sin(el) * sin(lat))
  sinAzNeg <- (sin(az) < 0)
  az[cosAzPos & sinAzNeg] <- az[cosAzPos & sinAzNeg] + twopi
  az[!cosAzPos] <- pi - az[!cosAzPos]
  
  el <- el / deg2rad
  az <- az / deg2rad
  lat <- lat / deg2rad
  
  return(list(elevation=el, azimuth=az))
}
