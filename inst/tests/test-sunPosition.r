context("sunPosition from location and time")

test_that("sunPosition() and NOAA calculator give same results to the nearest 10^-1", {
  testPts <- data.frame(lat = c(-41,-3,3, 41), long = c(0, 0, 0, 0))
  # Sun's position as returned by the NOAA Solar Calculator,
  elevNOAA = c(72.44, 69.57, 63.57, 25.6)
  azNOAA = c(359.09, 180.79, 180.62, 180.3)
  # Sun's position as returned by sunPosition()
  sunPos <- sunPosition(year = 2012, month = 12, day = 22,
                        hour = 12, min = 0, sec = 0,
                        lat = testPts$lat, long = testPts$long)
  
  expect_that(round(sunPos$elevation, digits=1), equal(round(elevNOAA, digits=1)))
  expect_that(round(sunPos$azimuth, digits=1), equal(round(azNOAA, digits=1)))
}