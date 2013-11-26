#' posx2ymd
#' @description Convert a POSIX object to a data frame.
#' @param posx The POSIX object to convert
#' @param width The desired number of columns desired in the data frame.
#' @author Yves
#' @export
#' @examples
#' posx2ymd(as.POSIXct("2011-11-01 01:02:03"))
#' posx2ymd(as.POSIXct("2011-11-01 01:02:03"), 3)
posx2ymd = function(posx, width=6){
  fmt <- c(Year="%Y", Month="%m", Day="%d",
           Hour="%H", Minute="%M", Second="%S")
  if (width != 6) fmt <- fmt[1:width]
  ymd <- as.data.frame(lapply(fmt, function(posx, fmt){as.numeric(format(posx, fmt))}, posx=posx))
  return(ymd)
}