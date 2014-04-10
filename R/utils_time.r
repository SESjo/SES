#' Convert a time object to another format...
#' 
#' ...among those commonly used in the package and data files.
#' 
#' @param x The object to convert
#' @param to The output format. Choose in 'posx', 'ymd', 'text'.
#' @param width The desired number of columns desired in the data frame.
#' @details width = 3 when 'text' format is involved.
#' @family generalUtils
#' @export
#' @examples
#' tm <- as.POSIXct("2011-11-01 01:02:03")
#' convertTime(tm, to="posx", width=4) # "2011-11-01 01:00:00 CET"
#' convertTime(tm, to="text")          # "20111101", width = 3 when 'text' is involved
#' (tm <- convertTime(tm, to="ymd"))   # 2011    11   1    1      2      3
#' convertTime(tm, to="posx")          # "2011-11-01 01:02:03 CET"
#' convertTime(tm, to="text")          # "20111101", width = 3 when 'text' is involved
convertTime <- function(x, to, width=6){
  if (inherits(x, "POSIXt") && to == "ymd") return(posx2ymd(x, width))
  else if (inherits(x, "POSIXt") && to == "posx") return(ymd2posx(posx2ymd(x, width)))
  else if (is.character(x) && to == "posx") return(text2posx(x))
  else if (inherits(x, "data.frame") && to == "posx") return(ymd2posx(x, width))
  else if (is.character(x) && to == "ymd") return(posx2ymd(text2posx(x)))
  else if (inherits(x, "POSIXt") && to == "text") return(format(x, format="%Y%m%d"))
  else if (inherits(x, "data.frame") && to == "text") return(format(ymd2posx(x), format="%Y%m%d"))
  else message("Requested conversion is not implemented")
}

#' Convert a POSIX object to a data frame with the desired number of columns.
#' 
#' @param posx The POSIX object to convert
#' @param width The desired number of columns desired in the data frame.
#' @export
#' @keywords internal
posx2ymd = function(posx, width=6){
  fmt <- c(Year="%Y", Month="%m", Day="%d", Hour="%H", Minute="%M", Second="%S")
  if (width != 6) fmt <- fmt[1:width]
  ymd <- as.data.frame(lapply(fmt, function(posx, fmt){as.numeric(format(posx, fmt))}, posx=posx))
  return(ymd)
}

#' Convert a text date ('YYYYMMDD' formatting) to a POSIXct object.
#' 
#' @param text The text date(s) to convert
#' @export
#' @keywords internal
text2posx <- function(text){
  # Extract 8 consecutive numeric characters begining with 20 (years 20**)
  dates.str <- regmatches(text, regexpr('(20+[0-9]{6})', text))
  if (length(dates.str) == length(text)){
    return(as.POSIXct(dates.str, format="%Y%m%d"))
  }else{
    stop('Several expression matched.')
  }
}

#' Convert a date matrix or data frame (yyyy|mm|dd) to a POSIXct object.
#' 
#' @param ymd 
#' @inheritParams posx2ymd
#' @export
#' @keywords internal
ymd2posx <- function(ymd, width=ncol(ymd)){
  ymd <- ymd[ , 1:width]
  fmt = c(Year="%Y", Month="%m", Day="%d", Hour="%H", Minute="%M", Second="%S")
  length(fmt[names(ymd)]) == ncol(ymd) || stop("Column names muste be 'Year', 'Month', ... 'Second'.")
  fmt = paste(fmt[names(ymd)], collapse=" ")
  ymd <- do.call(paste, ymd)
  return(as.POSIXct(ymd, format=fmt))
}
 
#' Convert a POSIXct object to a text date ('YYYYMMDD' formatting).
#' 
#' @inheritParams posx2ymd
#' @export
#' @keywords internal
posx2date <- function(posx){
  ymd2posx(posx2ymd(posx, width=3))
}

#' Utility to convert Matlab numeric detes to R's POSIXct format.
#' 
#' @param x The atomic vector of matlab dates.
#' @export
#' @keywords internal
datenum2posx <- function(x){as.POSIXct((x - 719529)*24*3600, tz="UTC", origin="1970-01-01")}
