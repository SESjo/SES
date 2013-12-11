# My package tools
is.error <- function(x) inherits(x, "try-error")
where <- function(x, f) vapply(x, f, logical(1))
"%wo%" <- function(x, y) x[!x %in% y] # x without y, asymetric
"%wi%" <- function(x, y) x[x %in% y] # x within y, equivalent to intersect, symetric

#' datenum2posx
#' @description Utility to convert Matlab numeric detes to R's POSIXct format
#' @param x The atomic vector of matlab dates.
#' @author Yves
#' @export
datenum2posx <- function(x){as.POSIXct((x - 719529)*24*3600, tz="UTC", origin="1970-01-01")}


#' SESname
#' @description Extract the ID of an individual from a string that contains it.
#' @param text Text containing the seal ID.
#' @return TheiID as character
#' @author Yves
#' @export
#' @examples 
#' SESname("Path/to/seal/file/2011-12_some_ses_file.txt")
SESname <- function(text){
  regmatches(text, regexpr('20+[0-9]{2}-[0-9]{2}', text))
}


#' replaceMissing
#' @description Replace values in an atomic vector.
#' @param x The atomic vector
#' @param na.0 The value to be replaced. Default is NaN.
#' @param na.1 The replacement. Default is NA.
#' @author Yves
#' @export
#' @keywords internal
#' @examples
#' x <- sample(c(1:3,NaN), 20, replace=TRUE)
#' x 
#' replaceMissing(x)
replaceMissing <- function(x, na.0=NaN, na.1=NA) {
  if (is.nan(na.0)) x[is.nan(x)] <- na.1
  else if (is.na(na.0)) x[is.na(x)] <- na.1
  else x[is.na(x)] <- na.1
  x
}
