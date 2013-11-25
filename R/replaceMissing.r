#' replaceMissing
#' @description Replace values in an atomic vector.
#' @param x The atomic vector
#' @param na.0 The value to be replaced. Default is NaN.
#' @param na.1 The replacement. Default is NA.
#' @author Yves
#' @export
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