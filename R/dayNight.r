#' dayNight
#' @description 
#' @param
#' @param
#' @param
#' @details
#' @seealso
#' @author Yves
#' @examples
dayNight <- function(time, loc, elim=c(-18, 18)) {
  
  if (any(is.na(time))) stop("NA not allowed in 'time' argument")
  if (any(is.na(loc))) {
    locNA <- apply(is.na(loc), 1, sum) != 0
    sunAngle <- rep(NA, length(time))
    sunAngle[!locNA] <- sunPosition(time=time[!locNA], loc=loc[!locNA, ])$el
  }
  else {
    sunAngle <- sunPosition(time=time, loc=loc)$el
  }
  return(sunAngle < elim[1] | sunAngle > elim[2])
  
}
