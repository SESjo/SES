#' Plot a random dive
#' 
#' \code{randomDv} Extract and optionally plots a random dive given TDR data 
#' and dive delimitation info.
#' @param obj A 'ses' or 'tdr' object
#' @param dvs Dive (and bottom delimitation) info as returned by 
#' \code{\link{anaDives}} or \code{\link{divesID}}.
#' @param plt Should a plot be drawn ?
#' @param n A dive number in case the dive should not be sampled randomly.
#' @param btt Should the bottom step be ignored by teh function ?
#' @return Return a list of the dive number and the TDR data extracted along 
#' the dive and the bottom.
#' @seealso \code{\link{anaDives}}
#' @export
randomDv <- function(obj, dvs, plt = TRUE, n = NULL, btt = TRUE){
  if (is.ses(obj)){
    obj <- obj$tdr
    if (missing(dvs)) dvs <- obj$stat
  }
  n <- n %else% sample(setdiff(unique(dvs$Dive.id), 0), 1)
  has.Btt <- partial(any %.% grepl, pattern = '^b.*x$')
  if (btt && has.Btt(names(dvs))){
    while (is.na(dvs$btt.st.idx[dvs$Dive.id == n]))
      n <- sample(setdiff(unique(dvs$Dive.id), 0), 1)
  }
  df <- function(cols)
    obj[do.call(seq, unname(dvs[dvs$Dive.id == n, grepl(cols, names(dvs))])), ]
  if (plt){
    plot(-Depth ~ Time, df('^[^b].*x$'), type = 'l', lwd = 2, main = paste('Dive number', n))
    abline(h = 0, col = 'gray', lty = 2)
    if (btt && has.Btt(names(dvs)))
      points(-Depth ~ Time, df('^b.*x$'), type = 'l', col='red', lwd = 2)
  }
  if (btt)
    invisible(list(n = n, dv = df('^[^b].*x$'), btt = df('^b.*x$')))
  else
    invisible(list(n = n, dv = df('^[^b].*x$')))
}



#' Apply function along a vector
#' 
#' \code{rollapply} apply the given function along a vector.
#' 
#' @param x An atomic vector.
#' @param FUN the function to be applied. This function must return a single value 
#' when applied to a an atomic vector.
#' @param w The width of the window.
#' @param wty The type of window. To choose in \code{c('m', 'f'))} where 'm' stands 
#' for "moving" and 'f' for "fixed".
#' @param ... Optional arguments to FUN.
#' @export
#' @examples
#' x <- seq(1, 3, length = 100) + rt(100, df = 2) / 3
#' plot(x)
#' lines(rollapply(x, mean, 5)  , col = "red", lwd = 2)
#' lines(rollapply(x, median, 5), col = "blue", lwd = 2)
#' lines(rollapply(x, mean, 5, wty='f'), col = "green", lwd = 2)
rollapply <- function(x, FUN, w = 10, wty = 'm', ...) {
  out <- rep(NA, length(x))
  offset <- trunc(w / 2)
  if (wty == 'm'){
    for (i in (offset + 1):(length(x) - w + offset + 1)) {
      out[i] <- FUN(x[(i - offset):(i + offset)], ...)
    }
  } else if (wty == 'f'){
    for (i in seq(offset + 1, length(x) - w + offset + 1, by = w)) {
      out[(i - offset):(i + offset)] <- rep(FUN(x[(i - offset):(i + offset)], ...), 
                                            2*offset + 1)
    }
  } else {
    stop('Unknown window type specified.')
  }
  out
}

#' Use time and location to find if events occured during the day or the night
#' 
#' Use time and loc info to find if events occured during the day or the night. SEAPODYm criteria 
#' elevlim=c(-18, 18). Transition periods filled with NAs.
#' 
#' @param time The time information in \code{POSIXct} format.
#' @param loc A data frame including latitude ('Lat' column) and longitude ('Lon' column).
#' @param stat A statdives object can be used instead of the two previous arguments.
#' @param elevlim Sun elevation the thresholds to distinguish between day and night
#' @param append Should the entire updated object be returned (if 'stat' argument was used).
#' @seealso \code{\link{sunPosition}}
#' @export
#' @examples
#' testPts <- data.frame(Lat = c(-41,-3,3, 41), 
#'                      Lon = c(0, 0, 0, 0))
#' time <- data.frame(Year=rep(2012, 4), Month=rep(12, 4), Day=rep(22, 4),
#'                    Hour=10:13, Minute=rep(0, 4), Second=rep(0,4))
#' isDay(time, testPts)
#' isDay(convertTime(time, to="posx"), testPts)
isDay <- function (time, loc, stat = NULL, elevlim = c(-18, 18), append = TRUE) 
{
  if (!is.null(stat)) {
    findDefaultVars(c("Time", "Lat", "Lon"), stat, type.obj = "stat")
    if (missing(time)) time <- Time
    if (missing(loc))  loc  <- data.frame(Lat = Lat, Lon = Lon)
  }
  if (any(is.na(time))) 
    stop("NA not allowed in 'time' argument")
  if (any(is.na(loc))) {
    locNA <- apply(is.na(loc), 1, sum) != 0
    sunAngle <- try(rep(NA, nrow(time)), silent = TRUE)
    if (inherits(sunAngle, "try-error")) 
      sunAngle <- rep(NA, length(time))
    sunAngle[!locNA] <- sunPosition(time = time[!locNA], 
                                    loc = loc[!locNA, ])$el
  }
  else {
    locNA <- try(rep(FALSE, nrow(time)), silent = TRUE)
    if (inherits(locNA, "try-error")) 
      locNA <- rep(FALSE, length(time))
    sunAngle <- sunPosition(time = time, loc = loc)$el
  }
  is.Day <- rep(NA, length(locNA))
  is.Day[!locNA & sunAngle > elevlim[2]] <- TRUE
  if (!is.null(stat) & append) {
    stat$is.Day <- is.Day
    return(stat)
  }
  else {
    return(is.Day)
  }
}

#' Copy values of a variable from an table to another within a 'ses' object.
#' 
#' @param var The name of the variable to be copied.
#' @inheritParams addLoc
#' @return Object given at the 'to' argument, updated with location information.
#' @details Missing values (NAs) of the choosen variable are transformed in zeros when pasted 
#' to a 'tdr' object.
#' @seealso \code{\link{addLoc}}
#' @export
addVar <- function(var, from, to, ses=NULL, append=TRUE){
  if (!is.null(ses)) {
    from <- eval(parse(text=paste0(substitute(ses), '$', from)))
    to <- eval(parse(text=paste0(substitute(ses), '$', to)))
  }
  argtdr <- match("tdr", setdiff(c(class(from), class(to)), "data.frame"), nomatch=0)
  
  dvidFrom <- userHeader("Dive.id", type=class(from)[1])
  findVars(dvidFrom, from, type="check")
  dvidTo <- userHeader("Dive.id", type=class(to)[1])
  findVars(dvidTo, to, type="check")
  
  if (argtdr == 1){
    to <- merge(to, unique(from[ , c(dvidFrom,  var)]), by.x=dvidTo, by.y=dvidFrom)
    if (!append) return(to[, var])
    class(to) <- c("statdives", "data.frame")
  } else if (argtdr == 2){
    if (any(is.na(from[ , var]))) {
      message("Missing values (NAs) pasted as zeros")
      from[ , var] <- replaceMissing(from[ , var], na.0=NA, na.1=0)
    }
    dvs <- per(to[ , dvidTo])
    dvs$value[dvs$value != 0] <- from[ , var] 
    if (!append) return(rep(dvs$value, dvs$length))
    to[ , var] <- rep(dvs$value, dvs$length)
  }else{
    stop("Input objects must be of class 'tdr', and 'statdives'.")
  }
  return(to)
}

#' Copy locations from one table to another within a 'ses' object
#' 
#' @param from Source of location information ('tdr' or 'statdives' object).
#' @param to Where to add location inforation (respectively a 'statdives' or 'tdr' object).
#' @param ses A SES from which use the data. If used 'from' and 'to' must be taken in "tdr" and "stat".
#' @param append logical. If TRUE, the function returns the entire object 'to' with the 
#' added column. if FALSE it returns the columns.
#' @return Object given at the 'to' argument, updated with location information.
#' @seealso \code{\link{addVar}} \code{\link{importSES}}
#' @export
#' @examples
#' path <- system.file("extdata", package="SES")
#' pathname <- file.path(path, "2011-16_SES_example_accelero.mat")
#' ses <- importSES(pathname)
#' ses$tdr <- addLoc(ses$stat, ses$tdr)
#' head(ses$tdr)
addLoc <- function(from, to, ses=NULL, append=TRUE){
  if (!is.null(ses)) {
    from <- eval(parse(text=paste0(substitute(ses), '$', from)))
    to <- eval(parse(text=paste0(substitute(ses), '$', to)))
  }
  
  vars <- c("Lat", "Lon")
  findDefaultVars(vars, from, type.obj=class(from)[1], type="check")
  if (!append) return(lapply(vars, addVar, from, to, append=FALSE))
  to[ , vars] <- lapply(vars, addVar, from, to, append=FALSE)
  return(to)
}

#' Decompose an atomic vector to its successive values and their length.
#' 
#' The reverse of 'base::rep()' function: decompose an atomic vector to its successive 
#' values and their length.
#' 
#' @param x The atomic vector to examine.
#' @param idx Should the indexes (start and end) of homogeneous sequences be returned as well ?
#' 
#' @return A data frame with values and lengths of the homogeneous sequences of x. The class of the 
#' column 'value' is copied from the input.
#'  
#' @family generalUtils
#' @export
#' @examples
#' (x <- rep(LETTERS[1:10], 10:1))
#' (y <- per(x))
#' # 'per()' is the reverse of 'rep()'
#' # identical(rep(y$value, y$length), x)   # TRUE
#' # Because characters are not converted to factors
#' # inherits(y$value, class(x))            # TRUE
per <- function(x, idx=FALSE) {
  # Save original object
  x.org <- x
  # Concert x to numeric
  if      (is.logical(x))   {x <- as.numeric(x)}
  else if (is.factor(x))    {x <- as.numeric(x)}
  else if (is.character(x)) {x <- as.numeric(as.factor(x))}
  # Compute and return
  chg <- diff(x, lag=1)
  end <- c(which(chg != 0), length(x)) ; start <- c(1, end[-length(end)] + 1)
  if (idx) return(data.frame(st.idx=start, ed.idx=end, value=x.org[start], length=end - start + 1, stringsAsFactors=FALSE))
  else return(data.frame(value=x.org[start], length=end - start + 1, stringsAsFactors=FALSE))
}
