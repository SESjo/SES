#' Find the dives and delimitate their bottom from TDR data
#' 
#' Method translated from Matlab.
#' 
#' @param tdr The TDR dataset.
#' @param dpthThres A depth threshold (m) (> 0) under which the animal is 
#' considered at surface.
#' @param durThres A duration threshold (s) under which a dive is considered as 
#' a surface period.
#' @param spdThres A vertical speed threshold (m/s) used to delimitate the 
#' dives' bootom.
#' @return A data frame with the following variable: indice of period start, 
#' indice of period end, type of period, duration (s), the dive number, 
#' indice of bottom start, indice of bottom end, bottom duration.
#' @details See Yves Le Bras M1 report for details about the method.
#' @seealso \code{\link{divesID}}, \code{\link{bottomDelim}}, \code{\link{anaBehav}}.
#' @export
#' @examples
#' path <- system.file("extdata", package="SES")
#' pathname <- file.path(path, "2011-16_SES_example_accelero.mat")
#' ses <- importSES(pathname)
#' 
#' dvs <- anaDives(ses$tdr)
#' 
#' n <- sample(setdiff(unique(dvs$dive), 0), 1)
#' df <- ses$tdr[dvs$st.idx[dvs$dive == n]:dvs$ed.idx[dvs$dive == n], ]
#' plot(-Depth ~ Time, df, type = 'l')
#' df <- ses$tdr[c(dvs$btt.st.idx[dvs$dive == n], dvs$btt.ed.idx[dvs$dive == n]), ]
#' points(-Depth ~ Time, df, type = 'p', col='red', pch = 19)
anaDives <- function(tdr, dpthThres = 15, durThres = 300, 
                     spdThres = .75, w = 12)
  bottomDelim(tdr, divesID(tdr))



#' Find the dives from TDR data
#' 
#' Method translated from Matlab.
#' 
#' @inheritParams anaDives
#' @return A data frame with the following variable: indice of period start, 
#' indice of period end, type of period, duration (s), the dive number.
#' @seealso \code{\link{bottomDelim}}, \code{\link{anaDives}}
#' @export
divesID <- function(tdr, durThres = 300, dpthThres = 15){
  
  findDefaultVars(c('Time', 'Depth'), tdr, type.obj='tdr')
  reso <- as.numeric(median(round(diff(Time))))
  
  # ID diving/surface periods given a depth threshold
  dvs <- per(Depth < dpthThres) 
  
  # Correct according to duration threshold
  dvs$value[dvs$value == FALSE & dvs$length < (durThres %/% reso)] <- TRUE
  tmp <- per(dvs$value)
  dvs$State.id <- rep(seq_along(tmp$value), tmp$length)
  dvs <- aggregate(length ~ State.id, data = dvs, sum)
  dvs$value <- tmp$value
  dvs <- per(rep(dvs$value, dvs$length), idx = TRUE)
  
  # Set better names for diving/surface periods
  dvs$value <- vapply(as.character(dvs$value), function(x) switch(x, 'FALSE' = 'Diving', 'TRUE' = 'Surface'), character(1))
  
  # Add Dive number column
  dvs$dive <- rep(0, nrow(dvs))
  dvs$dive[dvs$value == 'Diving'] <- seq_along(dvs$dive[dvs$value == 'Diving'])
  
  # Convert length into seconds
  dvs$length <- dvs$length * reso
  
  # Supress last line if not a surface
  dvs <- if (dvs$value[nrow(dvs)] == 'Diving'){dvs[-nrow(dvs), ]}
  names(dvs) <- c("st.idx", "ed.idx", "type", "Dive.dur", "Dive.numb")
  dvs
}

#' Find the dives' bottom from TDR data
#' 
#' Method translated from Matlab.
#' 
#' @param dvs The dives indices as returned by \code{\link{diveID}}.
#' @inheritParams anaDives
#' @return A data frame with the following variable: indice of period start, 
#' indice of period end, type of period, duration (s), the dive number, 
#' indice of bottom start, indice of bottom end, bottom duration.
#' @details See Yves Le Bras M1 report for details about the method.
#' @seealso \code{\link{divesID}}, \code{\link{anaDives}}
#' @export
bottomDelim <- function(tdr, dvs, spdThres = .75, w = 12) {
  
  findDefaultVars(c('Time', 'Depth'), tdr, type.obj='tdr')
  dTime <- as.numeric(diff(Time))
  reso <- median(round(dTime))
  
  # Compute smoothed vertical speed
  spd <- rollapply(diff(Depth) / dTime, mean, w)
  
  # Delim bottom
  bbtNames <- c("btt.st.idx", "btt.ed.idx", "Btt.dur")
  dvs[ , bbtNames] <- rep(NA, nrow(dvs))
  for (ii in seq_along(dvs$"Dive.numb")){
    if (dvs$type[ii] == 'Surface') next
    y <- spd[dvs$st.idx[ii]:dvs$ed.idx[ii]]
    x <- Time[dvs$st.idx[ii]:dvs$ed.idx[ii]]
    spdMod <- lm(y ~ poly(x, degree = 4))
    btt <- per(abs(predict(spdMod)) < spdThres, idx = TRUE)
    dvs[ii, bbtNames] <- unlist(btt[btt$value, -3]) + c(rep(dvs[ii, 1] - 1, 2), 0)
  }
  dvs$Btt.dur <- dvs$Btt.dur * reso
  names(dvs) <- c("st.idx", "ed.idx", "type", "Dive.dur", "Dive.numb", "btt.st.idx", 
                  "btt.ed.idx", "Btt.dur")
  return(dvs)
}

#' Apply a function to each dive/surface/bottom
#' 
#' \code{dvapply} is a utility to apply function to specific parts of a TDR dataset.
#' 
#' @param FUN Function to apply
#' @param obj A TDR object
#' @param dvs Optional. A table with dives/surfaces/bootoms indices as returned by 
#' \code{\link{divesID}} or \code{\link{anaDives}}.
#' @param type The periods involved: to choose in \code{c('dive', 'surface', 
#' 'both', 'bottom')}.
#' @param ... Other arguments to be passed to \code{FUN}.
#' @export
#' @examples
#' path <- system.file("extdata", package="SES")
#' pathname <- file.path(path, "2011-16_SES_example_accelero.mat")
#' ses <- importSES(pathname)
#' depth.max <- dvapply(function(tdr){max(tdr$Depth)}, ses$tdr)
dvapply <- function(FUN, obj, dvs, 
                    type = c('dive', 'surface', 'both', 'bottom', 'ascent', 'descent'), ...){
  if (missing(dvs))
    dvs <- switch(match.arg(type), bottom = anaDives(obj), divesID(obj))
  idx <- switch(match.arg(type),
                dive = dvs[dvs$type == 'Diving', 1:2],
                surface = dvs[dvs$type == 'Surface', 1:2],
                both = dvs[ , 1:2], 
                bottom = dvs[dvs$type == 'Diving', 6:7])
  f <- function(st, ed, x, ...) FUN(x[st:ed, ], ...)
  args <- if (is.null(list(...))) list(x = obj) else c(list(x = obj), list(...))
  mapply(f, st = idx[ , 1], ed = idx[ , 2], MoreArgs = args)
}

#' Compute behavioral variables from TDR dataset
#' 
#' @param tdr The TDR dataset.
#' @param dvs Optional. A table with dives/surfaces/bootoms indices as returned by 
#' \code{\link{divesID}} or \code{\link{anaDives}}.
anaBehav <- function(tdr, dvs){
  
  if (missing(dvs))
    dvs <- anaDives(tdr)
  
  dvs <- split(dvs, dvs$type)
  dvsVars <- c("Dive.dur", "Dive.numb", "Btt.dur")
  # List of functions to use on the entire dive
  funsD <- list(Depth.max = function(tdr){max(tdr$Depth)},
                Catch.numb = function(tdr){cch <- per(tdr$is.Catch) ; nrow(cch[cch[,1],])})
  # List of functions to use on on the bottom
  funsB <- list(SinuV = function(tdr){var(tdr$Depth)})
  
  stats <- lapply(funsD, dvapply, obj = tdr, dvs = dvs$Diving)
  stats <- c(stats, lapply(funsB, dvapply, obj = tdr, dvs = dvs$Diving, type = 'bottom'))
  stats[dvsVars] <- dvs$Diving[ , dvsVars]
  stats$Surf.dur.aft <- dvs$Surface$Dive.dur[-1]
  as.data.frame(stats)
}