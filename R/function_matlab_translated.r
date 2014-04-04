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
#' @param w Window width for rolling mean.
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
#' n <- sample(setdiff(unique(dvs$Dive.id), 0), 1)
#' while (is.na(dvs$btt.st.idx[dvs$Dive.id == n])) {
#'    n <- sample(setdiff(unique(dvs$Dive.id), 0), 1)
#' }
#' df <- ses$tdr[dvs$st.idx[dvs$Dive.id == n]:dvs$ed.idx[dvs$Dive.id == n], ]
#' plot(-Depth ~ Time, df, type = 'l')
#' df <- ses$tdr[c(dvs$btt.st.idx[dvs$Dive.id == n], 
#'                 dvs$btt.ed.idx[dvs$Dive.id == n]), ]
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
  names(dvs) <- c("st.idx", "ed.idx", "type", "Dive.dur", "Dive.id")
  dvs
}


#' Find the dives' bottom from TDR data
#' 
#' Method translated from Matlab.
#' 
#' @param dvs The dives indices as returned by \code{\link{divesID}}.
#' @inheritParams anaDives
#' @param bttDpth The minimun depth (percent of maximum depth) allowed
#' for the bottom limits.
#' @param w Window width for rolling mean.
#' @return A data frame with the following variable: indice of period start, 
#' indice of period end, type of period, duration (s), the dive number, 
#' indice of bottom start, indice of bottom end, bottom duration.
#' @details See Yves Le Bras M1 report for details about the method.
#' @seealso \code{\link{divesID}}, \code{\link{anaDives}}
#' @export
bottomDelim <- function(tdr, dvs, spdThres = .75, w = 12, bttDpth = .4) {
  
  findDefaultVars(c('Time', 'Depth'), tdr, type.obj='tdr')
  dTime <- as.numeric(diff(Time))
  reso <- median(round(dTime))
  
  # Compute smoothed vertical speed
  spd <- rollapply(diff(Depth) / dTime, mean, w)
  
  # Bottom delim
  bbtNames <- c("btt.st.idx", "btt.ed.idx", "Btt.dur")
  dvs[ , bbtNames] <- rep(NA, nrow(dvs))
  for (ii in seq_along(dvs$"Dive.id")){
    if (dvs$type[ii] == 'Surface') next
    y <- spd[dvs$st.idx[ii]:dvs$ed.idx[ii]]
    x <- Time[dvs$st.idx[ii]:dvs$ed.idx[ii]]
    spdMod <- lm(y ~ poly(x, degree = 4))
    btt <- per(abs(predict(spdMod)) < spdThres, idx = TRUE)
    dvs[ii, bbtNames] <- unlist(btt[btt$value, -3]) + c(rep(dvs[ii, 1] - 1, 2), 0)
  }
  
  # Checking: bottom limits must be deeper than 'bttDpth'% of dive Max depth
  chkFuns <- list(bttSt = function(x){x$Depth[1] >= bttDpth * max(x$Depth)},
                  bttEd = function(x){x$Depth[nrow(x)] >= bttDpth * max(x$Depth)})
  chkDel <- lapply(chkFuns, dvapply, obj = tdr, dvs = dvs, type = 'bottom')
  chkDel <- Map(`|`, e1 = lapply(chkDel, `!`), e2 = lapply(chkDel, is.na))
  
  # Compute the old fashion delim: first and last depth >= bttDpth * Max depth
  corDel <- function(x, lt = 'e'){
    per(x$Depth >= bttDpth*max(x$Depth), idx = TRUE)[2, switch(lt, s = 1, e = nrow(x))]
  }
  cond <- dvs$type == 'Diving'
  bttSt <- dvapply(corDel, tdr, dvs, lt = 's') + dvs$st.idx[cond] - 1
  bttEd <- dvapply(corDel, tdr, dvs) + dvs$st.idx[cond] - 1
  
  # Replace when it is needed (!chkDel$bttSt/Ed)
  dvs$btt.st.idx[cond][chkDel$bttSt] <- bttSt[chkDel$bttSt]
  dvs$btt.ed.idx[cond][chkDel$bttEd] <- bttEd[chkDel$bttEd]
  
  # Update Bottom duration and erase the remaining delimitation errors
  dvs$Btt.dur <- compose(abs, `-`)(dvs$btt.ed.idx, dvs$btt.st.idx) * reso
  dvs[dvs$Btt.dur %in% c(0, NA), 6:8] <- NA
  names(dvs) <- c("st.idx", "ed.idx", "type", "Dive.dur", "Dive.id", "btt.st.idx", 
                  "btt.ed.idx", "Btt.dur")
  return(dvs)
}

#' Apply a function to each dive/surface/bottom
#' 
#' \code{dvapply} is a utility to apply function to specific parts of a TDR dataset.
#' 
#' @param FUN Function to apply. The first argument has to be the TDR data 
#' subset (all columns but only the rows indicated by the \code{type} argument). 
#' See in the example section how it can be used to get Max depth.
#' @param obj A TDR object
#' @param dvs Optional. A table with dives/surfaces/bootoms indices as returned by 
#' \code{\link{divesID}} or \code{\link{anaDives}}.
#' @param type The periods involved: to choose in \code{c('dive', 'surface', 
#' 'any', 'bottom', 'ascent', 'descent')}. \code{any} means to apply the function
#' to both surface and diving periods.
#' @param ... Other arguments to be passed to \code{FUN}.
#' @export
#' @examples
#' path <- system.file("extdata", package="SES")
#' pathname <- file.path(path, "2011-16_SES_example_accelero.mat")
#' ses <- importSES(pathname)
#' depth.max <- dvapply(function(tdr){max(tdr$Depth)}, ses$tdr)
dvapply <- function(FUN, obj, dvs, 
                    type = c('dive', 'surface', 'any', 'bottom', 'ascent', 'descent'), ...){
  if (missing(dvs))
    dvs <- switch(match.arg(type), bottom = anaDives(obj), ascent = anaDives(obj), 
                  descent = anaDives(obj), divesID(obj))
  .idx <- switch(match.arg(type), dive = dvs[dvs$type == 'Diving', 1:2],
                 surface = dvs[dvs$type == 'Surface', 1:2], any = dvs[ , 1:2], 
                 bottom = dvs[dvs$type == 'Diving', 6:7],
                 ascent = dvs[dvs$type == 'Diving', c(7, 2)],
                 descent = dvs[dvs$type == 'Diving', c(1, 6)])
  .f <- function(ii, ...){
    out <- if (nNA(.idx[ii, ])) {NA} else {FUN(obj[.idx[ii, 1]:.idx[ii, 2], ], ...)}
    out %else% NA
  }
  
  sapply(seq_along(.idx[ , 1]), match.fun(.f), ...)
}

#' Compute some behavioral variables from TDR dataset
#' 
#' @param tdr The TDR dataset.
#' @param dvs Optional. A table with dives/surfaces/bootoms indices as returned by 
#' \code{\link{divesID}} or \code{\link{anaDives}}.
anaBehav <- function(tdr, dvs){
  
  if (missing(dvs))
    dvs <- anaDives(tdr)
  
  dvs <- split(dvs, dvs$type)
  dvsVars <- c("Dive.id", "st.idx", "ed.idx", "Dive.dur", "Btt.dur")
  
  # Few custom funs
  calcSpd <- function(x) {
    dTime <- as.numeric(diff(x$Time))
    median(round(dTime)) * diff(x$Depth) / dTime
  }
  meanSpd <- function(x)
    mean(calcSpd(x), na.rm = TRUE)
  countCatch <- function(x) {
    catch <- per(x$is.Catch)
    as.numeric(nrow(catch[catch[ , 1], ]))
  }
  straightness1D <- function(x) 
    abs(x$Depth[nrow(x)] - x$Depth[1]) / sum(abs(calcSpd(x)))
  
  # List of functions to use
  funsDv <- list(Time.start = function(x){x$Time[1]}, 
                 Depth.max = function(x){max(x$Depth)}, Catch.numb = countCatch)
  funsBtt <- list(Str1D = straightness1D, BCatch.numb = countCatch)
  funsAsc <- list(Spd.asc = meanSpd)
  funsDsc <- list(Spd.desc = meanSpd)
  
  stats <- c(lapply(dvs$Diving[ , dvsVars[1:3]], as.integer), 
             dvs$Diving[ , dvsVars[4:5]], 
             lapply(funsDv, dvapply, obj = tdr, dvs = dvs$Diving),
             lapply(funsAsc, dvapply, obj = tdr, dvs = dvs$Diving, type = 'ascent'),
             lapply(funsBtt, dvapply, obj = tdr, dvs = dvs$Diving, type = 'bottom'),
             lapply(funsDsc, dvapply, obj = tdr, dvs = dvs$Diving, type = 'descent'))
  stats$Surf.dur.aft <- dvs$Surface$Dive.dur[-1]
  stats$Time.start <- as.POSIXct(stats$Time.start, origin = '1970-01-01', tz = 'UTC')
  out <- as.data.frame(lapply(stats, replaceMissing))
  class(out) <- c('statdives', 'data.frame')
  out
}