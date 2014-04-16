#' Find the dives and delimitate their bottom from TDR data
#' 
#' Method translated from Matlab.
#' 
#' @param obj A 'ses' or 'tdr' object.
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
#' dvs <- anaDives(ses)       # Use dive statistics information for dive delim
#' dvs <- anaDives(ses$tdr)   # Recompute dive delim from TDR data.
#' randomDv(ses$tdr, dvs)
anaDives <- function(obj, dpthThres = 15, durThres = 300, 
                     spdThres = .75, w = 12) {
  if (is.ses(obj)){
    bottomDelim(obj$tdr, divesID(obj))
  } else if (is.tdr(obj)) {
    bottomDelim(obj, divesID(obj))
  } else {
    stop('Object class is inapropriate')
  }
}


#' Find the dives start and end indices
#' 
#' @inheritParams anaDives
#' @return A data frame with the following variable: indice of period start, 
#' indice of period end, type of period, duration (s), the dive number.
#' @seealso \code{\link{bottomDelim}}, \code{\link{anaDives}}
#' @export
divesID <- function(obj, ...){
  UseMethod('divesID')
}

#' @rdname divesID
#' @param reso Time resolution to use when converting the dive duration into a 
#' number of sensor recordings.
#' @export
divesID.statdives <- function(obj, reso = 1){
  n <- nrow(obj)
  dives <- data.frame(st.idx = obj$Start.idx,
                      ed.idx = obj$Start.idx + round(obj$Dive.dur), 
                      type = rep('Diving', n), Dive.dur = round(obj$Dive.dur),
                      Dive.id = obj$Dive.id, Period.id = seq(1, 2*n)[(seq(1, 2*n)%%2)!=0])
  surfaces <- data.frame(st.idx = obj$Start.idx[-n] + round(obj$Dive.dur[-n]) + 1, 
                         ed.idx = obj$Start.idx[-1] - 1, type = rep('Surface', n-1),
                         Dive.dur = obj$Start.idx[-1] - obj$Start.idx[-n] - 
                           round(obj$Dive.dur[-n]),
                         Dive.id = rep(0, n-1), Period.id = seq(1, 2*n-1)[(seq(1, 2*n-1)%%2)==0])
  dvs <- rbind(dives, surfaces)
  dvs$type <- as.character(dvs$type)
  rks <- order(dvs$Period.id)
  as.data.frame(lapply(dvs[, -6], function(x) x[rks]))
}

#' @rdname divesID
#' @export
divesID.ses <- function(obj){
  divesID(obj$stat, timeReso(obj))
}

#' @rdname divesID
#' @export
divesID.tdr <- function(obj, durThres = 300, dpthThres = 15){
  
  findDefaultVars(c('Time', 'Depth'), obj, type.obj='tdr')
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
  if (dvs$value[nrow(dvs)] == 'Diving'){dvs <- dvs[-nrow(dvs), ]}
  names(dvs) <- c("st.idx", "ed.idx", "type", "Dive.dur", "Dive.id")
  dvs
}

#' Get the sampling rate of a TDR dataset
#' 
#' @param obj A 'ses' or 'tdr' object
#' @param n the number of replicates to do.
#' @param type Should the result be returned in seconds ('period') or hertz ('frequence') ?
#' @export
timeReso <- function(obj, n = 10, type = c('period', 'frequence')){
  obj <- if (is.ses(obj)) {
    obj$tdr$Time 
  } else {
    if (is.tdr(obj)) 
      obj$Time 
    else 
      stop('Object class is inapropriate')
  }
  iis <- sample(1:length(obj), n, replace = FALSE)
  dT <- (round %.% unlist %.% lapply)(Map(seq, iis, iis + 3), function(I) diff(obj[I]))
  reso  <- median(dT)
  if (is.na(reso) | !all(dT == reso))
    warning('Some time jumps were found. Check the dataset !')
  switch(match.arg(type), period = reso, frequence = 1 / reso)
}

#' Find the dives' bottom from TDR data
#' 
#' Method translated from Matlab.
#' 
#' @param obj A'tdr' object
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
bottomDelim <- function(obj, dvs, spdThres = .75, w = 12, bttDpth = .4) {
  
  findDefaultVars(c('Time', 'Depth'), obj, type.obj='tdr')
  dTime <- as.numeric(diff(Time))
  reso <- timeReso(obj)
  
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
  chkDel <- lapply(chkFuns, dvapply, OBJ = obj, DVS = dvs, TYPE = 'btt')
  chkDel <- Map(`|`, e1 = lapply(chkDel, `!`), e2 = lapply(chkDel, is.na))
  
  # Compute the old fashion delim: first and last depth >= bttDpth * Max depth
  corDel <- function(x, lt = 'e'){
    per(x$Depth >= bttDpth*max(x$Depth), idx = TRUE)[2, switch(lt, s = 1, e = nrow(x))]
  }
  cond <- dvs$type == 'Diving'
  bttSt <- dvapply(corDel, obj, dvs, lt = 's') + dvs$st.idx[cond] - 1
  bttEd <- dvapply(corDel, obj, dvs) + dvs$st.idx[cond] - 1
  
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
             lapply(funsDv, dvapply, OBJ = tdr, DVS = dvs$Diving),
             lapply(funsAsc, dvapply, OBJ = tdr, DVS = dvs$Diving, TYPE = 'asc'),
             lapply(funsBtt, dvapply, OBJ = tdr, DVS = dvs$Diving, TYPE = 'btt'),
             lapply(funsDsc, dvapply, OBJ = tdr, DVS = dvs$Diving, TYPE = 'dsc'))
  stats$Surf.dur.aft <- dvs$Surface$Dive.dur[-1]
  stats$Time.start <- as.POSIXct(stats$Time.start, origin = '1970-01-01', tz = 'UTC')
  out <- as.data.frame(lapply(stats, replaceMissing))
  class(out) <- c('statdives', 'data.frame')
  out
}