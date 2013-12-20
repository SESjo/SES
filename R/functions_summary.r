#' summary.ses
#' @description S3 method for 'ses' objects.
#' @param object An object for which a summary is desired.
#' @param na.rm A logical value indicating whether \code{NA} values should be stripped before the computation proceeds.
#' @S3method summary ses
summary.ses <- function(object, na.rm=TRUE){
  ans <-  findVar(object, "Ind.id")$var
  ans <- if (nrow(object$tdr) > 0){c(ans, summary(object$tdr))}
  print(ans)
}

#' summary.tdr
#' @description S3 method for 'tdr' objects.
#' @inheritParams summary.ses
#' @S3method summary tdr
summary.tdr <- function(object, na.rm=TRUE){
  cond <- vapply(object, is.double, logical(1)) & !vapply(object, inherits, logical(1), what="POSIXt")
  obj <- object[ , cond]
  funs <- c(min=min, mean=mean, median=median, max=max)
  summary.funs <- function(object){lapply(funs, function(f) f(x=object, na.rm=na.rm))}
  ans.double <- matrix(unlist(lapply(object[ , cond], summary.funs)), nrow=length(funs))
  class(ans.double) <- "table"
  attr(ans.double, which="dim") <- c(length(funs), sum(cond))
  attr(ans.double, which="dimnames") <- list(names(funs), names(object)[cond])
  
  if (any(grepl("Dive.id", names(object)))) {ans.dv <- max(object$Dive.id)}
  
  ans <- list(nDives=ans.dv, Time=range(object$Time), 
              Reso=difftime(object$Time[2], object$Time[1], units="sec"),
              Vars=ans.double)
  return(ans)
}

#' summary.statdives
#' @description S3 method for 'statdives' objects.
#' @inheritParams summary.ses
#' @S3method summary statdives
summary.statdives <- function(object, na.rm=TRUE){}
