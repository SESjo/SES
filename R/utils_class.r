#' as.ses
#' 
#' Coerce an object to a 'ses' if possible
#' 
#' @param x The object to coerce to 'ses'
#' @param type The type of 'ses' desired. To choose in \code{c("classic", "3D")}.
#' @param ind The individual ID number. Ignored if x is given.
#' @param tdr The TDR data. Ignored if x is given.
#' @param stat The statdives data. Ignored if x is given.
#' @export
#' @examples
#' new.ses <- list(Ind.id="Example", tdr=data.frame(), stat=data.frame())
#' is.ses(new.ses)
#' new.ses <- as.ses(new.ses)
#' is.ses(new.ses)
as.ses <- function(x, type=c("classic", "3D"), ind, tdr, stat){
  if (missing(x)){
    sepCall <- as.list(match.call())[c("ind", "tdr", "stat")]
    sepCall.l <- vapply(sepCall, is.null, as.logical(1))
    if (any(sepCall.l)){
      stop(paste0(paste0(c("ind", "tdr", "stat")[sepCall.l], collapse=", "), " arguments must be provided if 'x' is empty."))
    }
    x <- list(Ind.id=ind, tdr=tdr, stat=stat)
  }
  UseMethod("as.ses")
}

#' as.ses.list
#' 
#' S3 method of \code{as.ses} for list objects.
#' 
#' @rdname as.ses
#' @inheritParams as.ses
#' @S3method as.ses list
as.ses.list <- function(x, type=c("classic", "3D")){
  findVars(vars=c("Ind.id", "tdr", "stat"), x, type="check", ignore.depth.error=TRUE)
  type <- match.arg(type)
  cl.vec <- switch(type,
                   classic = list(Ind.id = "character",
                                  tdr = c("tdr", "data.frame"),
                                  stat = c("statdives", "data.frame")),
                   '3D' = list(Ind.id = "character",
                               tdr = c("tdr3D", "tdr", "data.frame"),
                               stat = c("statdives3D", "statdives", "data.frame")))
  if (type == "3D"){class(x) <- c("ses3D", "ses", "list")}
  else {class(x) <- c("ses", "list")}
  for (i in seq_along(cl.vec)){
    class(x[[names(cl.vec)[i]]]) <- cl.vec[[i]]
  }
  x
}

#' is.ses
#' 
#' Test if an object belong to the 'ses' class
#' 
#' @param x The object
#' @export
#' @family is.thing
is.ses <- function(x){inherits(x, 'ses')}

#' is.SES
#' 
#' Test if an object belongs to the family of SES package object.
#' 
#' @rdname is.MAJses
#' @param x The object
#' @export
#' @family is.thing
is.SES <- function(x){inherits(x, c('ses', 'tdr', 'stat', 'stat3D', 'tdr3D'))}

#' is.tdr
#' 
#' Test if an object belong to the 'tdr' class
#' 
#' @param x The object
#' @export
#' @family is.thing
is.tdr <- function(x){inherits(x, 'tdr')}

#' is.tdr3D
#' 
#' Test if an object belong to the 'tdr3D' class
#' 
#' @param x The object
#' @export
#' @family is.thing
is.tdr3D <- function(x){inherits(x, 'tdr3D')}

#' is.stat
#' 
#' Test if an object belong to the 'stat' class
#' 
#' @param x The object
#' @export
#' @family is.thing
is.stat <- function(x){inherits(x, 'stat')}

#' is.stat3D
#' 
#' Test if an object belong to the 'stat3D' class
#' 
#' @param x The object
#' @export
#' @family is.thing
is.stat3D <- function(x){inherits(x, 'stat3D')}