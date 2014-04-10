#' Creates or tests for 'ses'.
#' 
#' @param x The object to coerce to 'ses'
#' @param type The type of 'ses' desired. To choose in \code{c("classic", "3D")}.
#' @param ind The individual ID number. Ignored if x is given.
#' @param tdr The TDR data. Ignored if x is given.
#' @param stat The statdives data. Ignored if x is given.
#' @details \code{as.ses} coerces an object to a 'ses'.
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


#' @rdname as.ses
#' @inheritParams as.ses
#' @export
#' @keywords internal
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

#' @rdname as.ses
#' @details \code{as.ses} tests if an object belong to the 'ses' class
#' @export
is.ses <- function(x){inherits(x, 'ses')}

#' @rdname as.ses
#' @details \code{as.SES} tests if an object belongs to the family of 
#' SES package object.
#' @export
is.SES <- function(x){inherits(x, c('ses', 'tdr', 'stat', 'stat3D', 'tdr3D'))}

#' @rdname as.ses
#' @details \code{is.tdr} Test if an object belong to the 'tdr' class
#' @export
is.tdr <- function(x){inherits(x, 'tdr')}

#' @rdname as.ses
#' @details \code{is.tdr3D} tests if an object belong to the 'tdr3D' class.
#' @export
is.tdr3D <- function(x){inherits(x, 'tdr3D')}

#' @rdname as.ses
#' @details \code{is.stat} tests if an object belong to the 'stat' class
#' @export
#' @family is.thing
is.stat <- function(x){inherits(x, 'stat')}

#' @rdname as.ses
#' @details \code{is.stat3D} tests if an object belong to the 'stat3D' class
#' @export
#' @family is.thing
is.stat3D <- function(x){inherits(x, 'stat3D')}