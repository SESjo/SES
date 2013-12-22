#' summary.ses
#' @aliases summary.ses
#' @description S3 method for 'ses' objects.
#' @param object An object for which a summary is desired.
#' @param na.rm A logical value indicating whether \code{NA} values should be 
#' stripped before the computation proceeds.
#' @param all Should the function return results with both os 'statdives' ans 'tdr'
#' objects
#' @S3method summary ses
summary.ses <- function(object, na.rm=TRUE, all=FALSE){
	ans <-  findVar(object, "Ind.id")$var
	if (!all){
		if (nrow(object$stat) > 0) {
			ans <- c(ans, Dive_stats=summary(object$stat, na.rm))
		} else {
			if (nrow(object$tdr) > 0) {
				ans <- c(ans, Dive_stats=summary(object$tdr, na.rm, complete=FALSE))
			}
		}
	} else {
		ans <- c(ans, Dive_stats=list(stat=summary(object$stat, na.rm), 
									  tdr=summary(object$tdr, na.rm, complete=TRUE)))
	}
	print(ans)
}

#' summary.tdr
#' @aliases summary.ses
#' @description S3 method for 'tdr' objects.
#' @inheritParams summary.ses
#' @param complete Should the function compute all statistics.
#' @S3method summary tdr
summary.tdr <- function(object, na.rm=TRUE, complete=TRUE){
	findDefaultVars("Dive.id", object, type.obj="tdr", ignore.depth.error=TRUE)
	dvs <-  Dive.id[Dive.id != 0]
	types <- sapply(object, typeof)
	ans <- list()
	if (complete){
		for (type in unique(types)){
			if (type == 'double'){
				cond <- vapply(object, is.double, logical(1)) & !vapply(object, inherits, logical(1), what="POSIXt")
				MoreArgs <- list(na.rm=na.rm)
			} else {
				cond <- types %in% type
				MoreArgs <- list()	
			}
			if (any(cond)){
				ans[[type]] <- list()
				x <- object[Dive.id != 0, cond]
				for (i in seq_along(x)){
					xx <- x[[i]]
					ans[[type]][[names(x)[i]]] <- list()
					funs <- statFuns(type)
					for (ii in seq_along(funs)){
						ans[[type]][[names(x)[i]]][[names(funs)[ii]]] <- do.call('tapply', c(list(X=xx, INDEX=dvs, FUN=funs[[ii]]), MoreArgs))
					}
					ans[[type]][[names(x)[i]]] <- sapply(ans[[type]][[names(x)[i]]], mean)
				}
				ans[[type]] <- as.data.frame(ans[[type]])
			}
		}
	}
	findDefaultVars("Time", object, type.obj="tdr")
	ans <- c(list(nDives=nval(dvs), Travel_Time=range(Time), 
				  Reso=round(difftime(Time[length(Time)], object$Time[1], units="sec") / length(Time))),
			 ans)
	ans
}

#' summary.statdives
#' @aliases summary.ses
#' @description S3 method for 'statdives' objects.
#' @inheritParams summary.tdr
#' @S3method summary statdives
summary.statdives <- function(object, na.rm=TRUE){
	findDefaultVars("Dive.id", object, type.obj="tdr", ignore.depth.error=TRUE)
	types <- sapply(object, typeof)
	ans <- list()
	for (type in unique(types)){
		if (type == 'double'){
			cond <- vapply(object, is.double, logical(1)) & !vapply(object, inherits, logical(1), what="POSIXt")
			MoreArgs <- list(na.rm=na.rm)
		} else {
			cond <- types %in% type
			MoreArgs <- list()	
		}
		if (any(cond)){
			ans[[type]] <- list()
			x <- object[Dive.id != 0, cond]
			for (i in seq_along(x)){
				ans[[type]][[names(x)[i]]] <- sapply(statFuns(type), function(f) f(x[[i]]))
			}
			ans[[type]] <- as.data.frame(ans[[type]])
		}
	}
	ans
}

#' statFuns
#' 
#' Functions to apply to compute summary statistics given a type of vector.
#' 
#' @param type The type of computation. To choose in 
#' \code{c("double", "integer", "logical", "factor", "character")}.
#' @export
#' @keywords internal
statFuns <- function(type=c("double", "integer", "logical", "factor", "character")){
	funs <- switch(match.arg(type),
				   double = list(min=min, mean=mean, median=median, max=max),
				   integer = list(N=nval),
				   logical = list(prop=mean, seq_length=function(x){mean(per(x)$length)}),
				   factor = list(table=table),
				   character = list(table=table))
	return(funs)
}