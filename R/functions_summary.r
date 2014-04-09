#' Summarizing SES dataset
#' 
#' @description S3 method for 'ses' objects.
#' @param object An object for which a summary is desired.
#' @param na.rm A logical value indicating whether \code{NA} values should be 
#' stripped before the computation proceeds.
#' @param all Should the function return results with both os 'statdives' ans 'tdr'
#' objects
#' @param digits The number of digits to show in dive statistics.
#' @export
summary.ses <- function(object, na.rm=TRUE, all=FALSE, digits=2){
	ans <-  findVar(object, "Ind.id")$var
	if (!all){
		if (nrow(object$stat) > 0) {
			ans <- c(ans, Dive_stats=summary(object$stat, na.rm))
		} else {
			if (nrow(object$tdr) > 0) {
				ans <- c(ans, Dive_stats=summary(object$tdr, na.rm, complete=FALSE, digits))
			}
		}
	} else {
		ans <- c(ans, Dive_stats=list(stat=summary(object$stat, na.rm, digits), 
									  tdr=summary(object$tdr, na.rm, complete=TRUE, digits)))
	}
	ans
}

#' @rdname summary.ses
#' @inheritParams summary.ses
#' @param complete Should the function compute all statistics.
#' @export
summary.tdr <- function(object, na.rm=TRUE, complete=TRUE, digits=2){
	findDefaultVars("Dive.id", object, type.obj="tdr", ignore.depth.error=TRUE)
	dvs <-  Dive.id[Dive.id != 0]
	types <- sapply(object, typeof)
	ans <- sapply(unique(types), assign, value=NULL)
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
				x <- object[Dive.id != 0, cond, drop=FALSE]
				ans[[type]] <- sapply(names(x), assign, value=NULL)
				for (i in seq_len(sum(cond))){
					xx <- x[[i]]
					funs <- statFuns(type)
					for (ii in seq_along(funs)){
						ans[[type]][[names(x)[i]]][[names(funs)[ii]]] <- do.call('tapply', c(list(X=xx, INDEX=dvs, FUN=funs[[ii]]), MoreArgs))
					}
					ans[[type]][[names(x)[i]]] <- sapply(ans[[type]][[names(x)[i]]], mean)
				}
				ans[[type]] <- round(as.data.frame(ans[[type]]), digits)
			}
		}
	}
	findDefaultVars("Time", object, type.obj="tdr")
	ans <- c(list(nDives=nUN(dvs), Travel_Time=range(Time), 
				  Reso=round(difftime(Time[length(Time)], object$Time[1], units="sec") / length(Time))),
			 ans)
	ans
}

#' @rdname summary.ses
#' @inheritParams summary.tdr
#' @export
summary.statdives <- function(object, na.rm=TRUE, digits=2){
	findDefaultVars("Dive.id", object, type.obj="tdr", ignore.depth.error=TRUE)
	types <- sapply(object, typeof)
	ans <- sapply(unique(types), assign, value=NULL)
	for (type in unique(types)){
		if (type == 'double'){
			cond <- vapply(object, is.double, logical(1)) & !vapply(object, inherits, logical(1), what="POSIXt")
			MoreArgs <- list(na.rm=na.rm)
		} else {
			cond <- types %in% type
			MoreArgs <- list()	
		}
		if (any(cond)){
			x <- object[Dive.id != 0, cond, drop=FALSE]
			ans[[type]] <- sapply(names(x), assign, value=NULL)
			for (i in seq_len(sum(cond))){
				ans[[type]][[names(x)[i]]] <- sapply(statFuns(type), 
                                   function(f) do.call(f, c(list(x[[i]]), MoreArgs)))
			}
			ans[[type]] <- round(as.data.frame(ans[[type]]), digits)
		}
	}
	ans
}

#' Apply summary statistics functions given a type of vector
#' 
#' @param type The type of computation. To choose in 
#' \code{c("double", "integer", "logical", "factor", "character")}.
#' @param x Alternatively, the vector can be given instead.
#' @return A list of functions to use in order to computre summary statistics.
#' @export
#' @keywords internal
statFuns <- function(type, x){
	funs <- switch(type %else% typeof(x),
				   double  = list(Min=min, Mean=mean, Median=median, Max=max),
				   integer = list(Number=nUN, Seq_length = partial(meanL, ref = 0)),
				   logical = list(Prop=mean, True_seq_length = partial(meanL, ref = FALSE)),
				   factor  = list(Table=table),
				   character = list(Table=table),
           stop('Unknown summary statistics functions for this type of data.'))
	return(funs)
}