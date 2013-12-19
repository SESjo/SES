summary.tdr <- function(x, na.rm=TRUE){
	
	funs <- c(min=min, mean=mean, median=median, max=max)
	
	fun.double <- function(x){
		lapply(funs, function(f) f(x=x, na.rm=na.rm))
	}
	cond <- where(x, is.double) & !sapply(myses$tdr, inherits, what="POSIXt")
	out.double <- matrix(unlist(lapply(x[ , cond], fun.double)), nrow=length(funs))
	class(out.double) <- "table"
	attr(out.double, which="dim") <- c(length(funs), sum(cond))
	attr(out.double, which="dimnames") <- list(names(funs), names(x)[cond])
	
	if (any(grepl("Dive.id", names(x)))) {out.dv <- max(x$Dive.id)}

	out <- list(nDives=out.dv, Time=range(x$Time), 
				Reso=difftime(x$Time[2], x$Time[1], units="sec"),
				Vars=out.double)
	return(out)
}

summary.statdives <- function(x, na.rm=TRUE){
	
}

summary.ses <- function(x, na.rm=TRUE) {

	out <-  list(Ind.id=x$Ind.id)
	if (nrow(x$tdr) > 0){out <- c(out, summary(x$tdr))}
	print(out)
}