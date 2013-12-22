#' SESname
#' 
#' Extract the ID of an individual from a string that contains it.
#' 
#' @param text Text containing the seal ID.
#' @return TheiID as character
#' @family generalUtils
#' @export
#' @examples 
#' SESname("Path/to/seal/file/2011-12_some_ses_file.txt")
SESname <- function(text){regmatches(text, regexpr('20+[0-9]{2}-[0-9]{2}', text))}

#' replaceMissing
#' 
#' Replace values in an atomic vector.
#' 
#' @param x The atomic vector
#' @param na.0 The value to be replaced. Default is NaN.
#' @param na.1 The replacement. Default is NA.
#' @family generalUtils
#' @export
#' @examples
#' x <- sample(c(1:3,NaN), 20, replace=TRUE)
#' x 
#' replaceMissing(x)
replaceMissing <- function(x, na.0=NaN, na.1=NA) {
  if (is.nan(na.0)) x[is.nan(x)] <- na.1
  else if (is.na(na.0)) x[is.na(x)] <- na.1
  else x[is.na(x)] <- na.1
  x
}

#' nval
#'
#' Count the number of distinct values in an atomic vector.
#' 
#' @param x The object to compute the number of distinct values.
#' @family generalUtils
#' @export
#' @examples
#' nval(rep(1:5, 5:1)) # 5
nval <- function(x){sum(!duplicated(x))}

#' depth
#' 
#' Depth of an R object. See plotrix::maxDepth().
#' 
#' @param x The object to analyse.
#' @export
#' @keywords internal
depth <- function (x) {
	if (is.list(x)) {
		maxdepth <- 1
		for (lindex in 1:length(x)) {
			newdepth <- listDepth(x[[lindex]]) + 1
			if (newdepth > maxdepth) 
				maxdepth <- newdepth
		}
	}
	else maxdepth <- 0
	return(maxdepth)
}

#' nstr
#' 
#' Recursive extraction of names (such as \code{names(c(x, recursive=TRUE))}) but
#' stops when a subelement is atomic.
#' 
#' @param x The object to analyse.
#' @export
#' @keywords internal
#' @examples
#' x <- data.frame(X=1:10, Y=10:1)
#' names(c(x, recursive=TRUE))
#' nstr(x)
nstr <- function(x) {
	n <- depth(x)
	name.vec <- c()
	if (n == 1){
		return(names(x))
	} else if (n > 1){
		for (i in seq_along(x)){
			name.vec <- c(name.vec,
						  names(x), 
						  paste(names(x)[i], nstr(x[[i]]), sep='.'))
		}
	}
	return(unique(name.vec[!grepl('\\.$', name.vec)]))
}