#' Extract the ID of an individual from a string
#' 
#' Extract the ID of an individual from a string that contains it. This function
#' just apply a the following regular expression \code{'20+[0-9]{2}-[0-9]{2}'}
#' 
#' @param text Text containing the seal ID.
#' @return TheiID as character
#' @family generalUtils
#' @export
#' @examples 
#' SESname("Path/to/seal/file/2011-12_some_ses_file.txt")
SESname <- function(text)
  grepo('20+[0-9]{2}-[0-9]{2}', text)

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

#' Count the number of NAs in a vector
#' 
#' Shortcut for \code{compose(sum, is.na, unlist)}
#' 
#' @param x a vector whose elements are to be tested.
#' @return Return the number of \code{NA} in \code{x}.
#' @details As any number different from 0 return a \code{TRUE} when coerced to
#' logical, this function can be used in \code{if} statements.
#' @export
#' @keywords internal
#' @examples
#' x <- c(rep(NA, 3), 1:3)
#' nNA(x)
#' if (nNA(x)) {TRUE} else {FALSE}
#' if (nNA(1:3)) {TRUE} else {FALSE}
nNA <- function(x)
  compose(sum, is.na, unlist)(x) 

#' Count the number of unique values in a vector
#' 
#' Shortcut for \code{compose(length, unique)}. Count the number of distinct 
#' values in an atomic vector.
#' 
#' @param x a vector whose unique elements are to be counted.
#' @export
#' @keywords internal
#' @examples
#' nUN(rep(1:5, 5:1)) # 5
nUN <- function(x)
  compose(length, unique)(x)

#' Else special operator
#' 
#' Discard first value if \code{FALSE}, \code{NULL}, empty or \code{"try-error"}
#' 
#' @param a default output.
#' @param b output if \code{a} is \code{FALSE}, \code{NULL} or empty.
#' @export
#' @keywords internal
#' @examples
#' "abc" %else% "Another value is returned"
#' NULL %else% "Another value is returned"
#' try(log("abc"), silent = TRUE) %else% "Another value is returned"
`%else%` <- function (a, b){
  if (identical(a, FALSE) || is.null(a) || length(a) == 0 || inherits(a, "try-error")) b else a
}

#' Special options of unix grep
#' 
#' \code{grepo} is similar to bash's \code{grep -o pattern file} it returns the matches. 
#'
#' @inheritParams base::grep
#' @param ... Other arguments to be passed to \code{regexpr}.
#' @export
#' @keywords internal
grepo <- function(pattern, text, ...) 
  regmatches(text, regexpr(pattern, text, ...))

#' @rdname grepo
#' @details \code{grepc} is similar to bash's \code{grep -c pattern file} it counts the number of matches.
#' @export
#' @keywords internal
grepc <- function(pattern, text, ...) 
  compose(lenght, grepo)(pattern, text, ...) 

#' Compute the average length of a sequences of successively repeated values
#' 
#' @param x a vector in which to search for sequences of successively 
#' repeated values.
#' @param ref A reference value to use when counting a specific event only.
#' @param cmp The comparison operator to use for comparison with the reference.
#' @details The default is to return the overall average length. If \code{ref} 
#' is given but not \code{cmp} then \code{cmp = '!='}. If \code{cmp} 
#' is given but not \code{ref} then \code{ref} is chosen to \code{FALSE}, 
#' \code{0L} or to the first value (decreasing order) of \code{x}.
#' @export
#' @keywords internal
#' @examples
#' meanL(rep(letters[1:2], 1:2))    # Average length of all sequences
#' x <- rep(letters[sequence(c(2, 2))], c(1, 2, 9, 1))
#' meanL(x, ref = 'a', cmp = '==')  # Average length of 'a' sequences, 5
#' meanL(x, ref = 'b', cmp = '!=')  # Average length of 'a' sequences, 5
#' meanL(x, ref = 'b', cmp = '==')  # Average length of 'b' sequences, 1.5
meanL <- function(x, ref = NULL, cmp = NULL){
  seqs <- per(x)
  if (!is.null(cmp) && is.null(ref))
    ref <- switch(typeof(x), logical = FALSE, integer = 0, sort(seqs$value)[1])
  if (!is.null(ref) && is.null(cmp)){cmp <- '!='}
  cond <- if(is.null(cmp)){rep(TRUE,nrow(seqs))}else{match.fun(cmp)(seqs$value,ref)}
  if (length(seqs$value[cond]) == 0) {
    return(0)
  } else {
    return(mean(seqs$length[cond]) %else% NA)
  }
}

#' Depth of an R object
#' 
#' See plotrix::maxDepth().
#' 
#' @param x The object to analyse.
#' @export
#' @keywords internal
depth <- function (x) {
	if (is.list(x)) {
		maxdepth <- 1
		for (lindex in 1:length(x)) {
			newdepth <- depth(x[[lindex]]) + 1
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

#' Convert the result of dive classification into a logical
#' 
#' Convert the result of dive classification into a logical that differentiate 
#' the drift dives from the other types.
#' 
#' @param x The object to convert.
#' @export
#' @keywords internal
#' @examples
#' \dontrun{
#' str(class2logical(ses$stat$Dive.type))
#' }
class2logical <- function(x) x == 1

#' Paste operator
#'
#' @param a,b Elements to paste
#' @export
#' @keywords internal
%+% <- function(a, b) paste0(a, b)
