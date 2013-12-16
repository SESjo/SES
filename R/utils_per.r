#' per
#' 
#' The reverse of 'base::rep()' function: decompose an atomic vector to its successive 
#' values and their length.
#' 
#' @param x The atomic vector to examine.
#' @param idx Should the indexes (start and end) of homogeneous sequences be returned as well ?
#' 
#' @return A data frame with values and lengths of the homogeneous sequences of x. The class of the 
#' column 'value' is copied from the input.
#'  
#' @family generalUtils
#' @export
#' @examples
#' (x <- rep(LETTERS[1:10], 10:1))
#' (y <- per(x))
#' # 'per()' is the reverse of 'rep()'
#' # identical(rep(y$value, y$length), x)   # TRUE
#' # Because characters are not converted to factors
#' # inherits(y$value, class(x))            # TRUE
per <- function(x, idx=FALSE) {
	# Save original object
	x.org <- x
	# Concert x to numeric
	if      (is.logical(x))   {x <- as.numeric(x)}
	else if (is.factor(x))    {x <- as.numeric(x)}
	else if (is.character(x)) {x <- as.numeric(as.factor(x))}
	# Compute and return
	chg <- diff(x, lag=1)
	end <- c(which(chg != 0), length(x)) ; start <- c(1, end[-length(end)] + 1)
	if (idx) return(data.frame(st.idx=start, ed.idx=end, value=x.org[start], length=end - start + 1, stringsAsFactors=FALSE))
	else return(data.frame(value=x.org[start], length=end - start + 1, stringsAsFactors=FALSE))
}