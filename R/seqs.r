#' seqs
#' @description Identify the homogenious sequences along an atomic vector.
#' @param vec The atomic vector to examine.
#' @return A data frame with indexing sequences with start and end index. Value and length of the sequences are also given
#' @author Yves
#' @export
#' @examples
#' x <- unname(unlist(mapply(rep, letters[1:10], 10:1)))
#' x
#' seqs(x)
seqs <- function(vec) {
	vec.org <- vec
	if(is.logical(vec)){vec <- as.numeric(vec)
	}else if(is.character(vec)){vec <- as.numeric(as.factor(vec))
	}else if(is.factor(vec)){vec <- as.numeric(vec)}
	chg <- diff(vec, lag=1)
	end <- c(which(chg != 0), length(vec))
	start <- c(1, end[-length(end)] + 1)
	value <- vec.org[start]
	lgth <- end - start + 1
	return(data.frame(start=start, end=end, value=value, length=lgth))
}