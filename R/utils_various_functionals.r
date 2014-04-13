#' Apply a function to each dive/surface/bottom/... of a TDR dataset
#' 
#' \code{dvapply} is a utility to apply functions to specific parts of a TDR dataset. 
#' This is faster than a loop and it helps to make readable code.
#' 
#' @param FUN Function to apply. The first argument has to be the TDR data 
#' subset (all columns but only the rows indicated by the \code{type} argument). 
#' See in the example section how it can be used to get Max depth.
#' @param OBJ A 'tdr' or 'ses' object
#' @param DVS Optional. A table with dives/surfaces/bottoms indices as returned by 
#' \code{\link{divesID}} or \code{\link{anaDives}}.
#' @param .type The periods involved: to choose in : \code{'dv'}, dives; \code{'sf'}, surfaces. 
#' \code{'all'}, both; \code{'dus'}, dives Union surfaces i.e. dives and their 
#' following surface period; \code{'btt'}, bottoms; \code{'asc'}, ascents; 
#' \code{'dsc'}, descents.
#' @param .ply The apply function to use: \code{s = sapply, l = lapply}. 
#' @param .numb Optional. The number(s) of the period(s) involved in when applying \code{FUN} 
#' on each one is not nessessary.
#' @param .idx Optional. In the case where none of the \code{.type} options matches your needs 
#' you may provide your own subset indices by using this argument. \code{.idx} must be a 
#' data frame with start indices in the first column and end indices in the second.
#' @param ... Other arguments to be passed to \code{FUN}.
#' @details The names of the argument in this function are in upper case (data) or 
#' preceded by a dot (options) in order to avoid that the names of \code{dvapply} 
#' arguments match with those of \code{FUN}. The output elements are named after 
#' the \code{.type} argument and their number (if \code{.idx} is given in input then 
#' \code{"cst"} (custom) is used).
#' @export
#' @examples
#' path <- system.file("extdata", package="SES")
#' pathname <- file.path(path, "2011-16_SES_example_accelero.mat")
#' ses <- importSES(pathname)
#' 
#' Dmax <- function(x) max(x$Depth)
#' dvDmax <- dvapply(Dmax, ses)  # Get max. depth of each dive
#' dvDmax2 <- dvapply(Dmax, ses, .n = 50:60) # dives #50 to #60 only
#' # identical(dvDmax2, dvDmax[50:60])  # TRUE
#' 
#' # Compute "boxplot statitics" of each surface period
#' # FUN returns several values so .ply = 'l'
#' sfDstat <- dvapply(function(x) fivenum(x$Depth), ses, .type = 'sf', .ply = 'l')
#' 
#' # Provide your own indices
#' dvapply(Dmax, ses, .idx = data.frame(c(1,10000), c(10, 20000)))
#' dvapply(Dmax, ses, .idx = data.frame(c(1,10000), c(10, 20000)), .n = 2)
dvapply <- function (FUN, OBJ, DVS, 
					 .type = c("dv", "sf", "all", "dus", "btt", "asc", "dsc"), 
					 .ply = c('s', 'l'), .numb = NULL, .idx = NULL, ...) {
	
	if (missing(DVS))
		DVS <- switch(match.arg(.type), btt = anaDives(OBJ), 
					  asc = anaDives(OBJ), dsc = anaDives(OBJ), 
					  divesID(OBJ))
	DVS$type <- DVS$type == "Diving"
	if (is.ses(OBJ))
		OBJ <- OBJ$tdr
	
	# .type = 'dus' implies additional precautions
	deletedDv <- FALSE
	if (match.arg(.type) == 'dus'){
		if (DVS$type[1] == 'Surface') DVS <- DVS[-1, ]
		if (DVS$type[nrow(DVS)] == 'Diving') DVS <- DVS[-nrow(DVS), ] ; deletedDv <- TRUE
	}
	
	# Get the list of indices
	.type <- ifelse(is.null(.idx), match.arg(.type), "cst")
	.idx <- switch(.type, cst = .idx, btt = DVS[DVS$type, 6:7], 
				   dv = DVS[DVS$type, 1:2], sf = DVS[!DVS$type, 1:2], all = DVS[, 1:2], 
				   asc = DVS[DVS$type, c(7, 2)], dsc = DVS[DVS$type, c(1, 6)],
				   dus = data.frame(DVS[DVS$type, 1], DVS[!DVS$type, 2]))
	numbers <- .numb %else% seq_along(.idx[ , 1])
	.idx <- .idx[numbers, ]
	
	# Make the work
	.f <- function(ii, ...) {
		out <- if (nNA(.idx[ii, ])) {NA} else {FUN(OBJ[.idx[ii, 1]:.idx[ii, 2], ], ...)}
		out %else% NA
	}
	out <- do.call(switch(match.arg(.ply), s = sapply, l = lapply),
				   list(seq_along(.idx[, 1]), match.fun(.f), ...))
	
	# Final formatting
	if (deletedDv & is.null(.numb)) {
		# Set result to NA when last dive is missing a surface period
		out <- c(out, do.call(switch(match.arg(.ply), l = list, c), list(NA)))
		numbers <- c(numbers, max(DVS$Dive.id) + 1)
	}
	
	names(out) <- paste0(.type, numbers) 
	out
} 


#' Function composition
#' 
#' @param ... The functions to compose.
#' @param funs A list of the functions to compose to use instead of the list 
#' of arguments.
#' @return \code{compose} returns the composed of the functions listed in/as arguments.
#' @details The order of arguments is important. \code{compose(h, g, f)}; \code{function(...) h(g(f(...)))} and \code{h \%.\% g \%.\% f} are equivalent.
#' @export
#' @keywords internal
#' @examples
#' x <- c(1:3, NA, 3:1)
#' compose(any, is.na)(x) 
#' # compose(funs = list(any, is.na))(x)      # The same
#' # compose(`!`, all, `!`, is.na)(x)         # The same
#' # (any %.% is.na)(x)                       # The same
#' compose(length, unique)(x)
#' compose(mean, na.omit)(x)        # mean(x= , na.rm=T) or partial(mean, na.rm=T)
#' compose(round, rnorm)(1000, 0, 1) -> x -> y
#' compose(all, `==`)(x, y)
#' (rep %.% per)(c(1:3, 3:2, 2)) 
compose <- function (..., funs){
	if (missing(funs)) funs <- list(...)
	funs <- lapply(funs, match.fun) ; n <- length(funs)
	out <- if (n == 1) {
		function(...) funs[[n]](...)
	} else {
		function(...) funs[[n - 1]](funs[[n]](...))
	}
	if (n > 2) out <- do.call(compose, c(funs[1:(n - 2)], list(out)))
	out
}

#' @rdname compose
#' @param f,g Two functions to compose for the infix form
#' @return \%.\% is a binary operator version of the compose function.
#' @keywords internal
#' @export
`%.%` <- function(g, f) compose(g, f)

#' Evaluate an expression in a specified environment
#' 
#' \code{let} attaches symbols to a values and evaluate an expression with 
#' respect to these.
#'  
#' @param .expr The expression to evaluate.
#' @param ... The context in which to do it.
#' @keywords internal
#' @export
#' @examples
#' let(a = 1, .expr = a + 1)# More intuitive syntax
#' let(a + b, a = 1, b = 2) # Without naming the expression...
let <- function (.expr, ...){
  eval(substitute(.expr), list2env(list(...), parent = parent.frame()))
}

#' Set function arguments to new defaults
#' 
#' @param FUN The function implied
#' @param ... Arguments to set
#' @keywords internal
#' @export
#' @examples
#' x <- c(1:3, NA, 3:1)
#' partial(mean, na.rm=TRUE)(x)
partial <- function (FUN, ...){
  let(ellipsis = list(...), 
      function(...) do.call(FUN, c(list(...), ellipsis)))
}
