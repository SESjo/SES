#' Apply a function to each dive/surface/bottom/... of a TDR dataset
#' 
#' \code{dvapply} is a functional programing utility designed to easily apply functions 
#' to the most interesting parts of TDR datasets. It is based on a call to 
#' \code{\link{mapply}}. The use of Apply-familly functions is generally encouraged
#' since it is faster than loops and  helps to make clearly readable code.
#' 
#' @param FUN Function to apply, found via \code{\link{match.fun}}. The function has 
#' to be written considering that it's first argument will be a subset of the TDR data 
#' (all columns but only the rows indicated by the \code{type} argument). 
#' See in the example section how it can be used to get Max depth.
#' @param OBJ A 'tdr' or 'ses' object
#' @param DVS Optional. A table with dives/surfaces/bottoms indices as returned by 
#' \code{\link{divesID}} or \code{\link{anaDives}}. If not given then computed within 
#' \code{dvapply}.
#' @param SIMPLIFY Logical or character string; attempt to reduce the result to a vector, 
#' matrix or higher dimensional array; see the simplify argument of \code{\link{sapply}}.
#' @param TYPE The periods involved: to choose in : \code{'dv'}, dives; \code{'sf'}, surfaces. 
#' \code{'all'}, both; \code{'dus'}, dives Union surfaces i.e. dives and their 
#' following surface period; \code{'btt'}, bottoms; \code{'asc'}, ascents; 
#' \code{'dsc'}, descents.
#' @param N Optional. The number(s) of the period(s) involved in when applying \code{FUN} 
#' on each one is not nessessary.
#' @param IDX Optional. In the case where none of the \code{TYPE} options matches with your needs, 
#' you may provide your own subset indices by using this argument. \code{IDX} must be a 
#' data frame or a matrix with start indices in the first column and end indices in the second.
#' @param ALONG Optional. A list of arguments of \code{FUN} whose values depend 
#' on the period involved. Arguments are recycled if necessary. See the last examples.
#' @param ... Other arguments to be passed to \code{FUN} shared by all the periods.
#' 
#' @details The names of the argument in this function are in upper case in order 
#' to avoid that the names of \code{dvapply} arguments match with those of 
#' \code{FUN}. The output elements are named after the \code{TYPE} argument and 
#' their number. If \code{IDX} is given in input then \code{"cst"} (custom) is used.
#' 
#' Some good explanations of Apply-familly functions on Stackoverflow.com: \url{http://stackoverflow.com/questions/3505701/r-grouping-functions-sapply-vs-lapply-vs-apply-vs-tapply-vs-by-vs-aggrega/7141669#7141669}
#' @export
#' @examples
#' path <- system.file("extdata", package="SES")
#' pathname <- file.path(path, "2011-16_SES_example_accelero.mat")
#' ses <- importSES(pathname)
#' 
#' # Most basic usage example: Get max. depth of each dive
#' Dmax <- function(x) max(x$Depth)
#' dvDmax <- dvapply(Dmax, ses)
#' 
#' # "N" argument: run FUN for some periods only
#' # Get max. depth of dives #50 to #55 only
#' identical(dvapply(Dmax, ses, N = 50:55), dvDmax[50:55])
#' 
#' # "..." argument: provide more arguments
#' # If dataset contains missing values, we can use the na.rm argument
#' dvapply(function(x, na.rm) max(x$Depth, na.rm), ses, N = 50:55, na.rm = TRUE) 
#' dvapply(function(x, ...) max(x$Depth, ...), ses, N = 50:55, na.rm = TRUE) 
#' 
#' # "SIMPLIFY" argument: when FUN returns several values
#' # Here we compute for each surface period Min., 1st Qu., Median, 3rd Qu., Max. of the depth.
#' dvapply(function(x) fivenum(x$Depth), ses, TYPE = 'sf', SIMPLIFY = FALSE, N = 50:55)
#' 
#' # "IDX" argument: provide custom indices
#' # For some reason I need the max depth between lines (1 to 10) and (10000 to 20000)
#' periods <- data.frame(start = c(1,10000), end = c(10, 20000))
#' dvapply(Dmax, ses, IDX = periods)
#' dvapply(Dmax, ses, IDX = periods, N = 2)
#' 
#' # "ALONG" argument: vectorize over periods along with other FUN's agument(s)
#' all(dvapply(function(x, y) max(x$Depth) / y, ses, ALONG = list(y = dvDmax)) == 1)
#' # notice the difference with "..." arguments that are constant along periods
#' all(dvapply(function(x, y, z) max(x$Depth) / y * z, ses, ALONG = list(y = dvDmax), z = 2) == 2)
#' # this is also valid because "ALONG" arguments are recycled if necessary.
#' all(dvapply(function(x, y, z) max(x$Depth) / y * z, ses, ALONG = list(y = dvDmax, z = 2)) == 2)
#' # Throw a warning because length(IDX) %% length(z) != 0 but still works.
#' table(dvapply(function(x, y, z) max(x$Depth) / y * z, ses, ALONG = list(y = dvDmax, z = 2:3)))
dvapply <- function (FUN, OBJ, DVS, SIMPLIFY = TRUE,
                     TYPE = c("dv", "sf", "all", "dus", "btt", "asc", "dsc"),
                     N = NULL, IDX = NULL, ALONG = NULL, ...) {
  
  if (missing(DVS))
    DVS <- switch(match.arg(TYPE), btt = anaDives(OBJ),
                  asc = anaDives(OBJ), dsc = anaDives(OBJ),
                  divesID(OBJ))
  DVS$type <- DVS$type == "Diving"
  if (is.ses(OBJ))
    OBJ <- OBJ$tdr
  
  # TYPE = 'dus' implies additional precautions
  deletedDv <- FALSE
  if (match.arg(TYPE) == 'dus'){
    if (DVS$type[1] == 'Surface') DVS <- DVS[-1, ]
    if (DVS$type[nrow(DVS)] == 'Diving') DVS <- DVS[-nrow(DVS), ] ; deletedDv <- TRUE
  }
  
  # Get the list of indices
  TYPE <- ifelse(is.null(IDX), match.arg(TYPE), "cst")
  IDX <- switch(TYPE, cst = IDX, btt = DVS[DVS$type, 6:7],
                dv = DVS[DVS$type, 1:2], sf = DVS[!DVS$type, 1:2], all = DVS[, 1:2],
                asc = DVS[DVS$type, c(7, 2)], dsc = DVS[DVS$type, c(1, 6)],
                dus = data.frame(DVS[DVS$type, 1], DVS[!DVS$type, 2]))
  numbers <- N %else% seq_along(IDX[ , 1])
  IDX <- IDX[numbers, ]
  
  # Make the work
  .f <- function(st, ed, ...) 
    FUN(OBJ[st:ed, ], ...)
  args <- c(list(FUN = .f, st = IDX[, 1], ed = IDX[ , 2], SIMPLIFY = SIMPLIFY), 
            if (is.null(ALONG)) c() else ALONG, 
            if (is.null(list(...))) c() else MoreArgs = list(...))
  out <- do.call(mapply, args)
  
  # Final formatting
  if (deletedDv & is.null(N)) {
    # Set result to NA when last dive is missing a surface period
    out <- c(out, do.call(ifelse(SIMPLIFY, c, list), list(NA)))
    numbers <- c(numbers, max(DVS$Dive.id) + 1)
  }
  
  names(out) <- paste0(TYPE, numbers)
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
