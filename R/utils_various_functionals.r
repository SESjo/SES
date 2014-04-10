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
