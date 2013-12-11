\name{findVars}
\alias{findVars}
\title{findVars}
\usage{
  findVars(vars, object, varnames = NULL, recursive = TRUE,
    ...)
}
\arguments{
  \item{vars}{The list of variables to search and to create
  in the current environment.}

  \item{object}{The object supposed to contain the
  variables.}

  \item{varnames}{An Optional atomic vector of type
  'character'. The names to give to the variables found
  when assigning them to the current environment. Default
  is the same as \code{vars}.}

  \item{recursive}{Should the object (if it is a list) be
  investigated on its first depth level (\code{FALSE}) or
  until its maximun depth instead (\code{TRUE}).}

  \item{...}{Other arguments to be passed to \code{grepl}.
  Set \code{fixed} to \code{TRUE} if \code{vars} is to be
  matched as is (not a regular expression).}
}
\description{
  Search and get variables in an object using its name. To
  use for error handling and initial checking in functions.
}
\examples{
x <- data.frame(A=1:3, b=c(TRUE, FALSE, TRUE), C=letters[1:3], D=LETTERS[1:3])
l <- list(A=1:3, b=list(C=c(TRUE, FALSE, TRUE), D=letters[1:3]))
fun <- function(x) {
vars <- c("A", "B", "c", "D")
findVars(vars, x)
mget(vars)
}
str(fun(x))
str(fun(l))
\dontrun{
# Check the output / warnings / errors
x <- data.frame(A=1:3, B=c(TRUE, FALSE, TRUE), c=letters[1:3], C=LETTERS[1:3])
l <- list(A=1:3, B=list(cD=c(TRUE, FALSE, TRUE), D=letters[1:3]))
str(fun(x))
str(fun(l))
}
}
\author{
  Yves
}
\seealso{
  \code{\link{existsVars}}, \code{\link{findVarlist}}
}
\keyword{internal}

