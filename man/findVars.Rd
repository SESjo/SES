\name{findVars}
\alias{findVars}
\title{findVars}
\usage{
  findVars(vars, obj, varnames = NULL, ...)
}
\arguments{
  \item{vars}{The list of variable to search and create in
  the current environment.}

  \item{obj}{The object in which to search and grab the
  values.}

  \item{varnames}{Optional. The names to give to the
  variables found if different from \code{vars}.}

  \item{...}{Arguments to be passed to
  \code{\link{assign}}.}
}
\description{
  Assign to given variable names the value of matching
  columns in an object. If no matching throw an error. If
  partial matching print a warning.
}
\examples{
x <- data.frame(A=1:3, B=letters[1:3], c=c(TRUE, FALSE, TRUE))
fun <- function(x) {
	vars <- c("A", "b", "C")
	findVars(vars, x)
	for (k in vars) print(get(k))
}
fun(x) # Check the warnings
}
\author{
  Yves
}
\seealso{
  \code{\link{existsVars}}
}
\keyword{internal}

