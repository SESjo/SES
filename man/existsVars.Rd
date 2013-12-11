\name{existsVars}
\alias{existsVars}
\title{existVar}
\usage{
  existsVars(vars, obj, idx = TRUE, substring = TRUE,
    ignore.case = TRUE, ...)
}
\arguments{
  \item{vars}{The list of variable names. Can be a regular
  expression.}

  \item{obj}{The object in which to search.}

  \item{idx}{Should the indexes of matching columns be
  returned ?}

  \item{substring}{If \code{FALSE} then check that the
  \code{pattern} and the matching variables have the same
  number of characters.}

  \item{ignore.case}{Should case variants of the pattern be
  investigated ?}

  \item{...}{Other arguments to be passed to \code{grepl}.
  Set \code{fixed} to \code{TRUE} if \code{vars} is to be
  matched as is (not a regular expression).}
}
\description{
  Search for variables in an object. Throw an error if not
  when a variable is not found. Print a warning if partial
  matching only.
}
\author{
  Yves
}
\seealso{
  \code{\link{findVars}}
}
\keyword{internal}

