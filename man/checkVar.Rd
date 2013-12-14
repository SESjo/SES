\name{checkVar}
\alias{checkVar}
\title{checkVar}
\usage{
  checkVar(var, object, mult = FALSE, substring = TRUE,
    ignore.case = TRUE, ignore.depth.error = FALSE)
}
\arguments{
  \item{var}{Character givin the name of the variable to
  search.}

  \item{object}{The object in which to search.}

  \item{substring}{If \code{FALSE} then check that the
  \code{pattern} and the matching variables have the same
  number of characters.}

  \item{ignore.case}{Should case variants of the pattern be
  investigated ?}

  \item{mult}{Is \code{var} a regular expression pattern to
  match several elements in \code{object} variable names ?}

  \item{ignore.depth.error}{Should the function check for
  ambiguities with the names of deeper element ?}
}
\value{
  the desired variable not simplified.
}
\description{
  Search and get the content of a variable anywhere in a
  'list' object using its name.
}
\examples{
l <- list(a=1, b=list(c=2, cD=3))
checkVar("a", l)
checkVar("b", l)
checkVar("c", l) # Error
checkVar("c", l, substring=F)
checkVar("d", l)
checkVar("d", l, ignore.case=F) # Error
}
\author{
  Yves
}
\seealso{
  \code{\link{findVars}}, \code{\link{findVar}}.
}
\keyword{internal}

