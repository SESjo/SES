\name{findVarlist}
\alias{findVarlist}
\title{findVarlist}
\usage{
  findVarlist(var, object, substring = TRUE,
    ignore.case = TRUE, ...)
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
}
\value{
  the desired variable
}
\description{
  Search and get the content of a variable anywhere in a
  'list' object using its name.
}
\details{
  Same warnings and error handling than \code{existsVars()}
}
\examples{
l <- list(a=1, b=list(c=2, cD=3))
findVarlist("a", l)
findVarlist("b", l)
findVarlist("c", l) # Error
findVarlist("c", l, substring=F)
findVarlist("d", l)
findVarlist("d", l, ignore.case=F) # Error
}
\author{
  Yves
}
\seealso{
  \code{\link{findVars}}, \code{\link{existsVars}},
  \code{\link{lnamesApply}}.
}
\keyword{internal}

