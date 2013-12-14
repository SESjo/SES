\name{findVar}
\alias{findVar}
\title{findVar}
\usage{
  findVar(object, pattern, ...)
}
\arguments{
  \item{object}{A list.}

  \item{pattern}{a function (must return a logical).}

  \item{...}{Optional arguments to be passed to
  \code{grepl}.}
}
\value{
  A list with the desired variable (\code{$var}) and the
  results of the function \code{fun} at the matching depth.
  Returns \code{NULL} if no match.
}
\description{
  Similar to rapply but only aims only the names of
  variables within \code{object}.
}
\examples{
l <- list(a=1, b=list(c=2))
findVar(l, grepl, pattern="a")
findVar(l, grepl, pattern="b")
findVar(l, grepl, pattern="c")
is.null(findVar(l, grepl, pattern="C")) # TRUE
findVar(l, grepl, pattern="C", ignore.case=TRUE)
}
\author{
  Yves
}
\seealso{
  \code{\link{checkVar}}
}
\keyword{internal}

