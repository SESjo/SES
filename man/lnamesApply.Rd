\name{lnamesApply}
\alias{lnamesApply}
\title{lnamesApply}
\usage{
  lnamesApply(object, fun, ...)
}
\arguments{
  \item{object}{A list.}

  \item{fun}{a function (must return a logical).}

  \item{...}{Optional arguments to be passed to
  \code{fun}.}
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
lnamesApply(l, grepl, pattern="a")
lnamesApply(l, grepl, pattern="b")
lnamesApply(l, grepl, pattern="c")
is.null(lnamesApply(l, grepl, pattern="C")) # TRUE
lnamesApply(l, grepl, pattern="C", ignore.case=TRUE)
}
\author{
  Yves
}
\seealso{
  \code{\link{findVarlist}}
}
\keyword{internal}

