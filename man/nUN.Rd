\name{nUN}
\alias{nUN}
\title{nUN}
\usage{
nUN(x)
}
\arguments{
  \item{x}{a vector whose unique elements are to be
  counted.}
}
\description{
Shortcut for \code{compose(length, unique)}. Count the
number of distinct values in an atomic vector.
}
\examples{
nUN(rep(1:5, 5:1)) # 5
}
\keyword{internal}

