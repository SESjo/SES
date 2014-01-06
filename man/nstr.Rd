\name{nstr}
\alias{nstr}
\title{nstr}
\usage{
nstr(x)
}
\arguments{
  \item{x}{The object to analyse.}
}
\description{
Recursive extraction of names (such as \code{names(c(x,
recursive=TRUE))}) but stops when a subelement is atomic.
}
\examples{
x <- data.frame(X=1:10, Y=10:1)
names(c(x, recursive=TRUE))
nstr(x)
}
\keyword{internal}

