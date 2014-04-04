\name{nNA}
\alias{nNA}
\title{nNA}
\usage{
nNA(x)
}
\arguments{
  \item{x}{a vector to test the elements.}
}
\value{
Return the number of \code{NA} in \code{x}.
}
\description{
Shortcut for \code{compose(sum, is.na, unlist)}
}
\details{
As any number different from 0 return a \code{TRUE} when
coerced to logical, this function can be used in \code{if}
statements.
}
\examples{
x <- c(rep(NA, 3), 1:3)
nNA(x)
if (nNA(x)) {TRUE} else {FALSE}
if (nNA(1:3)) {TRUE} else {FALSE}
}
\keyword{internal}

