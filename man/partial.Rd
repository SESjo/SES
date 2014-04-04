\name{partial}
\alias{partial}
\title{Set function arguments to new default}
\usage{
partial(FUN, ...)
}
\arguments{
  \item{FUN}{The function implied}

  \item{...}{Arguments to set}
}
\description{
Set function arguments to new default
}
\examples{
x <- c(1:3, NA, 3:1)
partial(mean, na.rm=TRUE)(x)
}
\keyword{internal}

