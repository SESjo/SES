\name{\%else\%}
\alias{\%else\%}
\title{Else special operator}
\usage{
a \%else\% b
}
\arguments{
  \item{a}{default output.}

  \item{b}{output if \code{a} is \code{FALSE}, \code{NULL}
  or empty.}
}
\description{
Discard first value if \code{FALSE}, \code{NULL} or empty.
}
\examples{
x <- NULL \%else\% 1
x
}
\keyword{internal}

