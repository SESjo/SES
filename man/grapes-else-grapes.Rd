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
Discard first value if \code{FALSE}, \code{NULL}, empty or
\code{"try-error"}
}
\examples{
"abc" \%else\% "Another value is returned"
NULL \%else\% "Another value is returned"
try(log("abc"), silent = TRUE) \%else\% "Another value is returned"
}
\keyword{internal}

