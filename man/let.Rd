\name{let}
\alias{let}
\title{Evaluate an expression in a specified environment}
\usage{
let(.expr, ...)
}
\arguments{
  \item{.expr}{The expression to evaluate.}

  \item{...}{The context in which to do it.}
}
\description{
\code{let} attaches symbols to a values and evaluate an
expression with respect to these.
}
\examples{
let(a = 1, .expr = a + 1)# More intuitive syntax
let(a + b, a = 1, b = 2) # Without naming the expression...
}
\keyword{internal}

