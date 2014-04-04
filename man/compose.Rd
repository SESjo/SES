\name{compose}
\alias{\%.\%}
\alias{compose}
\title{Function composition}
\usage{
compose(..., funs)

g \%.\% f
}
\arguments{
  \item{...}{The functions to compose.}

  \item{funs}{A list of the functions to compose to use
  instead of the list of arguments.}

  \item{f,g}{Two functions to compose for the infix form}
}
\value{
\code{compose} returns the composed of the functions listed
in/as arguments.

\%.\% is a binary operator version of the compose function.
}
\description{
Function composition
}
\details{
The order of arguments is important. \code{compose(h, g,
f)}; \code{function(...) h(g(f(...)))} and \code{h \%.\% g
\%.\% f} are equivalent.
}
\examples{
x <- c(1:3, NA, 3:1)
compose(any, is.na)(x)
# compose(funs = list(any, is.na))(x)      # The same
# compose(`!`, all, `!`, is.na)(x)         # The same
# (any \%.\% is.na)(x)                       # The same
compose(length, unique)(x)
compose(mean, na.omit)(x)        # mean(x= , na.rm=T) or partial(mean, na.rm=T)
compose(round, rnorm)(1000, 0, 1) -> x -> y
compose(all, `==`)(x, y)
(rep \%.\% per)(c(1:3, 3:2, 2))
}
\keyword{internal}

