\name{rollapply}
\alias{rollapply}
\title{Apply function along a vector}
\usage{
rollapply(x, FUN, w = 10, wty = "m", ...)
}
\arguments{
  \item{x}{An atomic vector.}

  \item{FUN}{the function to be applied. This function must
  return a single value when applied to a an atomic
  vector.}

  \item{w}{The width of the window.}

  \item{wty}{The type of window. To choose in \code{c('m',
  'f'))} where 'm' stands for "moving" and 'f' for
  "fixed".}

  \item{...}{Optional arguments to FUN.}
}
\description{
\code{rollapply} apply the given function along a vector.
}
\examples{
x <- seq(1, 3, length = 100) + rt(100, df = 2) / 3
plot(x)
lines(rollapply(x, mean, 5)  , col = "red", lwd = 2)
lines(rollapply(x, median, 5), col = "blue", lwd = 2)
lines(rollapply(x, mean, 5, wty='f'), col = "green", lwd = 2)
}

