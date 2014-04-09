\name{grepo}
\alias{grepc}
\alias{grepo}
\title{Special options of unix grep}
\usage{
grepo(pattern, text, ...)

grepc(pattern, text, ...)
}
\arguments{
  \item{...}{Other arguments to be passed to
  \code{regexpr}.}

  \item{pattern}{character string containing a
  \link{regular expression} (or character string for
  \code{fixed = TRUE}) to be matched in the given character
  vector.  Coerced by \code{\link{as.character}} to a
  character string if possible.  If a character vector of
  length 2 or more is supplied, the first element is used
  with a warning.  Missing values are allowed except for
  \code{regexpr} and \code{gregexpr}.}
}
\description{
\code{grepo} is similar to bash's \code{grep -o pattern
file} it returns the matches.
}
\details{
\code{grepc} is similar to bash's \code{grep -c pattern
file} it counts the number of matches.
}
\keyword{internal}

