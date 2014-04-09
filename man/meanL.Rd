\name{meanL}
\alias{meanL}
\title{Compute the average length of a sequences of successively repeated values}
\usage{
meanL(x, ref = NULL, cmp = NULL)
}
\arguments{
  \item{x}{a vector in which to search for sequences of
  successively repeated values.}

  \item{ref}{A reference value to use when counting a
  specific event only.}

  \item{cmp}{The comparison operator to use for comparison
  with the reference.}
}
\description{
Compute the average length of a sequences of successively
repeated values
}
\details{
The default is to return the overall average length. If
\code{ref} is given but not \code{cmp} then \code{cmp =
'!='}. If \code{cmp} is given but not \code{ref} then
\code{ref} is chosen to \code{FALSE}, \code{0L} or to the
first value (decreasing order) of \code{x}.
}
\examples{
meanL(rep(letters[1:2], 1:2))    # Average length of all sequences
x <- rep(letters[sequence(c(2, 2))], c(1, 2, 9, 1))
meanL(x, ref = 'a', cmp = '==')  # Average length of 'a' sequences, 5
meanL(x, ref = 'b', cmp = '!=')  # Average length of 'a' sequences, 5
meanL(, ref = 'b', cmp = '==')   # Average length of 'b' sequences, 1.5
}
\keyword{internal}

