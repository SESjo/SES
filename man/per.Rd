\name{per}
\alias{per}
\title{per}
\usage{
per(x, idx = FALSE)
}
\arguments{
  \item{x}{The atomic vector to examine.}

  \item{idx}{Should the indexes (start and end) of
  homogeneous sequences be returned as well ?}
}
\value{
A data frame with values and lengths of the homogeneous
sequences of x. The class of the column 'value' is copied
from the input.
}
\description{
The reverse of 'base::rep()' function: decompose an atomic
vector to its successive values and their length.
}
\examples{
(x <- rep(LETTERS[1:10], 10:1))
(y <- per(x))
# 'per()' is the reverse of 'rep()'
# identical(rep(y$value, y$length), x)   # TRUE
# Because characters are not converted to factors
# inherits(y$value, class(x))            # TRUE
}
\seealso{
Other generalUtils: \code{\link{SESname}};
\code{\link{convertTime}}; \code{\link{nval}};
\code{\link{replaceMissing}}; \code{\link{sunPosition}};
\code{\link{sunPos}}
}

