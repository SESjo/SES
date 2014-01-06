\name{replaceMissing}
\alias{replaceMissing}
\title{replaceMissing}
\usage{
replaceMissing(x, na.0 = NaN, na.1 = NA)
}
\arguments{
  \item{x}{The atomic vector}

  \item{na.0}{The value to be replaced. Default is NaN.}

  \item{na.1}{The replacement. Default is NA.}
}
\description{
Replace values in an atomic vector.
}
\examples{
x <- sample(c(1:3,NaN), 20, replace=TRUE)
x
replaceMissing(x)
}
\seealso{
Other generalUtils: \code{\link{SESname}};
\code{\link{convertTime}}; \code{\link{nval}};
\code{\link{per}}; \code{\link{sunPosition}};
\code{\link{sunPos}}
}

