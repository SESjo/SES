\name{class2logical}
\alias{class2logical}
\title{class2logical}
\usage{
class2logical(x)
}
\arguments{
  \item{x}{The object to convert.}
}
\description{
Convert the result of dive classification into a logical
that differentiate the drift dives from the other types.
}
\examples{
str(class2logical(ses$stat$Dive.type))
}
\keyword{internal}

