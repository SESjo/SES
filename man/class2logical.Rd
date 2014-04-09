\name{class2logical}
\alias{class2logical}
\title{Convert the result of dive classification into a logical}
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
\dontrun{
str(class2logical(ses$stat$Dive.type))
}
}
\keyword{internal}

