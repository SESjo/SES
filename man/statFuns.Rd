\name{statFuns}
\alias{statFuns}
\title{Apply summary statistics functions given a type of vector}
\usage{
statFuns(type, x)
}
\arguments{
  \item{type}{The type of computation. To choose in
  \code{c("double", "integer", "logical", "factor",
  "character")}.}

  \item{x}{Alternatively, the vector can be given instead.}
}
\value{
A list of functions to use in order to computre summary
statistics.
}
\description{
Apply summary statistics functions given a type of vector
}
\keyword{internal}

