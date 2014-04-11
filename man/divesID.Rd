\name{divesID}
\alias{divesID}
\alias{divesID.ses}
\alias{divesID.statdives}
\alias{divesID.tdr}
\title{Find the dives start and end indices}
\usage{
divesID(obj, ...)

\method{divesID}{statdives}(obj, reso = 1)

\method{divesID}{ses}(obj)

\method{divesID}{tdr}(obj, durThres = 300, dpthThres = 15)
}
\arguments{
  \item{reso}{Time resolution to use when converting the
  dive duration into a number of sensor recordings.}

  \item{obj}{A 'ses' or 'tdr' object.}

  \item{durThres}{A duration threshold (s) under which a
  dive is considered as a surface period.}

  \item{dpthThres}{A depth threshold (m) (> 0) under which
  the animal is considered at surface.}
}
\value{
A data frame with the following variable: indice of period
start, indice of period end, type of period, duration (s),
the dive number.
}
\description{
Find the dives start and end indices
}
\seealso{
\code{\link{bottomDelim}}, \code{\link{anaDives}}
}

