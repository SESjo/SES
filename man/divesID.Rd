\name{divesID}
\alias{divesID}
\title{Find the dives from TDR data}
\usage{
divesID(tdr, durThres = 300, dpthThres = 15)
}
\arguments{
  \item{tdr}{The TDR dataset.}

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
Method tranduced from Matlab.
}
\seealso{
\code{\link{poly4delim}}, \code{\link{anaDives}}
}

