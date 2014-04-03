\name{poly4delim}
\alias{poly4delim}
\title{Find the dives' bottom from TDR data}
\usage{
poly4delim(tdr, dvs, spdThres = 0.75, w = 12)
}
\arguments{
  \item{dvs}{The dives indices as returned by
  \code{\link{diveID}}.}

  \item{tdr}{The TDR dataset.}

  \item{spdThres}{A vertical speed threshold (m/s) used to
  delimitate the dives' bootom.}
}
\value{
A data frame with the following variable: indice of period
start, indice of period end, type of period, duration (s),
the dive number, indice of bottom start, indice of bottom
end, bottom duration.
}
\description{
Method tranduced from Matlab.
}
\details{
See Yves Le Bras M1 report for details about the method.
}
\seealso{
\code{\link{divesID}}, \code{\link{anaDives}}
}

