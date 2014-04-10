\name{anaDives}
\alias{anaDives}
\title{Find the dives and delimitate their bottom from TDR data}
\usage{
anaDives(tdr, dpthThres = 15, durThres = 300, spdThres = 0.75, w = 12)
}
\arguments{
  \item{tdr}{The TDR dataset.}

  \item{dpthThres}{A depth threshold (m) (> 0) under which
  the animal is considered at surface.}

  \item{durThres}{A duration threshold (s) under which a
  dive is considered as a surface period.}

  \item{spdThres}{A vertical speed threshold (m/s) used to
  delimitate the dives' bootom.}

  \item{w}{Window width for rolling mean.}
}
\value{
A data frame with the following variable: indice of period
start, indice of period end, type of period, duration (s),
the dive number, indice of bottom start, indice of bottom
end, bottom duration.
}
\description{
Method translated from Matlab.
}
\details{
See Yves Le Bras M1 report for details about the method.
}
\examples{
path <- system.file("extdata", package="SES")
pathname <- file.path(path, "2011-16_SES_example_accelero.mat")
ses <- importSES(pathname)

dvs <- anaDives(ses$tdr)
randDive(ses$tdr, dvs)
}
\seealso{
\code{\link{divesID}}, \code{\link{bottomDelim}},
\code{\link{anaBehav}}.
}

