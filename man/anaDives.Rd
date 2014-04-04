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

n <- sample(setdiff(unique(dvs$Dive.id), 0), 1)
while (is.na(dvs$btt.st.idx[dvs$Dive.id == n])) {
   n <- sample(setdiff(unique(dvs$Dive.id), 0), 1)
}
df <- ses$tdr[dvs$st.idx[dvs$Dive.id == n]:dvs$ed.idx[dvs$Dive.id == n], ]
plot(-Depth ~ Time, df, type = 'l')
df <- ses$tdr[c(dvs$btt.st.idx[dvs$Dive.id == n],
                dvs$btt.ed.idx[dvs$Dive.id == n]), ]
points(-Depth ~ Time, df, type = 'p', col='red', pch = 19)
}
\seealso{
\code{\link{divesID}}, \code{\link{bottomDelim}},
\code{\link{anaBehav}}.
}

