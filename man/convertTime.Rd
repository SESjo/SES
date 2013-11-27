\name{convertTime}
\alias{convertTime}
\title{convertTime}
\usage{
  convertTime(x, to, width = 6)
}
\arguments{
  \item{x}{The object to convert}

  \item{to}{The output format. Choose in 'posx', 'ymd',
  'text'.}

  \item{width}{The desired number of columns desired in the
  data frame.}
}
\description{
  Convert a time object to another format (among those used
  in the package and data files)
}
\details{
  Width = 3 when 'text' format is involved.
}
\examples{
tm <- as.POSIXct("2011-11-01 01:02:03")
convertTime(tm, to="posx", width=4) # "2011-11-01 01:00:00 CET"
convertTime(tm, to="text")          # "20111101", width = 3 when 'text' is involved
(tm <- convertTime(tm, to="ymd"))   # 2011    11   1    1      2      3
convertTime(tm, to="posx")          # "2011-11-01 01:02:03 CET"
convertTime(tm, to="text")          # "20111101", width = 3 when 'text' is involved
}
\author{
  Yves
}
\seealso{
  \code{\link{importSEAPOpred}}
}

