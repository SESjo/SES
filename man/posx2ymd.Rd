\name{posx2ymd}
\alias{posx2ymd}
\title{posx2ymd}
\usage{
  posx2ymd(posx, width = 6)
}
\arguments{
  \item{posx}{The POSIX object to convert}

  \item{width}{The desired number of columns desired in the
  data frame.}
}
\description{
  Convert a POSIX object to a data frame.
}
\examples{
posx2ymd(as.POSIXct("2011-11-01 01:02:03"))
posx2ymd(as.POSIXct("2011-11-01 01:02:03"), 3)
}
\author{
  Yves
}

