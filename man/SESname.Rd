\name{SESname}
\alias{SESname}
\title{SESname}
\usage{
  SESname(text)
}
\arguments{
  \item{text}{Text containing the seal ID.}
}
\value{
  TheiID as character
}
\description{
  Extract the ID of an individual from a string that
  contains it.
}
\examples{
SESname("Path/to/seal/file/2011-12_some_ses_file.txt")
}
\seealso{
  Other generalUtils: \code{\link{convertTime}},
  \code{\link{per}}, \code{\link{replaceMissing}},
  \code{\link{sunPos}}, \code{\link{sunPosition}}
}

