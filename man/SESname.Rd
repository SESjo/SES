\name{SESname}
\alias{SESname}
\title{Extract the ID of an individual from a string}
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
Extract the ID of an individual from a string that contains
it. This function just apply a the following regular
expression \code{'20+[0-9]{2}-[0-9]{2}'}
}
\examples{
SESname("Path/to/seal/file/2011-12_some_ses_file.txt")
}
\seealso{
Other generalUtils: \code{\link{convertTime}};
\code{\link{per}}; \code{\link{replaceMissing}};
\code{\link{sunPosition}}; \code{\link{sunPos}}
}

