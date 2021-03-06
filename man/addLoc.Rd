\name{addLoc}
\alias{addLoc}
\title{Copy locations from one table to another within a 'ses' object}
\usage{
addLoc(from, to, ses = NULL, append = TRUE)
}
\arguments{
  \item{from}{Source of location information ('tdr' or
  'statdives' object).}

  \item{to}{Where to add location inforation (respectively
  a 'statdives' or 'tdr' object).}

  \item{ses}{A SES from which use the data. If used 'from'
  and 'to' must be taken in "tdr" and "stat".}

  \item{append}{logical. If TRUE, the function returns the
  entire object 'to' with the added column. if FALSE it
  returns the columns.}
}
\value{
Object given at the 'to' argument, updated with location
information.
}
\description{
Copy locations from one table to another within a 'ses'
object
}
\examples{
path <- system.file("extdata", package="SES")
pathname <- file.path(path, "2011-16_SES_example_accelero.mat")
ses <- importSES(pathname)
ses$tdr <- addLoc(ses$stat, ses$tdr)
head(ses$tdr)
}
\seealso{
\code{\link{addVar}} \code{\link{importSES}}
}

