\name{addVar}
\alias{addVar}
\title{addVar}
\usage{
addVar(var, from, to, ses = NULL, append = TRUE)
}
\arguments{
  \item{var}{The name of the variable to be copied.}

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
Copy values of variable from an object to another within a
ses.
}
\details{
Missing values (NAs) of the choosen variable are
transformed in zeros when pasted to a 'tdr' object.
}
\seealso{
\code{\link{addLoc}}
}

