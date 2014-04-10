\name{as.ses}
\alias{as.ses}
\alias{as.ses.list}
\alias{is.SES}
\alias{is.ses}
\alias{is.stat}
\alias{is.stat3D}
\alias{is.tdr}
\alias{is.tdr3D}
\title{Creates or tests for 'ses'.}
\usage{
as.ses(x, type = c("classic", "3D"), ind, tdr, stat)

\method{as.ses}{list}(x, type = c("classic", "3D"))

is.ses(x)

is.SES(x)

is.tdr(x)

is.tdr3D(x)

is.stat(x)

is.stat3D(x)
}
\arguments{
  \item{x}{The object to coerce to 'ses'}

  \item{type}{The type of 'ses' desired. To choose in
  \code{c("classic", "3D")}.}

  \item{ind}{The individual ID number. Ignored if x is
  given.}

  \item{tdr}{The TDR data. Ignored if x is given.}

  \item{stat}{The statdives data. Ignored if x is given.}
}
\description{
Creates or tests for 'ses'.
}
\details{
\code{as.ses} coerces an object to a 'ses'.

\code{as.ses} tests if an object belong to the 'ses' class

\code{as.SES} tests if an object belongs to the family of
SES package object.

\code{is.tdr} Test if an object belong to the 'tdr' class

\code{is.tdr3D} tests if an object belong to the 'tdr3D'
class.

\code{is.stat} tests if an object belong to the 'stat'
class

\code{is.stat3D} tests if an object belong to the 'stat3D'
class
}
\examples{
new.ses <- list(Ind.id="Example", tdr=data.frame(), stat=data.frame())
is.ses(new.ses)
new.ses <- as.ses(new.ses)
is.ses(new.ses)
}
\keyword{internal}

