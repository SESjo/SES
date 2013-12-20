\name{as.ses}
\alias{as.ses}
\title{as.ses}
\usage{
as.ses(x, type = c("classic", "3D"), ind, tdr, stat)
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
Coerce an object to a 'ses' if possible
}
\examples{
new.ses <- list(Ind.id="Example", tdr=data.frame(), stat=data.frame())
is.ses(new.ses)
new.ses <- as.ses(new.ses)
is.ses(new.ses)
}
