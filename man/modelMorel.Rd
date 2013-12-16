\name{modelMorel}
\alias{modelMorel}
\title{modelMorel}
\usage{
  modelMorel(Chl, stat = NULL, append = TRUE)
}
\arguments{
  \item{Chl}{Vector giving the chlorophyll a concentration
  in the surface layer.}

  \item{stat}{A statdives object. 'chl' argument is
  ignored. The values are taken from this statdive object.}

  \item{append}{Should the variable be returned with the
  entire statdives object ?}
}
\description{
  Predict euphotic depth (m) from the surface chlorophyll
  concentration.
}
\references{
  Morel and Berthon (1989)
}
\seealso{
  Other chl: \code{\link{extractChl}},
  \code{\link{importChl}}, \code{\link{mZeu2layer}}
}

