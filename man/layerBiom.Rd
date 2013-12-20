\name{layerBiom}
\alias{layerBiom}
\title{layerBiom}
\usage{
  layerBiom(grp, all.col = FALSE, layers = NULL,
    is.day = NULL)
}
\arguments{
  \item{grp}{Atomic vector giving the functional groups
  biomass in the following order: \code{c(epi, meso, mmeso,
  bathy, mbathy, hmbathy)}.}

  \item{all.col}{Should the function return all columns:
  \code{Layer} and \code{is.Day}}

  \item{layers}{Should the function focus on a specific
  layer (to choose in \code{c("Bathy", "Epi", "Meso")}).
  Default is all layers.}

  \item{is.day}{Should the function focus on a specific
  period (to choose in \code{c(TRUE, FALSE)}).}
}
\description{
  Compute the biomass in each layer during the day and
  night periods.
}
\examples{
layerBiom(1:6) # Should be c(4, 10, 7, 15, 1, 5)
}

