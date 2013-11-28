\name{isDay}
\alias{isDay}
\title{isDay}
\usage{
  isDay(Time, loc, stat = NULL, elevlim = c(-18, 18),
    append = TRUE)
}
\arguments{
  \item{Time}{}

  \item{loc}{}

  \item{stat}{A statdives object can be used instead of the
  two previous arguments.}

  \item{elevlim}{Sun elevation the thresholds to
  distinguish between day and night}

  \item{append}{Should the entire updated object be
  returned (if 'stat' argument was used).}
}
\description{
  Use time and loc info to find if events occured during
  the day or the night.
}
\details{
  SEAPODYm criteria elevlim=c(-18, 18). Transition periods
  filled with NAs.
}
\author{
  Yves
}
\seealso{
  \code{\link{sunPosition}}
}

