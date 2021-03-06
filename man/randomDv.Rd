\name{randomDv}
\alias{randomDv}
\title{Plot a random dive}
\usage{
randomDv(obj, dvs, plt = TRUE, n = NULL, btt = TRUE)
}
\arguments{
  \item{obj}{A 'ses' or 'tdr' object}

  \item{dvs}{Dive (and bottom delimitation) info as
  returned by \code{\link{anaDives}} or
  \code{\link{divesID}}.}

  \item{plt}{Should a plot be drawn ?}

  \item{n}{A dive number in case the dive should not be
  sampled randomly.}

  \item{btt}{Should the bottom step be ignored by teh
  function ?}
}
\value{
Return a list of the dive number and the TDR data extracted
along the dive and the bottom.
}
\description{
\code{randomDv} Extract and optionally plots a random dive
given TDR data and dive delimitation info.
}
\seealso{
\code{\link{anaDives}}
}

