\name{summary.ses}
\alias{summary.ses}
\alias{summary.statdives}
\alias{summary.tdr}
\title{Summarizing SES dataset}
\usage{
\method{summary}{ses}(object, na.rm = TRUE, all = FALSE, digits = 2)

\method{summary}{tdr}(object, na.rm = TRUE, complete = TRUE, digits = 2)

\method{summary}{statdives}(object, na.rm = TRUE, digits = 2)
}
\arguments{
  \item{object}{An object for which a summary is desired.}

  \item{na.rm}{A logical value indicating whether \code{NA}
  values should be stripped before the computation
  proceeds.}

  \item{all}{Should the function return results with both
  os 'statdives' ans 'tdr' objects}

  \item{digits}{The number of digits to show in dive
  statistics.}

  \item{complete}{Should the function compute all
  statistics.}
}
\description{
S3 method for 'ses' objects.
}

