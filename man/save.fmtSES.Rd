\name{save.fmtSES}
\alias{save.fmtSES}
\title{save.fmtSES}
\usage{
  save.fmtSES(x, verbose = FALSE)
}
\arguments{
  \item{x}{The object to save}

  \item{verbose}{To print the filepath.}

  \item{envir}{The formatSES environment.}
}
\description{
  A kind of \code{save} S3 method designed for formatSES
  object of the SES package. Export formatSES settings to
  package files so that user preferences are automaticaly
  loaded at startup.
}
\author{
  Yves
}
\seealso{
  \code{\link{reset.fmtSES}}, \code{\link{print.fmtSES}}
}

