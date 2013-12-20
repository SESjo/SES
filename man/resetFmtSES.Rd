\name{resetFmtSES}
\alias{resetFmtSES}
\title{resetFmtSES}
\usage{
  resetFmtSES(type = c("files", "vars"), ...)
}
\arguments{
  \item{type}{\code{files} restore original settings in the
  package folder. \code{vars} restore the settings in this
  session but leave the package files unchanged.}

  \item{...}{arguments to be passed to \code{saveFmtSES}.}
}
\description{
  Reset formatSES to defaults settings (suggested alias
  column).
}
\seealso{
  Other settings: \code{\link{importSES}},
  \code{\link{print.fmtSES}}, \code{\link{renames}},
  \code{\link{saveFmtSES}}, \code{\link{userHeader}},
  \code{\link{whichSESobj}}
}

