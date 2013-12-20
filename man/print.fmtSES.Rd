\name{print.fmtSES}
\alias{print.fmtSES}
\title{print.fmtSES}
\usage{
  \method{print}{fmtSES} (x, ...)
}
\arguments{
  \item{x}{The object to print}

  \item{...}{Arguments to be passed to \code{edit()}.}
}
\description{
  S3 method for formatSES object of SES package. Allow to
  view and edit \code{importSES()} settings.
}
\examples{
\dontrun{
# First attach the Settings environement
attach(SESsettings)
# Then type the name of the object to view or edit
formatSES$tdr
# Process to modif ...
# Save to package files for automatic load in the futur
saveFmtSES(formatSES$tdr)
# Reset default settings
resetFmtSES()
}
}
\seealso{
  Other settings: \code{\link{importSES}},
  \code{\link{renames}}, \code{\link{resetFmtSES}},
  \code{\link{saveFmtSES}}, \code{\link{userHeader}},
  \code{\link{whichSESobj}}
}

