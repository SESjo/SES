\name{print.fmtSES}
\alias{print.fmtSES}
\title{print.fmtSES}
\usage{
  \method{print}{fmtSES} (x)
}
\arguments{
  \item{x}{The object to print}
}
\description{
  S3 method for formatSES object of SES package. Allow to
  view and edit \code{importSES()} settings.
}
\examples{
\dontrun{
# First attach the Settings environement
attach(efmtSES)
# Then type the name of the object to view or edit
formatSES$tdr
# Process to modif ...
# Save to package files for automatic load in the futur
save.fmtSES(formatSES$tdr)
# Reset default settings
reset.fmtSES()
}
}
\author{
  Yves
}
\seealso{
  \code{\link{save.fmtSES}}, \code{\link{reset.fmtSES}}
}

