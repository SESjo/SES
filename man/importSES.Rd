\name{importSES}
\alias{importSES}
\title{ImportSES}
\usage{
  importSES(matfile, type = c("both", "tdr", "stat"))
}
\arguments{
  \item{matfile}{Path to .mat file(s). If \code{matfiles}
  is an atomic vector, then the input is interpreted as a
  request to load a '3D' ses. Use \code{lapply()} to import
  several ses at once.}

  \item{type}{To choose among \code{tdr} (only TDR data),
  \code{stat} (only Statdives data) and \code{both} (for
  both of them).}
}
\value{
  An object of class \code{ses}. Includes: ID of the
  individual - TDR and/or dive statistics (according to the
  \code{type} argument).
}
\description{
  Import an individual from a .mat file to R workspace is a
  standart way
}
\details{
  The .mat file must be of version v7 or older (last MATLAB
  version v7.3): \code{R.matlab} requirement. Edit
  \code{formatSES} to modify the importation preferences
}
\examples{
path <- system.file("extdata", package="SES")
pathname <- file.path(path, "2011-16_SES_example_accelero.mat")
ses <- importSES(pathname)
}
\author{
  Yves
}
\seealso{
  \code{\link{formatSES}}, \code{\link{renames}}
}

