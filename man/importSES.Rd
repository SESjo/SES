\name{importSES}
\alias{importSES}
\title{ImportSES}
\usage{
  importSES(matfile, type = "both")
}
\arguments{
  \item{matfile}{Path to .mat file.}

  \item{type}{Type of data to import: "tdr" only TDR data,
  "stat" Only Statdives data. "Bot" both.}
}
\value{
  An object of class "ses". Includes: ID of the SES - TDR
  and or Stats of dive as requested with 'type' argument.
}
\description{
  Import an individual from a .mat file to R workspace is a
  standart way
}
\details{
  The .mat file must be of of version v7 or less (last
  MATLAB version v7.3).
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
  Edit 'SESformat' in the source code to modify the output
  format (type of columns)
}
\keyword{.mat}
\keyword{Import}
\keyword{SES}

