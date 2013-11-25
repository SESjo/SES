\name{importSES}
\alias{importSES}
\title{ImportSES}
\usage{
  importSES(matfile, type = c("both", "tdr", "stat"))
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
\author{
  Yves
}
\seealso{
  SESformat (df) to edit output format (column and class of
  columns)
}
\keyword{.mat}
\keyword{Import}
\keyword{SES}

