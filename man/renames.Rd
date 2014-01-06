\name{renames}
\alias{renames}
\title{renames}
\usage{
renames(type = c("tdr", "stat", "stat3D", "tdr3D"), obj, objtxt,
  convert = TRUE)
}
\arguments{
  \item{type}{The type of dataset}

  \item{obj}{The object to process.}

  \item{objtxt}{The original names. tdrcortxt or
  statdivestxt in the .mat file.}

  \item{convert}{Should the formating functions given in
  \code{formatSES} be applied to the variable ?}
}
\description{
Extract and rename tdr and statdives variables according to
formatSES instructions.
}
\seealso{
Other settings: \code{\link{importSES}};
\code{\link{print.fmtSES}}; \code{\link{resetFmtSES}};
\code{\link{saveFmtSES}}; \code{\link{userHeader}};
\code{\link{whichSESobj}}
}
\keyword{internal}

