\name{renames}
\alias{renames}
\title{renames}
\usage{
  renames(type = c("tdr", "stat", "stat3D", "tdr3D"), obj,
    objtxt)
}
\arguments{
  \item{type}{The type of dataset}

  \item{obj}{The object to process.}

  \item{objtxt}{The original names. tdrcortxt or
  statdivestxt in the .mat file.}
}
\description{
  Extract and rename tdr and statdives variables according
  to formatSES instructions.
}
\seealso{
  Other settings: \code{\link{importSES}},
  \code{\link{print.fmtSES}}, \code{\link{reset.fmtSES}},
  \code{\link{save.fmtSES}}, \code{\link{userHeader}},
  \code{\link{whichSESobj}}
}
\keyword{internal}

