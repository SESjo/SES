\name{save.fmtSES}
\alias{save.fmtSES}
\title{save.fmtSES}
\usage{
\method{save}{fmtSES}(x, verbose = FALSE)
}
\arguments{
  \item{x}{The object to save}

  \item{verbose}{To print the filepath.}
}
\description{
A kind of \code{save} (S3) method designed for formatSES
object of the SES package. Export formatSES settings to
package files so that user preferences are automaticaly
loaded at startup.
}
\seealso{
Other settings: \code{\link{importSES}};
\code{\link{print.fmtSES}}; \code{\link{renames}};
\code{\link{reset.fmtSES}}; \code{\link{userHeader}};
\code{\link{whichSESobj}}
}

