\name{reset.fmtSES}
\alias{reset.fmtSES}
\title{reset.fmtSES}
\usage{
reset.fmtSES(type = c("files", "vars"), ...)
}
\arguments{
  \item{type}{\code{files} restore original settings in the
  package folder. \code{vars} restore the settings in this
  session but leave the package files unchanged.}

  \item{...}{arguments to be passed to \code{save.fmtSES}.}
}
\description{
Reset formatSES to defaults settings (suggested alias
column).
}
\seealso{
Other settings: \code{\link{importSES}};
\code{\link{print.fmtSES}}; \code{\link{renames}};
\code{\link{save.fmtSES}}; \code{\link{userHeader}};
\code{\link{whichSESobj}}
}

