\name{dvapply}
\alias{dvapply}
\title{Apply a function to each dive/surface/bottom}
\usage{
dvapply(obj, FUN, dvs, type = c("dive", "surface", "both", "bottom"), ...)
}
\arguments{
  \item{obj}{A TDR object}

  \item{FUN}{Function to apply}

  \item{dvs}{Optional. A table with dives/surfaces/bootoms
  indices as returned by \code{\link{divesID}} or
  \code{\link{anaDive}}.}

  \item{type}{The periods involved: to choose in
  \code{c('dive', 'surface', 'both', 'bottom')}.}

  \item{...}{Other arguments to be passed to \code{FUN}.}
}
\description{
\code{dvapply} is a utility to apply function to specific
parts of a TDR dataset.
}
\examples{
path <- system.file("extdata", package="SES")
pathname <- file.path(path, "2011-16_SES_example_accelero.mat")
ses <- importSES(pathname)
depth.max <- dvapply(ses$tdr, function(tdr){max(tdr$Depth)})
}

