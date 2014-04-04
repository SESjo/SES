\name{dvapply}
\alias{dvapply}
\title{Apply a function to each dive/surface/bottom}
\usage{
dvapply(FUN, obj, dvs, type = c("dive", "surface", "any", "bottom", "ascent",
  "descent"), ...)
}
\arguments{
  \item{FUN}{Function to apply. The first argument has to
  be the TDR data subset (all columns but only the rows
  indicated by the \code{type} argument). See in the
  example section how it can be used to get Max depth.}

  \item{obj}{A TDR object}

  \item{dvs}{Optional. A table with dives/surfaces/bootoms
  indices as returned by \code{\link{divesID}} or
  \code{\link{anaDives}}.}

  \item{type}{The periods involved: to choose in
  \code{c('dive', 'surface', 'any', 'bottom', 'ascent',
  'descent')}. \code{any} means to apply the function to
  both surface and diving periods.}

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
depth.max <- dvapply(function(tdr){max(tdr$Depth)}, ses$tdr)
}

