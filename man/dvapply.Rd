\name{dvapply}
\alias{dvapply}
\title{Apply a function to each dive/surface/bottom/... of a TDR dataset}
\usage{
dvapply(FUN, obj, dvs, type = c("dive", "surface", "any", "both", "bottom",
  "ascent", "descent"), apply = c("s", "l"), ...)
}
\arguments{
  \item{FUN}{Function to apply. The first argument has to
  be the TDR data subset (all columns but only the rows
  indicated by the \code{type} argument). See in the
  example section how it can be used to get Max depth.}

  \item{obj}{A 'tdr' or 'ses' object}

  \item{dvs}{Optional. A table with dives/surfaces/bottoms
  indices as returned by \code{\link{divesID}} or
  \code{\link{anaDives}}.}

  \item{type}{The periods involved: to choose in
  \code{c('dive', 'surface', 'any', 'both', 'bottom',
  'ascent', 'descent')}. Choose \code{any} to apply the
  function to both surface and diving periods (1 result for
  each row of \code{dvs}) but \code{both} to apply the
  function to the dives and their following surface period
  grouped together (1 result for each couple dive+surface,
  \code{floor(norw(dvs)/2)} results).}

  \item{apply}{The prefix of the apply function to use
  (sapply, lapply, etc). Use \code{type = 'l'} if
  \code{FUN} returns more than just one value per dive (as
  in example).}

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

DivesDepthMax <- dvapply(function(tdr){max(tdr$Depth)}, ses)
SurfDepthStat <- dvapply(function(tdr){fivenum(tdr$Depth)}, ses,
							apply = 'l', type = 'surface')
par(mfrow = c(2, 1))
plot(DivesDepthMax, ylab = 'Max. depth of dives')
plot(sapply(SurfDepthStat, function(x) x[1]), ylab = 'Min. depth of surfaces')
}

