\name{dvapply}
\alias{dvapply}
\title{Apply a function to each dive/surface/bottom/... of a TDR dataset}
\usage{
dvapply(FUN, OBJ, DVS, .type = c("dv", "sf", "all", "dus", "btt", "asc",
  "dsc"), .ply = c("s", "l"), .numb = NULL, .idx = NULL, ...)
}
\arguments{
  \item{FUN}{Function to apply. The first argument has to
  be the TDR data subset (all columns but only the rows
  indicated by the \code{type} argument). See in the
  example section how it can be used to get Max depth.}

  \item{OBJ}{A 'tdr' or 'ses' object}

  \item{DVS}{Optional. A table with dives/surfaces/bottoms
  indices as returned by \code{\link{divesID}} or
  \code{\link{anaDives}}.}

  \item{.type}{The periods involved: to choose in :
  \code{'dv'}, dives; \code{'sf'}, surfaces. \code{'all'},
  both; \code{'dus'}, dives Union surfaces i.e. dives and
  their following surface period; \code{'btt'}, bottoms;
  \code{'asc'}, ascents; \code{'dsc'}, descents.}

  \item{.ply}{The apply function to use: \code{s = sapply,
  l = lapply}.}

  \item{.numb}{Optional. The number(s) of the period(s)
  involved in when applying \code{FUN} on each one is not
  nessessary.}

  \item{.idx}{Optional. In the case where none of the
  \code{.type} options matches your needs you may provide
  your own subset indices by using this argument.
  \code{.idx} must be a data frame with start indices in
  the first column and end indices in the second.}

  \item{...}{Other arguments to be passed to \code{FUN}.}
}
\description{
\code{dvapply} is a utility to apply functions to specific
parts of a TDR dataset. This is faster than a loop and it
helps to make readable code.
}
\details{
The names of the argument in this function are in upper
case (data) or preceded by a dot (options) in order to
avoid that the names of \code{dvapply} arguments match with
those of \code{FUN}. The output elements are named after
the \code{.type} argument and their number (if \code{.idx}
is given in input then \code{"cst"} (custom) is used).
}
\examples{
path <- system.file("extdata", package="SES")
pathname <- file.path(path, "2011-16_SES_example_accelero.mat")
ses <- importSES(pathname)

Dmax <- function(x) max(x$Depth)
dvDmax <- dvapply(Dmax, ses)  # Get max. depth of each dive
dvDmax2 <- dvapply(Dmax, ses, .n = 50:60) # dives #50 to #60 only
# identical(dvDmax2, dvDmax[50:60])  # TRUE

# Compute "boxplot statitics" of each surface period
# FUN returns several values so .ply = 'l'
sfDstat <- dvapply(function(x) fivenum(x$Depth), ses, .type = 'sf', .ply = 'l')

# Provide your own indices
dvapply(Dmax, ses, .idx = data.frame(c(1,10000), c(10, 20000)))
dvapply(Dmax, ses, .idx = data.frame(c(1,10000), c(10, 20000)), .n = 2)
}

