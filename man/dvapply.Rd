\name{dvapply}
\alias{dvapply}
\title{Apply a function to each dive/surface/bottom/... of a TDR dataset}
\usage{
dvapply(FUN, OBJ, DVS, SIMPLIFY = TRUE, TYPE = c("dv", "sf", "all", "dus",
  "btt", "asc", "dsc"), N = NULL, IDX = NULL, ALONG = NULL, ...)
}
\arguments{
  \item{FUN}{Function to apply, found via
  \code{\link{match.fun}}. The function has to be written
  considering that it's first argument will be a subset of
  the TDR data (all columns but only the rows indicated by
  the \code{type} argument). See in the example section how
  it can be used to get Max depth.}

  \item{OBJ}{A 'tdr' or 'ses' object}

  \item{DVS}{Optional. A table with dives/surfaces/bottoms
  indices as returned by \code{\link{divesID}} or
  \code{\link{anaDives}}. If not given then computed within
  \code{dvapply}.}

  \item{SIMPLIFY}{Logical or character string; attempt to
  reduce the result to a vector, matrix or higher
  dimensional array; see the simplify argument of
  \code{\link{sapply}}.}

  \item{TYPE}{The periods involved: to choose in :
  \code{'dv'}, dives; \code{'sf'}, surfaces. \code{'all'},
  both; \code{'dus'}, dives Union surfaces i.e. dives and
  their following surface period; \code{'btt'}, bottoms;
  \code{'asc'}, ascents; \code{'dsc'}, descents.}

  \item{N}{Optional. The number(s) of the period(s)
  involved in when applying \code{FUN} on each one is not
  nessessary.}

  \item{IDX}{Optional. In the case where none of the
  \code{TYPE} options matches with your needs, you may
  provide your own subset indices by using this argument.
  \code{IDX} must be a data frame or a matrix with start
  indices in the first column and end indices in the
  second.}

  \item{ALONG}{Optional. A list of arguments of \code{FUN}
  whose values depend on the period involved. Arguments are
  recycled when their length is smaller than the number of
  periods but truncated when longer. This is different from
  the behaviour of \code{\link{mapply}}. See the last
  examples.}

  \item{...}{Other arguments to be passed to \code{FUN}
  shared by all the periods.}
}
\description{
\code{dvapply} is a functional programing utility designed
to easily apply functions to the most interesting parts of
TDR datasets. It is based on a call to
\code{\link{mapply}}. The use of Apply-familly functions is
generally encouraged since it is faster than loops and
helps to make clearly readable code.
}
\details{
The names of the argument in this function are in upper
case in order to avoid that the names of \code{dvapply}
arguments match with those of \code{FUN}. The output
elements are named after the \code{TYPE} argument and their
number. If \code{IDX} is given in input then \code{"cst"}
(custom) is used.

Some good explanations of Apply-familly functions on
Stackoverflow.com:
\url{http://stackoverflow.com/questions/3505701/r-grouping-functions-sapply-vs-lapply-vs-apply-vs-tapply-vs-by-vs-aggrega/7141669#7141669}
}
\examples{
path <- system.file("extdata", package="SES")
pathname <- file.path(path, "2011-16_SES_example_accelero.mat")
ses <- importSES(pathname)

# Most basic usage example: Get max. depth of each dive
Dmax <- function(x) max(x$Depth)
dvDmax <- dvapply(Dmax, ses)

# "N" argument: run FUN for some periods only
# Get max. depth of dives #50 to #55 only
identical(dvapply(Dmax, ses, N = 50:55), dvDmax[50:55])

# "..." argument: provide more arguments
# If dataset contains missing values, we can use the na.rm argument
dvapply(function(x, na.rm) max(x$Depth, na.rm), ses, N = 50:55, na.rm = TRUE)
dvapply(function(x, ...) max(x$Depth, ...), ses, N = 50:55, na.rm = TRUE)

# "SIMPLIFY" argument: when FUN returns several values
# Here we compute for each surface period Min., 1st Qu., Median, 3rd Qu., Max. of the depth.
dvapply(function(x) fivenum(x$Depth), ses, TYPE = 'sf', SIMPLIFY = FALSE, N = 50:55)

# "IDX" argument: provide custom indices
# For some reason I need the max depth between lines (1 to 10) and (10000 to 20000)
periods <- data.frame(start = c(1,10000), end = c(10, 20000))
dvapply(Dmax, ses, IDX = periods)
dvapply(Dmax, ses, IDX = periods, N = 2)

# "ALONG" argument: vectorize over periods along with other FUN's agument(s)
all(dvapply(function(x, y) max(x$Depth) / y, ses, ALONG = list(y = dvDmax)) == 1)
# notice the difference with "..." arguments that are constant along periods
all(dvapply(function(x, y, z) max(x$Depth) / y * z, ses, ALONG = list(y = dvDmax), z = 2) == 2)
# this is also valid because "ALONG" arguments are recycled if necessary.
all(dvapply(function(x, y, z) max(x$Depth) / y * z, ses, ALONG = list(y = dvDmax, z = 2)) == 2)
# Throw a warning because length(IDX) \%\% length(z) != 0 but still works.
table(dvapply(function(x, y, z) max(x$Depth) / y * z, ses, ALONG = list(y = dvDmax, z = 2:3)))
# length(IDX) \%\% length(z) != 0 & length(IDX) < length(z) is a special case:
# z is truncated rather than recycled.
table(dvapply(function(x, z) z, ses, ALONG = list(z = c(rep(2, length(dvDmax)), 3))))
mapply(paste0, LETTERS[1:3], letters[1:3], 1:4) # For comparison with mapply behaviour
}

