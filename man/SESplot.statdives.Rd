\name{SESplot.statdives}
\alias{SESplot.ses}
\alias{SESplot.statdives}
\title{Plot the trajectory of a SES along with dive statistics.}
\usage{
\method{SESplot}{statdives}(obj, colorvar = NULL, cond = NULL,
  isobath = NULL, pts.args = list(), map.args = list(),
  img.args = list(), implt.args = list(), plt.args = list(), colorvarname)

\method{SESplot}{ses}(obj, colorvar = NULL, cond = NULL, isobath = NULL,
  pts.args = list(), map.args = list(), img.args = list(),
  implt.args = list(), plt.args = list(), ...)
}
\arguments{
  \item{obj}{An object to plot}

  \item{colorvar}{Optionnal \code{obj} variable to plot
  with the color of symbols.}

  \item{cond}{Optionnal logical vector to subset the rows
  of \code{obj}.}

  \item{isobath}{Isobaths to draw (Bathymetry) . Default is
  NULL (none). Isobath at -1000 m corresponds
  approximatively to the Kerguelen shell boarder. Slow down
  the function (shapefile loading). Available isobaths
  range from -200 to -4400 meters by 200 m.}

  \item{pts.args}{Other parameters to be passed to
  \code{\link{points}}. Defaults: \code{list(pch = 19, cex
  = .1)}.}

  \item{map.args}{Other parameters to be passed to
  \code{\link{map}}. Defaults: \code{list(fill = TRUE, col
  = "gray")}.}

  \item{img.args}{Other parameters to be passed to
  \code{\link{as.image}}. Notice the possibility to set
  \code{nx} and \code{ny} arguments.}

  \item{implt.args}{Other parameters to be passed to
  \code{\link{image.plot}}. Defaults:
  \code{list(legend.width = 1, legend.mar = 7.8,
  legend.shrink = .7, legend.lab =
  deparse(substitute(colorvar)), legend.line = 3.5)} with
  \code{par(mar = c(4.5,4.5,3.5,8.5))}. Notice the
  possibility to set \code{zlim}, \code{col}, \code{breaks}
  arguments.}

  \item{plt.args}{Other parameters to be passed to
  \code{\link{plot}}. Notice the possibility to set
  \code{xlim} and \code{ylim} arguments.}

  \item{colorvarname}{Name for the legend. For
  compatibility when called from \code{SESplot.ses()}}

  \item{...}{Other arguments to be passed to
  \code{SESplot.statdives}.}
}
\description{
S3 methods for objects of class 'ses' and 'statdives'. The
'ses' method is actually a call to the 'statdives' method
but adds additional information such as the name of the
individual in the title.
}
\details{
Includes bathymetry taken from
\url{https://www.ga.gov.au/products/servlet/controller?event=GEOCAT_DETAILS&catno=71552}.
In the case where this error show up \code{"data set
'worldMapEnv' not found"} try \code{require(maps)}.
}
\examples{
\dontrun{
require(maps)
path <- system.file("extdata", package="SES")
pathname <- file.path(path, "2011-16_SES_example_accelero.mat")
ses <- importSES(pathname)

# Example: class 'ses' / 'statdives'
SESplot(ses, isobath=-1000, colorvar=ses$stat$Catch.numb)
library("RColorBrewer")
mycol <- brewer.pal(n=10, name="BrBG")
SESplot(ses, ses$stat$Dive.dur, pts.args=list(pch=""), implt.args=list(col=mycol),
img.args=list(nx=150, ny=150))
}
}

