\name{SESplot.statdives}
\alias{SESplot.statdives}
\title{SESplot.statdives}
\usage{
\method{SESplot}{statdives}(obj, colorvar = NULL, cond = NULL,
  isobath = NULL, pts.args = list(), map.args = list(),
  img.args = list(), implt.args = list(), plt.args = list(), colorvarname)
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
}
\description{
Method for 'statdives' objects.
}
\details{
Includes bathymetry taken from
\url{https://www.ga.gov.au/products/servlet/controller?event=GEOCAT_DETAILS&catno=71552}.
If this error show up 'data set ‘worldMapEnv’ not
found' try \code{require(maps)}.
}

