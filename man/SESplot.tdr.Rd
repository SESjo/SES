\name{SESplot.tdr}
\alias{SESplot.tdr}
\title{SESplot.tdr}
\usage{
  \method{SESplot}{tdr} (obj, colorvar = NULL, cond = NULL,
    isobath = NULL, plt.args = list(),
    colscale.args = list(), implt.args = list())
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

  \item{colscale.args}{Other parameters to be passed to
  \code{\link{color.scale}}. Defaults: \code{list()}.}

  \item{implt.args}{Other parameters to be passed to
  \code{\link{image.plot}}. Defaults:
  \code{list(legend.width = 1, legend.mar = 7.8,
  legend.shrink = .7, legend.lab = colorvarname,
  legend.line = 3.5)}.}
}
\description{
  A method for \code{SESplot()} with tdr objects.
}
\details{
  With a \code{colorvar} of type 'integer', 'logical' or
  'factor' color the scale can be controled with
  \code{palette()} while type 'double' must be handled with
  \code{colscale.args} argument.
}

