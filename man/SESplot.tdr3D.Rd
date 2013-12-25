\name{SESplot.tdr3D}
\alias{SESplot.tdr3D}
\title{SESplot.tdr3D}
\usage{
  SESplot.tdr3D(obj, colorvar = NULL, cond = NULL,
    isobath = NULL, scatter.args = list(),
    colscale.args = list(), implt.args = list())
}
\arguments{
  \item{scatter.args}{Other parameters to be passed to
  \code{scatterplot3d}. Defaults: \code{list(pch = 19,
  cex.symbol = .1, mar = c(4.2,4.2,3.5,9)))}}

  \item{colscale.args}{Other parameters to be passed to
  \code{\link{color.scale}}. Defaults: \code{list()}.}

  \item{implt.args}{Other parameters to be passed to
  \code{\link{image.plot}}. Defaults:
  \code{list(legend.width = 1, legend.mar = 7.8,
  legend.shrink = .7, legend.lab = colorvarname,
  legend.line = 3.5)}.}

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
}
\description{
  A method for \code{SESplot()} with tdr3D objects.
}
\details{
  With a \code{colorvar} of type 'integer', 'logical' or
  'factor' color the scale can be controled with
  \code{palette()} while type 'double' must be handled with
  \code{colscale.args} argument.
}
\examples{
\dontrun{
SESplot(ses3d$tdr, cond=ses3d$tdr$Dive.id==1015, colorvar=ses3d$tdr$Distance)
# Use manipulate to change view angle
require(manipulate)
 manipulate(SESplot(ses3d$tdr, cond=ses3d$tdr$Dive.id==2500,
  scatter.args=list(angle=angle)), angle=slider(0, 360))
}
}
\seealso{
  Other SESplot: \code{\link{SESplot}},
  \code{\link{SESplot.ses}},
  \code{\link{SESplot.statdives}}
}

