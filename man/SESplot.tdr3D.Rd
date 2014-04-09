\name{SESplot.tdr3D}
\alias{SESplot.tdr}
\alias{SESplot.tdr3D}
\title{Plot dive profiles along with a TDR variable}
\usage{
\method{SESplot}{tdr3D}(obj, colorvar = NULL, cond = NULL, isobath = NULL,
  scatter.args = list(), colscale.args = list(), implt.args = list(),
  colorvarname, dev = c("rgl", "default"))

\method{SESplot}{tdr}(obj, colorvar = NULL, cond = NULL, isobath = NULL,
  plt.args = list(), colscale.args = list(), implt.args = list())
}
\arguments{
  \item{scatter.args}{Other parameters to be passed to
  \code{plot3d} if \code{dev} is set to \code{'rgl'}, to
  \code{scatterplot3d} otherwise. Defaults: \code{list(pch
  = 19, cex.symbol = .1, mar = c(4.2,4.2,3.5,9)))}}

  \item{colscale.args}{Other parameters to be passed to
  \code{\link{color.scale}}. Defaults: \code{list()}.}

  \item{implt.args}{Other parameters to be passed to
  \code{\link{image.plot}}. Defaults:
  \code{list(legend.width = 1, legend.mar = 7.8,
  legend.shrink = .7, legend.lab = colorvarname,
  legend.line = 3.5)}.}

  \item{dev}{The device to use: to choose in \code{c('rgl',
  'default')}. See consequences in details.}

  \item{plt.args}{Other parameters to be passed to
  \code{plot}.}

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

  \item{colorvarname}{Name for the legend. For
  compatibility when called from \code{SESplot.ses()}}
}
\description{
S3 methods for objects of class 'tdr' and 'tdr3d'. To plot
dive profiles. Any TDR variable may be included to the
graph using a color scale. 3D dives are drawn in a dynamic
'rgl' device.
}
\details{
With a \code{colorvar} of type 'integer', 'logical' or
'factor' color the scale can be controled with
\code{palette()} while type 'double' must be handled with
\code{colscale.args} argument. When \code{dev} is set
\code{'rgl'} the figure can be rotated and zoomed
interactively but no color legend can be added.
}
\examples{
\dontrun{
path <- system.file("extdata", package="SES")
pathname <- file.path(path, "2011-16_SES_example_accelero.mat")
ses <- importSES(pathname)

# Example: class 'tdr'
SESplot(ses$tdr, colorvar = ses$tdr$Light, cond = ses$tdr$Dive.id == 650)
SESplot(ses$tdr, colorvar = ses$tdr$is.Catch, cond = ses$tdr$Dive.id == 650)

# Example: class 'tdr3D'
#    No data sample provided for 'ses3D': Syntax memo
SESplot(ses3d) # Same as for classic 'ses' class
3D-Scatterplot of the 1015th dive with colored variable.
SESplot(ses3d$tdr, cond = ses3d$tdr$Dive.id == 1015, colorvar = ses3d$tdr$Distance)
}
}

