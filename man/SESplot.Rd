\name{SESplot}
\alias{SESplot}
\title{SESplot}
\usage{
  SESplot(obj, colorvar = NULL, isobath = NULL, ...)
}
\arguments{
  \item{obj}{An object to plot}

  \item{...}{Other parameters to be passed to methods.}
}
\description{
  To draw relevant chart of a 'ses-class' object (generated
  by importSES) such as tdr', 'statdives', 'tdr3D' or
  'ststdives3D'.
}
\details{
  Includes bathymetry taken from
  \url{https://www.ga.gov.au/products/servlet/controller?event=GEOCAT_DETAILS&catno=71552}.
  Available isobaths range from -200 to -4400 meters by 200
  m.
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
SESplot(ses, ses$stat$Dive.dur, pts.args=list(pch=""), implt.args=list(col=mycol), img.args=list(nx=150, ny=150))

# Example: class 'tdr'
SESplot(ses$tdr, colorvar=ses$tdr$Light, cond=ses$tdr$Dive.id == 650)
SESplot(ses$tdr, colorvar=ses$tdr$is.Catch, cond=ses$tdr$Dive.id == 650)

# No provided data for 'ses3D': Syntax memo
# SESplot(ses3d) # Same as for classic 'ses' class
# 3D-Scatterplot of the 1015th dive with colored variable.
# SESplot(ses3d$tdr, cond=ses3d$tdr$Dive.id==1015, colorvar=ses3d$tdr$Distance)
}
}
\seealso{
  Other SESplot: \code{\link{SESplot.ses}},
  \code{\link{SESplot.statdives}},
  \code{\link{SESplot.tdr3D}}
}

