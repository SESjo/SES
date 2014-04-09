\name{SESplot}
\alias{SESplot}
\title{Plots designed for SES objects.}
\usage{
SESplot(obj, colorvar = NULL, isobath = NULL, ...)
}
\arguments{
  \item{...}{Other parameters to be passed to methods.}

  \item{obj}{An object to plot}

  \item{colorvar}{Optionnal \code{obj} variable to plot
  with the color of symbols.}

  \item{isobath}{Isobaths to draw (Bathymetry) . Default is
  NULL (none). Isobath at -1000 m corresponds
  approximatively to the Kerguelen shell boarder. Slow down
  the function (shapefile loading). Available isobaths
  range from -200 to -4400 meters by 200 m.}
}
\value{
The plot type depends on the class of the input object. The
different types are described in the help of the
correponding methods, please refer to the "See also"
section.
}
\description{
\code{SESplot} is a generic function to draw relevant
charts from the objects returned by \code{importSES()}.
}
\details{
Includes bathymetry taken from
\url{https://www.ga.gov.au/products/servlet/controller?event=GEOCAT_DETAILS&catno=71552}.
Available isobaths range from -200 to -4400 meters by 200
m.
}
\seealso{
\code{\link{SESplot.statdives}},
\code{\link{SESplot.tdr3D}}
}

