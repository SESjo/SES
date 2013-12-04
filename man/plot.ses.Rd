\name{plot.ses}
\alias{plot.ses}
\title{plot.ses}
\usage{
  plot.ses(ses, pch = 19, cex = 1, fill = TRUE,
    m.col = "gray", isobath = -seq(1000, 4000, 1000), ...)
}
\arguments{
  \item{ses}{An ses object.}

  \item{pch}{Plotting character.}

  \item{cex}{Plotting character size.}

  \item{fill}{Should the terrestrial area be filled with
  color.}

  \item{m.col}{The color to use in order to fill the
  terrestrial area.}

  \item{...}{Other parameters to be passed to \code{plot()}
  or \code{par()}.}
}
\description{
  A \code{plot()} method to draw relevant chart of a ses
  object.
}
\details{
  Bathymetry taken from
  \url{https://www.ga.gov.au/products/servlet/controller?event=GEOCAT_DETAILS&catno=71552}.
  Isobaths from -200 to -4400 metters.
}
\examples{
path <- system.file("extdata", package="SES")
pathname <- file.path(path, "2011-16_SES_example_accelero.mat")
ses <- importSES(pathname)
plot(ses)
}

