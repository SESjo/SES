\name{checkVar}
\alias{checkVar}
\title{Search for a variable in a list}
\usage{
checkVar(var, object, mult = FALSE, substring = TRUE, ignore.case = TRUE,
  ignore.depth.error = FALSE)
}
\arguments{
  \item{var}{Character givin the name of the variable to
  search.}

  \item{object}{The object in which to search.}

  \item{mult}{Should the multiple matching of a variable
  name be allowed.}

  \item{substring}{If \code{FALSE} then check that the
  \code{pattern} and the matching variables have the same
  number of characters.}

  \item{ignore.case}{Should case variants of the pattern be
  investigated ?}

  \item{mult}{Is \code{var} a regular expression pattern to
  match several elements in \code{object} variable names ?}

  \item{ignore.depth.error}{Should the function check for
  ambiguities with the names of deeper element ?}
}
\value{
the desired variable not simplified.
}
\description{
Check if a variable exists anywhere in a 'list' object
using its name. Print messages, warning or errors according
according to the gravity of ambiguous situations. Include
various arguments letting specify how should be matched
these names.
}
\examples{
l <- list(a=1, b=list(c=2, cD=3))
checkVar("a", l)
checkVar("b", l)
checkVar("c", l, substring=FALSE) # Select only 'c'
\dontrun{
checkVar("d", l) # Print a warning
checkVar("c", l) # Print an error
}
}
\seealso{
Other checkUtils: \code{\link{findDefaultVars}};
\code{\link{findVars}}; \code{\link{findVar}}
}
\keyword{internal}

