\name{existsVars}
\alias{existsVars}
\title{existVar}
\usage{
  existsVars(vars, obj, idx = TRUE)
}
\arguments{
  \item{vars}{The list of variable names.}

  \item{obj}{The object in which to search.}

  \item{idx}{Should the indexes of matching columns be
  returned ?}
}
\description{
  Search for variables in an object. Throw an error if not
  when a variable is not found. Print a warning if partial
  matching only.
}
\author{
  Yves
}
\seealso{
  \code{\link{findVars}}
}

