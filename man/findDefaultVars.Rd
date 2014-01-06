\name{findDefaultVars}
\alias{findDefaultVars}
\title{findDefaultVars}
\usage{
findDefaultVars(vars, object, type.obj, varnames = NULL, type = c("assign",
  "check"), mult = FALSE, envir = parent.frame(1), ...)
}
\arguments{
  \item{vars}{The variables to search called by their
  default name.}

  \item{type.obj}{The type of object in wich to look for
  the user name. To choose in \code{c("tdr", "stat",
  "tdr3D", "stat3D")}.}

  \item{object}{The object supposed to contain the
  variables.}

  \item{varnames}{An Optional atomic vector of type
  'character'. The names to give to the variables found
  when assigning them to the current environment. Default
  is the same as \code{vars}.}

  \item{type}{To choose in \code{c("assign", "check")}. The
  second choice checks run the process but do not assign
  thematching variables in the environment.}

  \item{mult}{Should the multiple matching of a variable
  name be allowed.}

  \item{envir}{The environement in with the matching
  variables should be assigned. Default is the enviromnent
  in which the function is called.}

  \item{...}{Other arguments to be passed to
  \code{checkVar}.}
}
\description{
Search and/or get variables in an object using its default
name as it is given in formatSES. To use for error handling
and initial checking in functions.
}
\seealso{
Other checkUtils: \code{\link{checkVar}};
\code{\link{findVars}}; \code{\link{findVar}}
}
\keyword{internal}

