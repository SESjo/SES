\name{findVars}
\alias{findVars}
\title{findVars}
\usage{
findVars(vars, object, varnames = NULL, type = c("assign", "check"),
  mult = FALSE, envir = parent.frame(), ...)
}
\arguments{
  \item{vars}{The variables to search and to create in the
  current environment.}

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
Search and/or get variables in an object using its name. To
use for error handling and initial checking in functions.
}
\examples{
x <- list(A=1, a=2, B=list(b=3, C=4, ca=5, abc=6, c=7))
# Example 1: Basic matching
findVars("C", x, ignore.case=FALSE) # Finds 'C'
findVars("c", x, mult=TRUE) # Finds 'C' 'ca' 'abc' 'c'
findVars("c", x, ignore.case=FALSE, substring=FALSE) # Finds 'c'
findVars("c", x, substring=FALSE, mult=TRUE, type="check") # check that 'C' 'c' exist without creating them

# Example 2: Regular expressions
findVars("c$", x, ignore.case=FALSE) # Finds 'ac' 'c'
\dontrun{
# Equivalents
findVars("c$", x, ignore.case=FALSE, mult=TRUE) # idem
findVars("c$", x, ignore.case=FALSE, varnames=c('C', 'AC')) # idem with custom names for assignment
}

# Example 3: Errors and warnings
\dontrun{
findVars("a", x) # Error
# Because you might want one of the following:
findVars("a", x, ignore.case=FALSE)
findVars("A", x, ignore.case=FALSE)
# Or both:
findVars(c("a", "A"), x, ignore.case=FALSE)
findVars("a", x, mult=TRUE) # Also possible but prints a warning.

# Notice the error when matching elements are not at same depth:
findVars(c("b", "B"), x, ignore.case=FALSE) # Works
findVars("b", x, mult=TRUE) # Error
}
}
\seealso{
Other checkUtils: \code{\link{checkVar}};
\code{\link{findDefaultVars}}; \code{\link{findVar}}
}

