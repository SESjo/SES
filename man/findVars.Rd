\name{findVars}
\alias{findVars}
\title{existVar}
\usage{
  findVars(vars, obj)
}
\arguments{
  \item{vars}{The list of variable to search and create in
  the current environment.}

  \item{obj}{The object in which to search and grab the
  values.}
}
\description{
  Assign to given variable names the value of matching
  columns in an object. If no matching throw an error. If
  partial matching print a warning.
}
\examples{
x <- data.frame(A=1:3, B=letters[1:3])
fun <- function(x){
   # Check that variables 'A' and 'b' exists in object 'x'.
   # Print a warning for 'b' instead of 'B'.
   findVars(c("A", "b"), x)
   # These variable are created in the current environment
   cat("A: ", A, "\\nB: ", B)
   # 'C' does not exist in 'x'. Interupt execution.
   findVars("C", x)
 }
 fun(x)
}
\author{
  Yves
}
\seealso{
  \code{\link{existsVars}}
}

