\name{seqs}
\alias{seqs}
\title{seqs}
\usage{
  seqs(vec)
}
\arguments{
  \item{vec}{The atomic vector to examine.}
}
\value{
  A data frame with indexing sequences with start and end
  index. Value and length of the sequences are also given
}
\description{
  Identify the homogenious sequences along an atomic
  vector.
}
\examples{
x <- unname(unlist(mapply(rep, letters[1:10], 10:1)))
x
seqs(x)
}
\author{
  Yves
}

