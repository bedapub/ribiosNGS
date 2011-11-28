\name{fmecor}
\alias{fmecor}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Fast and Memory-Efficient correlation}
\description{Fast and Memory-Efficient correlation}
\usage{
fmecor(mat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mat}{Matrix without NA}
}
\details{
  This function is not yet fully finished. As of 28.11.2011,
  (1) Correlation coefficients are calculated row-wise
  (2) It is about 4x slower than the native cor C routine
  (3) When input matrix is large (2000), the program returns error
  stating the C stack is near to the limit; when the matrix is larger
  (2000), there is a core dump
  (4) It is not yet clear how to access the referenced object (a numeric
  vector in this case) from R.
}
\value{
  A correlation matrix
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\examples{
set.seed(1887); fN<-50
test.matrix <- matrix(rnorm(fN*fN, mean=2, sd=5), nrow=fN)
system.time(print(object.size(test.cor <- fmecor(test.matrix))))
rm(test.matrix)
}
