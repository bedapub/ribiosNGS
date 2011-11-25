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
  t.b.a
}
\value{
  A correlation matrix
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\examples{
library(ribiosUtils)
set.seed(1887); fN<-50
test.matrix <- matrix(rnorm(fN*fN, mean=2, sd=5), nrow=fN)
system.time(print(object.size(test.cor <- fmecor(test.matrix))))
rm(test.matrix)
}
