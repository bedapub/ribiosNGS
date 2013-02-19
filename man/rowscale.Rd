\name{rowscale}
\alias{rowscale}
\title{Scale a matrix by row}
\description{
  Scale a matrix by row
}
\usage{
rowscale(object, center, scale)
}
\arguments{
  \item{object}{An matrix}
  \item{center}{Logical, passed to \code{scale}. If missing the value is
  set to \code{TRUE}}
  \item{scale}{Logical, passed to \code{scale}. If missing the value is
  set to \code{TRUE}}
}
\details{
  Scaling a matrix by row can be slightly slower due to a transposing step.
}
\value{
  A matrix with each row scaled.
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\seealso{
\code{\link{scale}}
}
\examples{
mat <- matrix(rnorm(20), nrow=4)
rs.mat <- rowscale(mat)

print(mat)
print(rs.mat)
rowMeans(rs.mat)
apply(rs.mat, 1L, sd)

rowscale(mat, center=FALSE, scale=FALSE) ## equal to mat
rowscale(mat, center=TRUE, scale=FALSE)
rowscale(mat, center=FALSE, scale=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
