\name{ribios.ExpressionSet}
\alias{ribios.ExpressionSet}
\docType{data}
\title{
  An \code{ExpressionSet} for case demonstrations
}
\description{
  This object is adapted from the \code{sample.ExpressionSet} object,
  with feature annotations from GTI (Data stand: December 2011). It is
  used in case studies where functionalities of the
  \code{ribiosExpression} package are demonstrated.
}
\usage{data(ribios.ExpressionSet)}
\format{
  An \code{ExpressionSet} object.
}
\references{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\examples{
data(ribios.ExpressionSet)
tbl <- eSetToLongTable(ribios.ExpressionSet)
}
\keyword{datasets}
