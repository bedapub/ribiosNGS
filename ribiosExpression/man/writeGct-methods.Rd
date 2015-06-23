\name{writeGct-methods}
\docType{methods}
\alias{writeGct-methods}
\alias{writeGct,ExpressionSet-method}
\alias{writeGct,matrix-method}
\title{
  writeGct methods
}
\description{
  Write matrix or ExpressionSet in GCT file format
}
\section{Methods}{
\describe{
  The S4-method is a wrapper of the \code{write_gct} function
  implemented in the \code{ribiosIO} package.
\item{\code{signature(obj = "ExpressionSet")}}{
  An \code{ExpressionSet} object.
}

\item{\code{signature(obj = "matrix")}}{
  A matrix
}
}}
\keyword{methods}
