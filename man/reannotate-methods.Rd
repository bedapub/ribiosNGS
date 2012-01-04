\name{reannotate-methods}
\docType{methods}
\alias{reannotate-methods}
\alias{reannotate,ExpressionSet-method}
\title{Re-annotate: Transform an ExpressionSet object of Bioc-annotation in GTI-annotation}
\description{
  The method accepts an object to re-annotate them. The action of 
  \sQuote{reannotating} defined in \code{ribiosExpression} is to
  transform an ExpressionSet object of Bioconductor-annotation to GTI-annotation.
}
\section{Methods}{
\describe{

\item{\code{signature(object = "ExpressionSet")}}{
Currently the method only support an \code{ExpressionSet} with
a valid Bioconductor annotation set (for example \dQuote{hgu95av2}) in
the \code{annotation} slot. See \code{\link{reannotate}} for details and examples.
}
}}
\keyword{methods}
