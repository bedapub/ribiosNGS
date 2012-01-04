\name{kendallW-methods}
\docType{methods}
\alias{kendallW}
\alias{kendallW-methods}
\alias{kendallW,ExpressionSet-method}
\alias{kendallW,matrix-method}
\title{ Interface to \code{kendallWmat} }
\description{
  The S4 method acts as interface between advanced data structures (such
  as \code{ExpressionSet}) and the \code{kendallWmat} function. The
  function combines Kendall's W statistic with an iterative graph theory
  approach to detect sub-groups resembling strong correlations. 
}
\section{Methods}{
  \describe{
    \item{\code{signature(object = "matrix")}}{The method for
      \code{matrix} is just a wrapper for \code{kendallWmat}}
\item{\code{signature(object = "ExpressionSet")}}{This method accepts an
  \code{ExpressionSet} object, performs Kendall'W feature reduction, and
  store the sub-group information in the resulting object.
}
}}
\keyword{methods}
