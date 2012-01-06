\name{annotate-methods}
\docType{methods}
\alias{annotate-methods}
\alias{annotate,ExpressionSet,character,ANY-method}
\alias{annotate,ExpressionSet,character,missing-method}
\alias{annotate,character,character,ANY-method}
\alias{annotate,character,character,missing-method}
\title{Annotate an object}
\description{
  Annotate an object.
}
\section{Methods}{
\describe{

\item{\code{signature(object = "ExpressionSet", target = "character",
    check.target = "logical")}}{
  Annotate an \code{ExpressionSet} with the chip type defined by the
  value of \code{target}. \code{check.target} determines whether the
  chip type should be first checked.
}
\item{\code{signature(object = "ExpressionSet", target = "character",
    check.target = "missing")}}{
  \code{check.target} is set to FALSE when missing.
}

\item{\code{signature(object = "character", target = "character", check.target="logical")}}{
  Treat \code{object} as a vector of probesets, which are to be
  annotated according to the information of chip specified by \code{target}
}

\item{\code{signature(object = "character", target = "character", check.target="missing")}}{
  \code{check.target} is set to FALSE when missing.
}

}}
\keyword{methods}
