\name{annotate-methods}
\docType{methods}
\alias{annotate-methods}
\alias{annotate,ExpressionSet,character-method}
\alias{annotate,character,character-method}
\title{Annotate an object}
\description{
  Annotate an object.
}
\section{Methods}{
\describe{

\item{\code{signature(object = "ExpressionSet", target = "character")}}{
  Annotate an \code{ExpressionSet} with the chip type defined by the
  value of \code{target}.
}

\item{\code{signature(object = "character", target = "character")}}{
  Treat \code{object} as a vector of probesets, which are to be
  annotated according to the information of chip specified by \code{target}
}
}}
\keyword{methods}
