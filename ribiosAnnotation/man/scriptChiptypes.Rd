\name{scriptChiptypes}
\alias{scriptChiptypes}
\title{Rscript supported chiptypes}
\description{
  A wrapper function for Rscripts to prompt supported chip types for annotation
}
\usage{
scriptChiptypes()
}

\details{
  The functions returns a vector of \code{gtiChiptypes()}, as well as
  \code{GeneID} and \code{GeneSymbol}, as the result. These are commonly
  used chiptypes in Rscripts. Depending on the user input,
  \code{annotateAnyID} can be called subsequently to annotate user-input
  features.
}
\value{
  A character vector of supported chip types.
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\seealso{
  \code{\link{gtiChiptypes}}, and \code{\link{annotateAnyID}}
}
\examples{
options(error=utils::recover)
scriptChiptypes()
options(error=NULL)
}
