\name{annotate}
\alias{annotate}
\title{
  Annotate ExpressionSet or probesets
}
\description{
  The function annotates an object of \code{ExpressionSet}, or a vector
  of characters representing probesets.
}
\usage{
annotate(object, target, check.target, ...)
}
\arguments{
  \item{object}{An object of \code{ExpressionSet}, or a character vector
  of probesets}
  \item{target}{Chip type to be annotated}
  \item{check.target}{Logical, with \code{FALSE} as default. If set to \code{TRUE}, before looking up
    the annotations, it first check whether \code{target} is one of the
    valid chip types supported by GTI, and stops if it is not the
    case.}
  \item{\dots}{Currently not implemented}
}
\details{
  Once successfully annotated, the \code{annotation} slot of the
  \code{ExpressionSet} object is set to the value of \code{target}.
}
\value{
  An \code{ExpressionSet}, or a \code{data.frame} containing annotation
  information of the probesets.
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\note{
  Internal function \code{annChar} is called to annotate probesets, and
  the method for \code{ExpressionSet} calls the version for characters
  to do annotation.
}

\seealso{
  Internally the function \code{annotateProbesets} in the
  \code{ribiosAnnotation} package is used to connect to GTI and fetch
  annotation information.
}
\examples{
data(ribios.ExpressionSet)
myset <- ribios.ExpressionSet[100:105,]

## ExpressionSet
\dontrun{
annotate(myset, "HG_U95AV2")
annotate(myset, "HG_U_95AV2", check.target=TRUE)
}

## characters
\dontrun{
annotate(featureNames(myset), "HG_U95AV2")
}
}
