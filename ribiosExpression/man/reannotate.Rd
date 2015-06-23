\name{reannotate}
\alias{reannotate}
\title{
  Transform an ExpressionSet object of Bioc-annotation into of GTI-annotation
}
\description{
  The function is used to transform an ExpressionSet object, which is
  annotated by Bioconductor annotation packages, into an object with
  annotation information from GTI. 
}
\usage{
reannotate(object, check.target,...)
}
\arguments{
  \item{object}{An \code{ExpressionSet} object, with the
    \code{annotation} slot set as one of the valid annotations
    recognized by Bioconductor, for instance \code{hgu95av2}.}
  \item{check.target}{Logical,  with \code{FALSE} as default. When set to
    \code{TRUE}, beforing fetching database for annotations, the function
    first checks whether the chip type is supported by GTI. If it is not
    the case, the function will print error message and stop.}
  \item{\dots}{Currently not implemented}
}
\details{
  The translation between Bioconductor annotation package names and GTI
  chip types is performed by the \code{bioc2gti} function in the
  \code{ribiosAnnotation} package.

  Once the re-annotation succeeds, the \code{annotation} slot of the
  \code{ExpressionSet} object will be overwritten by the corresponding
  chip type name in GTI.
}
\value{
  An \code{ExpressionSet} object with feature annotations updated by
  GTI, and the \code{annotation} slot is changed to the chip type in GTI.
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\seealso{
  \code{\link{annotate}} to annotate an \code{ExpressionSet} object
  without prior information of bioc-annotation, or if that information
  is not saved in the \code{annotation} slot.
}
\examples{
data(ribios.ExpressionSet)
print(ribios.ExpressionSet)

\dontrun{
gti.ExpressionSet <- reannotate(ribios.ExpressionSet)
gti.ExpressionSet <- reannotate(ribios.ExpressionSet, check.target=FALSE)
print(gti.ExpressionSet)
}
}

