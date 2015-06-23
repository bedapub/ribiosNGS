\name{gtiTaxAnnotation}
\alias{gtiTaxAnnotation}
\title{Annotations of all genes associated with the given TaxID
}
\description{
   The function returns annotations (see details below) of all features
   (probably probesets) associated with the given taxon.
}
\usage{
gtiTaxAnnotation(taxid)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{taxid}{Character string, the TaxID of the species in
    interest. For instance \sQuote{9606} for Homo sapiens.}
}
\details{
    The function reads from the backend Oracle database.
}
\value{
   A \code{data.frame} object with very similar structure as the
   \code{EG_GENE_INFO} table in the database.

   Rownames of the \code{data.frame} are set to \code{NULL}.
}
\references{
  \url{http://bioinfo.bas.roche.com:8080/bicgi/gti_ui.cgi}
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\seealso{
  \code{\link{gtiChipAnnotation}} for annotating chips.
}
\examples{
\dontrun{
hsAnno <- gtiTaxAnnotation("9606")
dim(hsAnno)
head(hsAnno)
}
}
