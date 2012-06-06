\name{gseaFingerprint}
\alias{gseaFingerprint}
\alias{gseaFingerprintMatrix}
\title{
  Extract pathway fingerprints from GSEA results
}
\description{
  \code{gseaFingerprint} extracts pathway fingerprints from the result
  of one GSEA result. \code{gseaFingerprintMatrix} extracts multiple
  signatures and organizes into the form of rectangular matrix.
}
\usage{
gseaFingerprint(gseaDir, value = c("q", "es", "nes"), threshold = 1e-04,
sortByName = TRUE)
gseaFingerprintMatrix(gseaDirs, value = "es",...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{gseaDir}{Character, a GSEA output directory. Notice the directory must be
    accessible by the R session. A common mistake is to use a relative
    path which cannot be found.}
  \item{gseaDirs}{Character vector, GSEA output directories}
  \item{value}{Character, the statistic to extract, currently supporting
  \code{q}, \code{es} and \code{nes}}
  \item{threshold}{Numeric, minimum threshold of q-value, passed to \code{gseaQvalue}}
  \item{sortByName}{Logical, whether signatures should be sorted by
    name}
  \item{...}{Parameters passed to \code{gseaFingerprint} by \code{gseaFingerprintMatrix}}
}
\details{
  \code{gseaFingerprint} extracts pathway signature from one GSEA output
  directory. While \code{gseaFingerprintMatrix} simultaneously extracts
  from more than one GSEA output directories, and organizes pathway
  signatures in a rectangular matrix form.

  \code{gseaFingerprintMatrix} takes care of signature mapping between
  different GSEA result sets.
}
\value{
  \code{gseaFingerprint} returns a \code{data.frame} with two columns
  \code{name} and \code{value}, recording gene signature (pathway) names
  and the statistic chosen by the user.

  \code{gseaFingerprintMatrix} returns a \code{matrix}, with the union
  set of gene signatures from all GSEA output result sets as rows,
  and GSEA result names as columns. 
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\seealso{
  See \code{gseaQvalue} and \code{gseaES} for how to choose the
  statistic to produce pathway signatures.
}
\examples{
gsea.dir <- system.file(package="ribiosGSEA","extdata/gseaDirs/")
gsea.dirs <- dir(gsea.dir, full.names=TRUE)
gsea.fp <- gseaFingerprint(gsea.dirs[1], value="q")
gsea.fps <- gseaFingerprintMatrix(gsea.dirs, value="q")
}

