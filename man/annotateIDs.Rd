\name{annotateIDs}
\alias{annotateIDs}
\title{
  Internal function to annotate IDs with table returned by gtiChipAnnotation
}
\description{
  This function is used internally, by \code{annotateProbesets},
  \code{annotateGeneIDs} and \code{annotateGeneSymbols} to annotate IDs
  with a table returned by the \code{gtiChipAnnotation} function.
}
\usage{
annotateIDs(ids, chip, column)
}
\arguments{
  \item{ids}{Identifiers to be queried}
  \item{chip}{Chip type}
  \item{column}{Column name, to which identifiers are mapped to}
}
\details{
  If IDs are duplicated or has \code{NA} values, the returned annotation
  table will have \code{NULL} as rownames. Otherwise \code{ids} will be
  used.
}
\value{
  A \code{data.frame} containing annotations.
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\seealso{
  \code{\link{annotateProbesets}},  \code{\link{annotateGeneIDs}} and \code{\link{annotateGeneSymbols}}.
}
\examples{
\dontrun{
annotateIDs(ids=c("GI_38327631-A", "GI_31563533-I", "GI_42822885-S", NA),chip="HUMAN_REFSEQ-8", column="ProbeID")
annotateIDs(ids=c("780", "5982", "3310", NA),chip="HUMAN_REFSEQ-8", column="GeneID")
annotateIDs(ids=c("DDR1", "RFC2", "HSPA6", "HSAP6"),chip="HUMAN_REFSEQ-8", column="GeneSymbol")
}
}
% internal keyword: since annotateIDs is used internally
\keyword{ internal }

