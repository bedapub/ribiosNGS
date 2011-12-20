\name{annotateProbesets}
\alias{annotateProbesets}
\alias{annotateProbes}
\title{
  Annotate probesets by their identifiers (ID) and chip type
}
\description{
  Annotate probesets in chip types supported, as listed by the
  \code{\link{gtiChiptypes}} function.
}
\usage{
annotateProbesets(ids, chip = "HG-U133_PLUS_2")
}
\arguments{
  \item{ids}{Probeset identifiers}
  \item{chip}{Chip type identifier}
}
\details{
  The concept \sQuote{Probesets} here can either refer to a set of
  probes, or a single probe, depending on manufacturing techniques used
  by the microarray vendor. As long as the chip type is supported, and
  the ID is listed in the \code{affychip_xref_ll} table in Oracle, they
  can be annotated.
}
\value{
  A \code{data.frame} annotating probesets.

  Rows are in the same order as of the given probesets. When the input
  ids are unique, they are used as row names of the resulting
  \code{data.frame}; otherwise the row names is set to \code{NULL},
  which when printed will be shown as incremental integers.
}
\references{
\url{http://bioinfo.bas.roche.com:8080/apps/affychip/}
}
\author{
Jitao David Zhang <jitao_david.zhang@roche.com>
}
\note{
  Internally the function calls the \code{gtiChipAnnotation} to first
  query all annotations about the given chip. Subsequently they are
  matched against queried probesets, returning their annotations only.
}

\seealso{
  See \code{\link{gtiChiptypes}} for supported chip types.

  See \code{\link{gtiChipAnnotation}} to get annotation for all
  probesets in a chip.
}
\examples{
\dontrun{
myprobes <- c("1000_at", "1004_at", "1002_f_at", "nonsense_at")
annotateProbesets(myprobes, chip="HG_U95A")

myprobes2 <- c( "Hs2.356162.1.S1_3p_at","g4885506_3p_at",
"Hs.210202.0.A1_3p_at", "Hs.166262.0.A1_3p_x_at",
"Hs.48499.1.S1_3p_a_at", "Hs.143268.0.A1_3p_x_at",
"Hs.86386.1.A1_3p_at", "g11497042_3p_a_at", "Hs.279556.1.S2_3p_at",
"Hs.83484.1.A1_3p_at")
annotateProbesets(myprobes2, chip="U133_X3P")

## when using wrong chip it is likely that many (if not all)
## features will not be annotated
annotateProbesets(myprobes2, chip="HGU_95A")
}
}
