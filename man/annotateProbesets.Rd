\name{annotateProbesets}
\alias{annotateProbesets}
\alias{annotateProbeIDs}
\title{
  Annotate probesets by their identifiers (ID) and chip type
}
\description{
  Annotate probesets in chip types supported, as listed by the
  \code{\link{gtiChiptypes}} function. If no chip type is given,
  the function will automatically map probesets to genes via GTI.
}
\usage{
annotateProbesets(ids, chip, orthologue=FALSE)
}
\arguments{
  \item{ids}{Probeset identifiers}
  \item{chip}{Chip type identifier, for example
    \code{HG-U133_PLUS_2}. If missing (or \code{NA}, \code{NULL},
    \dQuote{any} and empty string), automatic mapping is done via GTI.}
  \item{orthologue}{Logical, should be human orthologues be returned?}
}
\details{
  \code{annotateProbeIDs} is a synonym of \code{annotateProbesets}.
  
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

  If \code{orthologue} is set to \code{TRUE}, orthologue information is returned.
}
\references{
\url{http://bioinfo.bas.roche.com:8080/apps/gti/architecture/gtiora.html}
}
\author{
Jitao David Zhang <jitao_david.zhang@roche.com>
}
\note{
  If the \code{chip} option is given, internally the function calls the
  \code{gtiChipAnnotation} to first query all annotations about the
  given chip. Subsequently they are  matched against queried probesets,
  returning their annotations only.

  If no \code{chip} is given, the function tries to fetch annotation
  information from GTI tables.
}

\seealso{
  See \code{\link{gtiChiptypes}} for supported chip types.

  See \code{\link{gtiChipAnnotation}} to get annotation for all
  probesets in a chip.
}
\examples{
options(error=utils::recover)

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

## automatically mapping
annotateProbesets(c("1373268_at", "1002_f_at", "103416_at"))
annotateProbesets(c("1373268_at", "1002_f_at", "103416_at"), orthologue=TRUE)

options(error=NULL)
}
