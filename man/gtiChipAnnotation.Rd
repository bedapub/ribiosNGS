\name{gtiChipAnnotation}
\alias{gtiChipAnnotation}
\alias{raceChipAnnotation}
\alias{biosCurrentGeneSymbol}
\title{
  Annotations of all features associated with the given chip type
}
\description{
  The function returns annotations (see details below) of all features
  (probably probesets) associated with the given chip type. 
}
\usage{
gtiChipAnnotation(chip)
}
\arguments{
  \item{chip}{Character string, the chip type in interest. For a
    complete list of supported chip names, use the
    \code{gtiChiptypes()} function.}
}
\details{
  The function reads from the backend Oracle database.
}
\value{
 A \code{data.frame} object with very similar structure as the
 \code{AFFYCHIP_XREF_LL} table in the database.

 The \code{data.frame} has currently following columns:
 \item{ProbeID}{Probeset ID}
 \item{GeneID}{Entrez Gene ID}
 \item{GeneSymbol}{Entrez (HUGO) Gene Symbol}
 \item{isSingleGeneID}{Logical, whether the probeset is mapped to single
   gene id}
 \item{Chip}{Chip type}

 Rownames of the \code{data.frame} are set to \code{NULL}. They are not
 set to the \code{ProbeID} since in some cases these identifiers are not
 unique, making them unqualified for rownames.
}
\references{
\url{http://bioinfo.bas.roche.com:8080/apps/affychip/}
}
\author{
Jitao David Zhang <jitao_david.zhang@roche.com>
}
\seealso{
  \code{\link{gtiChiptypes}} for a complete list of supported chip
  types.
}
\examples{
\dontrun{
mychip <- "HG-U133_PLUS_2"
mychipAnno <- gtiChipAnnotation(mychip)
dim(mychipAnno)
head(mychipAnno)
}
}
