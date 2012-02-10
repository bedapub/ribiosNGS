\name{gtiChipAnnotation}
\alias{ORACLE.IN.NMAX}
\alias{gtiChipAnnotation}
\alias{raceChipAnnotation}
\alias{biosCurrentGeneSymbol}
\title{
  Annotations of all features associated with the given chip type
}
\description{
  The function returns annotations (see details below) of all or selected features
  (probably probesets) associated with the given chip type. 
}
\usage{
gtiChipAnnotation(chip, inCol, inValues)
}
\arguments{
  \item{chip}{Character string, the chip type in interest. For a
    complete list of supported chip names, use the
    \code{gtiChiptypes()} function.}
  \item{inCol}{Optional, one character string. If specified, the
    \code{inValues} values are selected in the column by the \code{IN}
    SQL syntax. Allowed values: \code{ProbeID}, \code{GeneID} and \code{GeneSymbol}}
  \item{inValues}{Optional, character or numeric vector. The vector used
    in the \code{IN} SQL query.}
}
\details{
  The function reads from the backend Oracle database.
  
  In cases where \code{inCol} and \code{inValues} are specified, the
  returning \code{data.frame} is \emph{not} necessarily sorted according
  to the input values.
  
  The length of the \code{inValues} vector must not exceed the constant
  variable \code{ORACLE.IN.NMAX}, which is Oracle-implementation specific
  and is set to 1000 in the current machine (montale). If there are more
  query identifiers, the filtering of \code{inCol} by \code{inValues} is
  done in \code{R} instead of in \code{SQL}.This requires the whole
  table to be transfered between SQL and R, causing potential
  performance issues.
  
  If the function reports error \dQuote{ORA-01722: invalid number}, it
  suggests that non-numeric values (e.g. characters) have been used as
  \code{inValues} to query against the \code{GeneID} column.
}
\value{
 A \code{data.frame} object with very similar structure as the
 \code{AFFYCHIP_XREF_LL} table in the database.

 The \code{data.frame} has currently following columns:
 \item{ProbeID}{Probeset ID}
 \item{GeneID}{Entrez Gene ID}
 \item{GeneSymbol}{Entrez (HUGO) Gene Symbol}
  \item{GeneName}{Gene descriptions}
 \item{isSingleGeneID}{Logical, whether the probeset is mapped to single
   gene id}
 \item{Chip}{Chip type}

 Rownames of the \code{data.frame} are set to \code{NULL}. They are not
 set to the \code{ProbeID} since in some cases these identifiers are not
 unique, making them unqualified for rownames.
}
\references{
\url{http://kephar.bas.roche.com/bioinfo/apps/chip/Chip_probes_GTI_interface.htm}
}
\author{
Jitao David Zhang <jitao_david.zhang@roche.com>
}
\seealso{
  \code{\link{gtiChiptypes}} for a complete list of supported chip
  types.
}
\note{
  From version 1.0-15 and on, \code{isSingleGeneID} is always set to
  \code{TRUE} since only uniquely mapped probesets are
  stored. Provisionally this column shall be removed in the future
  version.
}
\examples{
\dontrun{
mychip <- "HG-U133_PLUS_2"
mychipAnno <- gtiChipAnnotation(mychip)
dim(mychipAnno)
head(mychipAnno)
}

options(error=utils::recover)
gtiChipAnnotation(mychip, inCol="ProbeID",
  inValues=c("1053_at","117_at", "121_at"))
gtiChipAnnotation(mychip, inCol="GeneID",
  inValues=c("1432","1", "2357"))
gtiChipAnnotation(mychip, inCol="GeneSymbol",
  inValues=c("UBA7","MAPK14", "RFC2"))
options(error=NULL)
}
