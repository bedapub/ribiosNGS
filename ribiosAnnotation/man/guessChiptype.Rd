\name{guessChiptype}
\alias{guessChiptype}
\title{Guess likely chiptype(s) from probesets}
\description{
  Given a vector of probesets, guess the most likely chiptype from which
  these probesets come from. This function can be used in situations
  where probesets are known to be all from a single, unknown chip type.
}
\usage{
guessChiptype(ids, maxOnly = FALSE, sample = 100)
}
\arguments{
  \item{ids}{Character vector, probeset IDs}
  \item{maxOnly}{Logical, should only the chiptype to which most
    probesets were mapped be returned? By default \code{FALSE}, namely
    all chiptypes are returned which at least one probeset could be mapped.}
  \item{sample}{Integer to sample probesets. See \code{details} below.}
}
\details{
  The function is used where a set of probesets have to be annotated,
  and they are probably coming from the same chip type. If they may come
  from different chips, use \code{\link{annotateAnyProbeset}} instead;
  if the chip type from which they come is known, use
  \code{\link{gtiChipAnnotation}} which is faster.
  
  The function works as follows: first it samples a subset of input
  probesets (given by the \code{sample} parameter, in order to speed up
  the function and to save computational resources). Then it determines
  via SQL query which chip types contain these probesets. All types
  containing at least one probeset as in the input are returned, and
  they are sorted by the number of matching probesets. If \code{maxOnly}
  is \code{TRUE}, only the most likely chip type is returned.
}
\value{
  Character vector containing the chip types, with an attribute named
  \code{count} recording number of matched probesets in the chiptypes.
}
\author{
    Jitao David Zhang <jitao_david.zhang@roche.com>
}
\seealso{
  \code{\link{annotateProbesets}}, \code{\link{gtiChipAnnotation}}, \code{\link{annotateAnyProbeset}}.
}
\examples{
options(error=utils::recover)
guessChiptype(ids=c("1399067_at","1378257_at", "1380581_at"))
guessChiptype(ids=c("1399067_at","1378257_at", "1380581_at"), maxOnly=TRUE)
options(error=NULL)
}

