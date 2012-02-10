\name{chrLen}
\alias{chrLen}
\title{
  Get chromosome lengths in basepairs
}
\description{
  Get chromosome lengths (in basepairs) of the organism specified.
}
\usage{
chrLen(organism = c("human", "mouse", "rat"))
}
\arguments{
  \item{organism}{Organism to query, supporting human, mouse and rat}
}
\value{
  A \code{data.frame} containing chromosome length information.
  \item{Chromosome}{Chromosome names (without the \code{CHR} prefix as
    in the database}
  \item{Description}{Description}
  \item{Length}{Length of the chromosome, in basepairs (bp)}
  \item{Rank}{Rank of chromosomes by lengths}
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>, Isabelle Wells <isabelle.wells@roche.com>
}
\examples{
\dontrun{
chrLen("human")
chrLen("mouse")
}
}
