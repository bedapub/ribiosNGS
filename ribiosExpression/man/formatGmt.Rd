\name{formatGmt}
\alias{formatGmt}
\title{
  Make GMT format strings
}
\description{
  Make strings in the GMT format
}
\usage{
formatGmt(title, comment, genes)
}
\arguments{
  \item{title}{Character, title(s) of gene set(s)}
  \item{comment}{Character, comment(s) of gene set(s). Can be of the
    same length as the \code{title}, or be of length one: in the latter
    case, it will be replicated in gene set. This option can also be
    left out: the comment field of the GMT file will be left blank.}
  \item{genes}{A character vector of gene names, or a list of such
    vectors. In the former case, one GMT line is produced; otherwise
    multiple lines are returned. In the latter case, the length of the
    list must match the length of \code{title}.}
}
\details{
  Resulting string(s) can be exported into \code{GMT} file by \code{\link{writeLines}}
}
\value{
  One or more lines of GMT file
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\examples{
formatGmt(title="GeneSet0", comment="My geneset", genes=c("MAPT", "MAPK", "AKT1"))
formatGmt(title="GeneSet0", genes=c("MAPT", "MAPK", "AKT1"))

formatGmt(title=c("GeneSet0", "GeneSet1"),
          comment=c("My geneset 0", "My geneset 1"),
          genes=list(c("MAPT", "MAPK", "AKT1"), c("EGFR", "CDC42")))
formatGmt(title=c("GeneSet0", "GeneSet1"),
          comment="My genesets",
          genes=list(c("MAPT", "MAPK", "AKT1"), c("EGFR", "CDC42")))
formatGmt(title=c("GeneSet0", "GeneSet1"),
          genes=list(c("MAPT", "MAPK", "AKT1"), c("EGFR", "CDC42")))

}
