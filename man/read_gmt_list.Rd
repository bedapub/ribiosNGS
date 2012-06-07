\name{read_gmt_list}
\alias{read_gmt_list}
\alias{read_gmt}
\title{
  Calling C routine to read GMT file into a list
}
\description{
  The function \code{read_gmt_list} calls the C routine \code{read_gmt}
  to read GMT file into a list.
}
\usage{
read_gmt_list(gmt.file)
}
\arguments{
  \item{gmt.file}{Character, name of one gmt-format file}
}
\details{
  Empty lines or lines without genes are omitted.Empty fields in
  \dQuote{genes} are omitted as well.
}
\value{
A list, the length of which equals the number of genesets. Each list
contains three items:
\item{name}{Character, gene set name}
\item{description}{Character, gene set description}
\item{genes}{Character vector, genes in the set}
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\examples{
idir <- system.file("extdata", package="ribiosIO")
sample.gmt.file <- file.path(idir, "test.gmt")

test.gmt <- read_gmt_list(sample.gmt.file)
}
