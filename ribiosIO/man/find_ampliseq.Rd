\name{find_ampliseq}
\alias{find_ampliseq}
\alias{read_ampliseq}
\alias{find_and_read_ampliseq}
\title{
  Find and read-in AmpliSeq files 
}
\description{
  Find and read-in AmpliSeq files into an expression matrix
}
\usage{
find_ampliseq(dir)
read_ampliseq(files)
find_and_read_ampliseq(dir)
}
\arguments{
  \item{dir}{The top-level directory where a AmpliSeq run is saved. An
    example:
    \sQuote{/data64/sequencing/iontorrent_data/Auto_user_PR1-139-AmpliSeqRNA_pathway_FD14_277_360/}}
  \item{files}{AmpliSeq files, potentially found by \code{find_ampliseq}}
}
\details{
  Directory is recursively checked for files that match the name pattern
  \sQuote{*.cov.xls} (cov means coverage). Invalid links (judged by file
  size) are excluded.

  Only data of total read counts are read-in.
}
\value{
  \code{find_ampliseq} returns a character vector of full names of valid
  files.

  \code{read_ampliseq} returns a numeric matrix of gene expression in
  counts. Row names are unique gene names.

  \code{find_and_read_ampliseq} combines the two functions and returns
  the expression matrix as \code{read_ampliseq} does.
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\examples{
ampdir <- system.file("extdata/ampliseq-data", package="ribiosIO")
ampfiles <- find_ampliseq(ampdir)
ampmat <- read_ampliseq(ampfiles)

ampmat.onestep <- find_and_read_ampliseq(ampdir)
}
