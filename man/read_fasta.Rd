\name{read_fasta}
\alias{read_fasta}
\alias{write_fasta}
\title{
  Read (write) FASTA sequences into (from) named character vectors
}
\description{
  \code{read_fasta} reads sequences in FASTA format in named character
  vectors. \code{write_fasta} writes sequences stored as named character
  vectors into FASTA file.
}
\usage{
read_fasta(file)
write_fasta(x, file)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{FASTA format file}
  \item{x}{Named characters}
}
\details{
  Names of sequences to be written do not have to begin with the
  greater-than sign, as they are appended by the function when
  writing. Similarly, the \code{read_fasta} removes the leading
  greater-than sign of sequence names.
}
\value{
  For \code{read_fasta}, a named character vector of FASTA sequences.

  For \code{write_fasta}, the side effect is used and no value is returned.
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}

\examples{
tmpfile <- tempfile()
test.seq <- c("mySeq1"="ATGCG", "mySeq2 correct"="TTGTTCGACGT")
write_fasta(test.seq, tmpfile)
read_fasta(tmpfile)
}

