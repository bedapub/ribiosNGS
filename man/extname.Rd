\name{extname}
\alias{extname}
\title{
  Get the extension(s) of file name(s)
}
\description{
  Many files have extensions in their names, for instance the \code{pdf}
  in the file name \code{mybook.pdf}. The \code{extname} function
  extracts this bit of information from one or more file names.
}
\usage{
extname(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Character vector of file names; other classes will be
    coereced to characters}
}
\value{
  The file name extension as characters, of the same length as the input
  file name character. In case that a file name does not contain a
  extension, \code{NA} will be returned.
}
\note{
  In case there are multiple dots in the input file name, the last field
  will be taken as the extension. For instance for file
  \code{test.out.txt}, the returned extension is \code{txt}.
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\examples{
extname("mybook.pdf")
extname("sequence.in.fasta")
extname(c("/path/mybook.pdf", "test.doc"))
extname("README")
extname("/path/my\ home/Holiday Plan.txt")
}

