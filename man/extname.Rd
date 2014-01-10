\name{extname}
\alias{basefilename}
\alias{extname}
\title{
  Get the base and extension(s) of file name(s)
}
\description{
  Many files have base and extensions in their names, for instance for
  the file \code{mybook.pdf}, the base is \code{mybook} and the
  extension is \code{pdf}. \code{basefilename} \code{extname} functions
  extract these information from one or more file names.
}
\usage{
basefilename(x, lower.case=FALSE)
extname(x, ifnotfound=NA, lower.case=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Character vector of file names; other classes will be
    coereced to characters}
  \item{ifnotfound}{If no extension name was found, the value to be
    returned. Default is \code{NA}}
  \item{lower.case}{Logical, should the names returned in lower case?}
}
\value{
  The base file name or the extension as characters, of the same length as the input
  file name character. In case that a file name does not contain a
  extension, \code{NA} will be returned.
}
\note{
  In case there are multiple dots in the input file name, the last field
  will be taken as the extension, and the rest as the base name. For instance for file
  \code{test.out.txt}, returned base name is \code{test.out} and extension is \code{txt}.
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\examples{
extname("mybook.pdf")
extname("sequence.in.fasta")
extname(c("/path/mybook.pdf", "test.doc"))
extname("README")
extname("README", ifnotfound="")
extname("/path/my\ home/Holiday Plan.txt")

basefilename("mybook.pdf")
basefilename("sequence.in.fasta")
basefilename(c("/path/mybook.pdf", "test.doc"))
basefilename("README")
basefilename("/path/my\ home/Holiday Plan.txt")

basefilename("myBook.pdf", lower.case=TRUE)
extname("myBook.PDF", lower.case=TRUE)
}

