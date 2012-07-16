\name{strtoken}
\alias{strtoken}
\title{
  Tokenize strings by character
}
\description{
  Tokenize strings by character in a similar way as the \code{strsplit}
  function in the \code{base} package. The function can return a matrix
  of tokenized items when \code{index} is missing. If \code{index} is
  given, tokenized items in the selected position(s) are returned. See examples.
}
\usage{
strtoken(x, split, index, ...)
}
\arguments{
  \item{x}{A vector of character strings; non-character vectors are cast
    into characters.}
  \item{split}{A character to split the strings.}
  \item{index}{Numeric vector indicating which fields should be
    returned; if missing or set to \code{NULL}, a matrix containing all
    fields are returned.}
  \item{\dots}{Other parameters passed to \code{\link{strsplit}}}
}
\value{
  A matrix if \code{index} is missing, \code{NULL}, or contains more
  than one integer indices; otherwise a character vector.
}
\references{
  The main body of the function is modified from the \code{strsplit2}
  function in the \code{limma} package.
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\seealso{
  \code{\link{strsplit}}
}
\examples{
myStr <- c("HSV\t1887", "FCB\t1900", "FCK\t1948")
strsplit(myStr, "\t")

strtoken(myStr, "\t")
strtoken(myStr, "\t", index=1L)
strtoken(myStr, "\t", index=2L)

myFac <- factor(myStr)
## do not run
\dontrun{
  strsplit(myFac, "\t")
}
strtoken(myFac, "\t")
strtoken(myFac, "\t", index=1L)
}

