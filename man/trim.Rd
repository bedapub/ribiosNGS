\name{trim}
\alias{trim}
\title{Trim leading and tailing spaces from string}
\description{
  The function trims leading and/or tailing spaces from string(s), using
  C function implemented in the BIOS library.
}
\usage{
trim(x, left = " \n\r\t", right = " \n\r\t")
}
\arguments{
  \item{x}{A character string, or a vector of strings}
  \item{left}{Characters that are trimmed from the left side.}
  \item{right}{Characters that are trimmed from the right side}
}
\details{
  \code{left} and \code{right} can be set to NULL. In such cases no
  trimming will be performed.
}
\value{
  Trimmed string(s)
}
\references{
  \url{http://bioinfo.bas.roche.com:8080/bios/common/libprpi/format.hdoc}
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\examples{
myStrings <- c("This is a fine day\n",
               " Hallo Professor!",
               "  NUR DER HSV  ")
trim(myStrings)
}
