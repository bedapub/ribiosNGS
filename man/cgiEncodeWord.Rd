\name{cgiEncodeWord}
\alias{cgiEncodeWord}
\alias{cgiDecodeWord}
\title{Encode and decode string for use in a URL}
\description{
  Encode and decode string for use in a URL
}
\usage{
cgiEncodeWord(word)
cgiDecodeWord(word)
}
\arguments{
  \item{word}{Character string to be encoded/decoded}
}
\value{
  Encoded/decoded strings
}
\references{
  BIOS documentation of the html module: \url{http://bioinfo.bas.roche.com:8080/bios/common/libprpi/html.hdoc}
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\examples{
(ex_enc <- cgiEncodeWord("abc=#3845AD,bcd=#AAFF00"))
cgiDecodeWord(ex_enc)

(ex_enc2 <- cgiEncodeWord(" _-+=\"'"))
cgiDecodeWord(ex_enc2)

}

