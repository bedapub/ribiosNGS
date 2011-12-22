\name{gti2bioc}
\alias{gtibioc}
\alias{gti2bioc}
\alias{bioc2gti}
\title{
  Translate chiptypes between GTI Bioconductor naming conventions
}
\description{
  \code{gti2bioc} converts chip types from GTI array names into Bioconductor
  names, and \code{bioc2gti} converts Bioconductor array names to GTI
  names. If the array name is not valid or not found, NA will be returned.
}
\usage{
gti2bioc(chipname)
bioc2gti(chipname)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{chipname}{Character vector, chip names (types)}
}
\details{
  The translation table \code{gtibioc} was compiled manually in December 2011.
}
\value{
  Chracter vector of the same length as the input
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\examples{
bioc2gti("hgu133plus2")
bioc2gti(c("hgu133plus2", "hgu95av2", "bad_array"))
gti2bioc("HG_U95AV2")
gti2bioc(c("HG_U95AV2", "CANINE", "HG_U95A"))
}

