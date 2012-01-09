\name{ksScore}
\alias{ksScore}
\title{Enrichment Score based on the Kolmogorov-Smirnov statistic}
\description{
  A enrichment score based on Kolmogorov-Smirnov, introduced in the
  Connectivity Map paper (Lamb et al., 2006). For each instance i (cell
  treated with one perturbagen), the enrichment score is calculated for
  the set of probe-sets (tags) representing the up- or down-regulated
  genes in the query signature (ks_up and ks_down). 
}
\usage{
ksScore(n, vec)
}
\arguments{
  \item{n}{The total number of probe-sets}
  
  \item{vec}{Vector of integer, a vector V of the position of each tag
    in the ordered list of all probesets, sorted in ascending order such
    that V_j is the position of tag j, where j = 1,2,\dots,t. The input
    vector does \emph{not} need to be sorted: it will be sorted
    ascendingly inside the function.}
}
\details{
  Two enrichment scores, ks_up and ks_down, are required to calculate the
  Connectivity Score \code{S} subsequently.

  Note that the maximum number in \code{vec} should be equal or less
  than \code{n}. This is not checked by the function, but of the
  responsibility of the calling function (for example using \code{match}
  to find the correct index).

  Based on the descriptions of the paper describing the method (Lamb et al., Science 2006), and
  from the website describing the method (see the Reference below), the
  position vector \code{V(j)} is sorted in ascending order. The
  implementation sorts the position vector and their tags so that the
  position indices are ascending. For example, if four tags
  \dQuote{a,b,c,d} has the
  position of \code{c(5,3, 4, 1)}. The sorted \code{V} will be
  \code{c(1,3,4,5)}, corresponding to tags \dQuote{d,b,c,a}.

  Two scores, \code{a} and \code{b}, are calculated separately. If
  \code{a>=b}, then \code{ks=a}, otherwise \code{ks=-b}. Note that
  different from the descriptions in the reference, the situation
  \code{a==b} is in this implementation explicitly addressed by
  assigning \code{ks=a}.
}
\value{
  The Kolmogorov-Smirnov statistic enrichment score.
}
\references{
  Lamb et al., \emph{The Connectivity Map: Using Gene-Expression
    Signatures to Connect Small Molecules, Genes, and Disease}, Science
  2006.

  Help manual of the Connectivity Map online tool
  \url{http://www.broadinstitute.org/cmap/help_topics_linkified.jsp}. 
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\note{
  This method is most probably called by other methods, not by end
  users. Please make sure you have read the descriptions above before
  you use this function.
}

\seealso{
  \code{\link{connScore}} for calculating Connectivity Score for one instance
  and a pair of tags. \code{\link{cmap}} for calculating Connectivity
  Score for multiple instances, possibly with permutation test.
}
\examples{
ksVec <- c(3,5,7,9)
## -0.3
ksScore(n=10L, vec=ksVec)

## -0.4285714
ksScore(n=7L, vec=c(5,6,3))

## 0.4285714
ksScore(n=7L, vec=c(4,2,1))
}
