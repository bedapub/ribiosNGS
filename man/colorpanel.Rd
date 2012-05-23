\name{colorpanel}
\alias{colorpanel}
\title{Generates a set of colors that varies smoothly}
\description{
  Generates a set of colors that varies smoothly.
}
\usage{
colorpanel(n, low, mid, high)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{Desired number of color elements in the panel}
  \item{low}{Color to use for the lowest value}
  \item{mid}{Color to use for the middle value. It may be ommited}
  \item{high}{Color to use for the highest value}
}
\details{
  (copied from the \code{colorpanel} man page from the \code{gplots}
  package. See NOTES below)

  The values for \sQuote{low, mid, high} can be given as color names
  (\sQuote{red}), plot color index (\code{2}=red), and HTML-style RGB,
  (\dQuote{\#FF0000}=red).

  If \sQuote{mid} is supplied, then the returned color panel will consist
  of \sQuote{n - floor(n/2)} HTML-style RGB elements which vary smoothly
  between \sQuote{low} and \sQuote{mid}, then between \sQuote{mid} and \sQuote{high}. Note that
  if \sQuote{n} is even, the color \sQuote{mid} will occur twice at the center of
  the sequence.
  
  If \sQuote{mid} is omitted, the color panel will vary smoothly beween
  \sQuote{low} and \sQuote{high}.

}
\value{
  Vector of HTML-style RGB colors.
}
\references{
  See \code{gplots} package.
}
\author{
  Originally by Gregory R. Warnes <greg@warnes.net>. Adapted by Jitao
  David Zhang <jitao_david.zhang@roche.com>.
}
\note{
  The colorpanel function is copied from the \code{gplots} package
  (written by Warnes et al.) under the GPL-2 license. The \code{gplots}
  require heavy dependencies that prevent this function being used in
  speed-sensitive scenarios, e.g. in command-line tools.
}
\seealso{
  \code{blackyellow} and \code{royalbluered} for two- and three-color panels.
}
\examples{
showpanel <- function(col) {
  image(z=matrix(1:100, ncol=1), col=col, xaxt="n", yaxt="n")
}
 
par(mfrow=c(3,3))
 
# two colors only:
showpanel(colorpanel(8,low="red",high="green"))
 
# three colors
showpanel(colorpanel(8,"red","black","green"))
# note the duplicatation of black at the center, using an odd
# number of elements resolves this:
showpanel(colorpanel(9,"red","black","green"))
 
showpanel(greenred(64))
showpanel(redgreen(64))
showpanel(bluered(64))
showpanel(redblue(64))

showpanel(royalbluered(64))
showpanel(royalredblue(64))
}

