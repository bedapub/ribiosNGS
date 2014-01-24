\name{i2xy}
\alias{i2xy}
\alias{xy2i}
\title{Convert (x,y)-coordinates to single-number indices and back.}
\description{Convert (x,y)-coordinates on the chip (and in the CEL file) to the
single-number indices used in AffyBatch and CDF environment, and back.}
\usage{i2xy(i)
xy2i(x,y)
}
\arguments{
\item{x}{numeric. x-coordinate (from 1 to 1048)}
\item{y}{numeric. y-coordinate (from 1 to 1048)}
\item{i}{numeric. single-number index (from 1 to 1098304)}
}
\details{Type \code{i2xy} and \code{xy2i} at the \R prompt to 
view the function definitions.}
\seealso{\code{\link{snowballs520824fcdf}}}
\keyword{datasets}

\examples{
  xy2i(5,5)
  i     = 1:(1048*1048)
  coord = i2xy(i)
  j     = xy2i(coord[, "x"], coord[, "y"])
  stopifnot(all(i==j))
  range(coord[, "x"])
  range(coord[, "y"])
}
