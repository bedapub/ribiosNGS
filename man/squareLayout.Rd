\name{squareLayout}
\alias{squareLayout}
\title{
  Plan a square/matrix layout of plots
}
\description{
  Plan a square/matrix layout of plots
}
\usage{
squareLayout(n)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{Number of plots}
}
\value{
  A vector of integers of length 2. Can be passed to \code{layout} or
  \code{mfrow} in \code{par} to make the layout.
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}

\examples{
  \dontrun{
    op <- par(mfrow=squareLayout(7))
    plot(1:5)
    plot(2:6)
    plot(3:7)
    plot(-9:-4)
    plot(8:5)
    plot(5:1)
    plot(1:9)
    par(op)
  }
}