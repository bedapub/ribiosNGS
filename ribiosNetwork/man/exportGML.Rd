\name{exportGML}
\alias{exportGML}
\title{
  Export Cytoscape/yEd friendly GML files
}
\description{
  \code{exportGML} exports an \code{igraph} object to GML files
  complying with specifications defined by Cytoscape and yEd. Compared
  to the native \code{write.graph} function provided by the
  \code{igraph} package, GML files exported with \code{exportGML} can be
  directly read and properly visualized by Cytoscape and yEd.
}
\usage{
exportGML(igraph, filename)
}
\arguments{
  \item{igraph}{An \code{igraph} object}
  \item{filename}{The filename to be exported}
}
\details{
  Currently the function uses supports following attributes:
  Node name: \code{V(igraph)$name}
  Node label: \code{V(igraph)$label}
  Node isInput: \code{V(igraph)$isInput}, controlling node shapes
  Edge label: \code{V(igraph)$label}, determining edge target arrow

  So far the function is mainly used by the \code{ronet.Rscript} script
  in the package. Users are invited to adapt the function for other purposes.
}
\value{
  This function is called for side effects: a GML file is written
}
\references{
  \url{http://wiki.cytoscape.org/Cytoscape_User_Manual/Visual_Styles}.

  The adaptation is inspired by \code{https://stat.ethz.ch/pipermail/r-help/2011-April/275218.html}.
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\seealso{
  \code{\link{write.graph}} in \code{igraph}
}
\examples{
g <- barabasi.game(100, directed=FALSE)
V(g)$label <- c(paste("node", 1:99, sep=""),"--")
V(g)$name <- 1:100
V(g)$isInput <- rbinom(100,1, 0.5)
E(g)$label <- "Expression"
gPosE <- as.logical(rbinom(ecount(g), 1, 0.25))
gNegE <- as.logical(rbinom(ecount(g), 1, 0.25))
E(g)$label[gPosE] <- "Expressoion_Positive"
E(g)$label[gNegE] <- "Expressoion_Negative"

gFile <- tempfile()
exportGML(g, gFile)
}
