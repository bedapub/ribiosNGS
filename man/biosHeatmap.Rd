\name{biosHeatmap}
\alias{biosHeatmap}
\title{CUSTOMED HEATMAP.2 FOR BIOS USERS}
\description{
  A tailored version of the \code{heatmap.2} function in the gplots
  package, by giving the default values in the paramter list.
}
\usage{
biosHeatmap(x, col = "greenred", trace = c("none", "column", "row",
"both"), density.info = c("none", "histogram", "density"), na.color = "darkgray", lwid = c(1, 7), lhei = c(1, 7), margins = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A matrix}
  \item{col}{Colors for the heatmap, by default green indicates low and
    red indicates high values}
  \item{trace}{Logical , whether drawing tracing lines, by default not}
  \item{density.info}{Logical, drawing density info in the key
    histogram, by default not}
  \item{na.color}{Color for \code{NA} cells, darkgray by default}
  \item{lwid}{Widths of columns}
  \item{lhei}{Heights of rows}
  \item{margins}{Margins of labs, automatically guessed if no value was provided}
  \item{\dots}{Other paramters passed to \code{heatmap.2} function}
}
\details{
  Customed version of the heatmap.2, with the common settings used by
  JItao David Zhang
}
\value{
  See \code{heatmap.2} in the \code{gplots} package.
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
%%\note{
%%  ~~further notes~~
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%%}
\examples{
set.seed(123)
test <- matrix(rnorm(100), nrow=10)
biosHeatmap(test)

## do not draw row-wise dendrogram
biosHeatmap(test, Rowv=FALSE, dendrogram="column")
## do not draw column-wise dendrogram
biosHeatmap(test, Colv=FALSE, dendrogram="row")
## do not re-sort columns/rows (e.g. for visualization purposes)
biosHeatmap(test, Rowv=FALSE, Colv=FALSE, dendrogram="none")

## define the color range by zlim
biosHeatmap(test, zlim=c(-5, 5))
}
