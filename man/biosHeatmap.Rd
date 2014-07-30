\name{biosHeatmap}
\alias{biosHeatmap}
\title{CUSTOMED HEATMAP.2 FOR BIOS USERS}
\description{
  A tailored version of the \code{heatmap.2} function in the gplots
  package, by giving the default values in the paramter list.
}
\usage{
biosHeatmap(x,

            ## dedrogram control
            Rowv = TRUE,
            Colv = if (symm) "Rowv" else TRUE,
            distfun = dist,
            hclustfun = hclust,
            dendrogram = c("both","row", "column", "none"),
            symm = FALSE,

            ## data scaling
            scale = c("none","row", "column"),
            na.rm = TRUE,

            ## impage plot
            revC = identical(Colv, "Rowv"),
            add.expr,

            ## mapping data to colors
            breaks,
            symbreaks = min(x < 0, na.rm = TRUE) || scale != "none",

            ## colors
            col = "greenred",
            na.color = "darkgray",
            
            ## block separation (not implemented in Rscript)
            colsep,
            rowsep, 
            sepcolor = "white",
            sepwidth = c(0.05, 0.05),

            ## cell labeling (not implemented in Rscript)
            cellnote,
            notecex = 1, 
            notecol = "cyan",

            ## level trace (not implemented in Rscript)
            trace = c("none", "column","row", "both"),
            tracecol = "cyan",
            hline = median(breaks), 
            vline = median(breaks),
            linecol = tracecol,

            ## Row/Column Labeling 
            margins = NULL,
            main = NULL,
            xlab = NULL,
            ylab = NULL,
            labRow = NULL,
            labCol = NULL,
            cexMain = NULL,
            cexRow = pmin(1, 0.2 + 1/log10(nr)), 
            cexCol = pmin(1, 0.2 + 1/log10(nc)),
            
            ## Row/Column color (not implemented in Rscript)
            ColSideColors, RowSideColors,
            
            ## color key (only title implemented)
            color.key.title="Color Key",                         
            key = TRUE,
            keysize = 1.5,
            density.info = c("none", "histogram","density"),
            denscol = tracecol,
            symkey = min(x < 0, na.rm = TRUE) || symbreaks,
            densadj = 0.25,
            zlim,

            ## layout
            lhei = c(1,7),
            lwid = c(1,7),
            lmat = NULL,

            ...) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A matrix}
  \item{Rowv}{Logical, whether row-wise dendrogram should be calculated}
  \item{Colv}{Logical, whether column-wise dendrogram should be calculated}
  \item{distfun}{Function, for calculating distance}
  \item{hclustfun}{Function, for hierarchical clustering}
  \item{dendrogram}{Character, specify which dendrogram to be
    drawn. Note that \code{Rowv} and \code{Colv} determines whether
    dendrograms are calculated and rows or columns are reordered.}
  \item{symm}{Logical. Should the matrix be treated as symmetric}
  \item{scale}{Logical, should the matrix be row-scaled}
  \item{na.rm}{Logical, should NA values should be omitted}
  \item{revC}{Logical, should columns be reversed}
  \item{add.expr}{Expression}
  \item{breaks}{Numeric vector, where to set breaks. Can be missing.}
  \item{symbreaks}{Logical, should be breaks symmetric}
  \item{colsep,rowsep}{Integer vector, positions at which columns or rows are separated}
  \item{sepcolor,sepwidth}{Color and width of separating lines}
  \item{cellnote}{Cell labelling}
  \item{notecex}{Cell labelling font size}
  \item{notecol}{Cell labelling font color}
  \item{tracecol}{Level trace}
  \item{hline}{Level trace hline}
  \item{vline}{Level trace vline}
  \item{linecol}{Level trace color}
  \item{main,xlab,ylab}{Heatmap title, X and Y axis labels}
  \item{labRow,labCol}{Row and column labels}
  \item{cexMain,cexRow,cexCol}{Title, row and column label font sizes}
  \item{ColSideColors,RowSideColors}{Column and row side colors}
  \item{color.key.title}{Color key title}
  \item{key}{Logical, whether key should be drawn}
  \item{keysize}{Key size}
  \item{denscol}{Logical, should density information be displayed}
  \item{symkey}{Logical, should the key be symmetric}
  \item{densadj}{densadj}
  \item{zlim}{zlim}
  \item{lmat}{lmat}
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
