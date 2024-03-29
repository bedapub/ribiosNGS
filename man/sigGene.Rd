% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/edgeR-funcs.R
\name{sigGene}
\alias{sigGene}
\alias{sigPosGene}
\alias{sigNegGene}
\title{Return significantly regulated genes}
\usage{
sigGene(countDgeResult, contrast, value = NULL)

sigPosGene(countDgeResult, contrast, value = NULL)

sigNegGene(countDgeResult, contrast, value = NULL)
}
\arguments{
\item{countDgeResult}{An EdgeResult object}

\item{contrast}{Character, contrast(s) of interest}

\item{value}{\code{NULL} or character string, if not \code{NULL}, it must be a column name in the feature annotation data.}
}
\value{
A vector of identifiers
}
\description{
Return significantly regulated genes
}
\section{Functions}{
\itemize{
\item \code{sigPosGene()}: Only return positively significantly regulated genes

\item \code{sigNegGene()}: Only return negatively significantly regulated genes

}}
\examples{
exMat <- matrix(rpois(120, 10), nrow=20, ncol=6)
exMat[2:4, 4:6] <- exMat[2:4, 4:6]+20
exMat[7:9, 1:3] <- exMat[7:9, 1:3]+20
exGroups <- gl(2,3, labels=c("Group1", "Group2"))
exDesign <- model.matrix(~0+exGroups)
colnames(exDesign) <- levels(exGroups)
exContrast <- matrix(c(-1,1), ncol=1, dimnames=list(c("Group1", "Group2"), c("Group2.vs.Group1")))
exDescon <- DesignContrast(exDesign, exContrast, groups=exGroups)
exFdata <- data.frame(GeneID=1:nrow(exMat),
  GeneSymbol=sprintf("Gene\%d", 1:nrow(exMat)))
exPdata <- data.frame(Name=sprintf("Sample\%d", 1:ncol(exMat)),
                     Group=exGroups)
exObj <- EdgeObject(exMat, exDescon, 
                   fData=exFdata, pData=exPdata)
exDgeRes <- dgeWithEdgeR(exObj)
sigGenes(exDgeRes)
sigPosGenes(exDgeRes)
sigNegGenes(exDgeRes)
## specify the value type to return
sigGenes(exDgeRes, value="GeneSymbol")
sigPosGenes(exDgeRes, value="GeneSymbol")
sigNegGenes(exDgeRes, value="GeneSymbol")
}
