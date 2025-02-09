% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/edgeR-funcs.R
\name{sigGeneDgeTable}
\alias{sigGeneDgeTable}
\title{Return dgeTable containing significantly regulated genes in respective contrasts}
\usage{
sigGeneDgeTable(countDgeResult, value = "FeatrueName")
}
\arguments{
\item{countDgeResult}{An EdgeResult object}

\item{value}{A character string, it must be a column name in the feature annotation data. Default: FeatureName.}
}
\value{
A data.frame containing dgeTable of positively and negatively regulated
   genes in respective contrasts
}
\description{
Return dgeTable containing significantly regulated genes in respective contrasts
}
\examples{
exMat <- matrix(rpois(120, 10), nrow=20, ncol=6)
exMat[2:4, 4:6] <- exMat[2:4, 4:6]+20
exMat[7:9, 1:3] <- exMat[7:9, 1:3]+20
exGroups <- gl(2,3, labels=c("Group1", "Group2"))
exDesign <- model.matrix(~0+exGroups)
colnames(exDesign) <- levels(exGroups)
exContrast <- matrix(c(-1,1), ncol=1, dimnames=list(c("Group1", "Group2"), c("Group2.vs.Group1")))
exDescon <- DesignContrast(exDesign, exContrast, groups=exGroups)
exFdata <- data.frame(GeneSymbol=sprintf("Gene\%d", 1:nrow(exMat)))
exPdata <- data.frame(Name=sprintf("Sample\%d", 1:ncol(exMat)),
                     Group=exGroups)
exObj <- EdgeObject(exMat, exDescon, 
                   fData=exFdata, pData=exPdata)
exDgeRes <- dgeWithEdgeR(exObj)
sigGeneDgeTable(exDgeRes, value="GeneSymbol")
}
