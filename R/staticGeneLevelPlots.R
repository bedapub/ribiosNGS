#' @include plotMethods.R edgeR-funcs.R
NULL

#' Get automatic group color
#'
#' @param edgeObj An EdgeObject or EdgeResult object
#' @param panel passed to \code{fcbrewer}
#' @return A fcbase object
#' @importFrom ribiosPlot fcbrewer
#' @export
groupCol <- function(edgeObj, panel="Set1") {
  ribiosPlot::fcbrewer(dispGroups(edgeObj), panel)
}

#' Make static gene-level plots of an EdgeResult object
#' 
#' @param edgeResult An EdgeResult object
#' @return \code{NULL}, side effect is used
#' 
#' @importFrom made4 plotarrays ord
#' @examples 
#' edgeObj <- exampleEdgeObject()
#' edgeRes <- dgeWithEdgeR(edgeObj)
#' staticGeneLevelPlots(edgeRes)
#' 
#' limmaVoomRes <- dgeWithEdgeR(edgeObj)
#' staticGeneLevelPlots(limmaVoomRes)
#' @export
staticGeneLevelPlots <- function(edgeResult) {
  objModLogCPM <- modLogCPM(edgeResult)
  groupLabels <- dispGroups(edgeResult)
  groupCol <- fcbrewer(groupLabels)
  
  ## Dimension reduction
  ### MDS
  limma::plotMDS(edgeResult, main="MDS plot")
  
  ### PCA (using modLogCPM)
  objPca <- prcomp(t(objModLogCPM))
  objPca.data <- ribiosPlot::plotPCA(objPca, points=FALSE, text=TRUE, main="modLogCPM PCA")
  
  ### COA (using mogLogCPM - leaving out for now, because array2ade4 reports an error, since matrix has two classes (matrix and vector) in R-4.0.1)
  ## objCoa <- made4::ord(objModLogCPM)$or$co
  ## made4::plotarrays(objCoa, classvec=dispGroups(edgeResult))
  
  ## BioQC
  ### TODO: RPKM/TPM calculation needed
  ## doLog("BioQC (TODO)")
  
  ## Normalization
  ### boxplot of read counts (before and after normalization)
  boxplot(edgeResult, type="modLogCPM")
  
  ### boxplot of normalization factors
  boxplot(edgeResult, type="normFactors")
  
  ## Dispersion
  ### BCV plot
  ribiosNGS::plotBCV(edgeResult, main="BCV plot")
  
  ## Significant differentially expressed genes
  ## number of significantly differentially expressed genes
  sigGeneBarchart(edgeResult, stack=FALSE)
  
  ## volcano plot
  volcanoPlot(edgeResult, multipage=TRUE)
  
  ## plotSmear
  smearPlot(edgeResult, freeRelation=TRUE, smooth.scatter=FALSE, multipage=TRUE)
  
  ## pairs of correlations
  if(ncol(contrastMatrix(edgeResult))>1) {
    pairs(edgeResult, freeRelation=TRUE)
  }
  
  return(invisible(NULL)) 
}

#' Export static gene-level plots in PDF
#' 
#' @param edgeResult An \code{EdgeResult} object
#' @param file Character string, the PDF file name
#' 
#' @export
exportStaticGeneLevelPlots <- function(edgeResult, file) {
  openFileDevice(file)
  staticGeneLevelPlots(edgeResult)
  closeFileDevice()
}


#' Barchart of significantly regulated genes
#'
#' @param edgeResult An EdgeResult object
#' @param logy Logical, whether y-axis should be log-10 transformed
#' @param scales passed to \code{lattice::barchart}
#' @param stack passed to \code{lattice::barchart}
#' @param ylab passed to \code{lattice::barchart}
#' @param col passed to \code{lattice::barchart}
#' @param auto.key passed to \code{lattice::barchart}
#' @param ... passed to \code{lattice::barchart}
#'
#' @importFrom ribiosUtils ofactor
#' @importFrom lattice barchart
#' @export
sigGeneBarchart <- function(edgeResult,
                            logy=FALSE,
                            scales=list(x=list(rot=45),
                              y=list(alternating=1, tck=c(1,0))),
                            stack=FALSE,
                            ylab="Significant DEGs",
                            col=c("positive"="orange",
                              "negative"="lightblue"),
                            auto.key=list(columns=2),
                            ...) {
  counts <- sigGeneCounts(edgeResult)
  contrasts <- ribiosUtils::ofactor(contrastNames(edgeResult))
  positive <- counts$posCount
  negative <- counts$negCount
  scales$y$log <- ifelse(logy, 10, FALSE)
  lattice::barchart(positive + negative ~ contrasts,
                    stack=stack,
                    ylab=ylab,
                    scales=scales,
                    par.settings=list(superpose.polygon=list(col=col)),
                    auto.key=auto.key,
                    origin=0,
                    ...)
}

