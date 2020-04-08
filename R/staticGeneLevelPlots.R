#' Make static gene-level plots of an EdgeResult object
#' 
#' @param edgeResult An EdgeResult object
#' @return \code{NULL}, side effect is used
#' 
#' @importFrom made4 plotarrays ord
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
  
  ### COA (using mogLogCPM)
  objCoa <- made4::ord(objModLogCPM)$or$co
  made4::plotarrays(objCoa, classvec=dispGroups(edgeResult))
  
  ## BioQC
  ### TODO: RPKM/TPM calculation needed
  ## doLog("BioQC (TODO)")
  
  ## Normalization
  ### boxplot of read counts (before and after normalization)
  normBoxplot(obj, edgeResult)
  
  ### boxplot of normalization factors
  boxplot(edgeResult, type="normFactors")
  
  ## Dispersion
  ### BCV plot
  plotBCV(edgeResult, main="BCV plot")
  
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
