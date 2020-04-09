#' @include AllClasses.R AllGenerics.R AllMethods.R edgeR-funcs.R

#' Perform differential gene expression analysis with edgeR
#' 
#' 
#' @param edgeObj An object of \code{EdgeObject}
#' 
#' The function performs end-to-end differential gene expression (DGE) analysis
#' with common best practice using edgeR
#' @return An \code{EdgeResult} object
#' @examples
#' 
#' exMat <- matrix(rpois(120, 10), nrow=20, ncol=6)
#' exGroups <- gl(2,3, labels=c("Group1", "Group2"))
#' exDesign <- model.matrix(~0+exGroups)
#' exContrast <- matrix(c(-1,1), ncol=1, dimnames=list(c("Group1", "Group2"), c("Group2.vs.Group1")))
#' exDescon <- DesignContrast(exDesign, exContrast, groups=exGroups)
#' exFdata <- data.frame(Identifier=sprintf("Gene%d", 1:nrow(exMat)))
#' exPdata <- data.frame(Name=sprintf("Sample%d", 1:ncol(exMat)),
#'                      Group=exGroups)
#' exObj <- EdgeObject(exMat, exDescon, 
#'                      fData=exFdata, pData=exPdata)
#' exDgeRes <- dgeWithEdgeR(exObj)
#' dgeTable(exDgeRes)
#' 
#' @export dgeWithEdgeR
dgeWithEdgeR <- function(edgeObj) {
  edgeObj.filter <- filterByCPM(edgeObj)
  edgeObj.norm <- normalize(edgeObj.filter)
  edgeObj.disp <- estimateGLMDisp(edgeObj.norm)
  ## in case of single replicate
  ## edgeR recommendation for common dispersion: 0.4 for human study, 0.1 for well-controlled, 0.01 for tech replicates
  if(!hasCommonDisp(edgeObj.disp)) {
    warning("No common dispersion estimate available. Possible reason may be no replicates")
    warning("Common dispersion is set as 0.4. Note that the number of DEGs is sensitive to this setting")
    edgeObj.disp <- setCommonDispIfMissing(edgeObj.disp, 0.4)
  }
  edgeObj.fit <- fitGLM(edgeObj.disp)
  dgeTest <- testGLM(edgeObj.disp, edgeObj.fit)
  return(dgeTest)
}
