#' @include ribiosNGS.R
NULL

ER_TCRATIO_DEFAULT <- Inf
ER_TRENDED_DEFAULT <- Inf
ER_AVELOGCPM_DEFAULT <- -Inf
ESF_POSLOGFC_DEFAULT <- 0
ESF_NEGLOGFC_DEFAULT <- 0
ESF_LOGCPM_DEFAULT <- -Inf
ESF_LR_DEFAULT <- 0
ESF_PVALUE_DEFAULT <- 1
ESF_FDR_DEFAULT <- 1

##-----------------------------------##
## EdgeObject
##-----------------------------------##
#' EdgeObject argumenting DGEList by including designContrast information
#' @slot dgeList A \code{DGEList} object
#' @slot designContrast A \code{designContrast} object
#' 
#' @exportClass EdgeObject
setClass("EdgeObject",
         representation=list("dgeList"="DGEList",
                             "designContrast"="DesignContrast"))


##-----------------------------------##
## RiboSeq classes
##-----------------------------------##

#' A S4-class object representing a RiboSeq experiment
#' @slot RNA A \code{DGEList} object that represents RNA data
#' @slot RPF A \code{DGEList} object that represents ribosome profiling 
#'     (RPF) data
#' @slot groups A factor vector that indicate sample groups
#' 
#' @exportClass RiboSeq
setClass("RiboSeq",
         representation=list(RNA="DGEList",
           RPF="DGEList",
           groups="factor"))

#' A S4-class represnts analysis results of a RiboSeq experiment
#' @exportClass riboSeqAnalysisObject
setClass("riboSeqAnalysisObject",
         representation=list(nFeature.RNA.raw="integer",
           nSample.RNA.raw="integer",
           nFeature.RPF.raw="integer",
           nSample.RPF.raw="integer",
           nFeature.comb.raw="integer",
           nSample.comb.raw="integer",
           nFeature.filter="integer",
           nSample.filter="integer",
           groups="factor",
           contrasts="character",
           nBootstrap="integer",
           outdir="character",
           plot.cpmBoxplot="character",
           plot.exprsScatter="character",
           plot.MDS="character",
           plot.teBoxplot="character",
           plot.logFCscatter="character",
           plot.babelVolcano="character",
           file.rspace="character",
           file.fulltable="character",
           rnkNames="character",
           file.rnks="character",
           file.upstream="character",
           file.function="character",
           file.pathway="character",
           file.indexHTML="character"),
         prototype=list(outdir=getwd(),
           plot.cpmBoxplot="cpm-boxplots.pdf",
           plot.exprsScatter="exprs-scatterplot.pdf",
           plot.MDS="MDS-RNA-RPF.pdf",
           plot.teBoxplot="boxplot-TErates.pdf",
           plot.logFCscatter="scatterplot-logFC.RPF-logFC.RNA.pdf",
           plot.babelVolcano="volcano-plot.pdf",
           file.rspace="ribioseq-analysis-workspace.RData",
           file.fulltable="riboseq-analysis-results.txt",
           file.upstream="riboseq-analysis-upstreamAnalysis.txt",
           file.function="riboseq-analysis-functionalAnalysis.txt",
           file.pathway="riboseq-analysis-pathwayAnalysis.txt",
           file.indexHTML="index.html"))


##-----------------------------##
## EdgeResult
##-----------------------------##

#' EdgeSigFilter: EdgeR result filter for significantly regulated genes
#' 
#' @slot posLogFC Numeric, positive logFC threshold (larger values are kept)
#' @slot negLogFC Numeric, negative logFC threshold (more negative values 
#'     are kept)
#' @slot logCPM Numeric, logCPM treshold (larger values are kept)
#' @slot LR Numeric, likelihood ratio (LR) threshold (larger values are kept)
#' @slot pValue Numeric, p-value treshold (smaller values are kept)
#' @slot FDR Numeric, FDR treshold
#' 
#' @export 
setClass("EdgeSigFilter",
         representation=list("posLogFC"="numeric",
                             "negLogFC"="numeric",
                             "logCPM"="numeric",
                             "LR"="numeric",
                             "pValue"="numeric",
                             "FDR"="numeric"),
         prototype=list(posLogFC=ESF_POSLOGFC_DEFAULT,
                        negLogFC=ESF_NEGLOGFC_DEFAULT ,
                        logCPM=ESF_LOGCPM_DEFAULT,
                        LR=ESF_LR_DEFAULT,
                        pValue=ESF_PVALUE_DEFAULT,
                        FDR=ESF_FDR_DEFAULT),
         validity=function(object) {
           stopifnot(validPosLogFC <- object@posLogFC >= 0)
           stopifnot(validNegLogFC <- object@negLogFC <= 0)
           stopifnot(validLR <- object@LR>=0)
           stopifnot(validPvalue <- object@pValue >= 0 & object@pValue <= 1)
           stopifnot(validFDR <- object@FDR >= 0 & object@FDR <= 1)
           return(validPosLogFC & validNegLogFC & validLR & validPvalue & validFDR)
         })

#' Update EdgeSigFilter
#' 
#' @param object An \code{EdgeSigFilter} object 
#' @param logFC Numeric, logFC filter value, optional.
#' @param posLogFC Numeric, positive logFC filter value, optional.
#' @param negLogFC Numeric, negative logFC filter value, optional.
#' @param logCPM Numeric, logCPM filter value, optional.
#' @param LR Numeric, LR filter value, optional
#' @param FDR Numeric, FDR filter value, optional
#' 
#' @return An updated \code{EdgeSigFilter} object.
#' 
#' @aliases `logFC<-` `negLogFC<-` `negLogFC<-` `logCPM<-`
#'          `LR<-` `pValue<-` `FDR<-`
#' 
#' @importFrom stats update
#' @export
update.EdgeSigFilter <- function(object, logFC, posLogFC, negLogFC, logCPM, LR, pValue, FDR) {
  if(!missing(logFC)) logFC(object) <- logFC
  if(!missing(posLogFC)) posLogFC(object) <- posLogFC
  if(!missing(negLogFC)) negLogFC(object) <- negLogFC
  if(!missing(logCPM)) logCPM(object) <- logCPM
  if(!missing(LR)) LR(object) <- LR
  if(!missing(pValue)) pValue(object) <- pValue
  if(!missing(FDR)) FDR(object) <- FDR
  validObject(object)
  return(object)
}

#' @describeIn update.EdgeSigFilter Updates the posLogFC threshold value
#' @export
`posLogFC<-` <- function(edgeSigDegFilter, value) {
  edgeSigDegFilter@posLogFC <- value
  return(edgeSigDegFilter)
}

#' @describeIn update.EdgeSigFilter Updates the negLogFC threshold value
#' @export
`negLogFC<-` <- function(edgeSigDegFilter, value) {
  edgeSigDegFilter@negLogFC <- value
  return(edgeSigDegFilter)
}

#' @describeIn update.EdgeSigFilter Updates the posLogFC threshold value
#' @export
`logFC<-` <- function(edgeSigDegFilter, value) {
  edgeSigDegFilter@posLogFC <- abs(value)
  edgeSigDegFilter@negLogFC <- -abs(value)
  return(edgeSigDegFilter)
}

#' @describeIn update.EdgeSigFilter Updates the logCPM threshold value
#' @export
`logCPM<-` <- function(edgeSigDegFilter, value) {
  edgeSigDegFilter@logCPM <- value
  return(edgeSigDegFilter)
}

#' @describeIn update.EdgeSigFilter Updates the LR threshold value
#' @export
`LR<-` <- function(edgeSigDegFilter, value) {
  edgeSigDegFilter@LR <- value
  return(edgeSigDegFilter)
}

#' @describeIn update.EdgeSigFilter Updates the pValue threshold value
#' @export
`pValue<-` <- function(edgeSigDegFilter, value) {
  edgeSigDegFilter@pValue <- value
  return(edgeSigDegFilter)
}

#' @describeIn update.EdgeSigFilter Updates the FDR threshold value
#' @export
`FDR<-` <- function(edgeSigDegFilter, value) {
  edgeSigDegFilter@FDR <- value
  return(edgeSigDegFilter)
}

EdgeSigFilter <- function(logFC, posLogFC, negLogFC, logCPM, LR, pValue, FDR) {
  object <- new("EdgeSigFilter")
  object <- update(object, logFC=logFC, posLogFC=posLogFC, negLogFC=negLogFC,
                   logCPM=logCPM, LR=LR, pValue=pValue, FDR=FDR)
  return(object)
}

ER_SIGFILTER_DEFAULT <- EdgeSigFilter(logFC=0.5, FDR=0.05)


setClass("EdgeResult",
         representation=list("dgeGLM"="DGEGLM",
           "dgeTables"="list",
           "sigFilter"="EdgeSigFilter"),
         prototype=list(sigFilter=ER_SIGFILTER_DEFAULT),
         contains="EdgeObject")

#' Return a list of differential gene expression tables
#' 
#' @param edgeResult An \code{EdgeResult} object
#' @return A list of \code{data.frame}s, each containing the DGEtable for one
#' contrast.
#' @seealso \code{dgeTable} which returns one \code{data.frame} for one or more
#' given contrasts.
#' @export dgeTables
EdgeResult <- function(edgeObj,
                       dgeGLM,
                       dgeTables) {
  new("EdgeResult",
      dgeList=edgeObj@dgeList,
      designContrast=edgeObj@designContrast,
      dgeGLM=dgeGLM,
      dgeTables=dgeTables)
}

setClass("FeatAnnoExprs",
         representation=list(exprs="matrix",
           genes="data.frame"))

setClass("EdgeGSE",
         representation=list(geneSets="GmtList",
           method="character",
           enrichTables="data.frame"),
         contains="EdgeResult")

EdgeGSE <- function(edgeObj, gsc) {
  haltifnot(all(c("GeneID", "GeneSymbol") %in% colnames(dgeList(edgeObj)$genes)),
            msg="Gene annotation of the edgeObj must contain columns 'GeneID' with EntrezGeneIDs and 'GeneSymbol' with official gene symbols")
  egse <- as(edgeObj,"EdgeGSE")
  egse@geneSets <- gsc
  return(egse)
}

#' An S4 class to represent a list of DGEList2 objects
setClass("DGEList2", representation("list"))

#' Construct a DGEList2 object
#' 
#' @param ... A list of DGEList2 objects, can be passed as individual objects
#' or in a list
#' 
#' @export
DGEList2 <- function(...) {
  li <- as.list(...)
  res <- new("DGEList2", li)
  names(res@.Data) <- names(li)
  return(res)
}
