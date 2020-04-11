#' @include ribiosNGS-package.R
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
#' @param pValue Numeric, LR filter value, optional
#' @param FDR Numeric, FDR filter value, optional
#' @param value Numeric, vssigned threshold value
#' @param ... not used now
#'
#' @return An updated \code{EdgeSigFilter} object.
#' 
#' @aliases `logFC<-` `negLogFC<-` `negLogFC<-` `logCPM<-`
#'          `LR<-` `pValue<-` `FDR<-`
#' 
#' @importFrom stats update
#' @export
update.EdgeSigFilter <- function(object, logFC, posLogFC, negLogFC, logCPM, LR, pValue, FDR, ...) {
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
`posLogFC<-` <- function(object, value) {
  object@posLogFC <- value
  return(object)
}

#' @describeIn update.EdgeSigFilter Updates the negLogFC threshold value
#' @export
`negLogFC<-` <- function(object, value) {
  object@negLogFC <- value
  return(object)
}

#' @describeIn update.EdgeSigFilter Updates the posLogFC threshold value
#' @export
`logFC<-` <- function(object, value) {
  object@posLogFC <- abs(value)
  object@negLogFC <- -abs(value)
  return(object)
}

#' @describeIn update.EdgeSigFilter Updates the logCPM threshold value
#' @export
`logCPM<-` <- function(object, value) {
  object@logCPM <- value
  return(object)
}

#' @describeIn update.EdgeSigFilter Updates the LR threshold value
#' @export
`LR<-` <- function(object, value) {
  object@LR <- value
  return(object)
}

#' @describeIn update.EdgeSigFilter Updates the pValue threshold value
#' @export
`pValue<-` <- function(object, value) {
  object@pValue <- value
  return(object)
}

#' @describeIn update.EdgeSigFilter Updates the FDR threshold value
#' @export
`FDR<-` <- function(object, value) {
  object@FDR <- value
  return(object)
}

EdgeSigFilter <- function(logFC, posLogFC, negLogFC, logCPM, LR, pValue, FDR) {
  object <- new("EdgeSigFilter")
  object <- update(object, logFC=logFC, posLogFC=posLogFC, negLogFC=negLogFC,
                   logCPM=logCPM, LR=LR, pValue=pValue, FDR=FDR)
  return(object)
}

ER_SIGFILTER_DEFAULT <- EdgeSigFilter(logFC=0.5, FDR=0.05)


#' Object that contains test results, dgeTable, and EdgeSigFilter
#' @slot dgeGLM A DGEGLM class object that contains GLM test results
#' @slot dgeTables A list of dgeTable
#' @slot sigFilter Significantly regulated gene filter
#' @export
setClass("EdgeResult",
         representation=list("dgeGLM"="DGEGLM",
           "dgeTables"="list",
           "sigFilter"="EdgeSigFilter"),
         prototype=list(sigFilter=ER_SIGFILTER_DEFAULT),
         contains="EdgeObject")

#' Return a list of differential gene expression tables
#' 
#' @param edgeObj An EdgeObject
#' @param dgeGLM A DGEGLM object
#' @param dgeTables A list of DGEtables.
#' @return An \code{EdgeResult object}
#' @export
EdgeResult <- function(edgeObj,
                       dgeGLM,
                       dgeTables) {
  new("EdgeResult",
      dgeList=edgeObj@dgeList,
      designContrast=edgeObj@designContrast,
      dgeGLM=dgeGLM,
      dgeTables=dgeTables)
}

#' A class that contain feature annotation and expression matrix
#' @param exprs A matrix of expression
#' @param genes A data.frame
#' @export
setClass("FeatAnnoExprs",
         representation=list(exprs="matrix",
           genes="data.frame"))

#' An S4 class to represent a list of DGEListList objects
#' @exportClass DGEListList
setClass("DGEListList", representation("list"))

#' Construct a DGEListList object
#' 
#' @param ... A list of DGEListList objects, can be passed as individual objects
#' or in a list
#' 
#' @export
DGEListList <- function(...) {
  li <- as.list(...)
  res <- new("DGEListList", li)
  names(res@.Data) <- names(li)
  return(res)
}
