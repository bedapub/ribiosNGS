#' @include ribiosNGS-package.R
NULL

ER_TCRATIO_DEFAULT <- Inf
ER_TRENDED_DEFAULT <- Inf
ESF_POSLOGFC_DEFAULT <- 0
ESF_NEGLOGFC_DEFAULT <- 0
ESF_AVEEXPR_DEFAULT <- -Inf
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

#' SigFilter: EdgeR result filter for significantly regulated genes
#' 
#' @slot posLogFC Numeric, positive logFC threshold (larger values are kept)
#' @slot negLogFC Numeric, negative logFC threshold (more negative values 
#'     are kept)
#' @slot aveExpr Numeric, threshold of average expression (larger values are kept)
#' @slot pValue Numeric, p-value treshold (smaller values are kept)
#' @slot FDR Numeric, FDR treshold
#' 
#' @export 
setClass("SigFilter",
         representation=list("posLogFC"="numeric",
                             "negLogFC"="numeric",
                             "aveExpr"="numeric",
                             "pValue"="numeric",
                             "FDR"="numeric"),
         prototype=list(posLogFC=ESF_POSLOGFC_DEFAULT,
                        negLogFC=ESF_NEGLOGFC_DEFAULT ,
                        aveExpr=ESF_AVEEXPR_DEFAULT,
                        pValue=ESF_PVALUE_DEFAULT,
                        FDR=ESF_FDR_DEFAULT),
         validity=function(object) {
           stopifnot(validPosLogFC <- object@posLogFC >= 0)
           stopifnot(validNegLogFC <- object@negLogFC <= 0)
           stopifnot(validPvalue <- object@pValue >= 0 & object@pValue <= 1)
           stopifnot(validFDR <- object@FDR >= 0 & object@FDR <= 1)
           return(validPosLogFC & validNegLogFC & validPvalue & validFDR)
         })

#' Update SigFilter
#' 
#' @param object An \code{SigFilter} object 
#' @param logFC Numeric, logFC filter value, optional.
#' @param posLogFC Numeric, positive logFC filter value, optional.
#' @param negLogFC Numeric, negative logFC filter value, optional.
#' @param aveExpr Numeric, AveExpr filter value, optional.
#' @param pValue Numeric, pValue filter value, optional
#' @param FDR Numeric, FDR filter value, optional
#' @param value Numeric, vssigned threshold value
#' @param ... not used now
#'
#' @return An updated \code{SigFilter} object.
#' 
#' @aliases `logFC<-` `negLogFC<-` `negLogFC<-` `aveExpr<-`
#'          `pValue<-` `FDR<-`
#' 
#' @importFrom stats update
#' @export
update.SigFilter <- function(object, logFC, posLogFC, negLogFC, aveExpr, pValue, FDR, ...) {
  if(!missing(logFC)) logFC(object) <- logFC
  if(!missing(posLogFC)) posLogFC(object) <- posLogFC
  if(!missing(negLogFC)) negLogFC(object) <- negLogFC
  if(!missing(aveExpr)) aveExpr(object) <- aveExpr
  if(!missing(pValue)) pValue(object) <- pValue
  if(!missing(FDR)) FDR(object) <- FDR
  validObject(object)
  return(object)
}

#' @describeIn update.SigFilter Updates the posLogFC threshold value
#' @export
`posLogFC<-` <- function(object, value) {
  object@posLogFC <- value
  return(object)
}

#' @describeIn update.SigFilter Updates the negLogFC threshold value
#' @export
`negLogFC<-` <- function(object, value) {
  object@negLogFC <- value
  return(object)
}

#' @describeIn update.SigFilter Updates the posLogFC threshold value
#' @export
`logFC<-` <- function(object, value) {
  object@posLogFC <- abs(value)
  object@negLogFC <- -abs(value)
  return(object)
}

#' @describeIn update.SigFilter Updates the aveExpr threshold value
#' @export
`aveExpr<-` <- function(object, value) {
  object@aveExpr <- value
  return(object)
}

#' @describeIn update.SigFilter Updates the pValue threshold value
#' @export
`pValue<-` <- function(object, value) {
  object@pValue <- value
  return(object)
}

#' @describeIn update.SigFilter Updates the FDR threshold value
#' @export
`FDR<-` <- function(object, value) {
  object@FDR <- value
  return(object)
}

SigFilter <- function(logFC, posLogFC, negLogFC, aveExpr, pValue, FDR) {
  object <- new("SigFilter")
  object <- update(object, logFC=logFC, posLogFC=posLogFC, negLogFC=negLogFC,
                   aveExpr=aveExpr, pValue=pValue, FDR=FDR)
  return(object)
}

ER_SIGFILTER_DEFAULT <- SigFilter(logFC=0.5, FDR=0.05)

#' Object that contains count data, dgeTables, and sigFilter
#' @slot dgeTables A list of dgeTable
#' @slot sigFilter Significantly regulated gene filter
#' 
#' The object is used only for inheritance
setClass("CountDgeResult",
         representation=list("dgeTables"="list",
                             "sigFilter"="SigFilter"),
         prototype=list(sigFilter=ER_SIGFILTER_DEFAULT),
         contains="EdgeObject")

#' Object that contains test results, dgeTable, and SigFilter
#' @slot dgeGLM A DGEGLM class object that contains GLM test results
#' @slot dgeTables A list of dgeTable
#' @slot sigFilter Significantly regulated gene filter
#' @export
setClass("EdgeResult",
         representation=list("dgeGLM"="DGEGLM"),
         contains="CountDgeResult")

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

#' The LimmaVoom Object that contains test results, dgeTable, and SigFilter
#' @slot marrayLM A \code{MArrayLM} class object that contains results of eBayesFit
#' @slot dgeTables A list of dgeTable
#' @slot sigFilter Significantly regulated gene filter
#' @export
setClass("LimmaVoomResult",
         representation=list("marrayLM"="MArrayLM"),
         prototype=list(sigFilter=ER_SIGFILTER_DEFAULT),
         contains="CountDgeResult")

#' Construct a LimmaVoomResult object
#' 
#' @param edgeObj An EdgeObject
#' @param marrayLM A MArrayLM object
#' @param dgeTables A list of DGEtables.
#' @return An \code{LimmaVoomResult} object.
#' @export
LimmaVoomResult <- function(edgeObj,
                       marrayLM,
                       dgeTables) {
  new("LimmaVoomResult",
      dgeList=edgeObj@dgeList,
      designContrast=edgeObj@designContrast,
      marrayLM=marrayLM,
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
