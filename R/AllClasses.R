#' @include ribiosNGS-package.R
NULL

##-----------------------------------##
## EdgeObject
##-----------------------------------##

#' EdgeObject argumenting DGEList by including designContrast information
#' @slot dgeList A \code{DGEList} object
#' @slot designContrast A \code{designContrast} object
#' 
#' @importClassesFrom edgeR DGEList
#' @importClassesFrom ribiosExpression DesignContrast
#' @exportClass EdgeObject
setClass("EdgeObject",
         representation=list("dgeList"="DGEList",
                             "designContrast"="DesignContrast"))

##-----------------------------------##
## SigFilter
##-----------------------------------##

ER_TCRATIO_DEFAULT <- Inf
ER_TRENDED_DEFAULT <- Inf
ESF_POSLOGFC_DEFAULT <- 0
ESF_NEGLOGFC_DEFAULT <- 0
ESF_LOGCPM_DEFAULT <- 0
ESF_AVEEXPR_DEFAULT <- -Inf
ESF_PVALUE_DEFAULT <- 1
ESF_FDR_DEFAULT <- 1

#' Base result filter for significantly regulated genes
#' @slot posLogFC Numeric, positive logFC threshold (larger values are kept)
#' @slot negLogFC Numeric, negative logFC threshold (more negative values 
#'     are kept)
#' @slot pValue Numeric, p-value treshold (smaller values are kept)
#' @slot FDR Numeric, FDR treshold
#' 
#' @exportClass SigFilter
setClass("SigFilter",
         representation=list("posLogFC"="numeric",
                             "negLogFC"="numeric",
                             "pValue"="numeric",
                             "FDR"="numeric"),
         prototype=list(posLogFC=ESF_POSLOGFC_DEFAULT,
                        negLogFC=ESF_NEGLOGFC_DEFAULT ,
                        pValue=ESF_PVALUE_DEFAULT,
                        FDR=ESF_FDR_DEFAULT),
         validity=function(object) {
           stopifnot(validPosLogFC <- object@posLogFC >= 0)
           stopifnot(validNegLogFC <- object@negLogFC <= 0)
           stopifnot(validPvalue <- object@pValue >= 0 & object@pValue <= 1)
           stopifnot(validFDR <- object@FDR >= 0 & object@FDR <= 1)
           return(validPosLogFC & validNegLogFC & validPvalue & validFDR)
         })

## some useful attributes

#' Get settings in the significance filter
#' @param sigFilter An SigFilter object
#' @returns Numeric values of the thresholds
#' @export
posLogFC <- function(sigFilter)  sigFilter@posLogFC

#' @rdname posLogFC
#' @export
negLogFC <- function(sigFilter) sigFilter@negLogFC

#' @rdname posLogFC
#' @export
pValue <- function(sigFilter) sigFilter@pValue

#' @rdname posLogFC
#' @export
FDR <- function(sigFilter)  sigFilter@FDR

#' Tells whether the threshold was not set
#' @param sigFilter An SigFilter object
#' @returns Logical, whether the thresholds are the default values
#' @export
isUnsetPosLogFC <-
  function(sigFilter)
    posLogFC(sigFilter) == ESF_POSLOGFC_DEFAULT

#' @rdname isUnsetPosLogFC
#' @export
isUnsetNegLogFC <-
  function(sigFilter)
    negLogFC(sigFilter) == ESF_NEGLOGFC_DEFAULT

#' @rdname isUnsetPosLogFC
#' @export
isUnsetPValue <-
  function(sigFilter)
    pValue(sigFilter) == ESF_PVALUE_DEFAULT

#' @rdname isUnsetPosLogFC
#' @export
isUnsetFDR <-
  function(sigFilter)
    FDR(sigFilter) == ESF_FDR_DEFAULT

#' Whether the SigFilter is the default one
#' @param object An SigFilter object
#' @return Logical, whether it is unset
#' @export
isUnsetSigFilter <- function(object) {
  res <- isUnsetPosLogFC(object) &
    isUnsetNegLogFC(object) &
    isUnsetAveExpr(object) &
    isUnsetPValue(object)  &
    isUnsetFDR(object)
  return(res)
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

##------------------------------------##
## EdgeSigFilter extending SigFilter
##------------------------------------##

#' Extends BaseSigFilter to filter genes base on logCPM and LR
#' @slot logCPM Numeric, logCPM threshold (larger values are kept)
#' @exportClass EdgeSigFilter
setClass("EdgeSigFilter",
         representation = list("logCPM"="numeric"),
         prototype=list(logCPM=ESF_LOGCPM_DEFAULT),
         contains="SigFilter")

#' Whether the logCPM filter is set
#' @param edgeSigFilter A \code{EdgeSigFilter} object
#' @export
isUnsetLogCPM <-
  function(edgeSigFilter)
    logCPM(edgeSigFilter) == ESF_LOGCPM_DEFAULT

#' @describeIn update.SigFilter Updates the logCPM threshold value
#' @export
`logCPM<-` <- function(object, value) {
  object@logCPM <- value
  return(object)
}

#' Get settings in the EdgeSigFilter
#' @param edgeSigFilter An \code{EdgeSigFilter} object
#' @returns Numeric values of the logCPM filter
#' @export
logCPM <- function(edgeSigFilter) { return(edgeSigFilter@logCPM)}

##------------------------------------##
## LimmaSigFilter extending SigFilter
##------------------------------------##

#' LimmaSigFilter Extending BaseSigFilter to filter genes base on aveExpr
#' @slot aveExpr Numeric, AveExpr threshold (larger values are kept)
#' @exportClass LimmaSigFilter
setClass("LimmaSigFilter",
         representation = list("aveExpr"="numeric"),
         prototype=list(aveExpr=ESF_AVEEXPR_DEFAULT),
         contains="SigFilter")

#' Whether the aveExpr filter is set
#' @param limmaSigFilter A \code{LimmaSigFilter} object
#' @export
isUnsetAveExpr <-
  function(limmaSigFilter)
    aveExpr(limmaSigFilter) == ESF_AVEEXPR_DEFAULT

#' Get aveExpr threshold in LimmaSigFilter
#' @param limmaSigFilter An \code{EdgeSigFilter} object
#' @returns Numeric values of the logCPM filter
#' @export
aveExpr <- function(limmaSigFilter) limmaSigFilter@aveExpr

#' @describeIn update.SigFilter Updates the aveExpr threshold value
#' @export
`aveExpr<-` <- function(object, value) {
  object@aveExpr <- value
  return(object)
}

#' Update SigFilter
#' 
#' @param object An \code{SigFilter} object 
#' @param logFC Numeric, logFC filter value, optional.
#' @param posLogFC Numeric, positive logFC filter value, optional.
#' @param negLogFC Numeric, negative logFC filter value, optional.
#' @param pValue Numeric, pValue filter value, optional
#' @param FDR Numeric, FDR filter value, optional
#' @param value Numeric, vssigned threshold value
#' @param ... not used now
#'
#' @return An updated \code{SigFilter} object.
#' 
#' @aliases `logFC<-` `posLogFC<-` `negLogFC<-` `aveExpr<-` `logCPM<-`
#'          `pValue<-` `FDR<-`
#' 
#' @importFrom stats update
#' @export
update.SigFilter <- function(object, logFC, posLogFC, negLogFC, pValue, FDR, ...) {
  if(!missing(logFC)) logFC(object) <- logFC
  if(!missing(posLogFC)) posLogFC(object) <- posLogFC
  if(!missing(negLogFC)) negLogFC(object) <- negLogFC
  if(!missing(pValue)) pValue(object) <- pValue
  if(!missing(FDR)) FDR(object) <- FDR
  validObject(object)
  return(object)
}

update.EdgeSigFilter <- function(object, logFC, posLogFC, negLogFC, pValue, FDR, logCPM, ...) {
  res <- update.SigFilter(object, logFC, posLogFC, negLogFC, pValue, FDR)
  if(!missing(logCPM)) logCPM(res) <- logCPM
  validObject(res)
  return(res)
}

update.LimmaSigFilter <- function(object, logFC, posLogFC, negLogFC, pValue, FDR, aveExpr, ...) {
  res <- update.SigFilter(object, logFC, posLogFC, negLogFC, pValue, FDR)
  if(!missing(aveExpr)) aveExpr(res) <- aveExpr
  validObject(res)
  return(res)
}


#' Build a SigFilter
#'
#' @param logFC Missing or positive numeric
#' @param posLogFC Missing or positive numeric
#' @param negLogFC Missing or negative numeric
#' @param pValue Missing or numeric between 0 and 1
#' @param FDR Missing or numeric between 0 and 1
#' @return A \code{SigFilter} object
#' @aliases EdgeSigFilter LimmaSigFilter sigFilter
#' @examples
#' SigFilter()
#' SigFilter(logFC=2)
#' SigFilter(negLogFC=-1)
#' SigFilter(FDR=0.05)
#'
#' @export
SigFilter <- function(logFC, posLogFC, negLogFC, pValue, FDR) {
  object <- new("SigFilter")
  object <- update(object, 
                   logFC=logFC, 
                   posLogFC=posLogFC, 
                   negLogFC=negLogFC,
                   pValue=pValue, FDR=FDR)
  return(object)
}

#' @describeIn SigFilter EdgeSigFilter
#' @examples
#' esf <- EdgeSigFilter(logFC=2, FDR=0.05, logCPM=0)
#' @export
EdgeSigFilter <- function(logFC, posLogFC, negLogFC, pValue, FDR, logCPM) {
  object <- new("EdgeSigFilter")
  object <- update(object, 
                   logFC=logFC, 
                   posLogFC=posLogFC, 
                   negLogFC=negLogFC,
                   pValue=pValue, 
                   FDR=FDR,
                   logCPM=logCPM)
  return(object)
}

#' @describeIn SigFilter LimmaSigFilter
#' @examples 
#' LimmaSigFilter(logFC=1, FDR=0.05, aveExpr=10)
#' @export
LimmaSigFilter <- function(logFC, posLogFC, negLogFC, pValue, FDR, aveExpr) {
  object <- new("EdgeSigFilter")
  object <- update(object, 
                   logFC=logFC, 
                   posLogFC=posLogFC, 
                   negLogFC=negLogFC,
                   pValue=pValue, 
                   FDR=FDR,
                   aveExpr=aveExpr)
  return(object)
}

ER_SIGFILTER_DEFAULT <- SigFilter(logFC=0.5, FDR=0.05)

ER_EDGESIGFILTER_DEFAULT <- EdgeSigFilter(logFC=0.5, FDR=0.05, logCPM=0)
ER_LIMMASIGFILTER_DEFALT <- LimmaSigFilter(logFC=0.5, FDR=0.05)

##-----------------------------##
## EdgeResult
##-----------------------------##

#' Object that contains count data, dgeTables, and sigFilter
#' @note The object is used only for inheritance
#' 
#' @slot dgeTables A list of dgeTable
#' @slot sigFilter Significantly regulated gene filter
#'
#' @exportClass CountDgeResult
setClass("CountDgeResult",
         representation=list("dgeTables"="list",
                             "sigFilter"="SigFilter"),
         prototype=list(sigFilter=ER_SIGFILTER_DEFAULT),
         contains=c("EdgeObject", "VIRTUAL"))

#' Object that contains test results, dgeTable, and SigFilter
#' @slot dgeGLM A DGEGLM class object that contains GLM test results
#' @slot dgeTables A list of dgeTable
#' @slot sigFilter Significantly regulated gene filter
#' @exportClass EdgeResult
setClass("EdgeResult",
         representation=list("dgeGLM"="DGEGLM"),
         prototype=list("sigFilter"=ER_EDGESIGFILTER_DEFAULT),
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
#' @slot voom The \code{voom} object
#' @exportClass LimmaVoomResult
setClass("LimmaVoomResult",
         representation=list("marrayLM"="MArrayLM",
                             "voom"="EList"),
         prototype=list(sigFilter=ER_LIMMASIGFILTER_DEFALT),
         contains="CountDgeResult")

#' Construct a LimmaVoomResult object
#' 
#' @param edgeObj An EdgeObject.
#' @param voom The voom (\code{EList}) object.
#' @param marrayLM A MArrayLM object.
#' @param dgeTables A list of DGEtables.
#' @return An \code{LimmaVoomResult} object.
#' @export
LimmaVoomResult <- function(edgeObj,
                            voom,
                            marrayLM,
                            dgeTables) {
  new("LimmaVoomResult",
      dgeList=edgeObj@dgeList,
      voom=voom,
      designContrast=edgeObj@designContrast,
      marrayLM=marrayLM,
      dgeTables=dgeTables)
}

##-----------------------------------##
## Helper classes
##-----------------------------------##
#' A class that contain feature annotation and expression matrix
#' @param exprs A matrix of expression
#' @param genes A data.frame
#' @exportClass FeatAnnoExprs
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
