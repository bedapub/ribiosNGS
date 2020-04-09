#' @include AllClasses.R
NULL

#' Construct an EdgeObject object by a count matrix and DesignContrast
#' 
#' @param object A matrix containing counts of features
#' @param designContrast A \code{DesignContrast} object
#' @param ... Other parameters
#' 
#' @examples 
#' exMat <- matrix(rpois(120, 10), nrow=20, ncol=6)
#' exGroups <- gl(2,3, labels=c("Group1", "Group2"))
#' exDesign <- model.matrix(~0+exGroups)
#' exContrast <- matrix(c(-1,1), ncol=1, 
#'     dimnames=list(c("Group1", "Group2"), c("Group2.vs.Group1")))
#' exDescon <- DesignContrast(exDesign, exContrast, groups=exGroups)
#' exFdata <- data.frame(Identifier=sprintf("Gene%d", 1:nrow(exMat)))
#' exPdata <- data.frame(Name=sprintf("Sample%d", 1:ncol(exMat)),
#'                      Group=exGroups)
#' exObj <- EdgeObject(exMat, exDescon)
#' exObj2 <- EdgeObject(exMat, exDescon, fData=exFdata)
#' exObj3 <- EdgeObject(exMat, exDescon, 
#'                      fData=exFdata, pData=exPdata)
#'             
#' fData(exObj3)
#' 
#' ## note that pData are appended after count information
#' pData(exObj2)
#' pData(exObj3)
#' @exportMethod EdgeObject
setGeneric("EdgeObject", function(object, designContrast, ...) standardGeneric("EdgeObject"))

#' @describeIn EdgeObject The method for matrix as input
#' @param object A matrix containing counts of features
#' @param designContrast A \code{DesignContrast} object
#' @param fData A \code{data.frame} containing annotation information for genes
#' @param pData A \code{data.frame} containing annotation information for samples
#' @param remove.zeros Logical, whether to remove rows that have 0 total count
#' @export
setMethod("EdgeObject",
          c("matrix", "DesignContrast"),
          function(object, designContrast, fData=NULL, pData=NULL, remove.zeros=FALSE) {
            object[is.na(object)] <- 0 ## NA is replaced with zero count
            if(is.null(fData) & !is.null(rownames(object))) {
              fData <- data.frame(InputID=rownames(object))
              if(is.null(fData) & !is.null(attr(object, "desc"))) {
                fData$Description <- attr(object, "desc")
              }
            }
            dgeList <- DGEList(counts=object,
                               group= groups(designContrast),
                               genes=fData, samples=pData, remove.zeros=remove.zeros)
            new("EdgeObject",
                dgeList=dgeList,
                designContrast=designContrast)
          })

#' @describeIn EdgeObject The method for FeatAnnoExprs as input
#' @param object A \code{FeatAnnoExprs} object
#' @param designContrast A \code{DesignContrast} object
#' @param pData A \code{data.frame} containing annotation information for samples
#' @param remove.zeros Logical, whether to remove rows that have 0 total count
#' @export
setMethod("EdgeObject",
          c("FeatAnnoExprs", "DesignContrast"),
          function(object, designContrast, pData=NULL,remove.zeros=FALSE) {
            dgeList <- DGEList(counts=object@exprs,
                               group=groups(designContrast),
                               genes=object@fData, samples=pData, remove.zeros=remove.zeros)
            new("EdgeObject",
                dgeList=dgeList,
                designContrast=designContrast)
          })
