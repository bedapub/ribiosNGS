#' Return counts in EdgeObject
#' 
#' @name counts,EdgeObject-method
#' @docType methods
#' @param object An EdgeObject
#' @param filter Logical, whether filtered matrix (by default) or unfiltered
#' matrix should be returned
#' @seealso \code{\link{filterByCPM}}
NULL


#' An S4 class to represent a list of DGEList2 objects
#' 
#' @name DGEList2-class
#' @docType class
NULL

#' Construct an EdgeObject object by a count matrix and DesignContrast
#' 
#' @name EdgeObject,matrix,DesignContrast-method
#' @docType methods
#' @param object A matrix containing counts of features
#' @param designContrast A \code{DesignContrast} object
#' @param fData A \code{data.frame} containing annotation information for each
#' gene
#' @param pData A \code{data.frame} containing annotation information for each
#' sample
#' @param remove.zeros Logical, whether to remove rows that have 0 total count
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
#' 
NULL

#' Get human gene symbols for gene-set enrichment analysis
#' 
#' 
NULL




#' Infer surrogate variable from DGEList object and design matrix
#' 
#' @name inferSV,DGEList,formula-method
#' @docType methods
#' @param object A DGEList object, \code{voom} will be used to transform the
#' data
#' @param design Formula
#' @return Surrogate variable matrix
NULL

#' Infer surrogate variable from DGEList object and design matrix
#' 
#' 
#' @name inferSV,DGEList,matrix-method
#' @docType methods
#' @param object A DGEList object, \code{voom} will be used to transform the
#' data
#' @param design Design matrix
#' @return Surrogate variable matrix
NULL

#' Infer surrogate variable from expression matrix and design matrix
#' 
#' 
#' @name inferSV,matrix,matrix-method
#' @docType methods
#' @param object An expression matrix
#' @param design Design matrix
#' @return Surrogate variable matrix
NULL

#' Split DGEList by a factor into a DGEList2 object
#' 
#' 
#' @name split,DGEList,factor-method
#' @docType methods
#' @param x A \code{DGEList} object
#' @param f A factor
#' @param drop Logical, whether unused levels in the factor should be dropped
#' @param keep.lib.sizes Logical, whether library sizes are kept
#' @param sampleDropLevels logical, whether unused levels of factors in the
#' sample annotation data frame should be dropped
NULL

#' Update design matrix by results of SVA
#' 
#' 
#' @name updateDesignMatrixBySVA,DGEList,formula-method
#' @docType methods
#' @param object A DGEList object
#' @param design Formula to infer design
#' @return Updated design matrix including surrogate variables
NULL

#' Detect surrogate variables from DGEList
#' 
#' 
#' @name voomSVA,DGEList,formula-method
#' @docType methods
#' @param object A DGEList object
#' @param design Formula to infer design matrix
#' @return A new DGEList object, including new items in the list: \code{voom},
#' \code{sv}, \code{designMatrix}, \code{designMatrixWithSV}, and
#' \code{voomSVRemoved}.
NULL

#' Detect surrogate variables from DGEList
#' 
#' 
#' @name voomSVA,DGEList,matrix-method
#' @docType methods
#' @param object A DGEList object
#' @param design Design matrix
#' @return A new DGEList object, including new items in the list: \code{voom},
#' \code{sv}, \code{designMatrix}, \code{designMatrixWithSV}, and
#' \code{voomSVRemoved}.
NULL


#' Run SVA on a count matrix transformed by voom
#' 
#' 
#' @name voomSVA,matrix,matrix-method
#' @docType methods
#' @param object A count matrix
#' @param design Design matrix
#' @return SV matrix
#' @examples
#' 
#' set.seed(1887)
#' exCounts <- matrix(rpois(12000, 10), nrow=2000, ncol=6)
#' exCounts[1:100, 2:3] <- exCounts[1:100,2:3]+20
#' exDesign <- model.matrix(~gl(2,3))
#' voomSVA(exCounts, design=exDesign)
#' 
NULL



