#' Merge two DGEList objects into one
#' 
#' @param firstDgeList First \code{DGEList} object
#' @param secondDgeList Second \code{DGELIst} object
#' @param DGEListLabels Labels, either \code{NULL} or a vector of character
#'   strings with length two
#'   
#' The function merges two \code{DGEList} objects. It does essentially 
#' three things: 
#' \itemize{
#'   \item{Feature annotation}{It extracts the common
#'   features from both objects, and use the feature annotation in the 
#'   \code{firstDgeList} object as the annotation for the final object. }
#'   \item{Sample annotation}{It extracts the common
#'   columns from sample annotation of both objects, and row-bind them as 
#'   the annotaiton for the final object.}
#'   \item{counts}{Matching final features and samples, the counts matrices
#'   are column-binded.}
#' }
#' 
#' In case \code{DGEListLabels} is available, its values will be turned into
#' a factor vector, and appended as the column \code{DGEListLabel} in the 
#' \code{samples} object of the returned value.
#' 
#' @examples 
#' y1 <- matrix(rnbinom(1000, mu=5, size=2), ncol=4)
#' genes1 <- data.frame(GeneSymbol=sprintf("Gene%d", 1:nrow(y1)))
#' rownames(y1) <- rownames(genes1) <- 1:nrow(y1)
#' anno1 <- data.frame(treatment=gl(2,2, labels=c("ctrl", "tmt")),
#'     donor=factor(rep(c(1,2), each=2)))
#' d1 <- DGEList(counts=y1, genes=genes1, samples=anno1)
#' 
#' y2 <- matrix(rnbinom(1000, mu=5, size=2), ncol=4)
#' genes2 <- data.frame(GeneSymbol=sprintf("Gene%d", 1:nrow(y2)+100))
#' rownames(y2) <- rownames(genes1) <- 1:nrow(y2)+100
#' anno2 <- data.frame(treatment=gl(2,2, labels=c("ctrl", "tmt")),
#'     sex=factor(rep(c("m", "f"), each=2)))
#' d2 <- DGEList(counts=y2, genes=genes2, samples=anno2)
#' 
#' md <- mergeDGEList(d1, d2)
#' md2 <- mergeDGEList(d1, d2, DGEListLabels=c("d1", "d2"))
#' 
#' @importFrom edgeR DGEList
#' @importFrom ribiosUtils removeColumns
#' @export
mergeDGEList <- function(firstDgeList, secondDgeList,
                         DGEListLabels=NULL) {
  stopifnot(is.null(DGEListLabels) || length(DGEListLabels)==2)
  commonFeat <- intersect(rownames(firstDgeList$counts),
                          rownames(secondDgeList$counts))
  commonSampleCols <- intersect(colnames(firstDgeList$samples),
                                colnames(secondDgeList$samples))
  
  mergedCounts <- cbind(firstDgeList$counts[commonFeat,],
                        secondDgeList$counts[commonFeat,])
  mergedGenes <- firstDgeList$genes[commonFeat,]
  mergedSamples <- rbind(firstDgeList$samples[, commonSampleCols],
                         secondDgeList$samples[, commonSampleCols])
  if(!is.null(DGEListLabels) & length(DGEListLabels)==2) {
    labels <- factor(rep(DGEListLabels,
                     c(nrow(firstDgeList$samples),
                       nrow(secondDgeList$samples))),
                     levels=DGEListLabels)
    mergedSamples$DGEListLabel <- labels
  }
  
  uniqSampleNames <- make.unique(colnames(mergedCounts))
  colnames(mergedCounts) <- rownames(mergedSamples) <- uniqSampleNames
  
  res <- DGEList(counts=mergedCounts,
                 samples=mergedSamples,
                 genes=mergedGenes)
  res$samples <- ribiosUtils::removeColumns(res$samples,
                                            c("lib.size.1", "norm.factors.1"))
  return(res)
}
