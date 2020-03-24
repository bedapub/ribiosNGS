mergeDGEList <- function(leftDgeList, rightDgeList,
                         DGEListLabels=NULL) {
  stopifnot(is.null(DGEListLabels) || length(DGEListLabels)==2)
  commonFeat <- intersect(rownames(leftDgeList$counts),
                          rownames(rightDgeList$counts))
  commonSampleCols <- intersect(colnames(leftDgeList$samples),
                                colnames(rightDgeList$samples))
  
  mergedCounts <- cbind(leftDgeList$counts[commonFeat,],
                        rightDgeList$counts[commonFeat,])
  mergedGenes <- leftDgeList$genes[commonFeat,]
  mergedSamples <- rbind(leftDgeList$samples[, commonSampleCols],
                         rightDgeList$samples[, commonSampleCols])
  if(!is.null(DGEListLabels) & length(DGEListLabels)==2) {
    labels <- factor(rep(DGEListLabels,
                     c(nrow(leftDgeList$samples),
                       nrow(rightDgeList$samples))),
                     levels=DGEListLabels)
    mergedSamples <- mergedSamples %>% dplyr::mutate(DGEListLabel=labels)
  }
  
  uniqSampleNames <- make.unique(colnames(mergedCounts))
  colnames(mergedCounts) <- rownames(mergedSamples) <- uniqSampleNames
  
  res <- DGEList(counts=mergedCounts,
                 samples=mergedSamples,
                 genes=mergedGenes)
  res$samples <- res$samples %>% dplyr::select(-lib.size.1, -norm.factors.1)
  return(res)
}
