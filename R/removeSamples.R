removeSamples <- function(eset, exclSamples) {
  qcLabels <- getQClabel(eset)
  sampleNames <- sampleNames(eset)

  isQcLabel <- exclSamples %in% qcLabels
  isSampleName <- exclSamples %in% sampleNames

  if(!all(isQcLabel) && !all(isSampleName)) {
    qqmsg("Following (to be excluded) sample names are not valid:",
          paste(setdiff(exclSamples, c(qcLabels, sampleNames)), collapse=","),
          status=1L)
  } else if(all(isQcLabel)) {
    idxOut <- match(exclSamples, qcLabels )
  } else if(all(isSampleName)) {
    idxOut <- match(exclSamples,  sampleNames)
  }
  eset <- eset[, -idxOut, drop=FALSE]
  return(eset)
}
