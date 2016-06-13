summarizeSamples <- function(eset, indSamples=eset$SAMPLEID, removeInvarCols=TRUE, fun=sum, ...) {
  if(!is.factor(indSamples)) indSamples <- as.factor(indSamples)
  stopifnot(length(indSamples)==ncol(eset))
  if(is.factor(indSamples))  indSamples <- droplevels(indSamples)
  eset.pool <- ribiosUtils::summarizeColumns(exprs(eset), indSamples, fun=fun, ...)
  eset.pd <- do.call(rbind, tapply(1:ncol(eset), indSamples, function(x) {
    pData(eset)[x[1],]
  }))
  if(removeInvarCols) eset.pd <- removeInvarCol(eset.pd)
  rownames(eset.pd) <- colnames(eset.pool)
  eset.poolEset <- new("ExpressionSet",
                       exprs=eset.pool,
                       phenoData=new("AnnotatedDataFrame", eset.pd),
                       featureData=featureData(eset))
  return(eset.poolEset)
}


poolReplicates <- function(eset, indSamples=eset$SAMPLEID, removeInvarCols=TRUE) {
  summarizeSamples(eset, indSamples=indSamples, removeInvarCols=TRUE, fun=sum, na.rm=TRUE)
}

avgReplicates <- function(eset, indSamples=eset$SAMPLEID, removeInvarCols=TRUE) {
  summarizeSamples(eset, indSamples=indSamples, removeInvarCols=TRUE, fun=mean, na.rm=TRUE)
}
