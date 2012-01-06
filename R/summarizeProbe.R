summarizeProbesets <- function(eset,
                               index.name,
                               fun=mean,
                               keep.nonindex=FALSE,
                               keep.featureNames=FALSE) {
  
  if(missing(index.name) || !index.name %in% colnames(fData(eset))) {
    stop("'index.name' must be a valid column name in fData(", as.character(match.call()$eset), ")")
  }
  fun <- match.fun(fun)
  
  probe.index <- as.character(fData(eset)[,index.name])
  probe.has.index <- !is.na(probe.index) & probe.index != "" 
  eset.indexed <- eset[probe.has.index,]
  
  probe.indexed.fac <- factor(fData(eset.indexed)[,index.name])
  eset.fun <- summarizeRows(exprs(eset.indexed),
                            probe.indexed.fac,
                            fun=fun)

  fun.fd.match <- match(levels(probe.indexed.fac), probe.index)
  eset.remain <- eset[fun.fd.match,]
  exprs(eset.remain) <- eset.fun

  if(keep.featureNames) {
    featureNames(eset.remain) <- rownames(fData(eset.remain))
  } else {
    rownames(fData(eset.remain)) <- featureNames(eset.remain)
  }
  
  if(keep.nonindex) {
    eset.inval <- eset[!probe.has.index,]
    fData(eset.remain) <- rbind(fData(eset.remain),
                                fData(eset.inval))
    exprs(eset.remain) <- rbind(exprs(eset.remain),
                                exprs(eset.inval))
  }
  return(eset.remain)
}
