meanProbe <- function(eset, probe.index.name) {
  stopifnot(probe.index.name %in% colnames(fData(eset)))
  
  probe.index <- as.character(fData(eset)[,probe.index.name])
  probe.has.index <- !is.na(probe.index) & probe.index != "" 
  eset.indexed <- eset[probe.has.index,]

  probe.indexed.fac <- factor(fData(eset.indexed)[,probe.index.name])
  probe.by.index <- split(1:dim(eset.indexed)[1], probe.indexed.fac)
  myMean <- function(x) {
    if(nrow(x)==1) {
      return(x)
    } else {
      return(colMeans(x))
    }
  }
  exp <- exprs(eset.indexed)
  eset.mean <- t(sapply(probe.by.index, function(x) myMean(exp[x,,drop=FALSE])))
  rm(exp)

  mean.fd.match <- match(levels(probe.indexed.fac), probe.index)
  
  eset.remain <- eset[mean.fd.match,]
  exprs(eset.remain) <- eset.mean

  return(eset.remain)
}
