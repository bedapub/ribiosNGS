## ribios pacakge filterFeatureByAnno function has been temporarily removed
## since it requires the AnnotationDbi package, which is bulky to load

keepMaxStatProbe <- function(eset, probe.index.name, keepNAprobes=TRUE,
                             stat=function(x) mean(x,na.rm=TRUE),...) {
  stopifnot(!missing(probe.index.name) && probe.index.name %in% colnames(fData(eset)))
  if(is.character(stat)) {
    stat <- get(stat, env=parent.frame())
  }

  if (!is.function(stat)) {
    stop("'stat' must be either a function for some certain statistic, e.g. sd, or the name of such a function\n")
  }
  
  probe.index <- as.character(fData(eset)[,probe.index.name])
  probe.has.index <- !is.na(probe.index) & probe.index != "" 
  eset.indexed <- eset[probe.has.index,]
  eset.indexed.featureStat <- apply(exprs(eset.indexed),1,stat, ...)
  if(any(is.na(eset.indexed.featureStat))) {
    warning("The statistic of some probesets is NA and they will be discarded. Are you sure?\n")
  }
  
  probe.indexed.fac <- factor(fData(eset.indexed)[,probe.index.name])
  probe.by.index <- split(1:dim(eset.indexed)[1], probe.indexed.fac)
  stat.by.index <- split(eset.indexed.featureStat, probe.indexed.fac)
  max.probes <- sapply(1:nlevels(probe.indexed.fac),
                       function(x) probe.by.index[[x]][ which.max(stat.by.index[[x]]) ])
  
  eset.remain <- rep(FALSE, dim(eset)[1])
  if(keepNAprobes)
    eset.remain[!probe.has.index] <- TRUE  
  eset.remain[probe.has.index][max.probes] <- TRUE

  return(eset[eset.remain,])
}
