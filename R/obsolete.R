remainHighestVarProbe <- function(eset, probe.index.name) {
  .Deprecated(new="keepMaxVarProbe", package="ribiosExpression")
  keepMaxStatProbe(eset, probe.index.name, stat=sd, na.rm=TRUE)
}
keepHighestVarProbe <- function(eset, probe.index.name) {
  .Deprecated(new="keepMaxVarProbe", package="ribiosExpression")
  keepMaxStatProbe(eset, probe.index.name, stat=sd, na.rm=TRUE)
}

## rank (better: sort) data frame by the order of one column
rankByCol <- function(data.frame, column, decreasing=TRUE) {
  .Deprecated(new="sortByCol", package="ribiosExpression")
  sortByCol(data.frame, column, decreasing=decreasing)
}

primaryComponentsHubert <- function(mat, k=0L, kmax=10L, scale=1, choices=1L:3L, ...) {
  .Deprecated(new="pcaHubertBiData", package="ribiosExpression")
  pcaHubertBiData(mat=mat, k=k, kmax=kmax, biplot.scale=scale, choices=choices,...)
}

eset2gct <- function(eset, file=stdout(), feature.name.col) {
  .Deprecated("writeGct", "ribiosExpression")
  writeGct(eset, file, feature.name.col)
}
eset2cls <- function(eset, file=stdout(), sample.group.col) {
  .Deprecated("writeCls", "ribiosExpression")
  writeCls(eset, file, sample.group.col)
}


##annotateIlluminaHumanv3 <- function(ids) {
##  require(illuminaHumanv3BeadID.db)
##  ids <- as.character(ids)
##  chr <- mget(ids, illuminaHumanv3BeadIDCHR, ifnotfound=NA)
##  chrloc <- mget(ids, illuminaHumanv3BeadIDCHRLOC, ifnotfound=NA)
##  genename <- mget(ids, illuminaHumanv3BeadIDGENENAME, ifnotfound=NA)
##  geneid <- mget(ids, illuminaHumanv3BeadIDENTREZID, ifnotfound=NA)
##  symbol <- mget(ids, illuminaHumanv3BeadIDSYMBOL, ifnotfound=NA)
##  anno <- data.frame(cbind(illID=ids, Chr=as.character(chr), ChrLoc=as.character(chrloc),
##                           EntrezGeneID = as.character(geneid), Symbol=as.character(symbol),  Name = as.character(genename)))
##  rownames(anno) <- ids
##  return(anno)
##}
##
##annotateHumanMethCancerPanelv1 <- function(ids) {
##  require(GGHumanMethCancerPanelv1.db)
##  require(org.Hs.eg.db)
##  
##  ids <- as.character(ids)
##  chr <- mget(ids, GGHumanMethCancerPanelv1CHR, ifnotfound=NA)
##  chrloc <- mget(ids, GGHumanMethCancerPanelv1CHRLOC, ifnotfound=NA)
##  genename <- mget(ids, GGHumanMethCancerPanelv1GENENAME, ifnotfound=NA)
##  geneid <- mget(ids, GGHumanMethCancerPanelv1ENTREZID, ifnotfound=NA)
##  symbol <- mget(ids, GGHumanMethCancerPanelv1SYMBOL, ifnotfound=NA)
##  chrlocend <- mget(ids, GGHumanMethCancerPanelv1CHRLOCEND, ifnotfound=NA)
##  anno <- data.frame(targetID=ids,
##                     Chr=I(as.character(chr)),
##                     ChrLoc=I(as.character(chrloc)),
##                     ChrLocEnd=I(as.character(chrlocend)),
##                     EntrezGeneID = I(as.character(geneid)),
##                     GeneSymbol=I(as.character(symbol)),
##                     Description = I(as.character(genename)),
##                     row.names=ids)
##
##  ## find those not-matching genes, and write information like position and direction
##  strs <- strsplit(ids, "_")
##  isNoEntrezID <- as.character(anno$EntrezGeneID)=="NA"
##  guessSymbol<- sapply(strs, function(x) x[[1]])
##  guessEntrezID <- as.character(guessHumanGeneID(guessSymbol[isNoEntrezID]))
##
##  chr <- as.character(mget(guessEntrezID, org.Hs.egCHR, ifnotfound=NA))
##  chrloc <- as.character(mget(guessEntrezID, org.Hs.egCHRLOC, ifnotfound=NA))
##  chrlocEnd <- as.character(mget(guessEntrezID, org.Hs.egCHRLOCEND, ifnotfound=NA))
##  genename <- as.character(mget(guessEntrezID, org.Hs.egGENENAME, ifnotfound=NA))
##  symbol <- as.character(mget(guessEntrezID, org.Hs.egSYMBOL, ifnotfound=NA))
##  genename <- as.character(mget(guessEntrezID, org.Hs.egSYMBOL, ifnotfound=NA))
##
##  anno$EntrezGeneID[isNoEntrezID] <- guessEntrezID
##  anno$Chr[isNoEntrezID] <- chr
##  anno$ChrLoc[isNoEntrezID] <- chrloc
##  anno$ChrLocEnd[isNoEntrezID] <- chrlocEnd
##  anno$GeneSymbol[isNoEntrezID] <- symbol
##  anno$Description[isNoEntrezID] <- genename
##
##  anno$pos <- sapply(strs,  function(x) paste(x[2:(length(x)-1)], collapse="_"))
##  anno$direction <- sapply(strs,  function(x) x[length(x)])
##  return(anno)
##}
##
##setMethod("exprsToLong", "ExpressionSetIllumina", function(x,...) {
##  exprsToLong(exprs(x),...)
##})
##
##setMethod("[", "ExpressionSetIllumina", function(x, i, j, ..., drop=FALSE) {
##  x <- callNextMethod()
##  if (!missing(j)) {
##    for (k in 1:length(x@QC))
##      if(all(dim(x@QC[[k]]) != c(0,0)))
##        x@QC[[k]] <- x@QC[[k]][, j,drop = drop]
##  }
##  
##  x
##})
