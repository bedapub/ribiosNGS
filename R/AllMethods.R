setMethod("dgeList", "EdgeObject", function(object) return(object@dgeList))
setMethod("dgeList", "EdgeResult", function(object) return(object@dgeList))

setMethod("designMatrix", "EdgeObject", function(object) designMatrix(object@designContrast))
setMethod("designMatrix", "EdgeResult", function(object) designMatrix(object@designContrast))

setMethod("contrastMatrix", "EdgeObject", function(object) contrastMatrix(object@designContrast))
setMethod("contrastMatrix", "EdgeResult", function(object) contrastMatrix(object@designContrast))

setMethod("contrastNames", "EdgeObject", function(object) colnames(contrastMatrix(object)))
setMethod("contrastNames", "EdgeResult", function(object) colnames(contrastMatrix(object)))


naOrSqrt <- function(x) {
  if(is.null(x)) { return (NA)}
  return(sqrt(x))
}
setMethod("commonBCV", "DGEList", function(x) {
  naOrSqrt(x$common.dispersion)
})
setMethod("tagwiseBCV", "DGEList", function(x) {
  naOrSqrt(x$tagwise.dispersion)
})
setMethod("trendedBCV", "DGEList", function(x) {
  naOrSqrt(x$trended.dispersion)
})
setMethod("commonBCV", "EdgeResult", function(x)  {
  commonBCV(dgeList(x))
})
setMethod("tagwiseBCV", "EdgeResult", function(x)  {
  tagwiseBCV(dgeList(x))
})
setMethod("trendedBCV", "EdgeResult", function(x)  {
  trendedBCV(dgeList(x))
})
setMethod("BCV", "DGEList", function(x) {
  A <- x$AveLogCPM
  if(is.null(getDispersion(x))) stop("No dispersion available")
  res <- data.frame(aveLogCPM=A,
                    commonBCV = commonBCV(x),
                    tagwiseBCV=tagwiseBCV(x),
                    trendedBCV=trendedBCV(x))
  rownames(res) <- rownames(x$counts)
  return(res)
})

setMethod("BCV", "EdgeResult", function(x) {
  BCV(dgeList(x))
})

setMethod("aveLogCPM", "EdgeResult", function(y, ...) {
  return(aveLogCPM(dgeList(y)))
})

## plotBCV
setMethod("plotBCV", "DGEList", function(x, ...) {
  edgeR::plotBCV(x, ...)
})
setMethod("plotBCV", "EdgeObject", function(x, ...) {
  edgeR::plotBCV(dgeList(x), ...,)
})
setMethod("plotBCV", "EdgeResult", function(x, ...) {
  edgeR::plotBCV(dgeList(x), ...,)
})

## groups
setMethod("groups", "EdgeObject", function(object) {
  return(groups(object@designContrast))
})
setMethod("dispGroups", "EdgeObject", function(object) {
  return(dispGroups(object@designContrast))
})

## volcanoPlot
setMethod("volcanoPlot", "EdgeResult",
          function(object, contrast=NULL,
                   freeRelation=FALSE,
                   colramp=ribiosPlot::heat,
                   ...) {
  tables <- dgeTableList(object, contrast)
  logFCs <- unlist(sapply(tables, function(x) x$logFC))
  ps <- unlist(sapply(tables, function(x) x$PValue))
  if(!freeRelation) {
    logFC.range <- quantile(logFCs, c(0.01, 0.99), na.rm=TRUE)
    pValue.range <- quantile(ps, c(0.01, 0.99), na.rm=TRUE)
    xlim <- logFC.range
    ylim <- c(0, max(-log10(pValue.range)))
  }

  op <- ribiosPlot::compactPar()
  on.exit(par(op))
  par(mfrow=grDevices::n2mfrow(length(tables)))
  for(i in seq(along=tables)) {
    if(freeRelation) {
      with(tables[[i]], smoothScatter(-log10(PValue)~logFC,
                                      colramp=colramp,
                                      main=names(tables[i]),...))
    }  else {
      with(tables[[i]], smoothScatter(-log10(PValue)~logFC,
                                      colramp=colramp,
                                      main=names(tables[i]),
                                      xlim=xlim, ylim=ylim, ...))
    }
    abline(h=0, col="lightgray")
    abline(v=0, col="lightgray")
  }
})
