setMethod("dgeList", "EdgeObject", function(object) return(object@dgeList))
setMethod("dgeList", "EdgeResult", function(object) return(object@dgeList))

setMethod("designMatrix", "EdgeObject", function(object) designMatrix(object@designContrast))
setMethod("designMatrix", "EdgeResult", function(object) designMatrix(object@designContrast))

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
