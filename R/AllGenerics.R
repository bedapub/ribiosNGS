setGeneric("pcaHubertBiData", function(x,...) standardGeneric("pcaHubertBiData"))
setGeneric("bidata", function(x) standardGeneric("bidata"))
setGeneric("expvar", function(x) standardGeneric("expvar"))

pcaHubertBi <- function(x, k=0L, kmax=10L, choices=1L:3L, biplot.scale=1L,...) {
  xx <- PcaHubert(x, k=k, kmax=kmax,...)
  if(xx@k<max(choices)){
    choices <- 1L:xx@k
  }
  lam <- sqrt(getEigenvalues(xx)[choices])
  scores <- getScores(xx)
  n <- NROW(scores)
  lam <- lam * sqrt(n)
  if (biplot.scale < 0 || biplot.scale > 1) 
    warning("'scale' is outside [0, 1]")
  if (biplot.scale != 0) 
    lam <- lam^biplot.scale
  else lam <- 1
  bi.data <- t(t(scores[, choices,drop=FALSE])/lam)
  res <- as(xx,"PcaHubertExt")
  res@biplot.data <- bi.data
  return(res)
}
## PcaHubert+biplot
setMethod("pcaHubertBiData", "matrix",
          function(x, k=0L, kmax=10L, choices=1L:3L, biplot.scale=1L,...) {
            pcaHubertBi(x, k=k, kmax=kmax, choices=choices, biplot.scale=biplot.scale,...)
})
setMethod("pcaHubertBiData", "data.frame",
          function(x, k=0L, kmax=10L, choices=1L:3L, biplot.scale=1L,...) {
            pcaHubertBi(x, k=k, kmax=kmax, choices=choices, biplot.scale=biplot.scale,...)
})
setMethod("pcaHubertBiData", "ExpressionSet",
          function(x, k=0L, kmax=10L, choices=1L:3L, biplot.scale=1L,...) {
            pcaHubertBi(t(exprs(x)),
                        k=k, kmax=kmax, choices=choices, biplot.scale=biplot.scale,...)
          })
setMethod("bidata", "PcaHubertExt", function(x) x@biplot.data)
setMethod("expvar", "PcaHubert", function(x) getEigenvalues(x)/sum(getEigenvalues(x)))
