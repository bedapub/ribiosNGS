ISA2globalHeatmap <- function(modules,
                              module,
                              eset,
                              norm=c("raw", "feature", "sample"),
                              scale=c("none", "row","column"),...) {
  norm <- match.arg(norm)
  scale <- match.arg(scale)
  eset <- eisa:::select.eset(eset, modules, norm)
  x <- getFeatureNames(modules, module)[[1]]
  dataM <- eset[x,,drop=FALSE]
  if (is(eset, "ExpressionSet")) {
    dataM <- exprs(dataM)
  }
  heatmap(dataM, scale = scale, ...)
}
