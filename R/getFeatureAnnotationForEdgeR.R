getFeatureAnnotationForEdgeR <- function(file = NULL,
                                         featureNames = NULL,
                                         noAnno = FALSE) {
  if (!noAnno & is.null(file)) {
    fd <- annotate(obj)
  } else if (!is.null(file)) {
    doLog("Annotate features with annotation file")
    featAnno <-
      ribiosExpression::readfile(file)
    ordFeatAnno <- matchColumn(rownames(counts(obj)), featAnno, 1L)
    objAnno <- obj
    fData(objAnno) <- ordFeatAnno
  } else {
    objAnno <- obj
  }
}
