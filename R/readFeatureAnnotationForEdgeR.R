#' Read feature annotation for EdgeR pipeline
#' @param featureNames Character string, feature names
#' @param file A tab-delimited file with header that provides feature annotation
#' @return A \code{data.frame}, with the first column named \code{FeatureName},
#'   which are the input feature names. The rest columns contain annotations.
#' @importFrom ribiosExpression readFeatureAnnotationFile
#' @importFrom ribiosUtils matchColumn
#' @importFrom ribiosAnnotation annotateAnyIDs
#' @export
readFeatureAnnotationForEdgeR <- function(featureNames,
                                          file=NULL) {
  haltifnot(!missing(featureNames),
            msg="`featureNames` must be given.")
  if (file.exists(file)) {
    featAnno <- ribiosExpression::readFeatureAnnotationFile(file)
    res <- ribiosUtils::matchColumn(featureNames, featAnno, "FeatureName")
  } else {
    res <- ribiosAnnotation::annotateAnyIDs(featureNames)
    colnames(res) <- "FeatureName"
  }
  return(res)
}
