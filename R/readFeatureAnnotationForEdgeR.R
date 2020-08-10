#' Read feature annotation for EdgeR pipeline
#' @param featureNames Character string, feature names
#' @param file A tab-delimited file with header that provides feature annotation
#' @return A \code{data.frame}, with the first column named \code{FeatureName},
#'   which are the input feature names. The rest columns contain annotations.
#'
#' The functions tries to parse feature annotation file if it is present. If
#' not, it will use \code{\link[ribiosAnnotation]{guessAndAnnotate}} to 
#' annotate the features.
#' 
#' @examples
#' anno <- "GeneID\tGeneSymbol\n1234\tCCR5\n1235\tCCR6"
#' annoFile <- tempfile()
#' writeLines(anno, annoFile)
#' 
#' featIds <- c("1235", "1234")
#' ## use file
#' readFeatureAnnotationForEdgeR(featIds, file=annoFile)
#' ## use ribiosAnnotation
#' readFeatureAnnotationForEdgeR(featIds, file=NULL)
#' 
#' @importFrom ribiosExpression readFeatureAnnotationFile
#' @importFrom ribiosUtils haltifnot matchColumn
#' @importFrom ribiosAnnotation guessAndAnnotate
#' @export
readFeatureAnnotationForEdgeR <- function(featureNames,
                                          file=NULL) {
  ribiosUtils::haltifnot(!missing(featureNames),
            msg="`featureNames` must be given.")
  if (!is.null(file) && file.exists(file)) {
    featAnno <- ribiosExpression::readFeatureAnnotationFile(file)
    res <- ribiosUtils::matchColumn(featureNames, featAnno, "FeatureName")
  } else {
    if(!"package:ribiosAnnotation" %in% search()) {
      attachNamespace("ribiosAnnotation")
    }
    anno <- ribiosAnnotation::guessAndAnnotate(featureNames)
    res <- cbind(FeatureName=featureNames, anno)
  }
  return(res)
}
