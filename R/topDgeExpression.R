utils::globalVariables(c("FeatureID", "FeatureName", "Contrast"))

#' Return raw expression of top differentially expressed genes of one contrast
#' @param edgeResult An \code{EdgeResult} object.
#' @param ntop Integer, number of top differentially expressed genes.
#' @param contrast A character string or an integer value, specifying which
#'    contrast is considered.
#' @param exprsFun A function to derive expression values from \code{EdgeResult}.
#' @return A \code{tibble} object with \code{Contrast} in the first column and
#'    a wide table of raw expression, feature annotation, and sample annotations
#'    in the rest columns
#' @importFrom dplyr mutate select rename filter
#' @importFrom magrittr %>%
#' @export
topDgeExpressionByContrast <-  function(edgeResult, ntop = 10, 
                                        contrast,
                                        exprsFun = function(dgeList) cpm(dgeList, log=TRUE)) {
  stopifnot(length(contrast)==1)
  if (is.numeric(contrast))
    contrast <- contrastNames(edgeResult)[contrast]
  topDgeGenes <- head(dgeTable(edgeResult, contrast = contrast), n = ntop)
  longdf <-DGEListToLongTable(dgeList(edgeResult), exprsFun=exprsFun)
  if ("FeatureName" %in% colnames(topDgeGenes)) {
    topDgeFeatureID <- as.character(topDgeGenes$FeatureName)
    sigdf <- longdf %>% dplyr::filter(FeatureName %in% topDgeFeatureID)
  } else if("FeatureID" %in% colnames(topDgeGenes)) {
    ## temporary: in the future the column is called FeatureName
    topDgeFeatureID <- as.character(topDgeGenes$FeatureID)
    sigdf <- longdf %>% dplyr::filter(FeatureID %in% topDgeFeatureID) %>%
      dplyr::rename(FeatureName=FeatureID)
  } else {
    stop("A column named 'FeatureName' or 'FeatureID' was not found.")
  }
  res <- sigdf %>%
    dplyr::mutate(Contrast = contrast) %>%
    dplyr::select("Contrast", dplyr::everything())
  return(res)
}

#' Return raw expression of top differentially expressed genes of multiple contrasts
#' @param edgeResult An \code{EdgeResult} object.
#' @param ntop Integer, number of top differentially expressed genes.
#' @param contrast \code{NULL}, which means all contrasts are considered, or
#'   a vector of character strings or of integer values, which specify
#'   which contrasts are considered.
#' @param exprsFun A function to derive expression values from \code{EdgeResult}.
#' @return A \code{tibble} object with \code{Contrast} in the first column and
#'    a wide table of raw expression, feature annotation, and sample annotations
#'    in the rest columns
#' @importFrom dplyr mutate select rename filter
#' @importFrom magrittr %>%
#' @seealso \code{\link{topDgeExpressionByContrast}}
#' @export
topDgeExpression <- function(edgeResult, ntop=10, 
                             contrast=NULL,
                             exprsFun = function(dgeList) cpm(dgeList, log=TRUE)) {
  if(is.null(contrast))
    contrast <- contrastNames(edgeResult)
  dfs <- lapply(contrast, function(cont) 
    topDgeExpressionByContrast(edgeResult, ntop=ntop, contrast=cont,
                               exprsFun=exprsFun)) 
  res <- do.call(rbind, dfs) %>%
    dplyr::mutate(Contrast=factor(Contrast, contrast))
  return(res)
}
