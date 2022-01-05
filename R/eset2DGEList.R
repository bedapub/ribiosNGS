#' Transform an EexpressionSet to a DGEList object
#' @param eset An \code{ExpressionSet} object
#' @param groupVar Character string, column in phenoDatta
#' @return A \code{DGEList} object
#' @importFrom Biobase pData
#' @importFrom edgeR DGEList
#' @examples
#' eset <- new("ExpressionSet",
#'   exprs=matrix(rpois(120, 20), nrow=20,
#'                dimnames=list(LETTERS[1:20], letters[1:6])),
#'   phenoData = new("AnnotatedDataFrame", data.frame(Sample=letters[1:6],
#'                   group=gl(2, 3), row.names=letters[1:6])),
#'   featureData = new("AnnotatedDataFrame", data.frame(Feature=LETTERS[1:20],
#'                   row.names=LETTERS[1:20])))
#' eset2DGEList(eset)
#' @export
eset2DGEList <-function(eset, groupVar="group") {
  stopifnot(groupVar %in% colnames(pData(eset)))
  groups <- pData(eset)[, groupVar]
  if(is.factor(groups)) {
    levels(groups) <- make.names(levels(groups))
  } else {
    groups <- make.names(as.character(groups))
  }
  res <- edgeR::DGEList(counts=exprs(eset),
                 samples=pData(eset),
                 genes=fData(eset))
  res$samples$group <- groups
  return(res)
}

